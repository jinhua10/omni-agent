package top.yumbo.ai.rag.h2;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.IndexStatistics;
import top.yumbo.ai.rag.api.model.SearchResult;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import java.sql.*;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 基于 H2 数据库的 RAG 实现
 * (H2 Database-based RAG Implementation)
 * 
 * <p>特性:
 * <ul>
 *   <li>嵌入式数据库，零配置启动</li>
 *   <li>全文搜索（H2 Full-Text Search）</li>
 *   <li>向量搜索（余弦相似度）</li>
 *   <li>混合检索（文本 + 向量）</li>
 *   <li>HikariCP连接池</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class H2RAGService implements RAGService {

    private final H2RAGProperties properties;
    private final HikariDataSource dataSource;
    private final ObjectMapper objectMapper;

    public H2RAGService(H2RAGProperties properties) {
        this.properties = properties;
        this.objectMapper = new ObjectMapper();
        this.dataSource = createDataSource();
    }

    @PostConstruct
    public void init() {
        try {
            createTables();
            createFullTextIndex();
            log.info("H2 RAG Service 初始化成功");
            log.info("数据库路径: {}", properties.getUrl());
        } catch (SQLException e) {
            log.error("初始化H2 RAG失败", e);
            throw new RuntimeException("初始化失败", e);
        }
    }

    @PreDestroy
    public void destroy() {
        if (dataSource != null && !dataSource.isClosed()) {
            dataSource.close();
            log.info("H2 RAG Service 已关闭");
        }
    }

    // ========== 文档索引 ==========

    @Override
    public String indexDocument(Document document) {
        if (document.getId() == null || document.getId().isEmpty()) {
            document.setId(UUID.randomUUID().toString());
        }

        String sql = """
            MERGE INTO rag_documents (id, title, content, summary, tags, type, source, author,
                                     embedding, metadata, created_at, updated_at)
            KEY(id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """;

        try (Connection conn = dataSource.getConnection();
             PreparedStatement stmt = conn.prepareStatement(sql)) {

            stmt.setString(1, document.getId());
            stmt.setString(2, document.getTitle());
            stmt.setString(3, document.getContent());
            stmt.setString(4, document.getSummary());
            stmt.setString(5, document.getTags() != null ? String.join(",", document.getTags()) : null);
            stmt.setString(6, document.getType());
            stmt.setString(7, document.getSource());
            stmt.setString(8, document.getAuthor());
            stmt.setString(9, serializeEmbedding(document.getEmbedding()));
            stmt.setString(10, serializeMetadata(document.getMetadata()));
            stmt.setTimestamp(11, new Timestamp(document.getCreatedAt() != null ? 
                document.getCreatedAt() : System.currentTimeMillis()));
            stmt.setTimestamp(12, new Timestamp(System.currentTimeMillis()));

            stmt.executeUpdate();

            log.debug("文档索引成功: {}", document.getId());
            return document.getId();

        } catch (SQLException e) {
            log.error("索引文档失败", e);
            throw new RuntimeException("索引文档失败", e);
        }
    }

    @Override
    public List<String> indexDocuments(List<Document> documents) {
        List<String> docIds = new ArrayList<>();
        
        for (Document document : documents) {
            try {
                String docId = indexDocument(document);
                docIds.add(docId);
            } catch (Exception e) {
                log.error("索引文档失败: {}", document.getId(), e);
            }
        }
        
        return docIds;
    }

    @Override
    public boolean updateDocument(Document document) {
        if (!documentExists(document.getId())) {
            log.warn("文档不存在: {}", document.getId());
            return false;
        }

        indexDocument(document);
        return true;
    }

    @Override
    public boolean deleteDocument(String documentId) {
        String sql = "DELETE FROM rag_documents WHERE id = ?";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement stmt = conn.prepareStatement(sql)) {

            stmt.setString(1, documentId);
            int rows = stmt.executeUpdate();

            if (rows > 0) {
                log.debug("文档删除成功: {}", documentId);
                return true;
            } else {
                log.warn("文档不存在: {}", documentId);
                return false;
            }

        } catch (SQLException e) {
            log.error("删除文档失败: {}", documentId, e);
            return false;
        }
    }

    @Override
    public void clearAll() {
        String sql = "DELETE FROM rag_documents";

        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement()) {

            stmt.executeUpdate(sql);
            log.info("所有索引已清空");

        } catch (SQLException e) {
            log.error("清空索引失败", e);
            throw new RuntimeException("清空索引失败", e);
        }
    }

    // ========== 文本搜索 ==========

    @Override
    public List<SearchResult> search(top.yumbo.ai.rag.api.model.Query query) {
        switch (query.getMode()) {
            case TEXT:
                return searchByText(query.getText(), query.getTopK());
            case VECTOR:
                return query.getEmbedding() != null ?
                        vectorSearch(query.getEmbedding(), query.getTopK()) :
                        Collections.emptyList();
            case HYBRID:
                return hybridSearch(query);
            default:
                return Collections.emptyList();
        }
    }

    @Override
    public List<SearchResult> searchByText(String text, int topK) {
        // H2 全文搜索
        String sql = """
            SELECT d.*, FT.SCORE
            FROM rag_documents d, FT_SEARCH_DATA(?, 0, 0) FT
            WHERE d.id = FT.KEYS[0]
            ORDER BY FT.SCORE DESC
            LIMIT ?
            """;

        try (Connection conn = dataSource.getConnection();
             PreparedStatement stmt = conn.prepareStatement(sql)) {

            stmt.setString(1, text);
            stmt.setInt(2, topK);

            ResultSet rs = stmt.executeQuery();
            List<SearchResult> results = new ArrayList<>();

            while (rs.next()) {
                Document doc = mapResultSetToDocument(rs);
                float score = rs.getFloat("SCORE");

                results.add(SearchResult.builder()
                    .document(doc)
                    .score(score)
                    .textScore(score)
                    .build());
            }

            return results;

        } catch (SQLException e) {
            log.error("文本搜索失败", e);
            return Collections.emptyList();
        }
    }

    // ========== 向量搜索 ==========

    @Override
    public List<SearchResult> vectorSearch(float[] embedding, int topK) {
        return vectorSearch(embedding, topK, null);
    }

    @Override
    public List<SearchResult> vectorSearch(float[] embedding, int topK,
                                          Map<String, Object> filters) {
        // 获取所有有向量的文档
        StringBuilder sql = new StringBuilder(
            "SELECT * FROM rag_documents WHERE embedding IS NOT NULL"
        );

        // 应用过滤器
        if (filters != null && !filters.isEmpty()) {
            for (String key : filters.keySet()) {
                sql.append(" AND ").append(key).append(" = ?");
            }
        }

        try (Connection conn = dataSource.getConnection();
             PreparedStatement stmt = conn.prepareStatement(sql.toString())) {

            // 设置过滤器参数
            if (filters != null && !filters.isEmpty()) {
                int paramIndex = 1;
                for (Object value : filters.values()) {
                    stmt.setObject(paramIndex++, value);
                }
            }

            ResultSet rs = stmt.executeQuery();
            List<SearchResult> results = new ArrayList<>();

            while (rs.next()) {
                Document doc = mapResultSetToDocument(rs);
                
                if (doc.getEmbedding() != null && doc.getEmbedding().length > 0) {
                    float similarity = cosineSimilarity(embedding, doc.getEmbedding());
                    
                    results.add(SearchResult.builder()
                        .document(doc)
                        .score(similarity)
                        .vectorScore(similarity)
                        .distance(1.0f - similarity)
                        .build());
                }
            }

            // 排序并返回topK
            return results.stream()
                .sorted((a, b) -> Float.compare(b.getScore(), a.getScore()))
                .limit(topK)
                .collect(Collectors.toList());

        } catch (SQLException e) {
            log.error("向量搜索失败", e);
            return Collections.emptyList();
        }
    }

    // ========== 混合检索 ==========

    @Override
    public List<SearchResult> hybridSearch(top.yumbo.ai.rag.api.model.Query query) {
        return hybridSearch(
            query.getText(),
            query.getEmbedding(),
            query.getTextWeight(),
            query.getVectorWeight(),
            query.getTopK()
        );
    }

    @Override
    public List<SearchResult> hybridSearch(String text, float[] embedding,
                                          float textWeight, float vectorWeight, int topK) {
        // 文本搜索结果
        List<SearchResult> textResults = Collections.emptyList();
        if (textWeight > 0 && text != null && !text.isEmpty()) {
            textResults = searchByText(text, topK * 2);
        }

        // 向量搜索结果
        List<SearchResult> vectorResults = Collections.emptyList();
        if (vectorWeight > 0 && embedding != null && embedding.length > 0) {
            vectorResults = vectorSearch(embedding, topK * 2);
        }

        // 合并结果
        Map<String, SearchResult> mergedResults = new HashMap<>();

        // 添加文本结果
        for (SearchResult result : textResults) {
            String docId = result.getDocument().getId();
            float weightedScore = result.getScore() * textWeight;

            mergedResults.put(docId, SearchResult.builder()
                .document(result.getDocument())
                .score(weightedScore)
                .textScore(result.getScore())
                .build());
        }

        // 合并向量结果
        for (SearchResult result : vectorResults) {
            String docId = result.getDocument().getId();
            float vectorScore = result.getScore();
            float weightedVectorScore = vectorScore * vectorWeight;

            if (mergedResults.containsKey(docId)) {
                SearchResult existing = mergedResults.get(docId);
                float combinedScore = existing.getScore() + weightedVectorScore;

                mergedResults.put(docId, SearchResult.builder()
                    .document(existing.getDocument())
                    .score(combinedScore)
                    .textScore(existing.getTextScore())
                    .vectorScore(vectorScore)
                    .distance(result.getDistance())
                    .build());
            } else {
                mergedResults.put(docId, SearchResult.builder()
                    .document(result.getDocument())
                    .score(weightedVectorScore)
                    .vectorScore(vectorScore)
                    .distance(result.getDistance())
                    .build());
            }
        }

        // 排序并返回topK
        return mergedResults.values().stream()
            .sorted((a, b) -> Float.compare(b.getScore(), a.getScore()))
            .limit(topK)
            .collect(Collectors.toList());
    }

    // ========== 语义搜索 ==========

    @Override
    public List<SearchResult> semanticSearch(String text, int topK) {
        // 需要集成Embedding服务
        log.warn("语义搜索需要Embedding服务支持");
        return searchByText(text, topK);
    }

    // ========== 文档管理 ==========

    @Override
    public Optional<Document> getDocument(String documentId) {
        String sql = "SELECT * FROM rag_documents WHERE id = ?";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement stmt = conn.prepareStatement(sql)) {

            stmt.setString(1, documentId);
            ResultSet rs = stmt.executeQuery();

            if (rs.next()) {
                return Optional.of(mapResultSetToDocument(rs));
            }

            return Optional.empty();

        } catch (SQLException e) {
            log.error("获取文档失败: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public boolean documentExists(String documentId) {
        String sql = "SELECT COUNT(*) FROM rag_documents WHERE id = ?";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement stmt = conn.prepareStatement(sql)) {

            stmt.setString(1, documentId);
            ResultSet rs = stmt.executeQuery();

            return rs.next() && rs.getInt(1) > 0;

        } catch (SQLException e) {
            log.error("检查文档存在失败: {}", documentId, e);
            return false;
        }
    }

    @Override
    public long getDocumentCount() {
        String sql = "SELECT COUNT(*) FROM rag_documents";

        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            return rs.next() ? rs.getLong(1) : 0;

        } catch (SQLException e) {
            log.error("获取文档总数失败", e);
            return 0;
        }
    }

    // ========== 统计与健康 ==========

    @Override
    public IndexStatistics getStatistics() {
        try {
            long totalDocs = getDocumentCount();

            return IndexStatistics.builder()
                .totalDocuments(totalDocs)
                .indexSize(totalDocs * 1024L) // 估算
                .indexType("H2")
                .healthy(isHealthy())
                .timestamp(System.currentTimeMillis())
                .build();

        } catch (Exception e) {
            log.error("获取统计信息失败", e);
            return IndexStatistics.builder()
                .totalDocuments(0L)
                .indexSize(0L)
                .healthy(false)
                .timestamp(System.currentTimeMillis())
                .build();
        }
    }

    @Override
    public boolean isHealthy() {
        try (Connection conn = dataSource.getConnection()) {
            return conn.isValid(1);
        } catch (SQLException e) {
            log.error("健康检查失败", e);
            return false;
        }
    }

    @Override
    public void rebuildIndex() {
        try {
            // 删除并重建全文索引
            try (Connection conn = dataSource.getConnection();
                 Statement stmt = conn.createStatement()) {
                
                stmt.execute("DROP ALIAS IF EXISTS FT_SEARCH_DATA");
                createFullTextIndex();
                
                log.info("索引重建完成");
            }

        } catch (SQLException e) {
            log.error("重建索引失败", e);
            throw new RuntimeException("重建索引失败", e);
        }
    }

    // ========== 工具方法 ==========

    private HikariDataSource createDataSource() {
        HikariConfig config = new HikariConfig();
        config.setJdbcUrl(properties.getUrl());
        config.setUsername(properties.getUsername());
        config.setPassword(properties.getPassword());
        config.setMaximumPoolSize(properties.getMaxPoolSize());
        config.setMinimumIdle(properties.getMinPoolSize());
        config.setConnectionTimeout(properties.getConnectionTimeout());
        config.setIdleTimeout(properties.getIdleTimeout());
        config.setMaxLifetime(properties.getMaxLifetime());

        return new HikariDataSource(config);
    }

    private void createTables() throws SQLException {
        String sql = """
            CREATE TABLE IF NOT EXISTS rag_documents (
                id VARCHAR(255) PRIMARY KEY,
                title VARCHAR(1000),
                content CLOB,
                summary CLOB,
                tags VARCHAR(2000),
                type VARCHAR(100),
                source VARCHAR(500),
                author VARCHAR(255),
                embedding CLOB,
                metadata CLOB,
                created_at TIMESTAMP,
                updated_at TIMESTAMP
            );
            
            CREATE INDEX IF NOT EXISTS idx_type ON rag_documents(type);
            CREATE INDEX IF NOT EXISTS idx_source ON rag_documents(source);
            CREATE INDEX IF NOT EXISTS idx_author ON rag_documents(author);
            """;

        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement()) {
            
            for (String s : sql.split(";")) {
                if (!s.trim().isEmpty()) {
                    stmt.execute(s);
                }
            }
        }
    }

    private void createFullTextIndex() throws SQLException {
        String sql = """
            CREATE ALIAS IF NOT EXISTS FT_INIT FOR "org.h2.fulltext.FullText.init";
            CALL FT_INIT();
            CALL FT_CREATE_INDEX('PUBLIC', 'RAG_DOCUMENTS', 'TITLE,CONTENT,SUMMARY');
            """;

        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement()) {
            
            for (String s : sql.split(";")) {
                if (!s.trim().isEmpty()) {
                    stmt.execute(s);
                }
            }
        }
    }

    private Document mapResultSetToDocument(ResultSet rs) throws SQLException {
        return Document.builder()
            .id(rs.getString("id"))
            .title(rs.getString("title"))
            .content(rs.getString("content"))
            .summary(rs.getString("summary"))
            .tags(parseTags(rs.getString("tags")))
            .type(rs.getString("type"))
            .source(rs.getString("source"))
            .author(rs.getString("author"))
            .embedding(deserializeEmbedding(rs.getString("embedding")))
            .metadata(deserializeMetadata(rs.getString("metadata")))
            .createdAt(rs.getTimestamp("created_at") != null ? 
                rs.getTimestamp("created_at").getTime() : null)
            .updatedAt(rs.getTimestamp("updated_at") != null ? 
                rs.getTimestamp("updated_at").getTime() : null)
            .build();
    }

    private List<String> parseTags(String tagsStr) {
        if (tagsStr == null || tagsStr.isEmpty()) {
            return Collections.emptyList();
        }
        return Arrays.asList(tagsStr.split(","));
    }

    private String serializeEmbedding(float[] embedding) {
        if (embedding == null || embedding.length == 0) {
            return null;
        }
        
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < embedding.length; i++) {
            if (i > 0) sb.append(",");
            sb.append(embedding[i]);
        }
        return sb.toString();
    }

    private float[] deserializeEmbedding(String embeddingStr) {
        if (embeddingStr == null || embeddingStr.isEmpty()) {
            return null;
        }
        
        String[] parts = embeddingStr.split(",");
        float[] embedding = new float[parts.length];
        for (int i = 0; i < parts.length; i++) {
            embedding[i] = Float.parseFloat(parts[i]);
        }
        return embedding;
    }

    private String serializeMetadata(Map<String, Object> metadata) {
        if (metadata == null || metadata.isEmpty()) {
            return null;
        }
        
        try {
            return objectMapper.writeValueAsString(metadata);
        } catch (JsonProcessingException e) {
            log.error("序列化metadata失败", e);
            return null;
        }
    }

    @SuppressWarnings("unchecked")
    private Map<String, Object> deserializeMetadata(String metadataStr) {
        if (metadataStr == null || metadataStr.isEmpty()) {
            return Collections.emptyMap();
        }
        
        try {
            return objectMapper.readValue(metadataStr, Map.class);
        } catch (JsonProcessingException e) {
            log.error("反序列化metadata失败", e);
            return Collections.emptyMap();
        }
    }

    private float cosineSimilarity(float[] vec1, float[] vec2) {
        if (vec1 == null || vec2 == null || vec1.length != vec2.length) {
            return 0.0f;
        }

        float dotProduct = 0.0f;
        float norm1 = 0.0f;
        float norm2 = 0.0f;

        for (int i = 0; i < vec1.length; i++) {
            dotProduct += vec1[i] * vec2[i];
            norm1 += vec1[i] * vec1[i];
            norm2 += vec2[i] * vec2[i];
        }

        if (norm1 == 0.0f || norm2 == 0.0f) {
            return 0.0f;
        }

        return dotProduct / (float) (Math.sqrt(norm1) * Math.sqrt(norm2));
    }
}
