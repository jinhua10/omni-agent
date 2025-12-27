package top.yumbo.ai.omni.rag.h2;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;
import top.yumbo.ai.omni.rag.model.IndexStatistics;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import java.sql.*;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 基于 H2 数据库的 RAG 实现（重构版本）
 * <p>
 * 符合新的多域架构设计
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class H2RAGService implements RagService {

    private final H2RAGProperties properties;
    private final HikariDataSource dataSource;
    private final ObjectMapper objectMapper;
    private final String domainId;

    public H2RAGService(H2RAGProperties properties) {
        this(properties, "h2-domain");
    }

    public H2RAGService(H2RAGProperties properties, String domainId) {
        this.properties = properties;
        this.domainId = domainId;
        this.objectMapper = new ObjectMapper();
        this.dataSource = createDataSource();
    }

    private HikariDataSource createDataSource() {
        HikariConfig config = new HikariConfig();
        config.setJdbcUrl(properties.getUrl());
        config.setUsername(properties.getUsername());
        config.setPassword(properties.getPassword());
        config.setMaximumPoolSize(properties.getMaxPoolSize());
        config.setMinimumIdle(properties.getMinPoolSize());
        config.setConnectionTimeout(properties.getConnectionTimeout());
        config.setIdleTimeout(properties.getIdleTimeout());
        config.setMaxLifetime(1800000);
        return new HikariDataSource(config);
    }

    @PostConstruct
    public void init() {
        try {
            createTables();
            log.info("H2 RAG Service 初始化成功: domain={}", domainId);
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

    // ========== RagService 接口实现 ==========

    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        log.debug("语义搜索: query={}, maxResults={}", query, maxResults);
        return searchByText(query, maxResults);
    }

    @Override
    public List<Document> vectorSearch(Vector vector, int maxResults) {
        if (vector == null || vector.getData() == null) {
            return Collections.emptyList();
        }
        return vectorSearchByFloatArray(vector.getData(), maxResults);
    }

    @Override
    public Vector embed(String text) {
        log.warn("H2RAGService 不提供嵌入功能，请使用外部 AI 服务");
        return null;
    }

    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        log.warn("H2RAGService 不提供批量嵌入功能");
        return Collections.emptyList();
    }

    @Override
    public void index(String id, Vector vector, Map<String, Object> metadata) {
        Document.DocumentBuilder builder = Document.builder().id(id);

        if (metadata != null) {
            if (metadata.containsKey("title")) builder.title((String) metadata.get("title"));
            if (metadata.containsKey("content")) builder.content((String) metadata.get("content"));
            if (metadata.containsKey("source")) builder.source((String) metadata.get("source"));
            if (metadata.containsKey("summary")) builder.summary((String) metadata.get("summary"));
        }

        if (vector != null && vector.getData() != null) {
            builder.embedding(vector.getData());
        }

        indexDocument(builder.build());
    }

    @Override
    public void batchIndex(List<Document> documents) {
        for (Document doc : documents) {
            indexDocument(doc);
        }
    }

    @Override
    public void delete(String id) {
        deleteDocument(id);
    }

    @Override
    public String getDomainId() {
        return domainId;
    }

    // ========== 内部实现方法 ==========

    private void createTables() throws SQLException {
        String createTableSQL = """
            CREATE TABLE IF NOT EXISTS rag_documents (
                id VARCHAR(255) PRIMARY KEY,
                title VARCHAR(1000),
                content CLOB,
                summary VARCHAR(5000),
                tags VARCHAR(1000),
                type VARCHAR(100),
                source VARCHAR(1000),
                author VARCHAR(255),
                embedding CLOB,
                metadata CLOB,
                score DOUBLE,
                created_at TIMESTAMP,
                updated_at TIMESTAMP
            )
            """;

        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement()) {
            stmt.execute(createTableSQL);
            stmt.execute("CREATE INDEX IF NOT EXISTS idx_type ON rag_documents(type)");
            stmt.execute("CREATE INDEX IF NOT EXISTS idx_source ON rag_documents(source)");
        }
    }

    private void indexDocument(Document document) {
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

        } catch (SQLException e) {
            log.error("索引文档失败", e);
            throw new RuntimeException("索引文档失败", e);
        }
    }

    private void deleteDocument(String documentId) {
        String sql = "DELETE FROM rag_documents WHERE id = ?";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement stmt = conn.prepareStatement(sql)) {

            stmt.setString(1, documentId);
            stmt.executeUpdate();
            log.debug("文档删除成功: {}", documentId);

        } catch (SQLException e) {
            log.error("删除文档失败: {}", documentId, e);
            throw new RuntimeException("删除文档失败", e);
        }
    }

    private List<Document> searchByText(String text, int topK) {
        // 简单的 LIKE 搜索（H2 全文搜索需要额外配置）
        String sql = """
            SELECT * FROM rag_documents
            WHERE content LIKE ? OR title LIKE ?
            ORDER BY created_at DESC
            LIMIT ?
            """;

        try (Connection conn = dataSource.getConnection();
             PreparedStatement stmt = conn.prepareStatement(sql)) {

            String pattern = "%" + text + "%";
            stmt.setString(1, pattern);
            stmt.setString(2, pattern);
            stmt.setInt(3, topK);

            ResultSet rs = stmt.executeQuery();
            List<Document> results = new ArrayList<>();

            while (rs.next()) {
                results.add(mapResultSetToDocument(rs));
            }

            return results;

        } catch (SQLException e) {
            log.error("文本搜索失败", e);
            return Collections.emptyList();
        }
    }

    private List<Document> vectorSearchByFloatArray(float[] embedding, int topK) {
        String sql = "SELECT * FROM rag_documents WHERE embedding IS NOT NULL";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement stmt = conn.prepareStatement(sql)) {

            ResultSet rs = stmt.executeQuery();
            List<Document> results = new ArrayList<>();

            while (rs.next()) {
                Document doc = mapResultSetToDocument(rs);

                if (doc.getEmbedding() != null && doc.getEmbedding().length > 0) {
                    float similarity = cosineSimilarity(embedding, doc.getEmbedding());
                    doc.setScore((double) similarity);
                    results.add(doc);
                }
            }

            return results.stream()
                    .sorted((a, b) -> Double.compare(
                            b.getScore() != null ? b.getScore() : 0.0,
                            a.getScore() != null ? a.getScore() : 0.0))
                    .limit(topK)
                    .collect(Collectors.toList());

        } catch (SQLException e) {
            log.error("向量搜索失败", e);
            return Collections.emptyList();
        }
    }

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

    @Override
    public List<Document> getAllDocuments(int offset, int limit) {
        String sql = """
            SELECT * FROM rag_documents
            ORDER BY created_at DESC
            LIMIT ? OFFSET ?
            """;

        List<Document> documents = new ArrayList<>();

        try (Connection conn = dataSource.getConnection();
             PreparedStatement pstmt = conn.prepareStatement(sql)) {

            pstmt.setInt(1, limit);
            pstmt.setInt(2, offset);

            try (ResultSet rs = pstmt.executeQuery()) {
                while (rs.next()) {
                    documents.add(mapResultSetToDocument(rs));
                }
            }

            return documents;

        } catch (SQLException e) {
            log.error("获取所有文档失败", e);
            return Collections.emptyList();
        }
    }

    @Override
    public IndexStatistics getStatistics() {
        try {
            long totalDocs = getDocumentCount();

            return IndexStatistics.builder()
                    .totalDocuments(totalDocs)
                    .indexSize(totalDocs * 1024L)
                    .indexType("H2")
                    .healthy(isHealthy())
                    .timestamp(System.currentTimeMillis())
                    .domainId(domainId)
                    .build();

        } catch (Exception e) {
            log.error("获取统计信息失败", e);
            return IndexStatistics.builder()
                    .totalDocuments(0L)
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
                    .domainId(domainId)
                    .build();
        }
    }

    // ========== 辅助方法 ==========

    private Document mapResultSetToDocument(ResultSet rs) throws SQLException {
        Document.DocumentBuilder builder = Document.builder();

        builder.id(rs.getString("id"));
        builder.title(rs.getString("title"));
        builder.content(rs.getString("content"));
        builder.summary(rs.getString("summary"));
        builder.type(rs.getString("type"));
        builder.source(rs.getString("source"));
        builder.author(rs.getString("author"));

        String tagsStr = rs.getString("tags");
        if (tagsStr != null && !tagsStr.isEmpty()) {
            builder.tags(Arrays.asList(tagsStr.split(",")));
        }

        String embeddingStr = rs.getString("embedding");
        if (embeddingStr != null) {
            builder.embedding(deserializeEmbedding(embeddingStr));
        }

        String metadataStr = rs.getString("metadata");
        if (metadataStr != null) {
            builder.metadata(deserializeMetadata(metadataStr));
        }

        Timestamp createdAt = rs.getTimestamp("created_at");
        if (createdAt != null) {
            builder.createdAt(createdAt.getTime());
        }

        return builder.build();
    }

    private String serializeEmbedding(float[] embedding) {
        if (embedding == null || embedding.length == 0) {
            return null;
        }
        try {
            return objectMapper.writeValueAsString(embedding);
        } catch (JsonProcessingException e) {
            log.error("序列化向量失败", e);
            return null;
        }
    }

    private float[] deserializeEmbedding(String json) {
        if (json == null || json.isEmpty()) {
            return null;
        }
        try {
            return objectMapper.readValue(json, float[].class);
        } catch (JsonProcessingException e) {
            log.error("反序列化向量失败", e);
            return null;
        }
    }

    private String serializeMetadata(Map<String, Object> metadata) {
        if (metadata == null || metadata.isEmpty()) {
            return null;
        }
        try {
            return objectMapper.writeValueAsString(metadata);
        } catch (JsonProcessingException e) {
            log.error("序列化元数据失败", e);
            return null;
        }
    }

    private Map<String, Object> deserializeMetadata(String json) {
        if (json == null || json.isEmpty()) {
            return new HashMap<>();
        }
        try {
            return objectMapper.readValue(json, new TypeReference<>() {});
        } catch (JsonProcessingException e) {
            log.error("反序列化元数据失败", e);
            return new HashMap<>();
        }
    }

    private float cosineSimilarity(float[] a, float[] b) {
        if (a.length != b.length) {
            return 0.0f;
        }

        float dotProduct = 0.0f;
        float normA = 0.0f;
        float normB = 0.0f;

        for (int i = 0; i < a.length; i++) {
            dotProduct += a[i] * b[i];
            normA += a[i] * a[i];
            normB += b[i] * b[i];
        }

        if (normA == 0.0f || normB == 0.0f) {
            return 0.0f;
        }

        return dotProduct / (float) (Math.sqrt(normA) * Math.sqrt(normB));
    }
}


