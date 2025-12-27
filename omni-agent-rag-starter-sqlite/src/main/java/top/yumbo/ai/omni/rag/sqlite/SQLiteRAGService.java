package top.yumbo.ai.omni.rag.sqlite;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Service;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.IndexStatistics;
import top.yumbo.ai.rag.api.model.Query;
import top.yumbo.ai.rag.api.model.SearchResult;

import jakarta.annotation.PostConstruct;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 基于 SQLite 的 RAG 实现
 * (SQLite-based RAG Implementation)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class SQLiteRAGService implements RAGService {

    private final JdbcTemplate jdbcTemplate;
    private final SQLiteRAGProperties properties;
    private final ObjectMapper objectMapper;

    public SQLiteRAGService(JdbcTemplate jdbcTemplate, SQLiteRAGProperties properties) {
        this.jdbcTemplate = jdbcTemplate;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();
    }

    @PostConstruct
    public void init() {
        log.info("初始化 SQLite RAG 服务，数据库路径: {}", properties.getDatabasePath());

        if (properties.isInitDatabase()) {
            initDatabase();
        }

        long count = getDocumentCount();
        log.info("SQLite RAG 服务初始化完成，文档总数: {}", count);
    }

    /**
     * 初始化数据库表
     */
    private void initDatabase() {
        log.info("初始化 SQLite 数据库表...");

        // 创建文档表
        String createTableSql = """
            CREATE TABLE IF NOT EXISTS rag_documents (
                id TEXT PRIMARY KEY,
                title TEXT,
                content TEXT NOT NULL,
                summary TEXT,
                source TEXT,
                type TEXT,
                author TEXT,
                tags TEXT,
                metadata TEXT,
                embedding TEXT,
                created_at INTEGER,
                updated_at INTEGER,
                indexed_at INTEGER
            )
        """;
        jdbcTemplate.execute(createTableSql);

        // 创建索引
        jdbcTemplate.execute("CREATE INDEX IF NOT EXISTS idx_rag_doc_type ON rag_documents(type)");
        jdbcTemplate.execute("CREATE INDEX IF NOT EXISTS idx_rag_doc_source ON rag_documents(source)");
        jdbcTemplate.execute("CREATE INDEX IF NOT EXISTS idx_rag_doc_author ON rag_documents(author)");
        jdbcTemplate.execute("CREATE INDEX IF NOT EXISTS idx_rag_doc_created ON rag_documents(created_at)");

        // 创建全文搜索表（FTS5）
        if (properties.isEnableFts()) {
            String createFtsSql = """
                CREATE VIRTUAL TABLE IF NOT EXISTS rag_documents_fts 
                USING fts5(id, title, content, summary, tags, content='rag_documents', content_rowid='rowid')
            """;
            jdbcTemplate.execute(createFtsSql);

            // 创建触发器保持FTS表同步
            jdbcTemplate.execute("""
                CREATE TRIGGER IF NOT EXISTS rag_documents_ai AFTER INSERT ON rag_documents BEGIN
                    INSERT INTO rag_documents_fts(rowid, id, title, content, summary, tags)
                    VALUES (new.rowid, new.id, new.title, new.content, new.summary, new.tags);
                END
            """);

            jdbcTemplate.execute("""
                CREATE TRIGGER IF NOT EXISTS rag_documents_ad AFTER DELETE ON rag_documents BEGIN
                    DELETE FROM rag_documents_fts WHERE rowid = old.rowid;
                END
            """);

            jdbcTemplate.execute("""
                CREATE TRIGGER IF NOT EXISTS rag_documents_au AFTER UPDATE ON rag_documents BEGIN
                    UPDATE rag_documents_fts SET 
                        title = new.title,
                        content = new.content,
                        summary = new.summary,
                        tags = new.tags
                    WHERE rowid = new.rowid;
                END
            """);

            log.info("FTS5 全文搜索表创建完成");
        }

        log.info("数据库表初始化完成");
    }

    // ========== 文档索引 ==========

    @Override
    public String indexDocument(Document document) {
        try {
            if (document.getId() == null || document.getId().isEmpty()) {
                document.setId(UUID.randomUUID().toString());
            }

            document.setIndexedAt(System.currentTimeMillis());

            // 删除旧文档（如果存在）
            deleteDocument(document.getId());

            // 插入新文档
            String sql = """
                INSERT INTO rag_documents (
                    id, title, content, summary, source, type, author, tags, 
                    metadata, embedding, created_at, updated_at, indexed_at
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """;

            jdbcTemplate.update(sql,
                    document.getId(),
                    document.getTitle(),
                    document.getContent(),
                    document.getSummary(),
                    document.getSource(),
                    document.getType(),
                    document.getAuthor(),
                    serializeTags(document.getTags()),
                    serializeMetadata(document.getMetadata()),
                    serializeEmbedding(document.getEmbedding()),
                    document.getCreatedAt(),
                    document.getUpdatedAt(),
                    document.getIndexedAt()
            );

            log.debug("文档索引成功: {}", document.getId());
            return document.getId();

        } catch (Exception e) {
            log.error("索引文档失败: {}", document.getId(), e);
            throw new RuntimeException("索引文档失败", e);
        }
    }

    @Override
    public List<String> indexDocuments(List<Document> documents) {
        List<String> indexedIds = new ArrayList<>();
        for (Document document : documents) {
            try {
                String id = indexDocument(document);
                indexedIds.add(id);
            } catch (Exception e) {
                log.error("批量索引文档失败: {}", document.getId(), e);
            }
        }
        log.info("批量索引完成，共 {} 个文档", indexedIds.size());
        return indexedIds;
    }

    @Override
    public boolean updateDocument(Document document) {
        try {
            if (document.getId() == null || document.getId().isEmpty()) {
                log.error("更新文档失败：文档ID为空");
                return false;
            }

            document.setUpdatedAt(System.currentTimeMillis());
            document.setIndexedAt(System.currentTimeMillis());

            String sql = """
                UPDATE rag_documents SET
                    title = ?,
                    content = ?,
                    summary = ?,
                    source = ?,
                    type = ?,
                    author = ?,
                    tags = ?,
                    metadata = ?,
                    embedding = ?,
                    updated_at = ?,
                    indexed_at = ?
                WHERE id = ?
            """;

            int updated = jdbcTemplate.update(sql,
                    document.getTitle(),
                    document.getContent(),
                    document.getSummary(),
                    document.getSource(),
                    document.getType(),
                    document.getAuthor(),
                    serializeTags(document.getTags()),
                    serializeMetadata(document.getMetadata()),
                    serializeEmbedding(document.getEmbedding()),
                    document.getUpdatedAt(),
                    document.getIndexedAt(),
                    document.getId()
            );

            log.debug("文档更新成功: {}", document.getId());
            return updated > 0;

        } catch (Exception e) {
            log.error("更新文档失败: {}", document.getId(), e);
            return false;
        }
    }

    @Override
    public boolean deleteDocument(String documentId) {
        try {
            String sql = "DELETE FROM rag_documents WHERE id = ?";
            int deleted = jdbcTemplate.update(sql, documentId);

            log.debug("文档删除成功: {}", documentId);
            return deleted > 0;

        } catch (Exception e) {
            log.error("删除文档失败: {}", documentId, e);
            return false;
        }
    }

    @Override
    public void clearAll() {
        try {
            jdbcTemplate.execute("DELETE FROM rag_documents");
            if (properties.isEnableFts()) {
                jdbcTemplate.execute("DELETE FROM rag_documents_fts");
            }
            log.info("所有索引已清空");

        } catch (Exception e) {
            log.error("清空索引失败", e);
            throw new RuntimeException("清空索引失败", e);
        }
    }

    // ========== 文本搜索 ==========

    @Override
    public List<SearchResult> search(Query query) {
        switch (query.getMode()) {
            case TEXT:
                return searchByText(query.getText(), query.getTopK());
            case VECTOR:
                return query.getEmbedding() != null ?
                        vectorSearch(query.getEmbedding(), query.getTopK()) :
                        Collections.emptyList();
            case HYBRID:
                return hybridSearch(query);
            case SEMANTIC:
                return semanticSearch(query.getText(), query.getTopK());
            default:
                return searchByText(query.getText(), query.getTopK());
        }
    }

    @Override
    public List<SearchResult> searchByText(String text, int topK) {
        try {
            List<SearchResult> results = new ArrayList<>();

            if (properties.isEnableFts()) {
                // 使用 FTS5 全文搜索
                String sql = """
                    SELECT d.*, 
                           rank as score
                    FROM rag_documents_fts fts
                    JOIN rag_documents d ON fts.id = d.id
                    WHERE rag_documents_fts MATCH ?
                    ORDER BY rank
                    LIMIT ?
                """;

                results = jdbcTemplate.query(sql, documentRowMapper(), text, topK);

            } else {
                // 使用 LIKE 查询（降级方案）
                String sql = """
                    SELECT * FROM rag_documents
                    WHERE title LIKE ? OR content LIKE ? OR summary LIKE ? OR tags LIKE ?
                    LIMIT ?
                """;

                String pattern = "%" + text + "%";
                results = jdbcTemplate.query(sql, documentRowMapper(), 
                        pattern, pattern, pattern, pattern, topK);
            }

            // 设置排名
            for (int i = 0; i < results.size(); i++) {
                results.get(i).setRank(i + 1);
                results.get(i).setReason("文本匹配");
            }

            log.debug("文本搜索完成，查询: {}, 结果数: {}", text, results.size());
            return results;

        } catch (Exception e) {
            log.error("文本搜索失败: {}", text, e);
            return Collections.emptyList();
        }
    }

    // ========== 向量搜索 ==========

    @Override
    public List<SearchResult> vectorSearch(float[] embedding, int topK) {
        try {
            // ⭐ Debug 日志：搜索开始
            log.debug("🔍 [RAG Vector Search] Starting search - Embedding dim: {}, topK: {}",
                embedding.length, topK);

            long startTime = System.currentTimeMillis();

            // 获取所有有向量的文档
            String sql = "SELECT * FROM rag_documents WHERE embedding IS NOT NULL AND embedding != ''";
            List<Document> allDocs = jdbcTemplate.query(sql, this::mapRowToDocument);

            // ⭐ Debug 日志：候选文档数量
            log.debug("🔍 [RAG Vector Search] Found {} candidate documents with embeddings", allDocs.size());

            // 计算余弦相似度
            List<SearchResult> results = new ArrayList<>();
            for (Document doc : allDocs) {
                if (doc.getEmbedding() != null && doc.getEmbedding().length > 0) {
                    float similarity = cosineSimilarity(embedding, doc.getEmbedding());
                    
                    SearchResult result = SearchResult.builder()
                            .document(doc)
                            .score(similarity)
                            .vectorScore(similarity)
                            .reason("向量相似度")
                            .build();
                    
                    results.add(result);

                    // ⭐ Debug 日志：每个候选文档的相似度
                    log.debug("🔍 [RAG Vector Search] Doc [{}]: similarity={:.4f}, title={}",
                        doc.getId(), similarity,
                        doc.getTitle() != null ? doc.getTitle() : doc.getContent().substring(0, Math.min(50, doc.getContent().length())));
                }
            }

            // 按相似度排序并返回 topK
            List<SearchResult> topResults = results.stream()
                    .sorted(Comparator.comparing(SearchResult::getScore).reversed())
                    .limit(topK)
                    .peek(r -> r.setRank(results.indexOf(r) + 1))
                    .collect(Collectors.toList());

            long duration = System.currentTimeMillis() - startTime;

            // ⭐ Debug 日志：搜索结果
            log.debug("🔍 [RAG Vector Search] Completed in {}ms - Returned {} results", duration, topResults.size());
            for (int i = 0; i < topResults.size(); i++) {
                SearchResult r = topResults.get(i);
                log.debug("🔍 [RAG Vector Search] Result #{}: score={:.4f}, docId={}, content preview: {}",
                    i + 1, r.getScore(), r.getDocument().getId(),
                    r.getDocument().getContent().substring(0, Math.min(100, r.getDocument().getContent().length())) + "...");
            }

            return topResults;

        } catch (Exception e) {
            log.error("向量搜索失败", e);
            return Collections.emptyList();
        }
    }

    @Override
    public List<SearchResult> vectorSearch(float[] embedding, int topK, Map<String, Object> filters) {
        // 先进行向量搜索，然后应用过滤器
        List<SearchResult> results = vectorSearch(embedding, topK * 2);

        if (filters == null || filters.isEmpty()) {
            return results.stream().limit(topK).collect(Collectors.toList());
        }

        // 应用过滤器
        return results.stream()
                .filter(result -> matchesFilters(result.getDocument(), filters))
                .limit(topK)
                .collect(Collectors.toList());
    }

    // ========== 混合检索 ==========

    @Override
    public List<SearchResult> hybridSearch(Query query) {
        // 执行文本搜索
        List<SearchResult> textResults = searchByText(query.getText(), query.getTopK() * 2);

        // 如果有向量，执行向量搜索
        if (query.getEmbedding() != null && query.getEmbedding().length > 0) {
            List<SearchResult> vectorResults = vectorSearch(query.getEmbedding(), query.getTopK() * 2);

            // 合并结果
            Map<String, SearchResult> mergedResults = new HashMap<>();

            for (SearchResult result : textResults) {
                String docId = result.getDocument().getId();
                Float scoreObj = result.getScore();
                float textScore = scoreObj != null ? scoreObj : 0.0f;
                float combinedScore = textScore * query.getTextWeight();

                result.setTextScore(textScore);
                result.setScore(combinedScore);
                mergedResults.put(docId, result);
            }

            for (SearchResult result : vectorResults) {
                String docId = result.getDocument().getId();
                Float scoreObj = result.getScore();
                float vectorScore = scoreObj != null ? scoreObj : 0.0f;

                if (mergedResults.containsKey(docId)) {
                    SearchResult existing = mergedResults.get(docId);
                    Float textScoreObj = existing.getTextScore();
                    float textScore = textScoreObj != null ? textScoreObj : 0.0f;
                    float combinedScore = textScore * query.getTextWeight() + vectorScore * query.getVectorWeight();
                    
                    existing.setVectorScore(vectorScore);
                    existing.setScore(combinedScore);
                    existing.setReason("混合检索");
                } else {
                    float combinedScore = vectorScore * query.getVectorWeight();
                    result.setVectorScore(vectorScore);
                    result.setScore(combinedScore);
                    result.setReason("向量匹配");
                    mergedResults.put(docId, result);
                }
            }

            // 排序并返回 topK
            return mergedResults.values().stream()
                    .sorted(Comparator.comparing(SearchResult::getScore).reversed())
                    .limit(query.getTopK())
                    .peek(r -> r.setRank(new ArrayList<>(mergedResults.values()).indexOf(r) + 1))
                    .collect(Collectors.toList());
        }

        // 只有文本搜索
        return textResults.stream().limit(query.getTopK()).collect(Collectors.toList());
    }

    @Override
    public List<SearchResult> hybridSearch(String text, float[] embedding,
                                          float textWeight, float vectorWeight, int topK) {
        Query query = Query.builder()
                .text(text)
                .embedding(embedding)
                .topK(topK)
                .mode(Query.SearchMode.HYBRID)
                .textWeight(textWeight)
                .vectorWeight(vectorWeight)
                .build();
        return hybridSearch(query);
    }

    // ========== 语义搜索 ==========

    @Override
    public List<SearchResult> semanticSearch(String text, int topK) {
        log.warn("语义搜索需要 AI Embedding 服务支持，当前使用文本搜索");
        return searchByText(text, topK);
    }

    // ========== 文档管理 ==========

    @Override
    public Optional<Document> getDocument(String documentId) {
        try {
            String sql = "SELECT * FROM rag_documents WHERE id = ?";
            List<Document> results = jdbcTemplate.query(sql, this::mapRowToDocument, documentId);

            return results.isEmpty() ? Optional.empty() : Optional.of(results.get(0));

        } catch (Exception e) {
            log.error("获取文档失败: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public boolean documentExists(String documentId) {
        String sql = "SELECT COUNT(*) FROM rag_documents WHERE id = ?";
        Integer count = jdbcTemplate.queryForObject(sql, Integer.class, documentId);
        return count != null && count > 0;
    }

    @Override
    public long getDocumentCount() {
        String sql = "SELECT COUNT(*) FROM rag_documents";
        Long count = jdbcTemplate.queryForObject(sql, Long.class);
        return count != null ? count : 0;
    }

    @Override
    public List<Document> getAllDocuments(int offset, int limit) {
        String sql = "SELECT id, title, content, source, type, metadata, created_at " +
                     "FROM rag_documents ORDER BY created_at DESC LIMIT ? OFFSET ?";

        try {
            return jdbcTemplate.query(sql, (rs, rowNum) -> {
                String metadataJson = rs.getString("metadata");
                Map<String, Object> metadata = new HashMap<>();
                if (metadataJson != null && !metadataJson.isEmpty()) {
                    try {
                        metadata = objectMapper.readValue(metadataJson,
                            new com.fasterxml.jackson.core.type.TypeReference<Map<String, Object>>() {});
                    } catch (Exception e) {
                        log.warn("解析metadata失败", e);
                    }
                }

                return Document.builder()
                    .id(rs.getString("id"))
                    .title(rs.getString("title"))
                    .content(rs.getString("content"))
                    .source(rs.getString("source"))
                    .type(rs.getString("type"))
                    .metadata(metadata)
                    .createdAt(rs.getTimestamp("created_at").getTime())
                    .build();
            }, limit, offset);

        } catch (Exception e) {
            log.error("获取所有文档失败", e);
            return Collections.emptyList();
        }
    }

    // ========== 统计与健康 ==========

    @Override
    public IndexStatistics getStatistics() {
        try {
            long totalDocs = getDocumentCount();

            // 获取数据库文件大小（近似）
            String sql = "SELECT page_count * page_size as size FROM pragma_page_count(), pragma_page_size()";
            Long dbSize = jdbcTemplate.queryForObject(sql, Long.class);

            return IndexStatistics.builder()
                    .totalDocuments(totalDocs)
                    .indexSize(dbSize != null ? dbSize : 0)
                    .indexType("SQLite-RAG")
                    .vectorSearchEnabled(true)
                    .healthy(true)
                    .lastIndexedAt(System.currentTimeMillis())
                    .timestamp(System.currentTimeMillis())
                    .build();

        } catch (Exception e) {
            log.error("获取统计信息失败", e);
            return IndexStatistics.builder()
                    .indexType("SQLite-RAG")
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
                    .build();
        }
    }

    @Override
    public boolean isHealthy() {
        try {
            jdbcTemplate.queryForObject("SELECT 1", Integer.class);
            return true;
        } catch (Exception e) {
            log.error("健康检查失败", e);
            return false;
        }
    }

    @Override
    public void rebuildIndex() {
        log.info("重建索引开始");
        try {
            if (properties.isEnableFts()) {
                // 重建 FTS 索引
                jdbcTemplate.execute("INSERT INTO rag_documents_fts(rag_documents_fts) VALUES('rebuild')");
                log.info("FTS 索引重建完成");
            }

            log.info("重建索引完成");

        } catch (Exception e) {
            log.error("重建索引失败", e);
            throw new RuntimeException("重建索引失败", e);
        }
    }

    // ========== 辅助方法 ==========

    /**
     * RowMapper for SearchResult
     */
    private RowMapper<SearchResult> documentRowMapper() {
        return (rs, rowNum) -> {
            Document document = mapRowToDocument(rs, rowNum);
            float score = rs.wasNull() ? 0.0f : Math.abs(rs.getFloat("score"));

            return SearchResult.builder()
                    .document(document)
                    .score(score)
                    .textScore(score)
                    .build();
        };
    }

    /**
     * Map ResultSet to Document
     */
    private Document mapRowToDocument(ResultSet rs, int rowNum) throws SQLException {
        return Document.builder()
                .id(rs.getString("id"))
                .title(rs.getString("title"))
                .content(rs.getString("content"))
                .summary(rs.getString("summary"))
                .source(rs.getString("source"))
                .type(rs.getString("type"))
                .author(rs.getString("author"))
                .tags(deserializeTags(rs.getString("tags")))
                .metadata(deserializeMetadata(rs.getString("metadata")))
                .embedding(deserializeEmbedding(rs.getString("embedding")))
                .createdAt(rs.getLong("created_at"))
                .updatedAt(rs.getLong("updated_at"))
                .indexedAt(rs.getLong("indexed_at"))
                .build();
    }

    /**
     * 序列化标签
     */
    private String serializeTags(List<String> tags) {
        if (tags == null || tags.isEmpty()) {
            return null;
        }
        try {
            return objectMapper.writeValueAsString(tags);
        } catch (JsonProcessingException e) {
            log.error("序列化标签失败", e);
            return null;
        }
    }

    /**
     * 反序列化标签
     */
    private List<String> deserializeTags(String json) {
        if (json == null || json.isEmpty()) {
            return Collections.emptyList();
        }
        try {
            return objectMapper.readValue(json, new TypeReference<List<String>>() {});
        } catch (JsonProcessingException e) {
            log.error("反序列化标签失败", e);
            return Collections.emptyList();
        }
    }

    /**
     * 序列化元数据
     */
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

    /**
     * 反序列化元数据
     */
    private Map<String, Object> deserializeMetadata(String json) {
        if (json == null || json.isEmpty()) {
            return Collections.emptyMap();
        }
        try {
            return objectMapper.readValue(json, new TypeReference<Map<String, Object>>() {});
        } catch (JsonProcessingException e) {
            log.error("反序列化元数据失败", e);
            return Collections.emptyMap();
        }
    }

    /**
     * 序列化向量
     */
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

    /**
     * 反序列化向量
     */
    private float[] deserializeEmbedding(String json) {
        if (json == null || json.isEmpty()) {
            return new float[0];
        }
        try {
            return objectMapper.readValue(json, float[].class);
        } catch (JsonProcessingException e) {
            log.error("反序列化向量失败", e);
            return new float[0];
        }
    }

    /**
     * 计算余弦相似度
     */
    private float cosineSimilarity(float[] vec1, float[] vec2) {
        if (vec1.length != vec2.length) {
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

    /**
     * 检查文档是否匹配过滤条件
     */
    private boolean matchesFilters(Document document, Map<String, Object> filters) {
        for (Map.Entry<String, Object> entry : filters.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            switch (key) {
                case "type":
                    if (!Objects.equals(document.getType(), value)) {
                        return false;
                    }
                    break;
                case "source":
                    if (!Objects.equals(document.getSource(), value)) {
                        return false;
                    }
                    break;
                case "author":
                    if (!Objects.equals(document.getAuthor(), value)) {
                        return false;
                    }
                    break;
                // 可以添加更多过滤条件
            }
        }
        return true;
    }
}
