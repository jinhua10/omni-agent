package top.yumbo.ai.omni.rag.sqlite;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;
import top.yumbo.ai.omni.rag.model.IndexStatistics;

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
public class SQLiteRAGService implements RagService {

    private final JdbcTemplate jdbcTemplate;
    private final SQLiteRAGProperties properties;
    private final ObjectMapper objectMapper;
    private final String domainId;

    public SQLiteRAGService(JdbcTemplate jdbcTemplate, SQLiteRAGProperties properties) {
        this(jdbcTemplate, properties, "sqlite-domain");
    }

    public SQLiteRAGService(JdbcTemplate jdbcTemplate, SQLiteRAGProperties properties, String domainId) {
        this.jdbcTemplate = jdbcTemplate;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();
        this.domainId = domainId;
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

    /**
     * 内部文档索引方法
     */
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

    /**
     * 内部批量索引方法
     */
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

    /**
     * 内部文档更新方法
     */
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

    /**
     * 内部文档删除方法
     */
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
                    .timestamp(System.currentTimeMillis())
                    .domainId(domainId)
                    .build();

        } catch (Exception e) {
            log.error("获取统计信息失败", e);
            return IndexStatistics.builder()
                    .indexType("SQLite-RAG")
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
                    .domainId(domainId)
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

    // ========== 新接口实现 ==========

    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        log.debug("语义搜索: query={}, maxResults={}", query, maxResults);
        // 使用文本搜索作为降级方案
        return searchByTextInternal(query, maxResults);
    }

    @Override
    public List<Document> vectorSearch(Vector vector, int maxResults) {
        if (vector == null || vector.getData() == null) {
            return Collections.emptyList();
        }
        return vectorSearchInternal(vector.getData(), maxResults);
    }

    @Override
    public Vector embed(String text) {
        log.warn("SQLiteRAGService 不提供嵌入功能");
        return null;
    }

    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        log.warn("SQLiteRAGService 不提供批量嵌入功能");
        return Collections.emptyList();
    }

    @Override
    public void index(String id, Vector vector, Map<String, Object> metadata) {
        Document.DocumentBuilder builder = Document.builder().id(id);

        if (metadata != null) {
            if (metadata.containsKey("title")) builder.title((String) metadata.get("title"));
            if (metadata.containsKey("content")) builder.content((String) metadata.get("content"));
            if (metadata.containsKey("source")) builder.source((String) metadata.get("source"));
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

    // ========== 内部辅助方法 ==========

    private List<Document> searchByTextInternal(String text, int topK) {
        try {
            List<Document> results = new ArrayList<>();

            if (properties.isEnableFts()) {
                String sql = """
                    SELECT d.*
                    FROM rag_documents_fts fts
                    JOIN rag_documents d ON fts.id = d.id
                    WHERE rag_documents_fts MATCH ?
                    ORDER BY rank
                    LIMIT ?
                """;
                results = jdbcTemplate.query(sql, documentOnlyRowMapper(), text, topK);
            } else {
                String sql = """
                    SELECT * FROM rag_documents
                    WHERE title LIKE ? OR content LIKE ? OR summary LIKE ?
                    LIMIT ?
                """;
                String pattern = "%" + text + "%";
                results = jdbcTemplate.query(sql, documentOnlyRowMapper(),
                        pattern, pattern, pattern, topK);
            }

            return results;
        } catch (Exception e) {
            log.error("文本搜索失败: {}", text, e);
            return Collections.emptyList();
        }
    }

    private List<Document> vectorSearchInternal(float[] embedding, int topK) {
        String sql = "SELECT * FROM rag_documents WHERE embedding IS NOT NULL";

        try {
            List<Document> allDocs = jdbcTemplate.query(sql, documentOnlyRowMapper());
            List<Document> results = new ArrayList<>();

            for (Document doc : allDocs) {
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

        } catch (Exception e) {
            log.error("向量搜索失败", e);
            return Collections.emptyList();
        }
    }

    private RowMapper<Document> documentOnlyRowMapper() {
        return (rs, rowNum) -> {
            Document.DocumentBuilder builder = Document.builder();
            builder.id(rs.getString("id"));
            builder.title(rs.getString("title"));
            builder.content(rs.getString("content"));
            builder.summary(rs.getString("summary"));
            builder.type(rs.getString("type"));
            builder.source(rs.getString("source"));
            builder.author(rs.getString("author"));

            String tagsJson = rs.getString("tags");
            if (tagsJson != null && !tagsJson.isEmpty()) {
                try {
                    List<String> tags = objectMapper.readValue(tagsJson, new TypeReference<>() {});
                    builder.tags(tags);
                } catch (Exception e) {
                    log.warn("解析tags失败: {}", e.getMessage());
                }
            }

            String embeddingJson = rs.getString("embedding");
            if (embeddingJson != null && !embeddingJson.isEmpty()) {
                try {
                    float[] embedding = objectMapper.readValue(embeddingJson, float[].class);
                    builder.embedding(embedding);
                } catch (Exception e) {
                    log.warn("解析embedding失败: {}", e.getMessage());
                }
            }

            return builder.build();
        };
    }
}
