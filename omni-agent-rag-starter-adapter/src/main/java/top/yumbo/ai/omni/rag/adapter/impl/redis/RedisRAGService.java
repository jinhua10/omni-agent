package top.yumbo.ai.omni.rag.adapter.impl.redis;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;
import top.yumbo.ai.omni.rag.model.IndexStatistics;

import jakarta.annotation.PostConstruct;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * 基于 Redis 的 RAG 实现
 * (Redis-based RAG Implementation)
 * 
 * <p>特性:
 * <ul>
 *   <li>高性能内存存储</li>
 *   <li>快速向量搜索（余弦相似度）</li>
 *   <li>支持文本搜索（基于关键词匹配）</li>
 *   <li>可配置TTL过期时间</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class RedisRAGService implements RagService {

    private final RedisTemplate<String, Object> redisTemplate;
    private final RedisRAGProperties properties;
    private final ObjectMapper objectMapper;
    private final String domainId;

    // Redis Key前缀
    private static final String DOC_PREFIX = "rag:doc:";           // 文档存储
    private static final String INDEX_PREFIX = "rag:index:";       // 索引集合
    private static final String VECTOR_PREFIX = "rag:vector:";     // 向量索引
    private static final String TEXT_PREFIX = "rag:text:";         // 文本索引
    private static final String STATS_KEY = "rag:stats";           // 统计信息

    public RedisRAGService(RedisTemplate<String, Object> redisTemplate, 
                          RedisRAGProperties properties) {
        this(redisTemplate, properties, "redis-domain");
    }

    public RedisRAGService(RedisTemplate<String, Object> redisTemplate,
                          RedisRAGProperties properties, String domainId) {
        this.redisTemplate = redisTemplate;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();
        this.domainId = domainId;
    }

    @PostConstruct
    public void init() {
        log.info("Redis RAG Service 初始化成功");
        log.info("Redis KeyPrefix: {}", properties.getKeyPrefix());
        log.info("文档TTL: {} 秒", properties.getDocumentTtl());
    }

    // ========== 文档索引 ==========

    public String indexDocument(Document document) {
        try {
            if (document.getId() == null || document.getId().isEmpty()) {
                document.setId(UUID.randomUUID().toString());
            }

            String docKey = getDocKey(document.getId());
            
            // 存储文档
            redisTemplate.opsForValue().set(docKey, document, 
                properties.getDocumentTtl(), TimeUnit.SECONDS);

            // 添加到索引集合
            redisTemplate.opsForSet().add(INDEX_PREFIX + "all", document.getId());

            // 索引文本（关键词提取）
            if (properties.isEnableTextIndex()) {
                indexText(document);
            }

            // 索引向量
            if (document.getEmbedding() != null && document.getEmbedding().length > 0) {
                indexVector(document);
            }

            // 更新统计
            redisTemplate.opsForHash().increment(STATS_KEY, "totalDocs", 1);

            log.debug("文档索引成功: {}", document.getId());
            return document.getId();

        } catch (Exception e) {
            log.error("索引文档失败", e);
            throw new RuntimeException("索引文档失败", e);
        }
    }

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

    public boolean updateDocument(Document document) {
        try {
            if (!documentExists(document.getId())) {
                log.warn("文档不存在: {}", document.getId());
                return false;
            }

            // 删除旧索引
            deleteIndexes(document.getId());

            // 重新索引
            indexDocument(document);

            log.debug("文档更新成功: {}", document.getId());
            return true;

        } catch (Exception e) {
            log.error("更新文档失败: {}", document.getId(), e);
            return false;
        }
    }

    public boolean deleteDocument(String documentId) {
        try {
            String docKey = getDocKey(documentId);

            if (!Boolean.TRUE.equals(redisTemplate.hasKey(docKey))) {
                log.warn("文档不存在: {}", documentId);
                return false;
            }

            // 删除文档
            redisTemplate.delete(docKey);

            // 删除索引
            deleteIndexes(documentId);

            // 从集合中移除
            redisTemplate.opsForSet().remove(INDEX_PREFIX + "all", documentId);

            // 更新统计
            redisTemplate.opsForHash().increment(STATS_KEY, "totalDocs", -1);

            log.debug("文档删除成功: {}", documentId);
            return true;

        } catch (Exception e) {
            log.error("删除文档失败: {}", documentId, e);
            return false;
        }
    }

    @Override
    public void clearAll() {
        try {
            // 获取所有文档ID
            Set<Object> allDocs = redisTemplate.opsForSet().members(INDEX_PREFIX + "all");

            if (allDocs != null) {
                for (Object docId : allDocs) {
                    deleteDocument(docId.toString());
                }
            }

            // 清空统计
            redisTemplate.delete(STATS_KEY);

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
            String docKey = getDocKey(documentId);
            Object docObj = redisTemplate.opsForValue().get(docKey);

            if (docObj instanceof Document) {
                return Optional.of((Document) docObj);
            }

            return Optional.empty();

        } catch (Exception e) {
            log.error("获取文档失败: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public boolean documentExists(String documentId) {
        try {
            String docKey = getDocKey(documentId);
            return Boolean.TRUE.equals(redisTemplate.hasKey(docKey));

        } catch (Exception e) {
            log.error("检查文档存在失败: {}", documentId, e);
            return false;
        }
    }

    @Override
    public long getDocumentCount() {
        try {
            Object count = redisTemplate.opsForHash().get(STATS_KEY, "totalDocs");
            return count != null ? Long.parseLong(count.toString()) : 0L;

        } catch (Exception e) {
            log.error("获取文档总数失败", e);
            return 0;
        }
    }

    @Override
    public List<Document> getAllDocuments(int offset, int limit) {
        try {
            // 获取所有文档的key模式
            String keyPattern = properties.getKeyPrefix() + DOC_PREFIX + "*";
            Set<String> keys = redisTemplate.keys(keyPattern);
            if (keys == null || keys.isEmpty()) {
                return Collections.emptyList();
            }

            // 提取文档ID并排序
            String docPrefix = properties.getKeyPrefix() + DOC_PREFIX;
            List<String> docIds = keys.stream()
                .filter(key -> key.startsWith(docPrefix))
                .map(key -> key.substring(docPrefix.length()))
                .sorted()
                .toList();

            // 分页
            int start = Math.min(offset, docIds.size());
            int end = Math.min(offset + limit, docIds.size());

            if (start >= docIds.size()) {
                return Collections.emptyList();
            }

            List<String> pagedIds = docIds.subList(start, end);

            // 获取文档内容
            List<Document> documents = new ArrayList<>();
            for (String docId : pagedIds) {
                Optional<Document> doc = getDocument(docId);
                doc.ifPresent(documents::add);
            }

            log.debug("获取文档列表: offset={}, limit={}, count={}", offset, limit, documents.size());
            return documents;

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

            return IndexStatistics.builder()
                .totalDocuments(totalDocs)
                .indexSize(totalDocs * 1024L) // 估算
                .indexType("Redis")
                .healthy(isHealthy())
                .timestamp(System.currentTimeMillis())
                .domainId(domainId)
                .build();

        } catch (Exception e) {
            log.error("获取统计信息失败", e);
            return IndexStatistics.builder()
                .totalDocuments(0L)
                .indexSize(0L)
                .healthy(false)
                .timestamp(System.currentTimeMillis())
                .domainId(domainId)
                .build();
        }
    }

    @Override
    public boolean isHealthy() {
        try {
            redisTemplate.opsForValue().get("health:check");
            return true;

        } catch (Exception e) {
            log.error("健康检查失败", e);
            return false;
        }
    }

    @Override
    public void rebuildIndex() {
        try {
            // Redis不需要重建索引，数据即索引
            log.info("Redis不需要重建索引");

        } catch (Exception e) {
            log.error("重建索引失败", e);
            throw new RuntimeException("重建索引失败", e);
        }
    }

    // ========== 工具方法 ==========

    private String getDocKey(String documentId) {
        return properties.getKeyPrefix() + DOC_PREFIX + documentId;
    }

    private void indexText(Document document) {
        try {
            Set<String> keywords = new HashSet<>();

            // 从标题提取关键词
            if (document.getTitle() != null) {
                keywords.addAll(extractKeywords(document.getTitle()));
            }

            // 从内容提取关键词
            if (document.getContent() != null) {
                keywords.addAll(extractKeywords(document.getContent()));
            }

            // 从标签提取
            if (document.getTags() != null) {
                keywords.addAll(document.getTags());
            }

            // 索引关键词
            for (String keyword : keywords) {
                String textKey = TEXT_PREFIX + keyword.toLowerCase();
                redisTemplate.opsForSet().add(textKey, document.getId());
                
                // 设置TTL
                redisTemplate.expire(textKey, properties.getDocumentTtl(), TimeUnit.SECONDS);
            }

        } catch (Exception e) {
            log.error("索引文本失败", e);
        }
    }

    private void indexVector(Document document) {
        try {
            // Redis中向量存储在文档对象中，搜索时动态计算
            // 可选：使用Redis Sorted Set存储向量维度信息用于优化
            
        } catch (Exception e) {
            log.error("索引向量失败", e);
        }
    }

    private void deleteIndexes(String documentId) {
        try {
            // 获取文档以删除其文本索引
            Optional<Document> docOpt = getDocument(documentId);
            if (docOpt.isPresent()) {
                Document doc = docOpt.get();
                
                // 删除文本索引
                Set<String> keywords = new HashSet<>();
                if (doc.getTitle() != null) {
                    keywords.addAll(extractKeywords(doc.getTitle()));
                }
                if (doc.getContent() != null) {
                    keywords.addAll(extractKeywords(doc.getContent()));
                }
                if (doc.getTags() != null) {
                    keywords.addAll(doc.getTags());
                }

                for (String keyword : keywords) {
                    String textKey = TEXT_PREFIX + keyword.toLowerCase();
                    redisTemplate.opsForSet().remove(textKey, documentId);
                }
            }

        } catch (Exception e) {
            log.error("删除索引失败", e);
        }
    }

    private Set<String> extractKeywords(String text) {
        // 简单的关键词提取：分词并过滤
        if (text == null || text.isEmpty()) {
            return Collections.emptySet();
        }

        return Arrays.stream(text.split("\\s+"))
            .map(String::toLowerCase)
            .map(String::trim)
            .filter(word -> word.length() > 2) // 过滤短词
            .filter(word -> !isStopWord(word))
            .collect(Collectors.toSet());
    }

    private boolean isStopWord(String word) {
        // 简单的停用词列表
        Set<String> stopWords = Set.of("the", "and", "or", "but", "in", "on", "at", "to", "for",
            "of", "with", "a", "an", "is", "are", "was", "were", "be", "been", "being",
            "的", "了", "和", "是", "在", "有", "我", "你", "他", "她", "它");
        return stopWords.contains(word);
    }

    private boolean matchFilters(Document doc, Map<String, Object> filters) {
        for (Map.Entry<String, Object> entry : filters.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            switch (key) {
                case "type":
                    if (!value.equals(doc.getType())) return false;
                    break;
                case "source":
                    if (!value.equals(doc.getSource())) return false;
                    break;
                case "author":
                    if (!value.equals(doc.getAuthor())) return false;
                    break;
                default:
                    // 检查metadata
                    if (doc.getMetadata() != null) {
                        if (!value.equals(doc.getMetadata().get(key))) return false;
                    } else {
                        return false;
                    }
            }
        }
        return true;
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

    // ========== 新接口实现 ==========

    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        log.debug("语义搜索: query={}, maxResults={}", query, maxResults);
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
        log.warn("RedisRAGService 不提供嵌入功能");
        return null;
    }

    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        log.warn("RedisRAGService 不提供批量嵌入功能");
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
        indexDocuments(documents);
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

    private List<Document> searchByTextInternal(String text, int maxResults) {
        try {
            Set<Object> docIdsObj = redisTemplate.opsForSet().members(INDEX_PREFIX + "all");
            if (docIdsObj == null || docIdsObj.isEmpty()) {
                return Collections.emptyList();
            }

            List<Document> results = new ArrayList<>();
            for (Object docIdObj : docIdsObj) {
                String docId = docIdObj.toString();
                Document doc = (Document) redisTemplate.opsForValue().get(DOC_PREFIX + docId);
                if (doc != null) {
                    String content = (doc.getContent() != null ? doc.getContent() : "") + " " +
                                   (doc.getTitle() != null ? doc.getTitle() : "") + " " +
                                   (doc.getSummary() != null ? doc.getSummary() : "");
                    if (content.toLowerCase().contains(text.toLowerCase())) {
                        results.add(doc);
                        if (results.size() >= maxResults) {
                            break;
                        }
                    }
                }
            }
            return results;
        } catch (Exception e) {
            log.error("文本搜索失败: {}", text, e);
            return Collections.emptyList();
        }
    }

    private List<Document> vectorSearchInternal(float[] embedding, int maxResults) {
        try {
            Set<Object> docIdsObj = redisTemplate.opsForSet().members(VECTOR_PREFIX + "all");
            if (docIdsObj == null || docIdsObj.isEmpty()) {
                return Collections.emptyList();
            }

            List<Document> results = new ArrayList<>();
            for (Object docIdObj : docIdsObj) {
                String docId = docIdObj.toString();
                Document doc = (Document) redisTemplate.opsForValue().get(DOC_PREFIX + docId);
                if (doc != null && doc.getEmbedding() != null && doc.getEmbedding().length > 0) {
                    float similarity = cosineSimilarity(embedding, doc.getEmbedding());
                    doc.setScore((double) similarity);
                    results.add(doc);
                }
            }

            return results.stream()
                    .sorted((a, b) -> Double.compare(
                        b.getScore() != null ? b.getScore() : 0.0,
                        a.getScore() != null ? a.getScore() : 0.0))
                    .limit(maxResults)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("向量搜索失败", e);
            return Collections.emptyList();
        }
    }
}
