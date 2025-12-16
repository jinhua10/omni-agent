package top.yumbo.ai.rag.mongodb;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.index.Index;
import org.springframework.data.mongodb.core.index.TextIndexDefinition;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.TextCriteria;
import org.springframework.data.mongodb.core.query.TextQuery;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.IndexStatistics;
import top.yumbo.ai.rag.api.model.SearchResult;

import jakarta.annotation.PostConstruct;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 基于 MongoDB 的 RAG 实现
 * (MongoDB-based RAG Implementation)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class MongoDBRAGService implements RAGService {

    private final MongoTemplate mongoTemplate;
    private final MongoDBRAGProperties properties;
    private final ObjectMapper objectMapper;

    public MongoDBRAGService(MongoTemplate mongoTemplate, MongoDBRAGProperties properties) {
        this.mongoTemplate = mongoTemplate;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();
    }

    @PostConstruct
    public void init() {
        try {
            // 创建集合
            if (!mongoTemplate.collectionExists(properties.getCollectionName())) {
                mongoTemplate.createCollection(properties.getCollectionName());
                log.info("MongoDB集合创建成功: {}", properties.getCollectionName());
            }

            // 创建索引
            createIndexes();

            log.info("MongoDB RAG Service 初始化成功");
        } catch (Exception e) {
            log.error("MongoDB RAG Service 初始化失败", e);
            throw new RuntimeException("初始化失败", e);
        }
    }

    private void createIndexes() {
        String collection = properties.getCollectionName();

        // 创建ID索引(唯一)
        mongoTemplate.indexOps(collection)
                .ensureIndex(new Index().on("id", Sort.Direction.ASC).unique());

        // 创建type索引
        mongoTemplate.indexOps(collection)
                .ensureIndex(new Index().on("type", Sort.Direction.ASC));

        // 创建source索引
        mongoTemplate.indexOps(collection)
                .ensureIndex(new Index().on("source", Sort.Direction.ASC));

        // 创建时间索引
        mongoTemplate.indexOps(collection)
                .ensureIndex(new Index().on("createdAt", Sort.Direction.DESC));

        // 创建文本索引(全文搜索)
        if (properties.isEnableTextSearch()) {
            TextIndexDefinition textIndex = TextIndexDefinition.builder()
                    .onField("title")
                    .onField("content")
                    .onField("summary")
                    .build();
            mongoTemplate.indexOps(collection).ensureIndex(textIndex);
            log.info("文本索引创建成功");
        }
    }

    // ========== 文档索引 ==========

    @Override
    public String indexDocument(Document document) {
        try {
            if (document.getId() == null || document.getId().isEmpty()) {
                document.setId(UUID.randomUUID().toString());
            }

            org.bson.Document mongoDoc = convertToMongoDoc(document);
            mongoDoc.put("createdAt", System.currentTimeMillis());
            mongoDoc.put("updatedAt", System.currentTimeMillis());

            mongoTemplate.save(mongoDoc, properties.getCollectionName());
            log.debug("文档索引成功: {}", document.getId());

            return document.getId();

        } catch (Exception e) {
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
        try {
            Query query = new Query(Criteria.where("id").is(document.getId()));

            if (!mongoTemplate.exists(query, properties.getCollectionName())) {
                log.warn("文档不存在: {}", document.getId());
                return false;
            }

            org.bson.Document mongoDoc = convertToMongoDoc(document);
            mongoDoc.put("updatedAt", System.currentTimeMillis());

            mongoTemplate.save(mongoDoc, properties.getCollectionName());
            log.debug("文档更新成功: {}", document.getId());

            return true;

        } catch (Exception e) {
            log.error("更新文档失败: {}", document.getId(), e);
            return false;
        }
    }

    @Override
    public boolean deleteDocument(String documentId) {
        try {
            Query query = new Query(Criteria.where("id").is(documentId));
            long deleted = mongoTemplate.remove(query, properties.getCollectionName()).getDeletedCount();

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
            mongoTemplate.dropCollection(properties.getCollectionName());
            mongoTemplate.createCollection(properties.getCollectionName());
            createIndexes();
            log.info("所有索引已清空");

        } catch (Exception e) {
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
        try {
            if (!properties.isEnableTextSearch()) {
                log.warn("文本搜索未启用");
                return Collections.emptyList();
            }

            TextCriteria criteria = TextCriteria.forDefaultLanguage().matching(text);
            Query query = TextQuery.queryText(criteria)
                    .sortByScore()
                    .limit(topK);

            List<org.bson.Document> results = mongoTemplate.find(
                    query,
                    org.bson.Document.class,
                    properties.getCollectionName()
            );

            return results.stream()
                    .map(doc -> {
                        Document document = convertFromMongoDoc(doc);
                        float score = 1.0f; // MongoDB文本搜索分数
                        return SearchResult.builder()
                                .document(document)
                                .score(score)
                                .textScore(score)
                                .build();
                    })
                    .collect(Collectors.toList());

        } catch (Exception e) {
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
    public List<SearchResult> vectorSearch(float[] embedding, int topK, Map<String, Object> filters) {
        try {
            Query query = new Query();

            // 应用过滤器
            if (filters != null && !filters.isEmpty()) {
                for (Map.Entry<String, Object> entry : filters.entrySet()) {
                    query.addCriteria(Criteria.where(entry.getKey()).is(entry.getValue()));
                }
            }

            // 查询所有符合条件的文档
            query.addCriteria(Criteria.where("embedding").exists(true));

            List<org.bson.Document> allDocs = mongoTemplate.find(
                    query,
                    org.bson.Document.class,
                    properties.getCollectionName()
            );

            // 计算余弦相似度
            List<SearchResult> results = new ArrayList<>();
            for (org.bson.Document mongoDoc : allDocs) {
                Document doc = convertFromMongoDoc(mongoDoc);

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

        } catch (Exception e) {
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
        try {
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

        } catch (Exception e) {
            log.error("混合检索失败", e);
            return Collections.emptyList();
        }
    }

    // ========== 语义搜索 ==========

    @Override
    public List<SearchResult> semanticSearch(String text, int topK) {
        // 需要集成Embedding服务才能实现
        log.warn("语义搜索需要Embedding服务支持");
        return searchByText(text, topK);
    }

    // ========== 文档管理 ==========

    @Override
    public Optional<Document> getDocument(String documentId) {
        try {
            Query query = new Query(Criteria.where("id").is(documentId));
            org.bson.Document mongoDoc = mongoTemplate.findOne(
                    query,
                    org.bson.Document.class,
                    properties.getCollectionName()
            );

            if (mongoDoc != null) {
                return Optional.of(convertFromMongoDoc(mongoDoc));
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
            Query query = new Query(Criteria.where("id").is(documentId));
            return mongoTemplate.exists(query, properties.getCollectionName());

        } catch (Exception e) {
            log.error("检查文档存在失败: {}", documentId, e);
            return false;
        }
    }

    @Override
    public long getDocumentCount() {
        try {
            return mongoTemplate.count(new Query(), properties.getCollectionName());

        } catch (Exception e) {
            log.error("获取文档总数失败", e);
            return 0;
        }
    }

    @Override
    public List<Document> getAllDocuments(int offset, int limit) {
        try {
            Query query = new Query()
                .with(org.springframework.data.domain.Sort.by(
                    org.springframework.data.domain.Sort.Direction.DESC, "createdAt"))
                .skip(offset)
                .limit(limit);

            List<org.bson.Document> mongoDocs = mongoTemplate.find(
                query, org.bson.Document.class, properties.getCollectionName());

            List<Document> documents = new ArrayList<>();
            for (org.bson.Document mongoDoc : mongoDocs) {
                Document document = convertFromMongoDoc(mongoDoc);
                documents.add(document);
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
                    .indexType("MongoDB")
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
        try {
            mongoTemplate.count(new Query().limit(1), properties.getCollectionName());
            return true;

        } catch (Exception e) {
            log.error("健康检查失败", e);
            return false;
        }
    }

    @Override
    public void rebuildIndex() {
        try {
            mongoTemplate.indexOps(properties.getCollectionName()).dropAllIndexes();
            createIndexes();
            log.info("索引重建成功");

        } catch (Exception e) {
            log.error("重建索引失败", e);
            throw new RuntimeException("重建索引失败", e);
        }
    }

    // ========== 工具方法 ==========

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

    private org.bson.Document convertToMongoDoc(Document document) {
        org.bson.Document mongoDoc = new org.bson.Document();

        mongoDoc.put("id", document.getId());
        mongoDoc.put("title", document.getTitle());
        mongoDoc.put("content", document.getContent());
        mongoDoc.put("summary", document.getSummary());
        mongoDoc.put("source", document.getSource());
        mongoDoc.put("type", document.getType());
        mongoDoc.put("author", document.getAuthor());
        mongoDoc.put("tags", document.getTags());
        mongoDoc.put("metadata", document.getMetadata());

        // 转换embedding
        if (document.getEmbedding() != null) {
            List<Float> embeddingList = new ArrayList<>();
            for (float value : document.getEmbedding()) {
                embeddingList.add(value);
            }
            mongoDoc.put("embedding", embeddingList);
        }

        return mongoDoc;
    }

    @SuppressWarnings("unchecked")
    private Document convertFromMongoDoc(org.bson.Document mongoDoc) {
        Document document = new Document();

        document.setId(mongoDoc.getString("id"));
        document.setTitle(mongoDoc.getString("title"));
        document.setContent(mongoDoc.getString("content"));
        document.setSummary(mongoDoc.getString("summary"));
        document.setSource(mongoDoc.getString("source"));
        document.setType(mongoDoc.getString("type"));
        document.setAuthor(mongoDoc.getString("author"));

        // 转换tags
        List<String> tags = (List<String>) mongoDoc.get("tags");
        if (tags != null) {
            document.setTags(tags);
        }

        // 转换metadata
        Map<String, Object> metadata = (Map<String, Object>) mongoDoc.get("metadata");
        if (metadata != null) {
            document.setMetadata(metadata);
        }

        // 转换embedding
        List<Number> embeddingList = (List<Number>) mongoDoc.get("embedding");
        if (embeddingList != null) {
            float[] embedding = new float[embeddingList.size()];
            for (int i = 0; i < embeddingList.size(); i++) {
                embedding[i] = embeddingList.get(i).floatValue();
            }
            document.setEmbedding(embedding);
        }

        return document;
    }
}
