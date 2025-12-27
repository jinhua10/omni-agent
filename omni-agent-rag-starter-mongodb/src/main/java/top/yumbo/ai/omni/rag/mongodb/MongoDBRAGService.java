package top.yumbo.ai.omni.rag.mongodb;

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
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;
import top.yumbo.ai.omni.rag.model.IndexStatistics;

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
public class MongoDBRAGService implements RagService {

    private final MongoTemplate mongoTemplate;
    private final MongoDBRAGProperties properties;
    private final ObjectMapper objectMapper;
    private final String domainId;

    public MongoDBRAGService(MongoTemplate mongoTemplate, MongoDBRAGProperties properties) {
        this(mongoTemplate, properties, "mongodb-domain");
    }

    public MongoDBRAGService(MongoTemplate mongoTemplate, MongoDBRAGProperties properties, String domainId) {
        this.mongoTemplate = mongoTemplate;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();
        this.domainId = domainId;
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
        log.warn("MongoDBRAGService 不提供嵌入功能");
        return null;
    }

    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        log.warn("MongoDBRAGService 不提供批量嵌入功能");
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
            if (properties.isEnableTextSearch()) {
                TextCriteria criteria = TextCriteria.forDefaultLanguage().matchingAny(text);
                Query query = TextQuery.queryText(criteria)
                        .sortByScore()
                        .limit(maxResults);
                return mongoTemplate.find(query, org.bson.Document.class, properties.getCollectionName())
                        .stream()
                        .map(this::convertFromMongoDoc)
                        .collect(Collectors.toList());
            } else {
                Query query = new Query();
                Criteria criteria = new Criteria().orOperator(
                        Criteria.where("title").regex(text, "i"),
                        Criteria.where("content").regex(text, "i"),
                        Criteria.where("summary").regex(text, "i")
                );
                query.addCriteria(criteria).limit(maxResults);
                return mongoTemplate.find(query, org.bson.Document.class, properties.getCollectionName())
                        .stream()
                        .map(this::convertFromMongoDoc)
                        .collect(Collectors.toList());
            }
        } catch (Exception e) {
            log.error("文本搜索失败: {}", text, e);
            return Collections.emptyList();
        }
    }

    private List<Document> vectorSearchInternal(float[] embedding, int maxResults) {
        try {
            Query query = new Query(Criteria.where("embedding").exists(true));
            List<org.bson.Document> mongoDocs = mongoTemplate.find(query, org.bson.Document.class, properties.getCollectionName());

            List<Document> results = new ArrayList<>();
            for (org.bson.Document mongoDoc : mongoDocs) {
                Document doc = convertFromMongoDoc(mongoDoc);
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
                    .limit(maxResults)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("向量搜索失败", e);
            return Collections.emptyList();
        }
    }

    // ========== 辅助方法 ==========

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
}
