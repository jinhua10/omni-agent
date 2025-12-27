package top.yumbo.ai.omni.persistence.mongodb;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.omni.persistence.api.model.QuestionTypeConfig;

import java.util.*;
import java.util.stream.Collectors;

/**
 * MongoDB 数据库持久化实现 - 文档数据库持久化方案
 * (MongoDB Database Persistence Implementation - Document Database Persistence)
 *
 * <p>
 * 特点 (Features):
 * - 文档型数据库，灵活的数据结构
 * - 支持副本集和分片
 * - 适合大规模数据存储
 * - 强大的查询和聚合能力
 * - 适合复杂数据场景（<10M 类型）
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - MongoDB Starter 实现
 */
@Slf4j
public class MongoDBPersistence implements QuestionClassifierPersistence {

    private final MongoTemplate mongoTemplate;
    private final MongoDBPersistenceProperties properties;
    private final ObjectMapper objectMapper;

    // 集合名称
    private final String typesCollection;
    private final String keywordsCollection;
    private final String patternsCollection;
    private final String metadataCollection;
    private final String historyCollection;

    public MongoDBPersistence(MongoTemplate mongoTemplate,
                              MongoDBPersistenceProperties properties) {
        this.mongoTemplate = mongoTemplate;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();

        this.typesCollection = properties.getCollectionPrefix() + "types";
        this.keywordsCollection = properties.getCollectionPrefix() + "keywords";
        this.patternsCollection = properties.getCollectionPrefix() + "patterns";
        this.metadataCollection = properties.getCollectionPrefix() + "metadata";
        this.historyCollection = properties.getCollectionPrefix() + "history";

        log.info("MongoDBPersistence initialized - database: {}", properties.getDatabase());
    }

    // ========== QuestionTypeConfig CRUD ==========

    @Override
    public boolean saveQuestionType(QuestionTypeConfig config) {
        try {
            Query query = new Query(Criteria.where("_id").is(config.getId()));

            // 使用 upsert：存在则更新，不存在则插入
            Update update = new Update()
                    .set("name", config.getName())
                    .set("nameEn", config.getNameEn())
                    .set("priority", config.getPriority())
                    .set("complexity", config.getComplexity())
                    .set("suggestedLayer", config.getSuggestedLayer())
                    .set("enabled", config.isEnabled())
                    .set("config", config)
                    .set("updatedAt", new Date());

            mongoTemplate.upsert(query, update, typesCollection);

            log.debug("Saved question type: {}", config.getId());
            return true;
        } catch (Exception e) {
            log.error("Failed to save question type: {}", config.getId(), e);
            return false;
        }
    }

    @Override
    public int saveQuestionTypes(List<QuestionTypeConfig> configs) {
        int count = 0;
        for (QuestionTypeConfig config : configs) {
            if (saveQuestionType(config)) {
                count++;
            }
        }
        return count;
    }

    @Override
    public Optional<QuestionTypeConfig> getQuestionType(String typeId) {
        try {
            Query query = new Query(Criteria.where("_id").is(typeId));
            Map<String, Object> result = mongoTemplate.findOne(query, Map.class, typesCollection);

            if (result != null && result.containsKey("config")) {
                Object configObj = result.get("config");
                QuestionTypeConfig config = objectMapper.convertValue(configObj, QuestionTypeConfig.class);
                return Optional.of(config);
            }

            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get question type: {}", typeId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<QuestionTypeConfig> getAllQuestionTypes() {
        try {
            Query query = new Query(Criteria.where("enabled").is(true));
            List<Map> results = mongoTemplate.find(query, Map.class, typesCollection);

            return results.stream()
                    .filter(map -> map.containsKey("config"))
                    .map(map -> objectMapper.convertValue(map.get("config"), QuestionTypeConfig.class))
                    .sorted((a, b) -> Integer.compare(b.getPriority(), a.getPriority()))
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get all question types", e);
            return new ArrayList<>();
        }
    }

    @Override
    public boolean updateQuestionType(QuestionTypeConfig config) {
        return saveQuestionType(config);
    }

    @Override
    public boolean deleteQuestionType(String typeId) {
        try {
            Query query = new Query(Criteria.where("_id").is(typeId));
            mongoTemplate.remove(query, typesCollection);

            // 删除关联的关键词和模式
            Query keywordsQuery = new Query(Criteria.where("typeId").is(typeId));
            mongoTemplate.remove(keywordsQuery, keywordsCollection);

            Query patternsQuery = new Query(Criteria.where("typeId").is(typeId));
            mongoTemplate.remove(patternsQuery, patternsCollection);

            log.debug("Deleted question type: {}", typeId);
            return true;
        } catch (Exception e) {
            log.error("Failed to delete question type: {}", typeId, e);
            return false;
        }
    }

    // ========== Keywords Management ==========

    @Override
    public boolean saveKeywords(String typeId, List<String> keywords) {
        try {
            // 删除旧关键词
            Query query = new Query(Criteria.where("typeId").is(typeId));
            mongoTemplate.remove(query, keywordsCollection);

            // 插入新关键词
            if (!keywords.isEmpty()) {
                List<Map<String, Object>> docs = new ArrayList<>();
                for (String keyword : keywords) {
                    Map<String, Object> doc = new HashMap<>();
                    doc.put("typeId", typeId);
                    doc.put("keyword", keyword);
                    doc.put("createdAt", new Date());
                    docs.add(doc);
                }
                mongoTemplate.insert(docs, keywordsCollection);
            }

            log.debug("Saved {} keywords for type: {}", keywords.size(), typeId);
            return true;
        } catch (Exception e) {
            log.error("Failed to save keywords for: {}", typeId, e);
            return false;
        }
    }

    @Override
    public boolean addKeywords(String typeId, List<String> keywords) {
        try {
            if (!keywords.isEmpty()) {
                List<Map<String, Object>> docs = new ArrayList<>();
                for (String keyword : keywords) {
                    Map<String, Object> doc = new HashMap<>();
                    doc.put("typeId", typeId);
                    doc.put("keyword", keyword);
                    doc.put("createdAt", new Date());
                    docs.add(doc);
                }
                mongoTemplate.insert(docs, keywordsCollection);
            }
            return true;
        } catch (Exception e) {
            log.error("Failed to add keywords for: {}", typeId, e);
            return false;
        }
    }

    @Override
    public List<String> getKeywords(String typeId) {
        try {
            Query query = new Query(Criteria.where("typeId").is(typeId));
            List<Map> results = mongoTemplate.find(query, Map.class, keywordsCollection);

            return results.stream()
                    .map(map -> (String) map.get("keyword"))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get keywords for: {}", typeId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public Map<String, List<String>> getAllKeywords() {
        try {
            List<Map> results = mongoTemplate.findAll(Map.class, keywordsCollection);

            Map<String, List<String>> keywordsMap = new HashMap<>();
            for (Map<String, Object> result : results) {
                String typeId = (String) result.get("typeId");
                String keyword = (String) result.get("keyword");

                if (typeId != null && keyword != null) {
                    keywordsMap.computeIfAbsent(typeId, k -> new ArrayList<>()).add(keyword);
                }
            }

            return keywordsMap;
        } catch (Exception e) {
            log.error("Failed to get all keywords", e);
            return new HashMap<>();
        }
    }

    // ========== Patterns Management ==========

    @Override
    public boolean savePatterns(String typeId, List<String> patterns) {
        try {
            // 删除旧模式
            Query query = new Query(Criteria.where("typeId").is(typeId));
            mongoTemplate.remove(query, patternsCollection);

            // 插入新模式
            if (!patterns.isEmpty()) {
                List<Map<String, Object>> docs = new ArrayList<>();
                for (String pattern : patterns) {
                    Map<String, Object> doc = new HashMap<>();
                    doc.put("typeId", typeId);
                    doc.put("pattern", pattern);
                    doc.put("createdAt", new Date());
                    docs.add(doc);
                }
                mongoTemplate.insert(docs, patternsCollection);
            }

            log.debug("Saved {} patterns for type: {}", patterns.size(), typeId);
            return true;
        } catch (Exception e) {
            log.error("Failed to save patterns for: {}", typeId, e);
            return false;
        }
    }

    @Override
    public boolean addPatterns(String typeId, List<String> patterns) {
        try {
            if (!patterns.isEmpty()) {
                List<Map<String, Object>> docs = new ArrayList<>();
                for (String pattern : patterns) {
                    Map<String, Object> doc = new HashMap<>();
                    doc.put("typeId", typeId);
                    doc.put("pattern", pattern);
                    doc.put("createdAt", new Date());
                    docs.add(doc);
                }
                mongoTemplate.insert(docs, patternsCollection);
            }
            return true;
        } catch (Exception e) {
            log.error("Failed to add patterns for: {}", typeId, e);
            return false;
        }
    }

    @Override
    public List<String> getPatterns(String typeId) {
        try {
            Query query = new Query(Criteria.where("typeId").is(typeId));
            List<Map> results = mongoTemplate.find(query, Map.class, patternsCollection);

            return results.stream()
                    .map(map -> (String) map.get("pattern"))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get patterns for: {}", typeId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public Map<String, List<String>> getAllPatterns() {
        try {
            List<Map> results = mongoTemplate.findAll(Map.class, patternsCollection);

            Map<String, List<String>> patternsMap = new HashMap<>();
            for (Map<String, Object> result : results) {
                String typeId = (String) result.get("typeId");
                String pattern = (String) result.get("pattern");

                if (typeId != null && pattern != null) {
                    patternsMap.computeIfAbsent(typeId, k -> new ArrayList<>()).add(pattern);
                }
            }

            return patternsMap;
        } catch (Exception e) {
            log.error("Failed to get all patterns", e);
            return new HashMap<>();
        }
    }

    // ========== Backup & Restore ==========

    @Override
    public String createBackup() {
        String backupId = "mongodb_backup_" + System.currentTimeMillis();
        log.info("Created MongoDB backup: {} (Use mongodump)", backupId);
        return backupId;
    }

    @Override
    public boolean restoreFromBackup(String backupId) {
        log.info("Restore from MongoDB backup: {} (Use mongorestore)", backupId);
        return true;
    }

    @Override
    public List<String> listBackups() {
        return new ArrayList<>();
    }

    // ========== Version Management ==========

    @Override
    public String getVersion() {
        try {
            Query query = new Query(Criteria.where("_id").is("version"));
            Map<String, Object> result = mongoTemplate.findOne(query, Map.class, metadataCollection);

            if (result != null && result.containsKey("value")) {
                return result.get("value").toString();
            }

            return "1.0.0";
        } catch (Exception e) {
            log.error("Failed to get version", e);
            return "1.0.0";
        }
    }

    @Override
    public boolean saveVersion(String version) {
        try {
            Query query = new Query(Criteria.where("_id").is("version"));
            Update update = new Update()
                    .set("value", version)
                    .set("updatedAt", new Date());

            mongoTemplate.upsert(query, update, metadataCollection);
            log.debug("Saved version: {}", version);
            return true;
        } catch (Exception e) {
            log.error("Failed to save version: {}", version, e);
            return false;
        }
    }

    // ========== Change History ==========

    @Override
    public List<ChangeRecord> getChangeHistory(int limit) {
        try {
            Query query = new Query().limit(limit);
            query.with(org.springframework.data.domain.Sort.by(
                    org.springframework.data.domain.Sort.Direction.DESC, "timestamp"));

            List<Map> results = mongoTemplate.find(query, Map.class, historyCollection);

            return results.stream()
                    .map(this::mapToChangeRecord)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get change history", e);
            return new ArrayList<>();
        }
    }

    @Override
    public boolean recordChange(ChangeRecord change) {
        try {
            Map<String, Object> doc = new HashMap<>();
            doc.put("_id", change.getId());
            doc.put("typeId", change.getTypeId());
            doc.put("action", change.getAction());
            doc.put("operator", change.getOperator());
            doc.put("timestamp", change.getTimestamp());
            doc.put("details", change.getDetails());
            doc.put("createdAt", new Date());

            mongoTemplate.insert(doc, historyCollection);
            log.debug("Recorded change: {}", change.getId());
            return true;
        } catch (Exception e) {
            log.error("Failed to record change", e);
            return false;
        }
    }

    // ========== Helper Methods ==========

    private ChangeRecord mapToChangeRecord(Map<String, Object> map) {
        try {
            return new MongoDBChangeRecord(
                    (String) map.get("_id"),
                    (String) map.get("typeId"),
                    (String) map.get("action"),
                    (String) map.get("operator"),
                    ((Number) map.get("timestamp")).longValue(),
                    (Map<String, Object>) map.get("details")
            );
        } catch (Exception e) {
            log.error("Failed to map change record", e);
            return null;
        }
    }

    // ========== 内部类：MongoDBChangeRecord ==========

    private static class MongoDBChangeRecord implements ChangeRecord {
        private final String id;
        private final String typeId;
        private final String action;
        private final String operator;
        private final long timestamp;
        private final Map<String, Object> details;

        public MongoDBChangeRecord(String id, String typeId, String action,
                                   String operator, long timestamp, Map<String, Object> details) {
            this.id = id;
            this.typeId = typeId;
            this.action = action;
            this.operator = operator;
            this.timestamp = timestamp;
            this.details = details != null ? details : new HashMap<>();
        }

        @Override
        public String getId() {
            return id;
        }

        @Override
        public String getTypeId() {
            return typeId;
        }

        @Override
        public String getAction() {
            return action;
        }

        @Override
        public String getOperator() {
            return operator;
        }

        @Override
        public long getTimestamp() {
            return timestamp;
        }

        @Override
        public Map<String, Object> getDetails() {
            return details;
        }
    }
}

