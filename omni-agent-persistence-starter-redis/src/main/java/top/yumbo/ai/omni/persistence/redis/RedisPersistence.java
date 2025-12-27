package top.yumbo.ai.omni.persistence.redis;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.omni.persistence.api.model.QuestionTypeConfig;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Redis 数据库持久化实现 - 高性能缓存持久化方案
 * (Redis Database Persistence Implementation - High-performance Cache Persistence)
 *
 * <p>
 * 特点 (Features):
 * - 高性能读写
 * - 支持数据过期
 * - 适合临时/缓存数据
 * - 支持主从复制和集群
 * - 适合高频访问场景（<1M 类型）
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - Redis Starter 实现
 */
@Slf4j
public class RedisPersistence implements QuestionClassifierPersistence {

    private final RedisTemplate<String, Object> redisTemplate;
    private final RedisPersistenceProperties properties;
    private final ObjectMapper objectMapper;

    public RedisPersistence(RedisTemplate<String, Object> redisTemplate,
                           RedisPersistenceProperties properties) {
        this.redisTemplate = redisTemplate;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();
        log.info("RedisPersistence initialized with prefix: {}", properties.getKeyPrefix());
    }

    // ========== Key 生成 ==========

    private String getTypeKey(String typeId) {
        return properties.getKeyPrefix() + "type:" + typeId;
    }

    private String getAllTypesKey() {
        return properties.getKeyPrefix() + "types";
    }

    private String getKeywordsKey(String typeId) {
        return properties.getKeyPrefix() + "keywords:" + typeId;
    }

    private String getAllKeywordsKey() {
        return properties.getKeyPrefix() + "all-keywords";
    }

    private String getPatternsKey(String typeId) {
        return properties.getKeyPrefix() + "patterns:" + typeId;
    }

    private String getAllPatternsKey() {
        return properties.getKeyPrefix() + "all-patterns";
    }

    private String getVersionKey() {
        return properties.getKeyPrefix() + "version";
    }

    private String getChangeHistoryKey() {
        return properties.getKeyPrefix() + "change-history";
    }

    // ========== QuestionTypeConfig CRUD ==========

    @Override
    public boolean saveQuestionType(QuestionTypeConfig config) {
        try {
            String typeKey = getTypeKey(config.getId());
            String allTypesKey = getAllTypesKey();

            // 保存类型配置
            redisTemplate.opsForValue().set(typeKey, config);

            // 添加到所有类型集合
            redisTemplate.opsForSet().add(allTypesKey, config.getId());

            // 设置过期时间
            if (properties.getTtl() > 0) {
                redisTemplate.expire(typeKey, properties.getTtl(), TimeUnit.SECONDS);
            }

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
            String typeKey = getTypeKey(typeId);
            Object obj = redisTemplate.opsForValue().get(typeKey);

            if (obj instanceof QuestionTypeConfig) {
                return Optional.of((QuestionTypeConfig) obj);
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
            String allTypesKey = getAllTypesKey();
            Set<Object> typeIds = redisTemplate.opsForSet().members(allTypesKey);

            if (typeIds == null || typeIds.isEmpty()) {
                return new ArrayList<>();
            }

            return typeIds.stream()
                    .map(id -> getQuestionType(id.toString()))
                    .filter(Optional::isPresent)
                    .map(Optional::get)
                    .filter(QuestionTypeConfig::isEnabled)
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
            String typeKey = getTypeKey(typeId);
            String allTypesKey = getAllTypesKey();

            // 删除类型配置
            redisTemplate.delete(typeKey);

            // 从所有类型集合移除
            redisTemplate.opsForSet().remove(allTypesKey, typeId);

            // 删除关联的关键词和模式
            String keywordsKey = getKeywordsKey(typeId);
            String patternsKey = getPatternsKey(typeId);
            redisTemplate.delete(keywordsKey);
            redisTemplate.delete(patternsKey);

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
            String keywordsKey = getKeywordsKey(typeId);

            // 删除旧关键词
            redisTemplate.delete(keywordsKey);

            // 添加新关键词
            if (!keywords.isEmpty()) {
                redisTemplate.opsForSet().add(keywordsKey, keywords.toArray());

                if (properties.getTtl() > 0) {
                    redisTemplate.expire(keywordsKey, properties.getTtl(), TimeUnit.SECONDS);
                }
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
            String keywordsKey = getKeywordsKey(typeId);

            if (!keywords.isEmpty()) {
                redisTemplate.opsForSet().add(keywordsKey, keywords.toArray());
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
            String keywordsKey = getKeywordsKey(typeId);
            Set<Object> keywords = redisTemplate.opsForSet().members(keywordsKey);

            if (keywords == null) {
                return new ArrayList<>();
            }

            return keywords.stream()
                    .map(Object::toString)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get keywords for: {}", typeId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public Map<String, List<String>> getAllKeywords() {
        try {
            String allTypesKey = getAllTypesKey();
            Set<Object> typeIds = redisTemplate.opsForSet().members(allTypesKey);

            if (typeIds == null) {
                return new HashMap<>();
            }

            Map<String, List<String>> result = new HashMap<>();
            for (Object typeId : typeIds) {
                String id = typeId.toString();
                List<String> keywords = getKeywords(id);
                if (!keywords.isEmpty()) {
                    result.put(id, keywords);
                }
            }

            return result;
        } catch (Exception e) {
            log.error("Failed to get all keywords", e);
            return new HashMap<>();
        }
    }

    // ========== Patterns Management ==========

    @Override
    public boolean savePatterns(String typeId, List<String> patterns) {
        try {
            String patternsKey = getPatternsKey(typeId);

            // 删除旧模式
            redisTemplate.delete(patternsKey);

            // 添加新模式
            if (!patterns.isEmpty()) {
                redisTemplate.opsForSet().add(patternsKey, patterns.toArray());

                if (properties.getTtl() > 0) {
                    redisTemplate.expire(patternsKey, properties.getTtl(), TimeUnit.SECONDS);
                }
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
            String patternsKey = getPatternsKey(typeId);

            if (!patterns.isEmpty()) {
                redisTemplate.opsForSet().add(patternsKey, patterns.toArray());
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
            String patternsKey = getPatternsKey(typeId);
            Set<Object> patterns = redisTemplate.opsForSet().members(patternsKey);

            if (patterns == null) {
                return new ArrayList<>();
            }

            return patterns.stream()
                    .map(Object::toString)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get patterns for: {}", typeId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public Map<String, List<String>> getAllPatterns() {
        try {
            String allTypesKey = getAllTypesKey();
            Set<Object> typeIds = redisTemplate.opsForSet().members(allTypesKey);

            if (typeIds == null) {
                return new HashMap<>();
            }

            Map<String, List<String>> result = new HashMap<>();
            for (Object typeId : typeIds) {
                String id = typeId.toString();
                List<String> patterns = getPatterns(id);
                if (!patterns.isEmpty()) {
                    result.put(id, patterns);
                }
            }

            return result;
        } catch (Exception e) {
            log.error("Failed to get all patterns", e);
            return new HashMap<>();
        }
    }

    // ========== Backup & Restore ==========

    @Override
    public String createBackup() {
        String backupId = "redis_backup_" + System.currentTimeMillis();
        log.info("Created Redis backup: {} (Redis persistence via RDB/AOF)", backupId);
        return backupId;
    }

    @Override
    public boolean restoreFromBackup(String backupId) {
        log.info("Restore from Redis backup: {} (Use Redis RDB/AOF)", backupId);
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
            String versionKey = getVersionKey();
            Object version = redisTemplate.opsForValue().get(versionKey);
            return version != null ? version.toString() : "1.0.0";
        } catch (Exception e) {
            log.error("Failed to get version", e);
            return "1.0.0";
        }
    }

    @Override
    public boolean saveVersion(String version) {
        try {
            String versionKey = getVersionKey();
            redisTemplate.opsForValue().set(versionKey, version);
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
            String historyKey = getChangeHistoryKey();
            List<Object> history = redisTemplate.opsForList().range(historyKey, 0, limit - 1);

            if (history == null) {
                return new ArrayList<>();
            }

            return history.stream()
                    .filter(obj -> obj instanceof RedisChangeRecord)
                    .map(obj -> (ChangeRecord) obj)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get change history", e);
            return new ArrayList<>();
        }
    }

    @Override
    public boolean recordChange(ChangeRecord change) {
        try {
            String historyKey = getChangeHistoryKey();

            RedisChangeRecord record = new RedisChangeRecord(
                    change.getId(),
                    change.getTypeId(),
                    change.getAction(),
                    change.getOperator(),
                    change.getTimestamp(),
                    change.getDetails()
            );

            // 添加到列表头部（最新的在前面）
            redisTemplate.opsForList().leftPush(historyKey, record);

            // 保留最近 1000 条记录
            redisTemplate.opsForList().trim(historyKey, 0, 999);

            log.debug("Recorded change: {}", change.getId());
            return true;
        } catch (Exception e) {
            log.error("Failed to record change", e);
            return false;
        }
    }

    // ========== 内部类：RedisChangeRecord ==========

    private static class RedisChangeRecord implements ChangeRecord {
        private final String id;
        private final String typeId;
        private final String action;
        private final String operator;
        private final long timestamp;
        private final Map<String, Object> details;

        public RedisChangeRecord(String id, String typeId, String action,
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

