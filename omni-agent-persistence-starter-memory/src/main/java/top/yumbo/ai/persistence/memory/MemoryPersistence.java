package top.yumbo.ai.persistence.memory;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.persistence.api.model.QuestionTypeConfig;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Memory 持久化实现 - 纯内存存储（用于开发和测试）
 * (Memory Persistence Implementation - Pure in-memory storage for development and testing)
 *
 * <p>
 * 特点 (Features):
 * - 纯内存存储，无需外部依赖
 * - 快速启动，适合开发测试
 * - 数据不持久化，重启后丢失
 * - 线程安全（使用 ConcurrentHashMap）
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - Memory Starter 实现
 */
@Slf4j
public class MemoryPersistence implements QuestionClassifierPersistence {

    // 问题类型配置存储
    private final Map<String, QuestionTypeConfig> typeConfigs = new ConcurrentHashMap<>();

    // 关键词存储：typeId -> keywords
    private final Map<String, List<String>> keywords = new ConcurrentHashMap<>();

    // 模式存储：typeId -> patterns
    private final Map<String, List<String>> patterns = new ConcurrentHashMap<>();

    public MemoryPersistence() {
        log.info("MemoryPersistence initialized - All data will be stored in memory");
    }

    @Override
    public boolean saveQuestionType(QuestionTypeConfig config) {
        if (config == null || config.getId() == null) {
            return false;
        }
        typeConfigs.put(config.getId(), config);
        log.debug("Saved question type: {}", config.getId());
        return true;
    }

    @Override
    public Optional<QuestionTypeConfig> getQuestionType(String typeId) {
        return Optional.ofNullable(typeConfigs.get(typeId));
    }

    @Override
    public List<QuestionTypeConfig> getAllQuestionTypes() {
        return new ArrayList<>(typeConfigs.values());
    }

    @Override
    public boolean deleteQuestionType(String typeId) {
        QuestionTypeConfig removed = typeConfigs.remove(typeId);
        if (removed != null) {
            keywords.remove(typeId);
            patterns.remove(typeId);
            log.debug("Deleted question type: {}", typeId);
            return true;
        }
        return false;
    }

    @Override
    public boolean updateQuestionType(QuestionTypeConfig config) {
        if (config == null || config.getId() == null) {
            return false;
        }
        if (!typeConfigs.containsKey(config.getId())) {
            return false;
        }
        typeConfigs.put(config.getId(), config);
        log.debug("Updated question type: {}", config.getId());
        return true;
    }

    @Override
    public int saveQuestionTypes(List<QuestionTypeConfig> configs) {
        if (configs == null) {
            return 0;
        }
        int count = 0;
        for (QuestionTypeConfig config : configs) {
            if (saveQuestionType(config)) {
                count++;
            }
        }
        return count;
    }

    @Override
    public boolean saveKeywords(String typeId, List<String> keywordList) {
        if (typeId == null || keywordList == null) {
            return false;
        }
        keywords.put(typeId, new ArrayList<>(keywordList));
        log.debug("Saved {} keywords for type: {}", keywordList.size(), typeId);
        return true;
    }

    @Override
    public boolean addKeywords(String typeId, List<String> newKeywords) {
        if (typeId == null || newKeywords == null || newKeywords.isEmpty()) {
            return false;
        }
        List<String> existing = keywords.computeIfAbsent(typeId, k -> new ArrayList<>());
        existing.addAll(newKeywords);
        log.debug("Added {} keywords to type: {}", newKeywords.size(), typeId);
        return true;
    }

    @Override
    public List<String> getKeywords(String typeId) {
        return keywords.getOrDefault(typeId, new ArrayList<>());
    }

    @Override
    public Map<String, List<String>> getAllKeywords() {
        return new HashMap<>(keywords);
    }

    @Override
    public boolean savePatterns(String typeId, List<String> patternList) {
        if (typeId == null || patternList == null) {
            return false;
        }
        patterns.put(typeId, new ArrayList<>(patternList));
        log.debug("Saved {} patterns for type: {}", patternList.size(), typeId);
        return true;
    }

    @Override
    public boolean addPatterns(String typeId, List<String> newPatterns) {
        if (typeId == null || newPatterns == null || newPatterns.isEmpty()) {
            return false;
        }
        List<String> existing = patterns.computeIfAbsent(typeId, k -> new ArrayList<>());
        existing.addAll(newPatterns);
        log.debug("Added {} patterns to type: {}", newPatterns.size(), typeId);
        return true;
    }

    @Override
    public List<String> getPatterns(String typeId) {
        return patterns.getOrDefault(typeId, new ArrayList<>());
    }

    @Override
    public Map<String, List<String>> getAllPatterns() {
        return new HashMap<>(patterns);
    }

    @Override
    public String createBackup() {
        log.warn("Memory persistence does not support backup");
        return null;
    }

    @Override
    public boolean restoreFromBackup(String backupId) {
        log.warn("Memory persistence does not support restore");
        return false;
    }

    @Override
    public List<String> listBackups() {
        return new ArrayList<>();
    }

    @Override
    public String getVersion() {
        return "1.0.0-memory";
    }

    @Override
    public boolean saveVersion(String version) {
        log.debug("Version save not supported in memory mode");
        return false;
    }

    @Override
    public List<ChangeRecord> getChangeHistory(int limit) {
        return new ArrayList<>(); // Memory mode doesn't store history
    }

    @Override
    public boolean recordChange(ChangeRecord change) {
        log.debug("Change recorded (not persisted): {}", change);
        return true; // Memory mode doesn't persist change history
    }
}


