package top.yumbo.ai.omni.core.hope.persistence.impl;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.core.hope.model.QuestionTypeConfig;
import top.yumbo.ai.omni.core.hope.persistence.HopePersistence;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 内存实现的 HOPE 持久化
 * 用于开发和测试环境，或作为后备方案
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class InMemoryHopePersistence implements HopePersistence {

    // 问题类型存储
    private final Map<String, QuestionTypeConfig> questionTypes = new ConcurrentHashMap<>();

    // 关键词存储
    private final Map<String, List<String>> keywords = new ConcurrentHashMap<>();

    // 模式存储
    private final Map<String, List<String>> patterns = new ConcurrentHashMap<>();

    public InMemoryHopePersistence() {
        log.info("📝 InMemoryHopePersistence initialized");
    }

    // ========== 问题类型管理 ==========

    @Override
    public boolean saveQuestionType(QuestionTypeConfig config) {
        if (config == null || config.getId() == null) {
            return false;
        }
        questionTypes.put(config.getId(), config);
        log.debug("💾 Saved question type: {}", config.getId());
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
        log.debug("💾 Batch saved {} question types", count);
        return count;
    }

    @Override
    public Optional<QuestionTypeConfig> getQuestionType(String typeId) {
        return Optional.ofNullable(questionTypes.get(typeId));
    }

    @Override
    public List<QuestionTypeConfig> getAllQuestionTypes() {
        return new ArrayList<>(questionTypes.values());
    }

    @Override
    public boolean updateQuestionType(QuestionTypeConfig config) {
        if (config == null || config.getId() == null) {
            return false;
        }
        if (!questionTypes.containsKey(config.getId())) {
            return false;
        }
        questionTypes.put(config.getId(), config);
        log.debug("✏️ Updated question type: {}", config.getId());
        return true;
    }

    @Override
    public boolean deleteQuestionType(String typeId) {
        if (typeId == null) {
            return false;
        }
        boolean removed = questionTypes.remove(typeId) != null;
        if (removed) {
            keywords.remove(typeId);
            patterns.remove(typeId);
            log.debug("🗑️ Deleted question type: {}", typeId);
        }
        return removed;
    }

    // ========== 关键词管理 ==========

    @Override
    public boolean saveKeywords(String typeId, List<String> keywordList) {
        if (typeId == null || keywordList == null) {
            return false;
        }
        keywords.put(typeId, new ArrayList<>(keywordList));
        log.debug("💾 Saved {} keywords for type: {}", keywordList.size(), typeId);
        return true;
    }

    @Override
    public boolean addKeywords(String typeId, List<String> keywordList) {
        if (typeId == null || keywordList == null) {
            return false;
        }
        List<String> existing = keywords.computeIfAbsent(typeId, k -> new ArrayList<>());
        existing.addAll(keywordList);
        log.debug("➕ Added {} keywords to type: {}", keywordList.size(), typeId);
        return true;
    }

    @Override
    public List<String> getKeywords(String typeId) {
        List<String> result = keywords.get(typeId);
        return result != null ? new ArrayList<>(result) : new ArrayList<>();
    }

    @Override
    public boolean removeKeywords(String typeId, List<String> keywordList) {
        if (typeId == null || keywordList == null) {
            return false;
        }
        List<String> existing = keywords.get(typeId);
        if (existing != null) {
            existing.removeAll(keywordList);
            log.debug("➖ Removed {} keywords from type: {}", keywordList.size(), typeId);
            return true;
        }
        return false;
    }

    // ========== 模式管理 ==========

    @Override
    public boolean savePatterns(String typeId, List<String> patternList) {
        if (typeId == null || patternList == null) {
            return false;
        }
        patterns.put(typeId, new ArrayList<>(patternList));
        log.debug("💾 Saved {} patterns for type: {}", patternList.size(), typeId);
        return true;
    }

    @Override
    public boolean addPatterns(String typeId, List<String> patternList) {
        if (typeId == null || patternList == null) {
            return false;
        }
        List<String> existing = patterns.computeIfAbsent(typeId, k -> new ArrayList<>());
        existing.addAll(patternList);
        log.debug("➕ Added {} patterns to type: {}", patternList.size(), typeId);
        return true;
    }

    @Override
    public List<String> getPatterns(String typeId) {
        List<String> result = patterns.get(typeId);
        return result != null ? new ArrayList<>(result) : new ArrayList<>();
    }

    @Override
    public boolean removePatterns(String typeId, List<String> patternList) {
        if (typeId == null || patternList == null) {
            return false;
        }
        List<String> existing = patterns.get(typeId);
        if (existing != null) {
            existing.removeAll(patternList);
            log.debug("➖ Removed {} patterns from type: {}", patternList.size(), typeId);
            return true;
        }
        return false;
    }

    /**
     * 清空所有数据（用于测试）
     */
    public void clear() {
        questionTypes.clear();
        keywords.clear();
        patterns.clear();
        log.info("🧹 Cleared all in-memory data");
    }

    /**
     * 获取统计信息
     */
    public Map<String, Integer> getStats() {
        Map<String, Integer> stats = new HashMap<>();
        stats.put("questionTypes", questionTypes.size());
        stats.put("keywords", keywords.values().stream().mapToInt(List::size).sum());
        stats.put("patterns", patterns.values().stream().mapToInt(List::size).sum());
        return stats;
    }
}

