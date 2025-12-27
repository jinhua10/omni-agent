package top.yumbo.ai.omni.core.hope.learning;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.omni.persistence.api.model.QuestionTypeConfig;

import java.util.*;

/**
 * 问题分类器学习服务 - 从用户交互中学习和优化分类规则
 * (Question Classifier Learning Service - Learn and optimize classification rules from user interactions)
 *
 * <p>
 * 重构说明 (Refactoring Notes):
 * - 使用 QuestionClassifierPersistence 接口存储学习结果
 * - 支持从用户反馈中学习
 * - 动态更新分类规则和关键词
 * - 完全可插拔的持久化后端
 * </p>
 *
 * 特点 (Features):
 * - 从用户反馈学习 (Learn from user feedback)
 * - 动态更新关键词库 (Dynamically update keyword library)
 * - 自动优化分类规则 (Automatically optimize classification rules)
 * - 支持增量学习 (Support incremental learning)
 *
 * @author OmniAgent Team
 * @since 1.0.0 (Refactored)
 * @version 3.0.0 - 重构为可插拔架构
 */
@Slf4j
@Service
public class QuestionClassifierLearningService {

    private final QuestionClassifierPersistence persistence;

    // 学习缓存 - 收集一批后统一学习
    private final Map<String, LearningRecord> learningCache = new LinkedHashMap<>();

    // 缓存大小阈值
    private static final int CACHE_THRESHOLD = 100;

    /**
     * 构造函数 - Spring 自动注入持久化接口
     *
     * @param persistence 持久化接口
     */
    @Autowired
    public QuestionClassifierLearningService(QuestionClassifierPersistence persistence) {
        this.persistence = persistence;
        log.info("QuestionClassifierLearningService initialized with persistence: {}",
                 persistence.getClass().getSimpleName());
    }

    /**
     * 记录分类结果用于学习
     *
     * @param question 问题
     * @param predictedType 预测的类型
     * @param actualType 实际的类型（用户反馈或验证后）
     * @param confidence 置信度
     */
    public void recordClassification(String question, String predictedType,
                                    String actualType, double confidence) {
        if (question == null || question.trim().isEmpty()) {
            return;
        }

        // 创建学习记录
        LearningRecord record = new LearningRecord(
            question, predictedType, actualType, confidence, System.currentTimeMillis()
        );

        // 添加到缓存
        learningCache.put(UUID.randomUUID().toString(), record);

        // 如果缓存达到阈值，触发学习
        if (learningCache.size() >= CACHE_THRESHOLD) {
            performLearning();
        }

        log.debug("Recorded classification: question={}, predicted={}, actual={}",
                 question, predictedType, actualType);
    }

    /**
     * 从用户反馈学习
     *
     * @param question 问题
     * @param correctType 正确的类型（用户反馈）
     */
    public void learnFromFeedback(String question, String correctType) {
        if (question == null || correctType == null) {
            return;
        }

        try {
            // 提取关键词
            List<String> keywords = extractKeywords(question);

            // 获取类型配置
            Optional<QuestionTypeConfig> configOpt = persistence.getQuestionType(correctType);

            if (configOpt.isPresent()) {
                // 添加新关键词
                List<String> existingKeywords = persistence.getKeywords(correctType);
                Set<String> allKeywords = new HashSet<>(existingKeywords);

                boolean added = false;
                for (String keyword : keywords) {
                    if (!allKeywords.contains(keyword)) {
                        allKeywords.add(keyword);
                        added = true;
                    }
                }

                if (added) {
                    // 更新关键词
                    persistence.saveKeywords(correctType, new ArrayList<>(allKeywords));
                    log.info("Learned new keywords for type {}: {}", correctType, keywords);
                }
            }
        } catch (Exception e) {
            log.error("Failed to learn from feedback", e);
        }
    }

    /**
     * 批量学习
     */
    public void performLearning() {
        if (learningCache.isEmpty()) {
            return;
        }

        log.info("Starting batch learning with {} records", learningCache.size());

        try {
            // 分析学习记录
            Map<String, List<LearningRecord>> typeRecords = new HashMap<>();

            for (LearningRecord record : learningCache.values()) {
                // 只学习实际类型
                if (record.getActualType() != null) {
                    typeRecords.computeIfAbsent(record.getActualType(), k -> new ArrayList<>())
                             .add(record);
                }
            }

            // 为每个类型更新关键词
            for (Map.Entry<String, List<LearningRecord>> entry : typeRecords.entrySet()) {
                String typeId = entry.getKey();
                List<LearningRecord> records = entry.getValue();

                updateKeywordsForType(typeId, records);
            }

            // 清空缓存
            learningCache.clear();

            log.info("Batch learning completed");
        } catch (Exception e) {
            log.error("Failed to perform batch learning", e);
        }
    }

    /**
     * 更新类型的关键词
     */
    private void updateKeywordsForType(String typeId, List<LearningRecord> records) {
        try {
            // 统计关键词频率
            Map<String, Integer> keywordFreq = new HashMap<>();

            for (LearningRecord record : records) {
                List<String> keywords = extractKeywords(record.getQuestion());
                for (String keyword : keywords) {
                    keywordFreq.merge(keyword, 1, Integer::sum);
                }
            }

            // 筛选高频关键词（出现次数 >= 3）
            List<String> newKeywords = new ArrayList<>();
            for (Map.Entry<String, Integer> entry : keywordFreq.entrySet()) {
                if (entry.getValue() >= 3) {
                    newKeywords.add(entry.getKey());
                }
            }

            if (!newKeywords.isEmpty()) {
                // 添加到现有关键词
                persistence.addKeywords(typeId, newKeywords);
                log.info("Added {} new keywords to type {}", newKeywords.size(), typeId);
            }
        } catch (Exception e) {
            log.error("Failed to update keywords for type: {}", typeId, e);
        }
    }

    /**
     * 提取关键词（简单实现）
     */
    private List<String> extractKeywords(String question) {
        List<String> keywords = new ArrayList<>();

        if (question == null || question.trim().isEmpty()) {
            return keywords;
        }

        // 简单的分词和过滤
        String[] words = question.toLowerCase()
                               .replaceAll("[^a-z0-9\\u4e00-\\u9fa5\\s]", " ")
                               .split("\\s+");

        // 过滤停用词和短词
        Set<String> stopWords = Set.of("the", "is", "at", "which", "on", "a", "an",
                                      "什么", "怎么", "如何", "为什么", "是", "的");

        for (String word : words) {
            if (word.length() >= 2 && !stopWords.contains(word)) {
                keywords.add(word);
            }
        }

        return keywords;
    }

    /**
     * 获取学习统计
     */
    public LearningStatistics getStatistics() {
        return LearningStatistics.builder()
            .cacheSize(learningCache.size())
            .cacheThreshold(CACHE_THRESHOLD)
            .totalTypes(persistence.getAllQuestionTypes().size())
            .build();
    }

    /**
     * 手动触发学习
     */
    public void triggerLearning() {
        log.info("Manually triggering learning...");
        performLearning();
    }

    /**
     * 学习记录
     */
    private static class LearningRecord {
        private final String question;
        private final String predictedType;
        private final String actualType;
        private final double confidence;
        private final long timestamp;

        public LearningRecord(String question, String predictedType, String actualType,
                            double confidence, long timestamp) {
            this.question = question;
            this.predictedType = predictedType;
            this.actualType = actualType;
            this.confidence = confidence;
            this.timestamp = timestamp;
        }

        public String getQuestion() {
            return question;
        }

        public String getPredictedType() {
            return predictedType;
        }

        public String getActualType() {
            return actualType;
        }

        public double getConfidence() {
            return confidence;
        }

        public long getTimestamp() {
            return timestamp;
        }
    }

    /**
     * 学习统计
     */
    @lombok.Data
    @lombok.Builder
    @lombok.NoArgsConstructor
    @lombok.AllArgsConstructor
    public static class LearningStatistics {
        /** 缓存大小 */
        private int cacheSize;

        /** 缓存阈值 */
        private int cacheThreshold;

        /** 类型总数 */
        private int totalTypes;
    }
}


