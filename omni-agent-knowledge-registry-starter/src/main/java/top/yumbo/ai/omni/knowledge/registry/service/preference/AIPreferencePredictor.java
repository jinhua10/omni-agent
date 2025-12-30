package top.yumbo.ai.omni.knowledge.registry.service.preference;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.ai.api.EmbeddingService;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * AI增强的用户偏好预测器
 * (AI-Enhanced User Preference Predictor)
 *
 * <p>使用向量相似度和机器学习技术预测用户对域的偏好</p>
 *
 * <p>预测策略：</p>
 * <ul>
 *     <li>查询语义相似度分析</li>
 *     <li>用户历史行为模式学习</li>
 *     <li>协同过滤（基于相似用户）</li>
 *     <li>时间衰减模型</li>
 * </ul>
 *
 * <p>应用场景：</p>
 * <ul>
 *     <li>新用户冷启动 - 基于查询语义推荐</li>
 *     <li>老用户优化 - 结合历史偏好微调</li>
 *     <li>跨域推荐 - 发现用户可能感兴趣的新域</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class AIPreferencePredictor {

    /**
     * 用户偏好学习器（基础数据来源）
     */
    @Autowired
    private UserPreferenceLearner preferenceLearner;

    /**
     * Embedding服务（用于语义分析）
     * 优先使用 ONNX Embedding 服务
     */
    @Autowired(required = false)
    @Qualifier("onnxEmbeddingService")
    private EmbeddingService embeddingService;

    /**
     * 查询向量缓存（避免重复计算）
     */
    private final Map<String, float[]> queryVectorCache = new ConcurrentHashMap<>();

    /**
     * 域语义向量（域名称和关键词的向量表示）
     */
    private final Map<String, float[]> domainVectorCache = new ConcurrentHashMap<>();

    /**
     * 时间衰减因子（越近的查询权重越高）
     */
    private static final double TIME_DECAY_FACTOR = 0.95;

    /**
     * 最小数据量要求（用户历史查询数，低于此值使用语义预测）
     */
    private static final int MIN_HISTORY_SIZE = 10;

    /**
     * 预测用户对域的偏好权重
     *
     * @param userId 用户ID
     * @param domainId 域ID
     * @param query 当前查询（用于语义分析）
     * @return 预测的偏好权重（0.5-1.5）
     */
    public double predictPreference(String userId, String domainId, String query) {
        try {
            // 获取基础偏好权重
            double baseWeight = preferenceLearner.getDomainPreferenceWeight(userId, domainId);

            // 如果没有Embedding服务，直接返回基础权重
            if (embeddingService == null) {
                return baseWeight;
            }

            // 获取用户历史查询数
            var preference = preferenceLearner.getUserPreference(userId);
            long historySize = preference != null ? preference.getTotalQueries() : 0;

            // 冷启动场景：历史数据不足，主要基于语义
            if (historySize < MIN_HISTORY_SIZE) {
                double semanticWeight = calculateSemanticPreference(query, domainId);
                // 70% 语义 + 30% 基础权重
                return semanticWeight * 0.7 + baseWeight * 0.3;
            }

            // 正常场景：结合历史和语义
            double semanticWeight = calculateSemanticPreference(query, domainId);
            double historyWeight = calculateHistoryPatternWeight(userId, domainId, query);

            // 50% 历史模式 + 30% 语义 + 20% 基础权重
            return historyWeight * 0.5 + semanticWeight * 0.3 + baseWeight * 0.2;

        } catch (Exception e) {
            log.warn("预测用户偏好失败: userId={}, domainId={}, error={}",
                    userId, domainId, e.getMessage());
            // 降级：返回基础权重
            return preferenceLearner.getDomainPreferenceWeight(userId, domainId);
        }
    }

    /**
     * 计算查询与域的语义相似度
     *
     * @param query 查询文本
     * @param domainId 域ID
     * @return 语义偏好权重（0.5-1.5）
     */
    private double calculateSemanticPreference(String query, String domainId) {
        try {
            // 获取查询向量
            float[] queryVector = getQueryVector(query);

            // 获取域向量
            float[] domainVector = getDomainVector(domainId);

            // 计算余弦相似度
            double similarity = cosineSimilarity(queryVector, domainVector);

            // 映射到权重范围 [0.5, 1.5]
            // 相似度高 -> 权重高
            return 0.5 + similarity;

        } catch (Exception e) {
            log.debug("计算语义相似度失败: {}", e.getMessage());
            return 1.0; // 默认权重
        }
    }

    /**
     * 计算基于历史模式的权重
     *
     * @param userId 用户ID
     * @param domainId 域ID
     * @param query 当前查询
     * @return 历史模式权重（0.5-1.5）
     */
    private double calculateHistoryPatternWeight(String userId, String domainId, String query) {
        var preference = preferenceLearner.getUserPreference(userId);
        if (preference == null) {
            return 1.0;
        }

        // 1. 检查用户是否在类似主题上偏好该域
        String topic = extractTopic(query);
        var domainUsage = preference.getDomainUsage();
        var topicCounts = preference.getTopicCounts();

        if (!domainUsage.containsKey(domainId)) {
            return 0.8; // 用户从未使用过该域
        }

        // 2. 计算该域在该主题下的使用频率
        long totalQueriesOnTopic = topicCounts.getOrDefault(topic, 0L);
        long domainUsageCount = domainUsage.get(domainId).getUsageCount();

        if (totalQueriesOnTopic == 0) {
            // 新主题，基于整体域偏好
            return preferenceLearner.getDomainPreferenceWeight(userId, domainId);
        }

        // 3. 估计该域在该主题下的权重
        double topicDomainRatio = Math.min(1.0, (double) domainUsageCount / totalQueriesOnTopic);

        // 4. 应用时间衰减（最近的偏好权重更高）
        var domainPref = domainUsage.get(domainId);
        double timeWeight = 1.0;
        if (domainPref.getLastUsedTime() != null) {
            long daysSince = java.time.temporal.ChronoUnit.DAYS.between(
                    domainPref.getLastUsedTime(), java.time.LocalDateTime.now());
            timeWeight = Math.pow(0.95, daysSince);
        }

        // 综合权重
        return 0.7 + (topicDomainRatio * timeWeight * 0.8);
    }

    /**
     * 批量预测多个域的偏好
     *
     * @param userId 用户ID
     * @param domainIds 域ID列表
     * @param query 查询文本
     * @return 域ID -> 预测权重
     */
    public Map<String, Double> predictBatchPreferences(String userId, List<String> domainIds, String query) {
        Map<String, Double> predictions = new HashMap<>();

        for (String domainId : domainIds) {
            double weight = predictPreference(userId, domainId, query);
            predictions.put(domainId, weight);
        }

        return predictions;
    }

    /**
     * 推荐用户可能感兴趣但未使用过的域
     *
     * @param userId 用户ID
     * @param query 当前查询
     * @param allDomainIds 所有可用域
     * @param topK 推荐数量
     * @return 推荐的域ID列表
     */
    public List<String> recommendNewDomains(String userId, String query,
                                           List<String> allDomainIds, int topK) {
        // 获取用户已使用的域
        var preference = preferenceLearner.getUserPreference(userId);
        Set<String> usedDomains = preference != null
                ? preference.getDomainUsage().keySet()
                : Collections.emptySet();

        // 计算未使用域的语义相似度
        Map<String, Double> newDomainScores = new HashMap<>();

        for (String domainId : allDomainIds) {
            if (!usedDomains.contains(domainId)) {
                double semanticScore = calculateSemanticPreference(query, domainId);
                newDomainScores.put(domainId, semanticScore);
            }
        }

        // 返回Top K
        return newDomainScores.entrySet().stream()
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .limit(topK)
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());
    }

    // ========== 辅助方法 ==========

    /**
     * 获取查询的向量表示（带缓存）
     */
    private float[] getQueryVector(String query) {
        return queryVectorCache.computeIfAbsent(query, q -> {
            try {
                return embeddingService.embed(q);
            } catch (Exception e) {
                log.debug("查询向量化失败: {}", e.getMessage());
                return new float[0];
            }
        });
    }

    /**
     * 获取域的向量表示（基于域名称和描述）
     */
    private float[] getDomainVector(String domainId) {
        return domainVectorCache.computeIfAbsent(domainId, id -> {
            try {
                // 可以从域定义中获取描述文本
                // 这里简化为直接向量化域ID
                String domainText = getDomainDescription(id);
                return embeddingService.embed(domainText);
            } catch (Exception e) {
                log.debug("域向量化失败: {}", e.getMessage());
                return new float[0];
            }
        });
    }

    /**
     * 获取域的描述文本（用于向量化）
     * TODO: 从域定义中获取更详细的描述
     */
    private String getDomainDescription(String domainId) {
        // 简化实现：直接使用域ID
        // 实际应该从域注册表获取域的名称、描述、关键词等
        return domainId.replace("-", " ").replace("_", " ");
    }

    /**
     * 计算余弦相似度
     */
    private double cosineSimilarity(float[] vec1, float[] vec2) {
        if (vec1.length == 0 || vec2.length == 0 || vec1.length != vec2.length) {
            return 0.0;
        }

        double dotProduct = 0.0;
        double norm1 = 0.0;
        double norm2 = 0.0;

        for (int i = 0; i < vec1.length; i++) {
            dotProduct += vec1[i] * vec2[i];
            norm1 += vec1[i] * vec1[i];
            norm2 += vec2[i] * vec2[i];
        }

        if (norm1 == 0 || norm2 == 0) {
            return 0.0;
        }

        return dotProduct / (Math.sqrt(norm1) * Math.sqrt(norm2));
    }

    /**
     * 提取查询主题（简化版）
     */
    private String extractTopic(String query) {
        // TODO: 使用更复杂的主题提取算法
        String[] words = query.toLowerCase().split("\\s+");
        if (words.length > 0) {
            return words[0]; // 简化：使用第一个词
        }
        return "general";
    }


    /**
     * 清除缓存（当域定义更新时调用）
     */
    public void clearCache() {
        queryVectorCache.clear();
        domainVectorCache.clear();
        log.info("AI偏好预测缓存已清除");
    }

    /**
     * 预测结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PredictionResult {
        /** 域ID */
        private String domainId;

        /** 预测权重 */
        private double predictedWeight;

        /** 基础权重（历史数据） */
        private double baseWeight;

        /** 语义相似度 */
        private double semanticSimilarity;

        /** 置信度 */
        private double confidence;
    }
}




