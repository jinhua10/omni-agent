package top.yumbo.ai.omni.knowledge.registry.service.preference;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 协同过滤推荐服务
 * (Collaborative Filtering Recommendation Service)
 *
 * <p>基于其他相似用户的偏好推荐域</p>
 *
 * <p>协同过滤策略：</p>
 * <ul>
 *     <li>用户相似度计算 - 基于查询历史和域偏好</li>
 *     <li>基于用户的协同过滤（User-based CF）</li>
 *     <li>基于物品的协同过滤（Item-based CF，域作为物品）</li>
 *     <li>混合推荐策略</li>
 * </ul>
 *
 * <p>应用场景：</p>
 * <ul>
 *     <li>冷启动用户推荐</li>
 *     <li>发现潜在兴趣域</li>
 *     <li>提升推荐多样性</li>
 * </ul>
 *
 * <p>注意：需要多用户场景才有效</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class CollaborativeFilteringService {

    /**
     * 用户偏好学习器
     */
    @Autowired
    private UserPreferenceLearner preferenceLearner;

    /**
     * 用户相似度缓存
     */
    private final Map<String, Map<String, Double>> userSimilarityCache = new ConcurrentHashMap<>();

    /**
     * 域相似度缓存
     */
    private final Map<String, Map<String, Double>> domainSimilarityCache = new ConcurrentHashMap<>();

    /**
     * 最小相似用户数
     */
    private static final int MIN_SIMILAR_USERS = 3;

    /**
     * 最大相似用户数（用于推荐计算）
     */
    private static final int MAX_SIMILAR_USERS = 10;

    /**
     * 相似度阈值（低于此值认为不相似）
     */
    private static final double SIMILARITY_THRESHOLD = 0.3;

    /**
     * 为用户推荐域（基于协同过滤）
     *
     * @param userId 用户ID
     * @param candidateDomains 候选域列表
     * @param topK 推荐数量
     * @return 推荐的域及其分数
     */
    public List<DomainRecommendation> recommendDomains(String userId,
                                                       List<String> candidateDomains,
                                                       int topK) {
        // 1. 查找相似用户
        List<UserSimilarity> similarUsers = findSimilarUsers(userId, MAX_SIMILAR_USERS);

        if (similarUsers.size() < MIN_SIMILAR_USERS) {
            log.debug("相似用户不足，无法进行协同过滤推荐: userId={}", userId);
            return Collections.emptyList();
        }

        // 2. 获取当前用户已使用的域
        var currentUserPreference = preferenceLearner.getUserPreference(userId);
        Set<String> usedDomains = currentUserPreference != null
                ? currentUserPreference.getDomainUsage().keySet()
                : Collections.emptySet();

        // 3. 计算候选域的推荐分数
        Map<String, Double> domainScores = new HashMap<>();

        for (String domainId : candidateDomains) {
            // 跳过已使用的域
            if (usedDomains.contains(domainId)) {
                continue;
            }

            // 基于相似用户的偏好计算分数
            double score = calculateDomainScore(domainId, similarUsers);
            if (score > 0) {
                domainScores.put(domainId, score);
            }
        }

        // 4. 排序并返回Top K
        return domainScores.entrySet().stream()
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .limit(topK)
                .map(entry -> DomainRecommendation.builder()
                        .domainId(entry.getKey())
                        .score(entry.getValue())
                        .reason("基于相似用户偏好推荐")
                        .build())
                .collect(Collectors.toList());
    }

    /**
     * 查找相似用户
     *
     * @param userId 目标用户ID
     * @param topK 返回前K个相似用户
     * @return 相似用户列表
     */
    public List<UserSimilarity> findSimilarUsers(String userId, int topK) {
        // 检查缓存
        Map<String, Double> cachedSimilarities = userSimilarityCache.get(userId);
        if (cachedSimilarities != null && !cachedSimilarities.isEmpty()) {
            return cachedSimilarities.entrySet().stream()
                    .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                    .limit(topK)
                    .map(entry -> UserSimilarity.builder()
                            .userId(entry.getKey())
                            .similarity(entry.getValue())
                            .build())
                    .collect(Collectors.toList());
        }

        // 计算与所有其他用户的相似度
        var targetPreference = preferenceLearner.getUserPreference(userId);
        if (targetPreference == null) {
            return Collections.emptyList();
        }

        Map<String, Double> similarities = new HashMap<>();
        List<String> allUsers = preferenceLearner.getAllUserIds();

        for (String otherUserId : allUsers) {
            if (otherUserId.equals(userId)) {
                continue;
            }

            var otherPreference = preferenceLearner.getUserPreference(otherUserId);
            if (otherPreference == null) {
                continue;
            }

            double similarity = calculateUserSimilarity(targetPreference, otherPreference);
            if (similarity >= SIMILARITY_THRESHOLD) {
                similarities.put(otherUserId, similarity);
            }
        }

        // 缓存结果
        userSimilarityCache.put(userId, similarities);

        // 返回Top K
        return similarities.entrySet().stream()
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .limit(topK)
                .map(entry -> UserSimilarity.builder()
                        .userId(entry.getKey())
                        .similarity(entry.getValue())
                        .build())
                .collect(Collectors.toList());
    }

    /**
     * 计算两个用户的相似度
     *
     * @param user1 用户1偏好
     * @param user2 用户2偏好
     * @return 相似度（0.0-1.0）
     */
    private double calculateUserSimilarity(UserPreferenceLearner.UserPreference user1,
                                          UserPreferenceLearner.UserPreference user2) {
        // 1. 域偏好相似度（Jaccard系数）
        Set<String> domains1 = user1.getDomainUsage().keySet();
        Set<String> domains2 = user2.getDomainUsage().keySet();

        Set<String> intersection = new HashSet<>(domains1);
        intersection.retainAll(domains2);

        Set<String> union = new HashSet<>(domains1);
        union.addAll(domains2);

        double domainSimilarity = union.isEmpty() ? 0.0 : (double) intersection.size() / union.size();

        // 2. 主题偏好相似度（余弦相似度）
        double topicSimilarity = calculateTopicSimilarity(
                user1.getTopicCounts(),
                user2.getTopicCounts()
        );

        // 3. 综合相似度（域60% + 主题40%）
        return domainSimilarity * 0.6 + topicSimilarity * 0.4;
    }

    /**
     * 计算主题相似度（基于主题查询次数）
     */
    private double calculateTopicSimilarity(Map<String, Long> topics1, Map<String, Long> topics2) {
        Set<String> allTopics = new HashSet<>();
        allTopics.addAll(topics1.keySet());
        allTopics.addAll(topics2.keySet());

        if (allTopics.isEmpty()) {
            return 0.0;
        }

        // 构建向量
        double dotProduct = 0.0;
        double norm1 = 0.0;
        double norm2 = 0.0;

        for (String topic : allTopics) {
            long count1 = topics1.getOrDefault(topic, 0L);
            long count2 = topics2.getOrDefault(topic, 0L);

            dotProduct += count1 * count2;
            norm1 += count1 * count1;
            norm2 += count2 * count2;
        }

        if (norm1 == 0 || norm2 == 0) {
            return 0.0;
        }

        return dotProduct / (Math.sqrt(norm1) * Math.sqrt(norm2));
    }

    /**
     * 计算域的推荐分数（基于相似用户的偏好）
     *
     * @param domainId 域ID
     * @param similarUsers 相似用户列表
     * @return 推荐分数
     */
    private double calculateDomainScore(String domainId, List<UserSimilarity> similarUsers) {
        double totalScore = 0.0;
        double totalSimilarity = 0.0;

        for (UserSimilarity userSim : similarUsers) {
            var otherPreference = preferenceLearner.getUserPreference(userSim.getUserId());
            if (otherPreference == null) {
                continue;
            }

            // 检查该用户是否使用过该域
            var domainUsage = otherPreference.getDomainUsage().get(domainId);
            if (domainUsage == null) {
                continue;
            }

            // 根据使用频率和反馈计算该域的偏好分数
            double userDomainScore = calculateUserDomainScore(domainUsage);

            // 加权（相似度越高，权重越大）
            totalScore += userDomainScore * userSim.getSimilarity();
            totalSimilarity += userSim.getSimilarity();
        }

        return totalSimilarity > 0 ? totalScore / totalSimilarity : 0.0;
    }

    /**
     * 计算用户对某个域的偏好分数
     */
    private double calculateUserDomainScore(UserPreferenceLearner.DomainUsageStats usage) {
        // 基于使用频率和反馈率
        double usageScore = Math.min(1.0, usage.getUsageCount() / 10.0); // 归一化
        double feedbackScore = usage.getFeedbackCount() > 0
                ? (double) usage.getPositiveFeedback() / usage.getFeedbackCount()
                : 0.5;

        return usageScore * 0.5 + feedbackScore * 0.5;
    }

    /**
     * 基于物品的协同过滤（域作为物品）
     *
     * @param userId 用户ID
     * @param currentDomainId 当前域ID
     * @param topK 推荐数量
     * @return 相似域推荐
     */
    public List<DomainRecommendation> recommendSimilarDomains(String userId,
                                                              String currentDomainId,
                                                              int topK) {
        // 查找与当前域相似的其他域
        Map<String, Double> similarDomains = findSimilarDomains(currentDomainId);

        // 过滤掉用户已使用的域
        var userPreference = preferenceLearner.getUserPreference(userId);
        Set<String> usedDomains = userPreference != null
                ? userPreference.getDomainUsage().keySet()
                : Collections.emptySet();

        return similarDomains.entrySet().stream()
                .filter(entry -> !usedDomains.contains(entry.getKey()))
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .limit(topK)
                .map(entry -> DomainRecommendation.builder()
                        .domainId(entry.getKey())
                        .score(entry.getValue())
                        .reason("与 " + currentDomainId + " 相似")
                        .build())
                .collect(Collectors.toList());
    }

    /**
     * 查找相似域（基于用户共同使用模式）
     */
    private Map<String, Double> findSimilarDomains(String domainId) {
        // 检查缓存
        Map<String, Double> cached = domainSimilarityCache.get(domainId);
        if (cached != null) {
            return cached;
        }

        // 查找所有使用过该域的用户
        List<String> allUsers = preferenceLearner.getAllUserIds();
        Set<String> domainUsers = new HashSet<>();

        for (String userId : allUsers) {
            var preference = preferenceLearner.getUserPreference(userId);
            if (preference != null && preference.getDomainUsage().containsKey(domainId)) {
                domainUsers.add(userId);
            }
        }

        if (domainUsers.isEmpty()) {
            return Collections.emptyMap();
        }

        // 统计这些用户还使用了哪些域
        Map<String, Integer> coOccurrence = new HashMap<>();

        for (String userId : domainUsers) {
            var preference = preferenceLearner.getUserPreference(userId);
            if (preference == null) {
                continue;
            }

            for (String otherDomain : preference.getDomainUsage().keySet()) {
                if (!otherDomain.equals(domainId)) {
                    coOccurrence.merge(otherDomain, 1, Integer::sum);
                }
            }
        }

        // 计算相似度（共现次数 / 使用该域的用户数）
        Map<String, Double> similarities = new HashMap<>();
        for (Map.Entry<String, Integer> entry : coOccurrence.entrySet()) {
            double similarity = (double) entry.getValue() / domainUsers.size();
            similarities.put(entry.getKey(), similarity);
        }

        // 缓存结果
        domainSimilarityCache.put(domainId, similarities);

        return similarities;
    }

    /**
     * 清除缓存
     */
    public void clearCache() {
        userSimilarityCache.clear();
        domainSimilarityCache.clear();
        log.info("协同过滤缓存已清除");
    }

    /**
     * 获取协同过滤统计信息
     */
    public CFStatistics getStatistics() {
        int totalUsers = preferenceLearner.getAllUserIds().size();
        int cachedUsers = userSimilarityCache.size();
        int cachedDomains = domainSimilarityCache.size();

        return CFStatistics.builder()
                .totalUsers(totalUsers)
                .cachedUserSimilarities(cachedUsers)
                .cachedDomainSimilarities(cachedDomains)
                .cacheHitRate(totalUsers > 0 ? (double) cachedUsers / totalUsers : 0.0)
                .build();
    }

    // ========== 数据模型 ==========

    /**
     * 用户相似度
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class UserSimilarity {
        /** 用户ID */
        private String userId;

        /** 相似度（0.0-1.0） */
        private double similarity;
    }

    /**
     * 域推荐
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DomainRecommendation {
        /** 域ID */
        private String domainId;

        /** 推荐分数 */
        private double score;

        /** 推荐理由 */
        private String reason;
    }

    /**
     * 协同过滤统计
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CFStatistics {
        /** 总用户数 */
        private int totalUsers;

        /** 已缓存的用户相似度数量 */
        private int cachedUserSimilarities;

        /** 已缓存的域相似度数量 */
        private int cachedDomainSimilarities;

        /** 缓存命中率 */
        private double cacheHitRate;
    }
}

