package top.yumbo.ai.omni.core.service.preference;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 用户偏好学习系统
 * (User Preference Learning System)
 *
 * <p>记录和学习用户的查询偏好，用于个性化域权重调整</p>
 *
 * <p>学习内容：</p>
 * <ul>
 *     <li>用户常用的域</li>
 *     <li>用户对不同域结果的反馈</li>
 *     <li>用户的查询主题偏好</li>
 *     <li>用户的时间偏好模式</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class UserPreferenceLearner {

    /**
     * 用户偏好数据（key: userId）
     */
    private final Map<String, UserPreference> userPreferences = new ConcurrentHashMap<>();

    /**
     * 记录用户查询
     *
     * @param userId 用户ID
     * @param query 查询文本
     * @param domainId 使用的域ID
     * @param resultCount 返回结果数
     */
    public void recordQuery(String userId, String query, String domainId, int resultCount) {
        UserPreference preference = userPreferences.computeIfAbsent(
                userId,
                k -> new UserPreference(userId)
        );

        preference.incrementTotalQueries();
        preference.recordDomainUsage(domainId);
        preference.recordQueryTopic(extractTopic(query));
        preference.setLastActiveTime(LocalDateTime.now());

        log.debug("记录用户 {} 查询: 域={}, 主题={}", userId, domainId, extractTopic(query));
    }

    /**
     * 记录用户对域结果的反馈
     *
     * @param userId 用户ID
     * @param domainId 域ID
     * @param isPositive 是否正面反馈
     */
    public void recordDomainFeedback(String userId, String domainId, boolean isPositive) {
        UserPreference preference = userPreferences.get(userId);
        if (preference != null) {
            preference.recordDomainFeedback(domainId, isPositive);
            log.info("用户 {} 对域 {} 的反馈: {}", userId, domainId, isPositive ? "👍" : "👎");
        }
    }

    /**
     * 获取用户对域的偏好权重
     *
     * @param userId 用户ID
     * @param domainId 域ID
     * @return 偏好权重（0.5 - 1.5，1.0为中性）
     */
    public double getDomainPreferenceWeight(String userId, String domainId) {
        UserPreference preference = userPreferences.get(userId);

        if (preference == null || preference.getTotalQueries() < 5) {
            // 新用户或查询次数太少，返回中性权重
            return 1.0;
        }

        // 计算域使用频率权重
        DomainUsageStats stats = preference.getDomainUsage().get(domainId);
        if (stats == null) {
            return 1.0; // 用户从未使用过这个域
        }

        // 1. 使用频率分数（0.0 - 1.0）
        double usageRate = (double) stats.getUsageCount() / preference.getTotalQueries();
        double frequencyScore = Math.min(1.0, usageRate * 5); // 20%使用率 = 1.0分

        // 2. 反馈分数（0.0 - 1.0）
        double feedbackScore = 0.5; // 默认中性
        if (stats.getFeedbackCount() > 0) {
            feedbackScore = (double) stats.getPositiveFeedback() / stats.getFeedbackCount();
        }

        // 3. 最近使用加成（0.0 - 0.2）
        double recencyBonus = 0.0;
        if (stats.getLastUsedTime() != null) {
            long daysSinceLastUse = java.time.temporal.ChronoUnit.DAYS.between(
                    stats.getLastUsedTime(), LocalDateTime.now());
            if (daysSinceLastUse < 7) {
                recencyBonus = 0.2 * (1.0 - daysSinceLastUse / 7.0);
            }
        }

        // 综合权重：0.5 - 1.5
        double weight = 0.5 + (frequencyScore * 0.3 + feedbackScore * 0.5 + recencyBonus);

        log.debug("用户 {} 对域 {} 的偏好权重: {:.2f} (频率:{:.2f}, 反馈:{:.2f}, 最近:{:.2f})",
                userId, domainId, weight, frequencyScore, feedbackScore, recencyBonus);

        return Math.max(0.5, Math.min(1.5, weight));
    }

    /**
     * 获取用户偏好的域列表（按偏好度排序）
     *
     * @param userId 用户ID
     * @param topK 返回Top K个域
     * @return 域ID列表
     */
    public List<String> getPreferredDomains(String userId, int topK) {
        UserPreference preference = userPreferences.get(userId);

        if (preference == null) {
            return Collections.emptyList();
        }

        return preference.getDomainUsage().entrySet().stream()
                .sorted((e1, e2) -> {
                    // 按使用次数和反馈综合排序
                    double score1 = e1.getValue().getPreferenceScore();
                    double score2 = e2.getValue().getPreferenceScore();
                    return Double.compare(score2, score1);
                })
                .limit(topK)
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());
    }

    /**
     * 获取用户偏好
     */
    public UserPreference getUserPreference(String userId) {
        return userPreferences.get(userId);
    }

    /**
     * 提取查询主题（简化版）
     */
    private String extractTopic(String query) {
        String lower = query.toLowerCase();

        // 简单的主题识别
        if (lower.contains("代码") || lower.contains("bug") || lower.contains("代码")) {
            return "代码";
        } else if (lower.contains("安全") || lower.contains("漏洞")) {
            return "安全";
        } else if (lower.contains("性能") || lower.contains("优化")) {
            return "性能";
        } else if (lower.contains("文档") || lower.contains("教程")) {
            return "文档";
        }

        return "通用";
    }

    /**
     * 用户偏好数据
     */
    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class UserPreference {
        /** 用户ID */
        private String userId;

        /** 总查询次数 */
        private long totalQueries;

        /** 域使用统计 */
        @Builder.Default
        private Map<String, DomainUsageStats> domainUsage = new ConcurrentHashMap<>();

        /** 查询主题统计 */
        @Builder.Default
        private Map<String, Long> topicCounts = new ConcurrentHashMap<>();

        /** 创建时间 */
        @Builder.Default
        private LocalDateTime createdAt = LocalDateTime.now();

        /** 最后活跃时间 */
        private LocalDateTime lastActiveTime;

        public UserPreference(String userId) {
            this.userId = userId;
            this.totalQueries = 0;
            this.domainUsage = new ConcurrentHashMap<>();
            this.topicCounts = new ConcurrentHashMap<>();
            this.createdAt = LocalDateTime.now();
        }

        public void incrementTotalQueries() {
            this.totalQueries++;
        }

        public void recordDomainUsage(String domainId) {
            DomainUsageStats stats = domainUsage.computeIfAbsent(
                    domainId,
                    k -> new DomainUsageStats(domainId)
            );
            stats.incrementUsageCount();
            stats.setLastUsedTime(LocalDateTime.now());
        }

        public void recordDomainFeedback(String domainId, boolean isPositive) {
            DomainUsageStats stats = domainUsage.get(domainId);
            if (stats != null) {
                stats.incrementFeedbackCount();
                if (isPositive) {
                    stats.incrementPositiveFeedback();
                }
            }
        }

        public void recordQueryTopic(String topic) {
            topicCounts.merge(topic, 1L, Long::sum);
        }
    }

    /**
     * 域使用统计
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DomainUsageStats {
        /** 域ID */
        private String domainId;

        /** 使用次数 */
        private long usageCount;

        /** 反馈次数 */
        private long feedbackCount;

        /** 正面反馈数 */
        private long positiveFeedback;

        /** 最后使用时间 */
        private LocalDateTime lastUsedTime;

        public DomainUsageStats(String domainId) {
            this.domainId = domainId;
            this.usageCount = 0;
            this.feedbackCount = 0;
            this.positiveFeedback = 0;
        }

        public void incrementUsageCount() {
            this.usageCount++;
        }

        public void incrementFeedbackCount() {
            this.feedbackCount++;
        }

        public void incrementPositiveFeedback() {
            this.positiveFeedback++;
        }

        /**
         * 计算偏好分数
         */
        public double getPreferenceScore() {
            double usage = usageCount;
            double feedbackRate = feedbackCount > 0
                    ? (double) positiveFeedback / feedbackCount
                    : 0.5;

            return usage * feedbackRate;
        }
    }
}

