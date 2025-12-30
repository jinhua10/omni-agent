package top.yumbo.ai.omni.knowledge.registry.service.quality;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 域质量评分系统
 * (Domain Quality Scoring System)
 *
 * <p>记录和评估每个知识域的查询质量，用于动态调整域权重</p>
 *
 * <p>评分维度：</p>
 * <ul>
 *     <li>查询准确率 - 用户反馈的准确性</li>
 *     <li>响应速度 - 查询响应时间</li>
 *     <li>结果数量 - 能返回结果的比例</li>
 *     <li>���用频率 - 域的活跃度</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class DomainQualityScorer {

    /**
     * 域质量统计数据（内存存储 + 持久化）
     */
    private final Map<String, DomainQualityStats> qualityStats = new ConcurrentHashMap<>();

    /**
     * 持久化存储服务
     */
    @Autowired(required = false)
    private DocumentStorageService storageService;

    /**
     * JSON序列化工具
     */
    private final ObjectMapper objectMapper = new ObjectMapper()
            .registerModule(new JavaTimeModule());

    /**
     * 持久化存储ID
     */
    private static final String STORAGE_ID = "domain-quality-stats";

    /**
     * 启动时加载持久化数据
     */
    @PostConstruct
    public void init() {
        if (storageService != null) {
            loadPersistedStats();
        }
        log.info("✅ 域质量评分系统已初始化 (持久化: {})", storageService != null);
    }

    /**
     * 关闭时保存数据
     */
    @PreDestroy
    public void destroy() {
        if (storageService != null) {
            persistStats();
        }
    }

    /**
     * 记录查询事件
     *
     * @param domainId 域ID
     * @param resultCount 返回结果数
     * @param responseTime 响应时间（毫秒）
     */
    public void recordQuery(String domainId, int resultCount, long responseTime) {
        DomainQualityStats stats = qualityStats.computeIfAbsent(
                domainId,
                k -> new DomainQualityStats(domainId)
        );

        stats.incrementQueryCount();
        stats.addResponseTime(responseTime);

        if (resultCount > 0) {
            stats.incrementSuccessCount();
        }

        stats.setLastQueryTime(LocalDateTime.now());

        log.debug("记录域 {} 查询: 结果数={}, 响应时间={}ms", domainId, resultCount, responseTime);
    }

    /**
     * 记录用户反馈
     *
     * @param domainId 域ID
     * @param isPositive 是否正面反馈
     */
    public void recordFeedback(String domainId, boolean isPositive) {
        DomainQualityStats stats = qualityStats.get(domainId);
        if (stats != null) {
            stats.incrementFeedbackCount();
            if (isPositive) {
                stats.incrementPositiveFeedback();
            }
            log.info("记录域 {} 反馈: {}", domainId, isPositive ? "正面" : "负面");
        }
    }

    /**
     * 计算域的质量分数
     *
     * @param domainId 域ID
     * @return 质量分数（0.0 - 2.0，1.0为基准）
     */
    public double calculateQualityScore(String domainId) {
        DomainQualityStats stats = qualityStats.get(domainId);

        if (stats == null || stats.getQueryCount() < 10) {
            // 新域或查询次数太少，返回中等分数
            return 1.0;
        }

        // 1. 成功率分数（权重 0.4）
        double successRate = (double) stats.getSuccessCount() / stats.getQueryCount();
        double successScore = successRate; // 0.0 - 1.0

        // 2. 准确率分数（基于用户反馈，权重 0.4）
        double accuracyScore = 0.5; // 默认中等
        if (stats.getFeedbackCount() > 0) {
            accuracyScore = (double) stats.getPositiveFeedback() / stats.getFeedbackCount();
        }

        // 3. 性能分数（基于响应时间，权重 0.2）
        double avgResponseTime = stats.getAverageResponseTime();
        double performanceScore = calculatePerformanceScore(avgResponseTime);

        // 综合分数
        double totalScore =
                successScore * 0.4 +
                accuracyScore * 0.4 +
                performanceScore * 0.2;

        // 转换到 0.5 - 1.5 范围（避免极端值）
        double finalScore = 0.5 + totalScore;

        log.debug("域 {} 质量分数: {:.2f} (成功率:{:.2f}, 准确率:{:.2f}, 性能:{:.2f})",
                domainId, finalScore, successScore, accuracyScore, performanceScore);

        return Math.max(0.5, Math.min(1.5, finalScore));
    }

    /**
     * 计算性能分数
     */
    private double calculatePerformanceScore(double avgResponseTime) {
        // 响应时间越短，分数越高
        if (avgResponseTime < 100) {
            return 1.0; // 优秀
        } else if (avgResponseTime < 300) {
            return 0.8; // 良好
        } else if (avgResponseTime < 500) {
            return 0.6; // 一般
        } else if (avgResponseTime < 1000) {
            return 0.4; // 较慢
        } else {
            return 0.2; // 很慢
        }
    }

    /**
     * 获取域的统计信息
     */
    public DomainQualityStats getStats(String domainId) {
        return qualityStats.get(domainId);
    }

    /**
     * 获取所有域的统计信息
     */
    public Map<String, DomainQualityStats> getAllStats() {
        return new ConcurrentHashMap<>(qualityStats);
    }

    /**
     * 清空统计数据
     */
    public void clearStats() {
        qualityStats.clear();
        log.info("已清空所有域质量统计数据");
    }

    // ========== 持久化相关方法 ==========

    /**
     * 加载持久化的统计数据
     */
    private void loadPersistedStats() {
        try {
            log.info("🔄 开始加载域质量统计数据...");

            Optional<String> jsonOpt = storageService.getExtractedText(STORAGE_ID);
            if (jsonOpt.isPresent()) {
                Map<String, DomainQualityStats> loaded = objectMapper.readValue(
                        jsonOpt.get(),
                        objectMapper.getTypeFactory().constructMapType(
                                ConcurrentHashMap.class, String.class, DomainQualityStats.class)
                );
                qualityStats.putAll(loaded);
                log.info("✅ 域质量统计数据加载完成: {} 个域", loaded.size());
            } else {
                log.info("📋 无持久化数据，使用空统计");
            }
        } catch (Exception e) {
            log.error("加载域质量统计数据失败: {}", e.getMessage());
        }
    }

    /**
     * 持久化统计数据
     */
    private void persistStats() {
        try {
            log.info("💾 开始持久化域质量统计数据...");

            String json = objectMapper.writeValueAsString(qualityStats);
            storageService.saveExtractedText(STORAGE_ID, json);

            log.info("✅ 域质量统计数据持久化完成: {} 个域", qualityStats.size());
        } catch (Exception e) {
            log.error("持久化域质量统计数据失败: {}", e.getMessage());
        }
    }

    /**
     * 手动触发持久化（用于定时任务）
     */
    public void triggerPersist() {
        if (storageService != null) {
            persistStats();
        } else {
            log.warn("持久化服务未配置，跳过");
        }
    }

    /**
     * 域质量统计数据
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DomainQualityStats {
        /** 域ID */
        private String domainId;

        /** 查询总次数 */
        private long queryCount;

        /** 成功返回结果的次数 */
        private long successCount;

        /** 总响应时间（毫秒） */
        private long totalResponseTime;

        /** 反馈总数 */
        private long feedbackCount;

        /** 正面反馈数 */
        private long positiveFeedback;

        /** 最后查询时间 */
        private LocalDateTime lastQueryTime;

        /** 创建时间 */
        @Builder.Default
        private LocalDateTime createdAt = LocalDateTime.now();

        public DomainQualityStats(String domainId) {
            this.domainId = domainId;
            this.queryCount = 0;
            this.successCount = 0;
            this.totalResponseTime = 0;
            this.feedbackCount = 0;
            this.positiveFeedback = 0;
            this.createdAt = LocalDateTime.now();
        }

        public void incrementQueryCount() {
            this.queryCount++;
        }

        public void incrementSuccessCount() {
            this.successCount++;
        }

        public void addResponseTime(long responseTime) {
            this.totalResponseTime += responseTime;
        }

        public void incrementFeedbackCount() {
            this.feedbackCount++;
        }

        public void incrementPositiveFeedback() {
            this.positiveFeedback++;
        }

        /**
         * 获取成功率
         */
        public double getSuccessRate() {
            return queryCount > 0 ? (double) successCount / queryCount : 0.0;
        }

        /**
         * 获取平均响应时间
         */
        public double getAverageResponseTime() {
            return queryCount > 0 ? (double) totalResponseTime / queryCount : 0.0;
        }

        /**
         * 获取准确率（基于用户反馈）
         */
        public double getAccuracyRate() {
            return feedbackCount > 0 ? (double) positiveFeedback / feedbackCount : 0.0;
        }

        /**
         * 获取综合质量分数
         */
        public double getOverallScore() {
            if (queryCount < 10) {
                return 1.0; // 数据不足，返回默认值
            }

            double successScore = getSuccessRate();
            double accuracyScore = feedbackCount > 0 ? getAccuracyRate() : 0.5;

            // 简化计算
            return 0.5 + (successScore * 0.4 + accuracyScore * 0.4 + 0.2);
        }
    }
}

