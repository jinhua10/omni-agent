package top.yumbo.ai.omni.core.optimization.metrics;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 优化性能数据收集服务
 *
 * 收集和分析RAG优化算法的性能指标，用于Dashboard展示
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class OptimizationMetricsCollector {

    // 存储性能指标数据（生产环境应使用时序数据库如InfluxDB）
    private final Map<String, List<OptimizationMetric>> metricsStore = new ConcurrentHashMap<>();
    private final Map<String, AlgorithmStatistics> statisticsCache = new ConcurrentHashMap<>();

    /**
     * 优化性能指标
     */
    @Data
    public static class OptimizationMetric {
        private String metricId;
        private String documentId;
        private String algorithmType;
        private long timestamp;
        private double precisionGain;      // 精度提升(%)
        private int latencyMs;              // 延迟(ms)
        private double relevanceScore;      // 相关度评分
        private int resultCount;            // 结果数量
        private Map<String, Object> metadata;

        public OptimizationMetric() {
            this.metricId = UUID.randomUUID().toString();
            this.timestamp = Instant.now().toEpochMilli();
            this.metadata = new HashMap<>();
        }
    }

    /**
     * 算法统计数据
     */
    @Data
    public static class AlgorithmStatistics {
        private String algorithmType;
        private long totalExecutions;
        private double avgPrecisionGain;
        private double avgLatencyMs;
        private double successRate;
        private long lastUpdated;
        private List<Double> precisionHistory;  // 最近的精度记录
        private List<Integer> latencyHistory;   // 最近的延迟记录
    }

    /**
     * Dashboard数据
     */
    @Data
    public static class DashboardData {
        private long timestamp;
        private Map<String, AlgorithmStatistics> algorithmStats;
        private List<OptimizationMetric> recentMetrics;
        private OverallStatistics overall;
        private List<TrendData> trends;
    }

    /**
     * 整体统计
     */
    @Data
    public static class OverallStatistics {
        private long totalQueries;
        private double avgPrecisionGain;
        private double avgLatencyMs;
        private String mostUsedAlgorithm;
        private String bestPerformingAlgorithm;
    }

    /**
     * 趋势数据
     */
    @Data
    public static class TrendData {
        private long timestamp;
        private String label;
        private Map<String, Double> values;  // algorithm -> value
    }

    /**
     * 记录优化指标
     */
    public void recordMetric(OptimizationMetric metric) {
        String algorithm = metric.getAlgorithmType();

        metricsStore.computeIfAbsent(algorithm, k -> new ArrayList<>()).add(metric);

        // 限制存储大小（最多保留1000条）
        List<OptimizationMetric> metrics = metricsStore.get(algorithm);
        if (metrics.size() > 1000) {
            metrics.remove(0);
        }

        // 更新统计缓存
        updateStatistics(algorithm);

        log.debug("Recorded metric for algorithm {}: precision={}%, latency={}ms",
                algorithm, metric.getPrecisionGain(), metric.getLatencyMs());
    }

    /**
     * 批量记录指标
     */
    public void recordMetrics(List<OptimizationMetric> metrics) {
        metrics.forEach(this::recordMetric);
    }

    /**
     * 获取Dashboard数据
     */
    public DashboardData getDashboardData() {
        DashboardData data = new DashboardData();
        data.setTimestamp(Instant.now().toEpochMilli());

        // 算法统计
        data.setAlgorithmStats(getAllAlgorithmStatistics());

        // 最近的指标（最多100条）
        data.setRecentMetrics(getRecentMetrics(100));

        // 整体统计
        data.setOverall(calculateOverallStatistics());

        // 趋势数据
        data.setTrends(calculateTrends(24)); // 最近24小时

        return data;
    }

    /**
     * 获取特定算法的统计数据
     */
    public AlgorithmStatistics getAlgorithmStatistics(String algorithmType) {
        return statisticsCache.computeIfAbsent(algorithmType, this::calculateStatistics);
    }

    /**
     * 获取所有算法的统计数据
     */
    public Map<String, AlgorithmStatistics> getAllAlgorithmStatistics() {
        Map<String, AlgorithmStatistics> stats = new HashMap<>();
        metricsStore.keySet().forEach(algorithm -> {
            stats.put(algorithm, getAlgorithmStatistics(algorithm));
        });
        return stats;
    }

    /**
     * 获取最近的指标
     */
    public List<OptimizationMetric> getRecentMetrics(int limit) {
        return metricsStore.values().stream()
                .flatMap(List::stream)
                .sorted(Comparator.comparing(OptimizationMetric::getTimestamp).reversed())
                .limit(limit)
                .collect(Collectors.toList());
    }

    /**
     * 计算整体统计
     */
    private OverallStatistics calculateOverallStatistics() {
        OverallStatistics overall = new OverallStatistics();

        List<OptimizationMetric> allMetrics = metricsStore.values().stream()
                .flatMap(List::stream)
                .collect(Collectors.toList());

        overall.setTotalQueries((long) allMetrics.size());

        if (!allMetrics.isEmpty()) {
            overall.setAvgPrecisionGain(
                allMetrics.stream()
                    .mapToDouble(OptimizationMetric::getPrecisionGain)
                    .average()
                    .orElse(0.0)
            );

            overall.setAvgLatencyMs(
                allMetrics.stream()
                    .mapToInt(OptimizationMetric::getLatencyMs)
                    .average()
                    .orElse(0.0)
            );

            // 最常用算法
            overall.setMostUsedAlgorithm(
                metricsStore.entrySet().stream()
                    .max(Comparator.comparing(e -> e.getValue().size()))
                    .map(Map.Entry::getKey)
                    .orElse("unknown")
            );

            // 最佳性能算法（按精度提升）
            overall.setBestPerformingAlgorithm(
                statisticsCache.entrySet().stream()
                    .max(Comparator.comparing(e -> e.getValue().getAvgPrecisionGain()))
                    .map(Map.Entry::getKey)
                    .orElse("unknown")
            );
        }

        return overall;
    }

    /**
     * 计算趋势数据
     */
    private List<TrendData> calculateTrends(int hours) {
        List<TrendData> trends = new ArrayList<>();
        long now = Instant.now().toEpochMilli();
        long hourInMs = 3600000;

        for (int i = hours - 1; i >= 0; i--) {
            long startTime = now - (i + 1) * hourInMs;
            long endTime = now - i * hourInMs;

            TrendData trend = new TrendData();
            trend.setTimestamp(endTime);
            trend.setLabel(i == 0 ? "Now" : i + "h ago");

            Map<String, Double> values = new HashMap<>();
            for (Map.Entry<String, List<OptimizationMetric>> entry : metricsStore.entrySet()) {
                String algorithm = entry.getKey();
                double avgPrecision = entry.getValue().stream()
                        .filter(m -> m.getTimestamp() >= startTime && m.getTimestamp() < endTime)
                        .mapToDouble(OptimizationMetric::getPrecisionGain)
                        .average()
                        .orElse(0.0);
                values.put(algorithm, avgPrecision);
            }

            trend.setValues(values);
            trends.add(trend);
        }

        return trends;
    }

    /**
     * 更新统计数据
     */
    private void updateStatistics(String algorithmType) {
        AlgorithmStatistics stats = calculateStatistics(algorithmType);
        statisticsCache.put(algorithmType, stats);
    }

    /**
     * 计算算法统计
     */
    private AlgorithmStatistics calculateStatistics(String algorithmType) {
        List<OptimizationMetric> metrics = metricsStore.getOrDefault(algorithmType, new ArrayList<>());

        AlgorithmStatistics stats = new AlgorithmStatistics();
        stats.setAlgorithmType(algorithmType);
        stats.setTotalExecutions((long) metrics.size());
        stats.setLastUpdated(Instant.now().toEpochMilli());

        if (!metrics.isEmpty()) {
            stats.setAvgPrecisionGain(
                metrics.stream()
                    .mapToDouble(OptimizationMetric::getPrecisionGain)
                    .average()
                    .orElse(0.0)
            );

            stats.setAvgLatencyMs(
                metrics.stream()
                    .mapToInt(OptimizationMetric::getLatencyMs)
                    .average()
                    .orElse(0.0)
            );

            stats.setSuccessRate(100.0); // 简化处理，实际应根据成功/失败记录计算

            // 最近的精度和延迟历史（最多50条）
            stats.setPrecisionHistory(
                metrics.stream()
                    .sorted(Comparator.comparing(OptimizationMetric::getTimestamp).reversed())
                    .limit(50)
                    .map(OptimizationMetric::getPrecisionGain)
                    .collect(Collectors.toList())
            );

            stats.setLatencyHistory(
                metrics.stream()
                    .sorted(Comparator.comparing(OptimizationMetric::getTimestamp).reversed())
                    .limit(50)
                    .map(OptimizationMetric::getLatencyMs)
                    .collect(Collectors.toList())
            );
        }

        return stats;
    }

    /**
     * 清除旧数据
     */
    public void clearOldData(long beforeTimestamp) {
        metricsStore.values().forEach(metrics ->
            metrics.removeIf(m -> m.getTimestamp() < beforeTimestamp)
        );
        log.info("Cleared metrics before timestamp: {}", beforeTimestamp);
    }

    /**
     * 获取统计摘要
     */
    public Map<String, Object> getStatisticsSummary() {
        Map<String, Object> summary = new HashMap<>();
        summary.put("totalAlgorithms", metricsStore.size());
        summary.put("totalMetrics", metricsStore.values().stream().mapToInt(List::size).sum());
        summary.put("algorithmStats", getAllAlgorithmStatistics());
        summary.put("overall", calculateOverallStatistics());
        return summary;
    }
}


