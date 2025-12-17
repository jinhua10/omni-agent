package top.yumbo.ai.omni.web.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.core.optimization.metrics.OptimizationMetricsCollector;
import top.yumbo.ai.omni.core.optimization.metrics.OptimizationMetricsCollector.*;

import java.util.List;
import java.util.Map;

/**
 * RAG优化效果Dashboard REST API
 *
 * 提供优化指标的收集、查询和统计功能
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/optimization")
@CrossOrigin(origins = "*")
@Tag(name = "Optimization Dashboard", description = "RAG优化效果监控API")
public class OptimizationDashboardController {

    @Autowired
    private OptimizationMetricsCollector metricsCollector;

    /**
     * 获取Dashboard完整数据
     */
    @GetMapping("/dashboard")
    @Operation(summary = "获取Dashboard数据", description = "获取包含算法统计、趋势和最近指标的完整Dashboard数据")
    public ResponseEntity<DashboardData> getDashboardData() {
        log.debug("Fetching dashboard data");
        DashboardData data = metricsCollector.getDashboardData();
        return ResponseEntity.ok(data);
    }

    /**
     * 获取所有算法统计
     */
    @GetMapping("/statistics")
    @Operation(summary = "获取所有算法统计", description = "获取所有算法的执行统计数据")
    public ResponseEntity<Map<String, AlgorithmStatistics>> getAllStatistics() {
        log.debug("Fetching all algorithm statistics");
        Map<String, AlgorithmStatistics> stats = metricsCollector.getAllAlgorithmStatistics();
        return ResponseEntity.ok(stats);
    }

    /**
     * 获取特定算法统计
     */
    @GetMapping("/statistics/{algorithmType}")
    @Operation(summary = "获取特定算法统计", description = "获取指定算法的详细统计数据")
    public ResponseEntity<AlgorithmStatistics> getAlgorithmStatistics(
            @PathVariable String algorithmType) {
        log.debug("Fetching statistics for algorithm: {}", algorithmType);
        AlgorithmStatistics stats = metricsCollector.getAlgorithmStatistics(algorithmType);
        return ResponseEntity.ok(stats);
    }

    /**
     * 获取最近的指标
     */
    @GetMapping("/metrics/recent")
    @Operation(summary = "获取最近指标", description = "获取最近N条优化指标记录")
    public ResponseEntity<List<OptimizationMetric>> getRecentMetrics(
            @RequestParam(defaultValue = "100") int limit) {
        log.debug("Fetching recent metrics, limit: {}", limit);
        List<OptimizationMetric> metrics = metricsCollector.getRecentMetrics(limit);
        return ResponseEntity.ok(metrics);
    }

    /**
     * 记录新的指标
     */
    @PostMapping("/metrics")
    @Operation(summary = "记录优化指标", description = "记录单条优化算法的性能指标")
    public ResponseEntity<String> recordMetric(@RequestBody OptimizationMetric metric) {
        log.debug("Recording new metric for algorithm: {}", metric.getAlgorithmType());
        metricsCollector.recordMetric(metric);
        return ResponseEntity.ok("Metric recorded successfully");
    }

    /**
     * 批量记录指标
     */
    @PostMapping("/metrics/batch")
    @Operation(summary = "批量记录指标", description = "批量记录多条优化指标")
    public ResponseEntity<String> recordMetrics(@RequestBody List<OptimizationMetric> metrics) {
        log.debug("Recording {} metrics", metrics.size());
        metricsCollector.recordMetrics(metrics);
        return ResponseEntity.ok(metrics.size() + " metrics recorded successfully");
    }

    /**
     * 获取统计摘要
     */
    @GetMapping("/summary")
    @Operation(summary = "获取统计摘要", description = "获取所有算法的统计摘要信息")
    public ResponseEntity<Map<String, Object>> getStatisticsSummary() {
        log.debug("Fetching statistics summary");
        Map<String, Object> summary = metricsCollector.getStatisticsSummary();
        return ResponseEntity.ok(summary);
    }

    /**
     * 清除旧数据
     */
    @DeleteMapping("/metrics/old")
    @Operation(summary = "清除旧数据", description = "删除指定时间戳之前的历史数据")
    public ResponseEntity<String> clearOldData(
            @RequestParam long beforeTimestamp) {
        log.info("Clearing old data before timestamp: {}", beforeTimestamp);
        metricsCollector.clearOldData(beforeTimestamp);
        return ResponseEntity.ok("Old data cleared successfully");
    }
}

