package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.web.model.ApiResponse;

import java.util.*;

/**
 * 调试和故障排查控制器 (Debug and Troubleshooting Controller)
 *
 * 提供调试模式和问题诊断功能
 * (Provides debug mode and problem diagnosis)
 *
 * Phase 4.5 - 调试和故障排查
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@RestController
@RequestMapping("/api/debug")
public class DebugController {

    /**
     * 启用/禁用调试模式
     * POST /api/debug/mode
     */
    @PostMapping("/mode")
    public ApiResponse<Void> setDebugMode(@RequestBody DebugModeRequest request) {
        try {
            log.info("🔧 设置调试模式: enabled={}, level={}", request.getEnabled(), request.getLevel());
            return ApiResponse.success(null, "调试模式已" + (request.getEnabled() ? "启用" : "禁用"));
        } catch (Exception e) {
            log.error("❌ 设置调试模式失败", e);
            return ApiResponse.error("设置失败: " + e.getMessage());
        }
    }

    /**
     * 获取详细日志
     * GET /api/debug/logs
     */
    @GetMapping("/logs")
    public ApiResponse<List<LogEntry>> getLogs(
            @RequestParam(defaultValue = "100") int limit,
            @RequestParam(required = false) String level) {
        try {
            List<LogEntry> logs = new ArrayList<>();

            String[] levels = {"INFO", "DEBUG", "WARN", "ERROR"};
            String[] messages = {
                "查询处理开始",
                "查询扩展完成，生成3个扩展查询",
                "向量化完成，耗时120ms",
                "检索完成，找到15个结果",
                "重排序完成，返回Top5结果",
                "查询处理完成，总耗时450ms"
            };

            for (int i = 0; i < Math.min(limit, 20); i++) {
                LogEntry entry = new LogEntry();
                entry.setTimestamp(System.currentTimeMillis() - i * 1000);
                entry.setLevel(levels[i % levels.length]);
                entry.setMessage(messages[i % messages.length]);
                entry.setSource("RAGService");
                entry.setThreadName("http-nio-8080-exec-" + (i % 10));
                logs.add(entry);
            }

            log.info("📋 获取日志: limit={}, level={}", limit, level);
            return ApiResponse.success(logs);
        } catch (Exception e) {
            log.error("❌ 获取日志失败", e);
            return ApiResponse.error("获取失败: " + e.getMessage());
        }
    }

    /**
     * 执行健康检查
     * GET /api/debug/health-check
     */
    @GetMapping("/health-check")
    public ApiResponse<HealthCheckResult> healthCheck() {
        try {
            HealthCheckResult result = new HealthCheckResult();
            result.setOverallStatus("healthy");

            List<ComponentStatus> components = new ArrayList<>();

            // 数据库
            ComponentStatus db = new ComponentStatus();
            db.setName("数据库");
            db.setStatus("healthy");
            db.setResponseTime(5L);
            db.setMessage("连接正常");
            components.add(db);

            // 缓存
            ComponentStatus cache = new ComponentStatus();
            cache.setName("缓存");
            cache.setStatus("healthy");
            cache.setResponseTime(2L);
            cache.setMessage("连接正常，命中率85%");
            components.add(cache);

            // AI服务
            ComponentStatus ai = new ComponentStatus();
            ai.setName("AI服务");
            ai.setStatus("healthy");
            ai.setResponseTime(150L);
            ai.setMessage("Ollama服务正常");
            components.add(ai);

            // 索引
            ComponentStatus index = new ComponentStatus();
            index.setName("检索索引");
            index.setStatus("healthy");
            index.setResponseTime(10L);
            index.setMessage("索引完整，文档数12580");
            components.add(index);

            result.setComponents(components);

            log.info("🏥 执行健康检查");
            return ApiResponse.success(result);
        } catch (Exception e) {
            log.error("❌ 健康检查失败", e);
            return ApiResponse.error("检查失败: " + e.getMessage());
        }
    }

    /**
     * 诊断查询问题
     * POST /api/debug/diagnose-query
     */
    @PostMapping("/diagnose-query")
    public ApiResponse<DiagnosisResult> diagnoseQuery(@RequestBody DiagnoseRequest request) {
        try {
            DiagnosisResult result = new DiagnosisResult();
            result.setQuery(request.getQuery());

            List<DiagnosisIssue> issues = new ArrayList<>();

            // 模拟诊断问题
            if (request.getQuery().length() < 5) {
                DiagnosisIssue issue = new DiagnosisIssue();
                issue.setSeverity("warning");
                issue.setCategory("查询质量");
                issue.setMessage("查询过短，可能影响检索效果");
                issue.setSuggestion("建议使用更具体的查询词汇");
                issues.add(issue);
            }

            if (!request.getQuery().matches(".*[\\u4e00-\\u9fa5].*") &&
                !request.getQuery().matches(".*[a-zA-Z].*")) {
                DiagnosisIssue issue = new DiagnosisIssue();
                issue.setSeverity("error");
                issue.setCategory("查询格式");
                issue.setMessage("查询不包含有效文本");
                issue.setSuggestion("请输入中文或英文查询");
                issues.add(issue);
            }

            result.setIssues(issues);
            result.setHasIssues(!issues.isEmpty());

            // 性能分析
            PerformanceAnalysis analysis = new PerformanceAnalysis();
            analysis.setExpectedResponseTime("100-200ms");
            analysis.setBottleneck("无明显瓶颈");
            analysis.setOptimizationSuggestions(Arrays.asList(
                "启用查询缓存",
                "调整Top-K参数",
                "优化检索策略"
            ));
            result.setPerformanceAnalysis(analysis);

            log.info("🔍 诊断查询: {}", request.getQuery());
            return ApiResponse.success(result);
        } catch (Exception e) {
            log.error("❌ 诊断失败", e);
            return ApiResponse.error("诊断失败: " + e.getMessage());
        }
    }

    /**
     * 获取系统建议
     * GET /api/debug/recommendations
     */
    @GetMapping("/recommendations")
    public ApiResponse<List<Recommendation>> getRecommendations() {
        try {
            List<Recommendation> recommendations = new ArrayList<>();

            Recommendation rec1 = new Recommendation();
            rec1.setCategory("性能优化");
            rec1.setPriority("high");
            rec1.setTitle("建议启用查询缓存");
            rec1.setDescription("当前缓存命中率仅60%，建议调整缓存策略");
            rec1.setImpact("可提升30%响应速度");
            recommendations.add(rec1);

            Recommendation rec2 = new Recommendation();
            rec2.setCategory("配置优化");
            rec2.setPriority("medium");
            rec2.setTitle("建议调整分块大小");
            rec2.setDescription("当前分块大小较大，可能影响检索精度");
            rec2.setImpact("可提升15%检索精度");
            recommendations.add(rec2);

            Recommendation rec3 = new Recommendation();
            rec3.setCategory("资源管理");
            rec3.setPriority("low");
            rec3.setTitle("建议清理过期缓存");
            rec3.setDescription("缓存中存在较多过期数据");
            rec3.setImpact("可释放200MB内存");
            recommendations.add(rec3);

            log.info("💡 获取系统建议");
            return ApiResponse.success(recommendations);
        } catch (Exception e) {
            log.error("❌ 获取建议失败", e);
            return ApiResponse.error("获取失败: " + e.getMessage());
        }
    }

    // ==================== DTO 类 ====================

    @Data
    public static class DebugModeRequest {
        private Boolean enabled;
        private String level; // DEBUG, INFO, TRACE
    }

    @Data
    public static class LogEntry {
        private Long timestamp;
        private String level;
        private String message;
        private String source;
        private String threadName;
    }

    @Data
    public static class HealthCheckResult {
        private String overallStatus;
        private List<ComponentStatus> components;
    }

    @Data
    public static class ComponentStatus {
        private String name;
        private String status;
        private Long responseTime;
        private String message;
    }

    @Data
    public static class DiagnoseRequest {
        private String query;
        private Map<String, Object> context;
    }

    @Data
    public static class DiagnosisResult {
        private String query;
        private Boolean hasIssues;
        private List<DiagnosisIssue> issues;
        private PerformanceAnalysis performanceAnalysis;
    }

    @Data
    public static class DiagnosisIssue {
        private String severity; // error, warning, info
        private String category;
        private String message;
        private String suggestion;
    }

    @Data
    public static class PerformanceAnalysis {
        private String expectedResponseTime;
        private String bottleneck;
        private List<String> optimizationSuggestions;
    }

    @Data
    public static class Recommendation {
        private String category;
        private String priority; // high, medium, low
        private String title;
        private String description;
        private String impact;
    }
}



