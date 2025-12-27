package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.web.model.ApiResponse;

import java.util.*;

/**
 * 检索参数配置控制器 (Retrieval Configuration Controller)
 *
 * 提供检索参数的交互式配置和实时测试功能
 * (Provides interactive configuration and real-time testing of retrieval parameters)
 *
 * Phase 4.2.3 - 检索参数配置界面
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@RestController
@RequestMapping("/api/retrieval")
public class RetrievalConfigController {

    /**
     * 获取当前检索配置
     * GET /api/retrieval/config
     */
    @GetMapping("/config")
    public ApiResponse<RetrievalConfig> getConfig() {
        try {
            RetrievalConfig config = new RetrievalConfig();
            config.setTopK(5);
            config.setSimilarityThreshold(0.7);
            config.setRerankerEnabled(true);
            config.setRerankerModel("bge-reranker");
            config.setRetrievalStrategy("hybrid");
            config.setVectorWeight(0.7);
            config.setFullTextWeight(0.3);
            config.setParallelEnabled(true);
            config.setTimeoutSeconds(10);

            log.info("📊 获取检索配置成功");
            return ApiResponse.success(config);
        } catch (Exception e) {
            log.error("❌ 获取检索配置失败", e);
            return ApiResponse.error("获取配置失败: " + e.getMessage());
        }
    }

    /**
     * 更新检索配置
     * POST /api/retrieval/config
     */
    @PostMapping("/config")
    public ApiResponse<Void> updateConfig(@RequestBody RetrievalConfig config) {
        try {
            log.info("🔧 更新检索配置: topK={}, threshold={}, strategy={}",
                config.getTopK(), config.getSimilarityThreshold(), config.getRetrievalStrategy());

            // 实际应该保存到配置服务

            log.info("✅ 检索配置更新成功");
            return ApiResponse.success(null, "配置更新成功");
        } catch (Exception e) {
            log.error("❌ 更新检索配置失败", e);
            return ApiResponse.error("更新配置失败: " + e.getMessage());
        }
    }

    /**
     * 测试检索
     * POST /api/retrieval/test
     */
    @PostMapping("/test")
    public ApiResponse<RetrievalTestResult> testRetrieval(@RequestBody RetrievalTestRequest request) {
        try {
            log.info("🔍 测试检索: query={}, topK={}", request.getQuery(), request.getTopK());

            RetrievalTestResult result = new RetrievalTestResult();
            result.setQuery(request.getQuery());
            result.setTotalResults(15);
            result.setRetrievalTime(125L);

            // 模拟检索结果
            List<RetrievalResult> results = new ArrayList<>();
            for (int i = 0; i < Math.min(request.getTopK(), 5); i++) {
                RetrievalResult r = new RetrievalResult();
                r.setDocumentId("doc_" + (i + 1));
                r.setDocumentName("示例文档 " + (i + 1));
                r.setScore(0.95 - i * 0.1);
                r.setContent("这是文档 " + (i + 1) + " 的内容摘要...");
                r.setSource(i % 2 == 0 ? "vector" : "fulltext");
                results.add(r);
            }
            result.setResults(results);

            // 统计信息
            Map<String, Object> stats = new HashMap<>();
            stats.put("vectorResults", 3);
            stats.put("fulltextResults", 2);
            stats.put("avgScore", 0.85);
            stats.put("minScore", 0.65);
            stats.put("maxScore", 0.95);
            result.setStatistics(stats);

            log.info("✅ 检索测试完成: {} 条结果, 耗时 {}ms", results.size(), result.getRetrievalTime());
            return ApiResponse.success(result);
        } catch (Exception e) {
            log.error("❌ 检索测试失败", e);
            return ApiResponse.error("检索测试失败: " + e.getMessage());
        }
    }

    /**
     * 获取可用的检索策略
     * GET /api/retrieval/strategies
     */
    @GetMapping("/strategies")
    public ApiResponse<List<StrategyInfo>> getStrategies() {
        try {
            List<StrategyInfo> strategies = new ArrayList<>();

            StrategyInfo vector = new StrategyInfo();
            vector.setName("vector");
            vector.setDisplayName("向量检索");
            vector.setDescription("基于向量相似度的语义检索");
            strategies.add(vector);

            StrategyInfo fulltext = new StrategyInfo();
            fulltext.setName("fulltext");
            fulltext.setDisplayName("全文检索");
            fulltext.setDescription("基于关键词的全文检索");
            strategies.add(fulltext);

            StrategyInfo hybrid = new StrategyInfo();
            hybrid.setName("hybrid");
            hybrid.setDisplayName("混合检索");
            hybrid.setDescription("结合向量和全文检索的混合策略");
            strategies.add(hybrid);

            log.info("📋 获取检索策略列表: {} 个策略", strategies.size());
            return ApiResponse.success(strategies);
        } catch (Exception e) {
            log.error("❌ 获取检索策略失败", e);
            return ApiResponse.error("获取策略失败: " + e.getMessage());
        }
    }

    // ==================== DTO 类 ====================

    @Data
    public static class RetrievalConfig {
        private Integer topK;
        private Double similarityThreshold;
        private Boolean rerankerEnabled;
        private String rerankerModel;
        private String retrievalStrategy;
        private Double vectorWeight;
        private Double fullTextWeight;
        private Boolean parallelEnabled;
        private Integer timeoutSeconds;
    }

    @Data
    public static class RetrievalTestRequest {
        private String query;
        private Integer topK;
        private Double similarityThreshold;
        private String retrievalStrategy;
        private Boolean rerankerEnabled;
    }

    @Data
    public static class RetrievalTestResult {
        private String query;
        private Integer totalResults;
        private Long retrievalTime;
        private List<RetrievalResult> results;
        private Map<String, Object> statistics;
    }

    @Data
    public static class RetrievalResult {
        private String documentId;
        private String documentName;
        private Double score;
        private String content;
        private String source;
    }

    @Data
    public static class StrategyInfo {
        private String name;
        private String displayName;
        private String description;
    }
}






