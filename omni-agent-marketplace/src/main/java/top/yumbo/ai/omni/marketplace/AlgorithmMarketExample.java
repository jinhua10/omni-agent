package top.yumbo.ai.omni.marketplace;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.storage.api.model.OptimizationData;

import java.util.List;
import java.util.Map;

/**
 * 算法市场使用示例
 *
 * 展示如何使用三种类型的算法：
 * 1. Pipeline（配置化）
 * 2. Script（脚本）
 * 3. Remote（远程服务）
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public class AlgorithmMarketExample {

    private final AlgorithmMarketService marketService;

    public AlgorithmMarketExample(AlgorithmMarketService marketService) {
        this.marketService = marketService;
    }

    /**
     * 示例1: 发布并执行Pipeline算法（推荐）
     */
    public void examplePipelineAlgorithm() {
        try {
            log.info("=== 示例1: Pipeline算法 ===");

            // 1. 创建Pipeline算法
            MarketAlgorithm algorithm = MarketAlgorithm.builder()
                    .name("EnhancedSearchPipeline")
                    .version("1.0")
                    .author("张三")
                    .description("结合查询扩展和语义分块的增强检索")
                    .type(MarketAlgorithm.AlgorithmType.PIPELINE)
                    .pipelineConfig(MarketAlgorithm.PipelineConfig.builder()
                            .maxExecutionTimeMs(5000)  // 5秒超时
                            .steps(List.of(
                                    MarketAlgorithm.PipelineStep.builder()
                                            .type("query_expansion")
                                            .params(Map.of("method", "synonym", "threshold", 0.8))
                                            .build(),
                                    MarketAlgorithm.PipelineStep.builder()
                                            .type("semantic_chunking")
                                            .params(Map.of("chunkSize", 512, "overlap", 50))
                                            .build(),
                                    MarketAlgorithm.PipelineStep.builder()
                                            .type("rerank")
                                            .params(Map.of("topK", 10))
                                            .build()
                            ))
                            .build())
                    .tags(List.of("RAG", "检索增强", "Pipeline"))
                    .build();

            // 2. 发布算法
            String algorithmId = marketService.publishAlgorithm(algorithm);
            log.info("算法已发布: id={}", algorithmId);

            // 3. 审核通过（实际应由管理员操作）
            marketService.approveAlgorithm(algorithmId);
            log.info("算法已审核通过");

            // 4. 执行算法
            OptimizationData result = marketService.executeMarketAlgorithm(
                    algorithmId,
                    "doc-123",
                    Map.of("query", "用户查询内容")
            );

            log.info("执行结果: metrics={}", result.getMetrics());
            log.info("✅ Pipeline算法执行成功！");

        } catch (Exception e) {
            log.error("Pipeline算法示例失败", e);
        }
    }

    /**
     * 示例2: 发布并执行Script算法（需要沙箱）
     */
    public void exampleScriptAlgorithm() {
        try {
            log.info("=== 示例2: Script算法 ===");

            // JavaScript脚本
            String scriptCode = """
                function optimize(documentId, context) {
                    // 自定义算法逻辑
                    var query = context.query || "";
                    var keywords = query.split(" ");
                    
                    var score = 0;
                    for (var i = 0; i < keywords.length; i++) {
                        score += keywords[i].length * 2;
                    }
                    
                    // 返回结果
                    return {
                        data: {
                            processedDocumentId: documentId,
                            relevanceScore: score,
                            matchedKeywords: keywords
                        },
                        metrics: {
                            precisionGain: 15.5,
                            latency: 50.0
                        }
                    };
                }
                """;

            // 创建Script算法
            MarketAlgorithm algorithm = MarketAlgorithm.builder()
                    .name("CustomScoreAlgorithm")
                    .version("1.0")
                    .author("李四")
                    .description("基于关键词长度的自定义评分算法")
                    .type(MarketAlgorithm.AlgorithmType.SCRIPT)
                    .script(scriptCode)
                    .scriptLanguage("javascript")
                    .tags(List.of("自定义", "评分", "JavaScript"))
                    .build();

            // 发布并审核
            String algorithmId = marketService.publishAlgorithm(algorithm);
            marketService.approveAlgorithm(algorithmId);

            log.info("Script算法已发布并审核: id={}", algorithmId);

            // 执行算法（会在沙箱中执行）
            OptimizationData result = marketService.executeMarketAlgorithm(
                    algorithmId,
                    "doc-456",
                    Map.of("query", "用户搜索关键词")
                );

            log.info("执行结果: data={}", result.getData());
            log.info("✅ Script算法执行成功！");

        } catch (Exception e) {
            log.error("Script算法示例失败", e);
        }
    }

    /**
     * 示例3: 发布Remote算法（需要外部服务）
     */
    public void exampleRemoteAlgorithm() {
        try {
            log.info("=== 示例3: Remote算法 ===");

            // 创建Remote算法
            MarketAlgorithm algorithm = MarketAlgorithm.builder()
                    .name("PythonMLAlgorithm")
                    .version("1.0")
                    .author("王五")
                    .description("Python深度学习模型算法")
                    .type(MarketAlgorithm.AlgorithmType.REMOTE)
                    .remoteEndpoint("https://api.example.com/ml-algorithm")
                    .remoteAuthToken("your-auth-token-here")
                    .tags(List.of("深度学习", "Python", "远程服务"))
                    .build();

            // 发布并审核
            String algorithmId = marketService.publishAlgorithm(algorithm);
            marketService.approveAlgorithm(algorithmId);

            log.info("Remote算法已发布并审核: id={}", algorithmId);
            log.info("⚠️ 注意：需要先启动远程服务才能执行");

            // 执行算法（会调用HTTP服务）
            // OptimizationData result = marketService.executeMarketAlgorithm(
            //         algorithmId,
            //         "doc-789",
            //         Map.of("query", "查询内容")
            // );

            log.info("✅ Remote算法配置成功（需要外部服务支持）");

        } catch (Exception e) {
            log.error("Remote算法示例失败", e);
        }
    }

    /**
     * 示例4: 搜索和浏览算法
     */
    public void exampleSearchAlgorithms() {
        log.info("=== 示例4: 搜索算法 ===");

        // 搜索包含"检索"的算法
        List<MarketAlgorithm> searchResults = marketService.searchAlgorithms("检索");
        log.info("搜索结果: 找到 {} 个算法", searchResults.size());

        for (MarketAlgorithm alg : searchResults) {
            log.info("  - {} v{} (作者: {}, 类型: {})",
                    alg.getName(), alg.getVersion(), alg.getAuthor(), alg.getType());
        }

        // 列出所有已审核的算法
        List<MarketAlgorithm> allAlgorithms = marketService.listPublishedAlgorithms(false);
        log.info("市场中共有 {} 个已审核的算法", allAlgorithms.size());
    }

    /**
     * 运行所有示例
     */
    public void runAllExamples() {
        examplePipelineAlgorithm();
        exampleScriptAlgorithm();
        exampleRemoteAlgorithm();
        exampleSearchAlgorithms();
    }

    /**
     * 主方法（用于测试）
     */
    public static void main(String[] args) {
        // 创建服务实例
        AlgorithmMarketService marketService = new AlgorithmMarketService();

        // 运行示例
        AlgorithmMarketExample example = new AlgorithmMarketExample(marketService);
        example.runAllExamples();
    }
}

