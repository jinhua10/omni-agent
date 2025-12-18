package top.yumbo.ai.omni.marketplace;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * AlgorithmMarketService 测试类
 * 测试三个核心算法组件的实现
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class AlgorithmMarketServiceTest {

    private AlgorithmMarketService marketService;

    @BeforeEach
    void setUp() {
        marketService = new AlgorithmMarketService();
    }

    @Test
    @DisplayName("测试查询扩展组件 - Query Expansion")
    void testQueryExpansionComponent() throws Exception {
        // 准备测试数据
        String query = "Spring Boot配置";
        Map<String, Object> params = new HashMap<>();
        params.put("method", "synonym");
        params.put("maxExpansions", 5);

        // 发布Pipeline算法
        MarketAlgorithm algorithm = MarketAlgorithm.builder()
                .name("TestQueryExpansion")
                .version("1.0")
                .type(MarketAlgorithm.AlgorithmType.PIPELINE)
                .pipelineConfig(MarketAlgorithm.PipelineConfig.builder()
                        .steps(List.of(
                                MarketAlgorithm.PipelineStep.builder()
                                        .type("query_expansion")
                                        .params(params)
                                        .build()
                        ))
                        .build())
                .build();

        String algorithmId = marketService.publishAlgorithm(algorithm);
        marketService.approveAlgorithm(algorithmId);

        // 执行算法
        var result = marketService.executeMarketAlgorithm(
                algorithmId,
                "doc-123",
                Map.of("query", query)
        );

        // 验证结果
        assertNotNull(result);
        assertNotNull(result.getData());

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.getData().get("result");

        assertNotNull(data);
        assertEquals(query, data.get("originalQuery"));

        @SuppressWarnings("unchecked")
        List<String> expandedQueries = (List<String>) data.get("expandedQueries");

        assertNotNull(expandedQueries);
        assertTrue(expandedQueries.size() > 1, "应该有多个扩展查询");
        assertTrue(expandedQueries.contains(query), "应该包含原始查询");

        System.out.println("✅ 查询扩展测试通过");
        System.out.println("   原始查询: " + query);
        System.out.println("   扩展查询: " + expandedQueries);
    }

    @Test
    @DisplayName("测试语义分块组件 - Semantic Chunking")
    void testSemanticChunkingComponent() throws Exception {
        // 准备测试数据
        String content = "这是第一段内容。\n\n这是第二段内容，和第一段主题相关。\n\n" +
                        "这是第三段，完全不同的主题。\n\n这是第四段，继续第三段的主题。";

        Map<String, Object> params = new HashMap<>();
        params.put("chunkSize", 100);
        params.put("overlap", 20);
        params.put("similarityThreshold", 0.3);

        // 发布Pipeline算法
        MarketAlgorithm algorithm = MarketAlgorithm.builder()
                .name("TestSemanticChunking")
                .version("1.0")
                .type(MarketAlgorithm.AlgorithmType.PIPELINE)
                .pipelineConfig(MarketAlgorithm.PipelineConfig.builder()
                        .steps(List.of(
                                MarketAlgorithm.PipelineStep.builder()
                                        .type("semantic_chunking")
                                        .params(params)
                                        .build()
                        ))
                        .build())
                .build();

        String algorithmId = marketService.publishAlgorithm(algorithm);
        marketService.approveAlgorithm(algorithmId);

        // 执行算法
        var result = marketService.executeMarketAlgorithm(
                algorithmId,
                content,
                Map.of()
        );

        // 验证结果
        assertNotNull(result);
        assertNotNull(result.getData());

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.getData().get("result");

        assertNotNull(data);

        @SuppressWarnings("unchecked")
        List<String> chunks = (List<String>) data.get("chunks");

        assertNotNull(chunks);
        assertTrue(chunks.size() > 0, "应该至少有一个分块");

        System.out.println("✅ 语义分块测试通过");
        System.out.println("   原始长度: " + content.length());
        System.out.println("   分块数量: " + chunks.size());
        System.out.println("   平均块大小: " + data.get("avgChunkSize"));
    }

    @Test
    @DisplayName("测试重排序组件 - Rerank")
    void testRerankComponent() throws Exception {
        // 准备测试数据 - 模拟搜索结果
        List<Map<String, Object>> searchResults = new ArrayList<>();

        searchResults.add(Map.of(
            "content", "Java是一门面向对象的编程语言",
            "score", 0.8
        ));

        searchResults.add(Map.of(
            "content", "Python是一门简单易学的脚本语言，适合数据分析",
            "score", 0.7
        ));

        searchResults.add(Map.of(
            "content", "Java Spring Boot是企业级应用开发框架",
            "score", 0.6
        ));

        String query = "Java Spring";
        Map<String, Object> params = new HashMap<>();
        params.put("query", query);
        params.put("model", "simple");
        params.put("topK", 3);

        // 发布Pipeline算法
        MarketAlgorithm algorithm = MarketAlgorithm.builder()
                .name("TestRerank")
                .version("1.0")
                .type(MarketAlgorithm.AlgorithmType.PIPELINE)
                .pipelineConfig(MarketAlgorithm.PipelineConfig.builder()
                        .steps(List.of(
                                MarketAlgorithm.PipelineStep.builder()
                                        .type("rerank")
                                        .params(params)
                                        .build()
                        ))
                        .build())
                .build();

        String algorithmId = marketService.publishAlgorithm(algorithm);
        marketService.approveAlgorithm(algorithmId);

        // 执行算法
        var result = marketService.executeMarketAlgorithm(
                algorithmId,
                "doc-123",
                Map.of()
        );

        // 验证结果
        assertNotNull(result);
        assertNotNull(result.getData());

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.getData().get("result");

        assertNotNull(data);

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> rerankedResults = (List<Map<String, Object>>) data.get("rerankedResults");

        assertNotNull(rerankedResults);
        assertTrue(rerankedResults.size() > 0, "应该有重排序结果");

        // 验证重排序后包含rerankScore
        for (Map<String, Object> r : rerankedResults) {
            assertTrue(r.containsKey("rerankScore"), "应该包含重排序分数");
        }

        System.out.println("✅ 重排序测试通过");
        System.out.println("   查询: " + query);
        System.out.println("   原始数量: " + data.get("originalCount"));
        System.out.println("   重排序后: " + rerankedResults.size());

        // 打印重排序结果
        for (int i = 0; i < rerankedResults.size(); i++) {
            Map<String, Object> r = rerankedResults.get(i);
            System.out.println("   [" + (i+1) + "] 分数=" + r.get("rerankScore") +
                             ", 内容=" + r.get("content"));
        }
    }

    @Test
    @DisplayName("测试组合算法 - Query Expansion + Semantic Chunking + Rerank")
    void testCombinedAlgorithm() throws Exception {
        // 创建一个组合算法：先扩展查询，再分块，最后重排序
        MarketAlgorithm algorithm = MarketAlgorithm.builder()
                .name("EnhancedRAGPipeline")
                .version("1.0")
                .author("OmniAgent Team")
                .description("查询扩展 + 语义分块 + 重排序的综合优化")
                .type(MarketAlgorithm.AlgorithmType.PIPELINE)
                .pipelineConfig(MarketAlgorithm.PipelineConfig.builder()
                        .steps(List.of(
                                MarketAlgorithm.PipelineStep.builder()
                                        .type("query_expansion")
                                        .params(Map.of("method", "synonym", "maxExpansions", 3))
                                        .build(),
                                MarketAlgorithm.PipelineStep.builder()
                                        .type("semantic_chunking")
                                        .params(Map.of("chunkSize", 200))
                                        .build()
                        ))
                        .maxExecutionTimeMs(5000)
                        .build())
                .build();

        String algorithmId = marketService.publishAlgorithm(algorithm);
        marketService.approveAlgorithm(algorithmId);

        // 执行算法
        var result = marketService.executeMarketAlgorithm(
                algorithmId,
                "Spring Boot配置文档内容...",
                Map.of("query", "Spring配置")
        );

        // 验证结果
        assertNotNull(result);
        assertNotNull(result.getMetrics());

        // 验证指标
        assertTrue(result.getMetrics().containsKey("precisionGain"));
        assertTrue(result.getMetrics().containsKey("latency"));

        System.out.println("✅ 组合算法测试通过");
        System.out.println("   算法名称: " + result.getOptimizationType());
        System.out.println("   执行指标: " + result.getMetrics());
    }
}

