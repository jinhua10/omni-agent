package top.yumbo.ai.omni.core.optimization;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import top.yumbo.ai.omni.core.optimization.AutoOptimizationSelector.*;
import top.yumbo.ai.storage.api.model.OptimizationType;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 自动算法选择引擎单元测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class AutoOptimizationSelectorTest {

    private AutoOptimizationSelector selector;

    @BeforeEach
    void setUp() {
        selector = new AutoOptimizationSelector();
    }

    // ========== 查询长度测试 ==========

    @Test
    void testExtremelyShortQuery() {
        // Given: 极短查询
        QueryContext context = QueryContext.fromQuery("API");

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        assertTrue(recommendation.getPrimaryAlgorithms().contains(OptimizationType.QUERY_EXPANSION.getCode()));
        assertTrue(recommendation.getExpectedPrecisionGain() > 0);
        assertNotNull(recommendation.getReasoning());
    }

    @Test
    void testShortQuery() {
        // Given: 短查询
        QueryContext context = QueryContext.fromQuery("Spring Boot配置");

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        List<String> allAlgorithms = recommendation.getPrimaryAlgorithms();
        allAlgorithms.addAll(recommendation.getSecondaryAlgorithms());

        assertTrue(allAlgorithms.contains(OptimizationType.PPL.getCode()) ||
                  allAlgorithms.contains(OptimizationType.QUERY_EXPANSION.getCode()));
    }

    @Test
    void testMediumQuery() {
        // Given: 中等查询
        QueryContext context = QueryContext.fromQuery("如何在Spring Boot中配置MySQL数据库连接");

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        assertTrue(recommendation.getPrimaryAlgorithms().contains(OptimizationType.PPL.getCode()));
    }

    @Test
    void testLongQuery() {
        // Given: 长查询
        String longQuery = "我想在Spring Boot项目中使用MySQL数据库，" +
                "但是不知道如何配置连接池参数以及如何优化查询性能，" +
                "请提供详细的配置示例和最佳实践建议";
        QueryContext context = QueryContext.fromQuery(longQuery);

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        assertTrue(recommendation.getPrimaryAlgorithms().contains(OptimizationType.CONTEXT_COMPRESSION.getCode()));
    }

    // ========== 文档类型测试 ==========

    @Test
    void testTechnicalDocument() {
        // Given: 技术文档
        QueryContext context = QueryContext.fromQuery("如何使用Java实现二叉树");
        context.setDocumentType("technical");

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        List<String> allAlgorithms = recommendation.getPrimaryAlgorithms();
        allAlgorithms.addAll(recommendation.getSecondaryAlgorithms());

        assertTrue(allAlgorithms.contains(OptimizationType.SEMANTIC_CHUNKING.getCode()));
    }

    @Test
    void testFAQDocument() {
        // Given: FAQ文档
        QueryContext context = QueryContext.fromQuery("如何重置密码");
        context.setDocumentType("faq");

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        List<String> allAlgorithms = recommendation.getPrimaryAlgorithms();
        allAlgorithms.addAll(recommendation.getSecondaryAlgorithms());

        assertTrue(allAlgorithms.contains(OptimizationType.HOPE_ROUTING.getCode()));
    }

    @Test
    void testAcademicDocument() {
        // Given: 学术文献
        QueryContext context = QueryContext.fromQuery("深度学习在自然语言处理中的应用");
        context.setDocumentType("academic");

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        List<String> allAlgorithms = recommendation.getPrimaryAlgorithms();
        allAlgorithms.addAll(recommendation.getSecondaryAlgorithms());

        assertTrue(allAlgorithms.contains(OptimizationType.KNOWLEDGE_GRAPH.getCode()) ||
                  allAlgorithms.contains(OptimizationType.RERANK.getCode()));
    }

    @Test
    void testEcommerceDocument() {
        // Given: 电商产品
        QueryContext context = QueryContext.fromQuery("iPhone手机");
        context.setDocumentType("ecommerce");

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        List<String> allAlgorithms = recommendation.getPrimaryAlgorithms();
        allAlgorithms.addAll(recommendation.getSecondaryAlgorithms());

        assertTrue(allAlgorithms.contains(OptimizationType.METADATA_FILTER.getCode()));
    }

    // ========== 性能要求测试 ==========

    @Test
    void testLowLatencyRequirement() {
        // Given: 低延迟要求 (<100ms)
        QueryContext context = QueryContext.fromQuery("快速查询测试");
        context.setLatencyRequirementMs(50);

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        // 不应包含慢速算法
        List<String> allAlgorithms = recommendation.getPrimaryAlgorithms();
        allAlgorithms.addAll(recommendation.getSecondaryAlgorithms());

        assertFalse(allAlgorithms.contains(OptimizationType.MULTI_MODEL_VOTING.getCode()));
        assertFalse(allAlgorithms.contains(OptimizationType.KNOWLEDGE_GRAPH.getCode()));

        // 预期延迟应该较低
        assertTrue(recommendation.getExpectedLatencyMs() < 150);
    }

    @Test
    void testHighPrecisionRequirement() {
        // Given: 高精度要求 (>95%)
        QueryContext context = QueryContext.fromQuery("高精度查询测试");
        context.setPrecisionRequirement(0.96);
        context.setLatencyRequirementMs(400); // 可接受较高延迟

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        assertTrue(recommendation.getPrimaryAlgorithms().contains(OptimizationType.RERANK.getCode()));
        assertTrue(recommendation.getExpectedPrecisionGain() > 30.0); // 高精度增益
    }

    @Test
    void testMediumPrecisionRequirement() {
        // Given: 中等精度要求 (90-95%)
        QueryContext context = QueryContext.fromQuery("中等精度查询");
        context.setPrecisionRequirement(0.92);

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        assertNotNull(recommendation);
        assertTrue(recommendation.getExpectedPrecisionGain() > 0);
    }

    // ========== 综合场景测试 ==========

    @Test
    void testCustomerServiceScenario() {
        // Given: 客服场景 - 短查询、FAQ、低延迟、高精度
        QueryContext context = QueryContext.fromQuery("如何退款");
        context.setDocumentType("faq");
        context.setLatencyRequirementMs(80);
        context.setPrecisionRequirement(0.93);

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        assertNotNull(recommendation);
        assertTrue(recommendation.getExpectedLatencyMs() < 150);
        assertTrue(recommendation.getExpectedPrecisionGain() > 25.0);

        // 应包含HOPE Routing（客服高频查询）
        List<String> allAlgorithms = recommendation.getPrimaryAlgorithms();
        allAlgorithms.addAll(recommendation.getSecondaryAlgorithms());
        assertTrue(allAlgorithms.contains(OptimizationType.HOPE_ROUTING.getCode()));
    }

    @Test
    void testTechnicalDocumentScenario() {
        // Given: 技术文档场景 - 中等查询、技术文档、中延迟、高精度
        QueryContext context = QueryContext.fromQuery("Spring Boot自动配置原理详解");
        context.setDocumentType("technical");
        context.setLatencyRequirementMs(250);
        context.setPrecisionRequirement(0.94);

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        List<String> allAlgorithms = recommendation.getPrimaryAlgorithms();
        allAlgorithms.addAll(recommendation.getSecondaryAlgorithms());

        assertTrue(allAlgorithms.contains(OptimizationType.PPL.getCode()));
        assertTrue(allAlgorithms.contains(OptimizationType.SEMANTIC_CHUNKING.getCode()));
    }

    @Test
    void testAcademicSearchScenario() {
        // Given: 学术检索场景 - 长查询、学术文档、高延迟可接受、极高精度
        String longQuery = "请详细介绍Transformer模型在自然语言处理中的应用及其改进方向";
        QueryContext context = QueryContext.fromQuery(longQuery);
        context.setDocumentType("academic");
        context.setLatencyRequirementMs(500);
        context.setPrecisionRequirement(0.97);

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        List<String> allAlgorithms = recommendation.getPrimaryAlgorithms();
        allAlgorithms.addAll(recommendation.getSecondaryAlgorithms());

        assertTrue(allAlgorithms.contains(OptimizationType.CONTEXT_COMPRESSION.getCode()));
        assertTrue(allAlgorithms.contains(OptimizationType.RERANK.getCode()));
        assertTrue(recommendation.getExpectedPrecisionGain() > 35.0);
    }

    // ========== 批量评估测试 ==========

    @Test
    void testEvaluateMultipleScenarios() {
        // Given: 多个场景
        List<QueryContext> contexts = Arrays.asList(
            createContext("API", "technical", 50, 0.90),
            createContext("如何配置数据库", "faq", 100, 0.92),
            createContext("深度学习研究综述论文分析", "academic", 400, 0.96)
        );

        // When
        Map<String, OptimizationRecommendation> results = selector.evaluateScenarios(contexts);

        // Then
        assertEquals(3, results.size());

        for (OptimizationRecommendation recommendation : results.values()) {
            assertNotNull(recommendation);
            assertFalse(recommendation.getPrimaryAlgorithms().isEmpty());
            assertTrue(recommendation.getExpectedPrecisionGain() > 0);
            assertTrue(recommendation.getExpectedLatencyMs() > 0);
            assertNotNull(recommendation.getReasoning());
        }
    }

    // ========== 边界条件测试 ==========

    @Test
    void testEmptyQuery() {
        // Given: 空查询
        QueryContext context = QueryContext.fromQuery("");

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        assertNotNull(recommendation);
        assertFalse(recommendation.getPrimaryAlgorithms().isEmpty());
    }

    @Test
    void testNullQuery() {
        // Given: null查询
        QueryContext context = QueryContext.fromQuery(null);

        // When
        OptimizationRecommendation recommendation = selector.selectOptimalAlgorithms(context);

        // Then
        assertNotNull(recommendation);
        assertFalse(recommendation.getPrimaryAlgorithms().isEmpty());
    }

    @Test
    void testExtremeLatencyRequirement() {
        // Given: 极端延迟要求
        QueryContext context1 = QueryContext.fromQuery("测试");
        context1.setLatencyRequirementMs(10); // 极低延迟

        QueryContext context2 = QueryContext.fromQuery("测试");
        context2.setLatencyRequirementMs(1000); // 极高延迟可接受

        // When
        OptimizationRecommendation rec1 = selector.selectOptimalAlgorithms(context1);
        OptimizationRecommendation rec2 = selector.selectOptimalAlgorithms(context2);

        // Then
        assertTrue(rec1.getExpectedLatencyMs() < rec2.getExpectedLatencyMs());
        assertTrue(rec2.getExpectedPrecisionGain() >= rec1.getExpectedPrecisionGain());
    }

    // ========== 辅助方法 ==========

    private QueryContext createContext(String query, String docType, int latency, double precision) {
        QueryContext context = QueryContext.fromQuery(query);
        context.setDocumentType(docType);
        context.setLatencyRequirementMs(latency);
        context.setPrecisionRequirement(precision);
        return context;
    }
}

