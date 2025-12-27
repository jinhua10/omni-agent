package top.yumbo.ai.omni.example.basic;

import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import top.yumbo.ai.omni.marketplace.EnhancedQueryService;
import top.yumbo.ai.omni.rag.model.SearchResult;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Phase 1 增强查询服务性能测试
 * (Phase 1 Enhanced Query Service Performance Test)
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@SpringBootTest
@DisplayName("Phase 1 增强查询服务测试")
class EnhancedQueryServicePhase1Test {

    @Autowired
    private EnhancedQueryService enhancedQueryService;

    @BeforeEach
    void setUp() {
        log.info("========================================");
        log.info("开始测试 Phase 1 增强查询功能");
        log.info("========================================");
    }

    @Test
    @DisplayName("测试1: 基础查询扩展")
    void testBasicQueryExpansion() {
        log.info("【测试1】基础查询扩展");

        String question = "Spring Boot如何配置?";
        int topK = 10;

        long startTime = System.currentTimeMillis();
        List<SearchResult> results = enhancedQueryService
                .enhancedSearchWithExpansion(question, topK);
        long duration = System.currentTimeMillis() - startTime;

        assertNotNull(results, "结果不应为空");
        assertTrue(results.size() <= topK, "结果数量应不超过topK");

        log.info("✅ 查询完成: 耗时 {}ms, 返回 {} 个结果", duration, results.size());

        // 打印前3个结果
        for (int i = 0; i < Math.min(3, results.size()); i++) {
            SearchResult result = results.get(i);
            log.info("   结果 {}: score={:.4f}, content={}",
                    i + 1, result.getScore(),
                    result.getDocument().getContent().substring(0, Math.min(50, result.getDocument().getContent().length())));
        }
    }

    @Test
    @DisplayName("测试2: 完整增强查询（扩展+重排序）")
    void testFullyEnhancedSearch() {
        log.info("【测试2】完整增强查询");

        String question = "Spring Boot如何配置?";
        int topK = 10;

        long startTime = System.currentTimeMillis();
        List<SearchResult> results = enhancedQueryService
                .fullyEnhancedSearch(question, topK);
        long duration = System.currentTimeMillis() - startTime;

        assertNotNull(results, "结果不应为空");
        assertTrue(results.size() <= topK, "结果数量应不超过topK");

        log.info("✅ 完整增强查询完成: 耗时 {}ms, 返回 {} 个结果", duration, results.size());
    }

    @Test
    @DisplayName("测试3: 缓存性能测试")
    void testCachePerformance() {
        log.info("【测试3】缓存性能测试");

        String question = "Spring Boot如何配置?";
        int topK = 10;

        // 第一次查询（未命中缓存）
        long startTime1 = System.currentTimeMillis();
        List<SearchResult> results1 = enhancedQueryService
                .enhancedSearchWithExpansion(question, topK);
        long duration1 = System.currentTimeMillis() - startTime1;

        // 第二次查询（应该命中缓存）
        long startTime2 = System.currentTimeMillis();
        List<SearchResult> results2 = enhancedQueryService
                .enhancedSearchWithExpansion(question, topK);
        long duration2 = System.currentTimeMillis() - startTime2;

        log.info("✅ 第一次查询（未命中缓存）: {}ms", duration1);
        log.info("✅ 第二次查询（命中缓存）: {}ms", duration2);
        log.info("✅ 性能提升: {:.2f}x", (double) duration1 / duration2);

        assertTrue(duration2 < duration1 * 0.5,
                "缓存命中应该显著减少响应时间（至少50%）");
    }

    @Test
    @DisplayName("测试4: 并行执行性能测试")
    void testParallelPerformance() {
        log.info("【测试4】并行执行性能测试");

        String question = "Spring Boot如何配置?";
        int topK = 10;

        // 清除缓存
        enhancedQueryService.clearCache();

        long startTime = System.currentTimeMillis();
        List<SearchResult> results = enhancedQueryService
                .enhancedSearchWithExpansion(question, topK);
        long duration = System.currentTimeMillis() - startTime;

        assertNotNull(results, "结果不应为空");

        log.info("✅ 并行查询完成: 耗时 {}ms", duration);
        log.info("   提示: 对比串行执行可以看到性能提升");
    }

    @Test
    @DisplayName("测试5: 统计信息测试")
    void testStatistics() {
        log.info("【测试5】统计信息测试");

        // 执行几次查询
        enhancedQueryService.enhancedSearchWithExpansion("Spring Boot如何配置?", 10);
        enhancedQueryService.enhancedSearchWithExpansion("Spring Boot如何配置?", 10); // 应命中缓存
        enhancedQueryService.enhancedSearchWithExpansion("Java如何学习?", 10);

        // 获取统计信息
        Map<String, Object> stats = enhancedQueryService.getStatistics();

        assertNotNull(stats, "统计信息不应为空");

        log.info("✅ 统计信息:");
        log.info("   算法市场可用: {}", stats.get("algorithmMarketAvailable"));
        log.info("   AI服务可用: {}", stats.get("aiServiceAvailable"));
        log.info("   缓存服务可用: {}", stats.get("cacheServiceAvailable"));
        log.info("   配置启用: {}", stats.get("configEnabled"));
        log.info("   LLM启用: {}", stats.get("llmEnabled"));
        log.info("   并行启用: {}", stats.get("parallelEnabled"));

        // 打印缓存统计
        @SuppressWarnings("unchecked")
        Map<String, Object> cacheStats = (Map<String, Object>) stats.get("cacheStatistics");
        if (cacheStats != null) {
            log.info("   缓存统计:");
            log.info("     查询缓存大小: {}", cacheStats.get("queryCacheSize"));
            log.info("     查询缓存命中: {}", cacheStats.get("queryCacheHits"));
            log.info("     查询缓存未命中: {}", cacheStats.get("queryCacheMisses"));
            log.info("     查询缓存命中率: {}", cacheStats.get("queryCacheHitRate"));
            log.info("     扩展缓存大小: {}", cacheStats.get("expansionCacheSize"));
            log.info("     扩展缓存命中: {}", cacheStats.get("expansionCacheHits"));
            log.info("     扩展缓存未命中: {}", cacheStats.get("expansionCacheMisses"));
            log.info("     扩展缓存命中率: {}", cacheStats.get("expansionCacheHitRate"));
            log.info("     总体命中率: {}", cacheStats.get("overallHitRate"));
        }
    }

    @Test
    @DisplayName("测试6: 自定义增强查询")
    void testCustomEnhancedSearch() {
        log.info("【测试6】自定义增强查询");

        String question = "Spring Boot如何配置?";
        int topK = 10;

        // 测试不同组合
        log.info("   测试组合1: 仅扩展");
        long start1 = System.currentTimeMillis();
        List<SearchResult> results1 = enhancedQueryService
                .enhancedSearch(question, topK, true, false);
        long duration1 = System.currentTimeMillis() - start1;
        log.info("   耗时: {}ms, 结果数: {}", duration1, results1.size());

        log.info("   测试组合2: 仅重排序");
        long start2 = System.currentTimeMillis();
        List<SearchResult> results2 = enhancedQueryService
                .enhancedSearch(question, topK, false, true);
        long duration2 = System.currentTimeMillis() - start2;
        log.info("   耗时: {}ms, 结果数: {}", duration2, results2.size());

        log.info("   测试组合3: 扩展+重排序");
        long start3 = System.currentTimeMillis();
        List<SearchResult> results3 = enhancedQueryService
                .enhancedSearch(question, topK, true, true);
        long duration3 = System.currentTimeMillis() - start3;
        log.info("   耗时: {}ms, 结果数: {}", duration3, results3.size());

        log.info("✅ 所有组合测试完成");
    }

    @Test
    @DisplayName("测试7: 多查询性能基准")
    void testMultiQueryBenchmark() {
        log.info("【测试7】多查询性能基准");

        String[] questions = {
                "Spring Boot如何配置?",
                "Java如何学习?",
                "数据库如何优化?",
                "Redis如何使用?",
                "Docker如何部署?"
        };

        // 清除缓存
        enhancedQueryService.clearCache();

        long totalTime = 0;
        int totalResults = 0;

        for (String question : questions) {
            long startTime = System.currentTimeMillis();
            List<SearchResult> results = enhancedQueryService
                    .enhancedSearchWithExpansion(question, 10);
            long duration = System.currentTimeMillis() - startTime;

            totalTime += duration;
            totalResults += results.size();

            log.info("   问题: '{}', 耗时: {}ms, 结果: {}", question, duration, results.size());
        }

        long avgTime = totalTime / questions.length;
        double avgResults = (double) totalResults / questions.length;

        log.info("✅ 基准测试完成:");
        log.info("   总耗时: {}ms", totalTime);
        log.info("   平均耗时: {}ms/查询", avgTime);
        log.info("   平均结果数: {:.1f}/查询", avgResults);
    }

    @Test
    @DisplayName("测试8: 清除缓存功能")
    void testClearCache() {
        log.info("【测试8】清除缓存功能");

        String question = "Spring Boot如何配置?";

        // 执行查询并缓存
        enhancedQueryService.enhancedSearchWithExpansion(question, 10);

        // 获取统计（应有缓存）
        Map<String, Object> stats1 = enhancedQueryService.getStatistics();
        @SuppressWarnings("unchecked")
        Map<String, Object> cacheStats1 = (Map<String, Object>) stats1.get("cacheStatistics");

        if (cacheStats1 != null) {
            log.info("   清除前缓存大小: {}", cacheStats1.get("queryCacheSize"));
        }

        // 清除缓存
        enhancedQueryService.clearCache();
        log.info("✅ 缓存已清除");

        // 再次获取统计（缓存应为空）
        Map<String, Object> stats2 = enhancedQueryService.getStatistics();
        @SuppressWarnings("unchecked")
        Map<String, Object> cacheStats2 = (Map<String, Object>) stats2.get("cacheStatistics");

        if (cacheStats2 != null) {
            log.info("   清除后缓存大小: {}", cacheStats2.get("queryCacheSize"));
            assertEquals(0L, cacheStats2.get("queryCacheSize"), "缓存应已清空");
        }
    }
}


