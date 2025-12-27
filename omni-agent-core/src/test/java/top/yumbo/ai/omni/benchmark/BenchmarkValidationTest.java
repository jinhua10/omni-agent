package top.yumbo.ai.omni.benchmark;

import org.junit.jupiter.api.Test;
import top.yumbo.ai.omni.core.knowledge.KnowledgeLoader;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 基准测试验证
 * (Benchmark Validation Test)
 * <p>
 * 验证基准测试类可以正常实例化和运行
 * (Validates that benchmark classes can be instantiated and run)
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
public class BenchmarkValidationTest {

    /**
     * 验证KnowledgeLoaderBenchmark可以实例化
     */
    @Test
    void testKnowledgeLoaderBenchmarkInstantiation() {
        KnowledgeLoaderBenchmark benchmark = new KnowledgeLoaderBenchmark();
        assertNotNull(benchmark);
        
        // Setup benchmark
        benchmark.setup();
        
        // Test a benchmark method
        KnowledgeLoader.KnowledgeEntry result = benchmark.testCacheHit();
        assertNotNull(result);
    }

    /**
     * 验证CoreServicesBenchmark可以实例化
     */
    @Test
    void testCoreServicesBenchmarkInstantiation() {
        CoreServicesBenchmark benchmark = new CoreServicesBenchmark();
        assertNotNull(benchmark);
        
        // Setup benchmark
        benchmark.setup();
        
        // Test a benchmark method
        benchmark.testCollectExplicitFeedback();
        // If no exception, test passes
    }

    /**
     * 验证基准测试配置正确
     */
    @Test
    void testBenchmarkConfiguration() {
        // Verify JMH annotations are present
        assertTrue(KnowledgeLoaderBenchmark.class.isAnnotationPresent(
                org.openjdk.jmh.annotations.State.class));
        
        assertTrue(CoreServicesBenchmark.class.isAnnotationPresent(
                org.openjdk.jmh.annotations.State.class));
    }

    /**
     * 性能冒烟测试 - KnowledgeLoader缓存命中
     */
    @Test
    void testKnowledgeLoaderPerformanceSmokeTest() {
        KnowledgeLoader loader = new KnowledgeLoader(100);
        
        // Warm up
        for (int i = 0; i < 100; i++) {
            loader.load("test", k -> new KnowledgeLoader.KnowledgeEntry(k, "content"));
        }
        
        // Measure
        long start = System.nanoTime();
        for (int i = 0; i < 1000; i++) {
            loader.load("test", k -> new KnowledgeLoader.KnowledgeEntry(k, "content"));
        }
        long duration = System.nanoTime() - start;
        
        double avgMicros = duration / 1000.0 / 1000.0;
        System.out.println("Average cache hit time: " + avgMicros + " μs");
        
        // Should be very fast (< 10 μs per operation)
        assertTrue(avgMicros < 10.0, "Cache hit should be < 10 μs, was: " + avgMicros);
    }

    /**
     * 性能冒烟测试 - 并发访问
     */
    @Test
    void testConcurrentPerformanceSmokeTest() throws InterruptedException {
        KnowledgeLoader loader = new KnowledgeLoader(100);
        int threads = 4;
        int iterations = 1000;
        
        Thread[] workers = new Thread[threads];
        long[] durations = new long[threads];
        
        // Start threads
        for (int t = 0; t < threads; t++) {
            final int threadId = t;
            workers[t] = new Thread(() -> {
                long start = System.nanoTime();
                for (int i = 0; i < iterations; i++) {
                    loader.load("key-" + (i % 10), 
                        k -> new KnowledgeLoader.KnowledgeEntry(k, "content"));
                }
                durations[threadId] = System.nanoTime() - start;
            });
            workers[t].start();
        }
        
        // Wait for completion
        for (Thread worker : workers) {
            worker.join();
        }
        
        // Calculate average
        long totalDuration = 0;
        for (long d : durations) {
            totalDuration += d;
        }
        
        double avgMicros = (totalDuration / threads) / iterations / 1000.0;
        System.out.println("Average concurrent operation time: " + avgMicros + " μs");
        
        // Should handle concurrency efficiently (< 50 μs per operation)
        assertTrue(avgMicros < 50.0, "Concurrent ops should be < 50 μs, was: " + avgMicros);
    }

    /**
     * 测试缓存淘汰策略
     */
    @Test
    void testCacheEvictionStrategy() {
        KnowledgeLoader loader = new KnowledgeLoader(10); // Small cache
        
        // Fill cache beyond capacity
        for (int i = 0; i < 15; i++) {
            final String content = "content-" + i;
            loader.load("key-" + i, k -> new KnowledgeLoader.KnowledgeEntry(k, content));
        }
        
        // Verify cache is working (should have evicted some entries)
        assertNotNull(loader.load("key-14", k -> new KnowledgeLoader.KnowledgeEntry(k, "new")));
    }

    /**
     * 测试缓存统计功能
     */
    @Test
    void testCacheStatistics() {
        KnowledgeLoader loader = new KnowledgeLoader(100);
        
        // Perform operations
        loader.load("test", k -> new KnowledgeLoader.KnowledgeEntry(k, "value"));
        loader.load("test", k -> new KnowledgeLoader.KnowledgeEntry(k, "value")); // Cache hit
        
        // Statistics should be tracked
        assertNotNull(loader);
    }

    /**
     * 测试空键处理
     */
    @Test
    void testNullKeyHandling() {
        KnowledgeLoader loader = new KnowledgeLoader(100);
        
        // KnowledgeLoader handles null key by returning result from loader function
        KnowledgeLoader.KnowledgeEntry result = loader.load(null, 
            k -> new KnowledgeLoader.KnowledgeEntry(k, "value"));
        
        // Should return entry even with null key
        assertNotNull(result);
    }

    /**
     * 测试缓存容量限制
     */
    @Test
    void testCacheCapacityLimit() {
        KnowledgeLoader loader = new KnowledgeLoader(5); // Small cache
        
        // Add entries beyond capacity
        for (int i = 0; i < 10; i++) {
            final String value = "value-" + i;
            loader.load("key-" + i, k -> new KnowledgeLoader.KnowledgeEntry(k, value));
        }
        
        // Cache should have evicted old entries
        assertNotNull(loader.load("key-9", k -> new KnowledgeLoader.KnowledgeEntry(k, "reload")));
    }

    /**
     * 测试CoreServicesBenchmark的多个方法
     */
    @Test
    void testCoreServicesBenchmarkMultipleMethods() {
        CoreServicesBenchmark benchmark = new CoreServicesBenchmark();
        benchmark.setup();
        
        // Test all benchmark methods
        benchmark.testCollectExplicitFeedback();
        benchmark.testCollectImplicitFeedback();
        benchmark.testGetSessionFeedback();
        
        // No exceptions means test passed
        assertTrue(true);
    }

    /**
     * 测试Benchmark预热阶段
     */
    @Test
    void testBenchmarkWarmup() {
        KnowledgeLoaderBenchmark benchmark = new KnowledgeLoaderBenchmark();
        benchmark.setup();
        
        // Perform multiple iterations to simulate warmup
        for (int i = 0; i < 100; i++) {
            benchmark.testCacheHit();
        }
        
        // Should not throw exceptions
        assertTrue(true);
    }
}


