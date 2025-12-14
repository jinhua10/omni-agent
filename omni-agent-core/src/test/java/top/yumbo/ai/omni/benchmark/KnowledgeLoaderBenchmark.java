package top.yumbo.ai.omni.benchmark;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import top.yumbo.ai.omni.core.knowledge.KnowledgeLoader;

import java.util.concurrent.TimeUnit;

/**
 * KnowledgeLoader性能基准测试
 * (KnowledgeLoader Performance Benchmark)
 * <p>
 * 使用JMH测试关键性能指标
 * (Uses JMH to test key performance metrics)
 * <p>
 * 运行方式 (Run with):
 * mvn test -Dtest=KnowledgeLoaderBenchmark
 * 或直接运行main方法 (or run main method directly)
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
@Fork(value = 1, warmups = 1)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
public class KnowledgeLoaderBenchmark {

    private KnowledgeLoader knowledgeLoader;
    private static final int CACHE_SIZE = 1000;
    private static final String TEST_KEY = "benchmark-key";

    /**
     * 基准测试初始化
     */
    @Setup(Level.Trial)
    public void setup() {
        knowledgeLoader = new KnowledgeLoader(CACHE_SIZE);
        
        // Pre-populate cache with some entries
        for (int i = 0; i < 100; i++) {
            String key = "preload-" + i;
            knowledgeLoader.load(key, k -> 
                new KnowledgeLoader.KnowledgeEntry(k, "Preload content " + i));
        }
    }

    /**
     * 基准测试1: 缓存命中性能
     * (Benchmark 1: Cache hit performance)
     */
    @Benchmark
    public KnowledgeLoader.KnowledgeEntry testCacheHit() {
        // First load to populate cache
        knowledgeLoader.load(TEST_KEY, key -> 
            new KnowledgeLoader.KnowledgeEntry(key, "Test content"));
        
        // Second load should hit cache
        return knowledgeLoader.load(TEST_KEY, key -> null);
    }

    /**
     * 基准测试2: 缓存未命中性能
     * (Benchmark 2: Cache miss performance)
     */
    @Benchmark
    public KnowledgeLoader.KnowledgeEntry testCacheMiss() {
        String key = "miss-" + System.nanoTime();
        return knowledgeLoader.load(key, k -> 
            new KnowledgeLoader.KnowledgeEntry(k, "New content"));
    }

    /**
     * 基准测试3: 批量加载性能
     * (Benchmark 3: Batch load performance)
     */
    @Benchmark
    public int testBatchLoad() {
        java.util.List<String> keys = java.util.stream.IntStream.range(0, 10)
            .mapToObj(i -> "batch-" + i)
            .toList();
        
        var results = knowledgeLoader.batchLoad(keys, k -> 
            new KnowledgeLoader.KnowledgeEntry(k, "Batch content"));
        
        return results.size();
    }

    /**
     * 基准测试4: LRU淘汰性能
     * (Benchmark 4: LRU eviction performance)
     */
    @Benchmark
    public KnowledgeLoader.KnowledgeEntry testLRUEviction() {
        // Fill cache to trigger eviction
        for (int i = 0; i < CACHE_SIZE + 10; i++) {
            String key = "evict-" + i;
            knowledgeLoader.load(key, k -> 
                new KnowledgeLoader.KnowledgeEntry(k, "Content " + i));
        }
        
        // Load one more to trigger eviction
        return knowledgeLoader.load("final-key", k -> 
            new KnowledgeLoader.KnowledgeEntry(k, "Final content"));
    }

    /**
     * 基准测试5: 统计信息访问性能
     * (Benchmark 5: Statistics access performance)
     */
    @Benchmark
    public double testStatisticsAccess() {
        return knowledgeLoader.getStatistics().getHitRate();
    }

    /**
     * 基准测试6: 缓存刷新性能
     * (Benchmark 6: Cache refresh performance)
     */
    @Benchmark
    public void testCacheRefresh() {
        knowledgeLoader.refresh(TEST_KEY);
    }

    /**
     * 基准测试7: 并发读取性能（单线程基准）
     * (Benchmark 7: Concurrent read performance - single thread baseline)
     */
    @Benchmark
    @Threads(1)
    public KnowledgeLoader.KnowledgeEntry testSingleThreadRead() {
        return knowledgeLoader.load("concurrent-key", k -> 
            new KnowledgeLoader.KnowledgeEntry(k, "Concurrent content"));
    }

    /**
     * 基准测试8: 并发读取性能（4线程）
     * (Benchmark 8: Concurrent read performance - 4 threads)
     */
    @Benchmark
    @Threads(4)
    public KnowledgeLoader.KnowledgeEntry testMultiThreadRead() {
        return knowledgeLoader.load("concurrent-key", k -> 
            new KnowledgeLoader.KnowledgeEntry(k, "Concurrent content"));
    }

    /**
     * 基准测试9: 混合工作负载（70%读，30%写）
     * (Benchmark 9: Mixed workload - 70% read, 30% write)
     */
    @Benchmark
    public KnowledgeLoader.KnowledgeEntry testMixedWorkload() {
        int random = (int) (System.nanoTime() % 10);
        
        if (random < 7) {
            // 70% reads
            return knowledgeLoader.load("mixed-read", k -> 
                new KnowledgeLoader.KnowledgeEntry(k, "Read content"));
        } else {
            // 30% writes (cache misses)
            String writeKey = "mixed-write-" + random;
            return knowledgeLoader.load(writeKey, k -> 
                new KnowledgeLoader.KnowledgeEntry(k, "Write content"));
        }
    }

    /**
     * 主函数 - 运行所有基准测试
     * (Main function - run all benchmarks)
     */
    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(KnowledgeLoaderBenchmark.class.getSimpleName())
                .forks(1)
                .build();

        new Runner(opt).run();
    }
}
