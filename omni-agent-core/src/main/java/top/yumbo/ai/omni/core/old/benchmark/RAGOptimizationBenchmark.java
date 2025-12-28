package top.yumbo.ai.omni.core.benchmark;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.core.optimization.RAGOptimizationService;

import java.util.*;
import java.util.concurrent.*;

/**
 * RAG优化算法性能基准测试
 * <p>
 * 测试各种优化算法的性能指标：
 * - 保存性能（TPS）
 * - 查询性能（QPS）
 * - 延迟分布（P50, P95, P99）
 * - 内存占用
 * - 并发性能
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@Component
public class RAGOptimizationBenchmark {

    @Autowired
    private RAGOptimizationService optimizationService;

    /**
     * 基准测试配置
     */
    @Data
    public static class BenchmarkConfig {
        private int warmupIterations = 100;      // 预热次数
        private int testIterations = 1000;       // 测试次数
        private int concurrentThreads = 10;      // 并发线程数
        private int dataSize = 1000;             // 数据大小（字节）
        private boolean enableGC = true;         // 每轮测试前是否GC
    }

    /**
     * 基准测试结果
     */
    @Data
    public static class BenchmarkResult {
        private String algorithmType;
        private String operation;
        private long totalOperations;
        private long totalTimeMs;
        private double avgLatencyMs;
        private double throughput;
        private long p50LatencyMs;
        private long p95LatencyMs;
        private long p99LatencyMs;
        private long minLatencyMs;
        private long maxLatencyMs;
        private long memoryUsedMB;
    }

    /**
     * 运行完整的基准测试套件
     */
    public Map<String, List<BenchmarkResult>> runFullBenchmarkSuite() {
        log.info("========== 开始运行完整基准测试套件 ==========");

        Map<String, List<BenchmarkResult>> allResults = new LinkedHashMap<>();
        BenchmarkConfig config = new BenchmarkConfig();

        // 测试各种算法的保存性能
        allResults.put("Save Performance", Arrays.asList(
                benchmarkSaveOperation("ppl", config),
                benchmarkSaveOperation("hyde", config),
                benchmarkSaveOperation("rerank", config),
                benchmarkSaveOperation("query_expansion", config)
        ));

        // 测试查询性能
        allResults.put("Query Performance", Arrays.asList(
                benchmarkQueryOperation("ppl", config),
                benchmarkQueryOperation("hyde", config),
                benchmarkQueryOperation("rerank", config)
        ));

        // 测试并发性能
        allResults.put("Concurrent Performance", Arrays.asList(
                benchmarkConcurrentOperations("ppl", config)
        ));

        // 测试数据大小影响
        allResults.put("Data Size Impact",
                benchmarkDataSizeImpact("ppl", config)
        );

        log.info("========== 基准测试套件完成 ==========");
        printBenchmarkSummary(allResults);

        return allResults;
    }

    /**
     * 基准测试：保存操作性能
     */
    public BenchmarkResult benchmarkSaveOperation(String algorithmType, BenchmarkConfig config) {
        log.info("测试保存操作性能: {}", algorithmType);

        List<Long> latencies = new ArrayList<>();
        long startMem = getUsedMemoryMB();

        // 预热
        for (int i = 0; i < config.getWarmupIterations(); i++) {
            performSaveOperation(algorithmType, i);
        }

        if (config.isEnableGC()) {
            System.gc();
            sleep(100);
        }

        // 正式测试
        long startTime = System.currentTimeMillis();
        for (int i = 0; i < config.getTestIterations(); i++) {
            long opStart = System.nanoTime();
            performSaveOperation(algorithmType, i);
            long opEnd = System.nanoTime();
            latencies.add((opEnd - opStart) / 1_000_000); // 转换为毫秒
        }
        long endTime = System.currentTimeMillis();
        long endMem = getUsedMemoryMB();

        return calculateBenchmarkResult(
                algorithmType,
                "save",
                config.getTestIterations(),
                endTime - startTime,
                latencies,
                endMem - startMem
        );
    }

    /**
     * 基准测试：查询操作性能
     */
    public BenchmarkResult benchmarkQueryOperation(String algorithmType, BenchmarkConfig config) {
        log.info("测试查询操作性能: {}", algorithmType);

        // 先插入测试数据
        String testDocId = "benchmark-doc-" + algorithmType;
        performSaveOperation(algorithmType, 0, testDocId);

        List<Long> latencies = new ArrayList<>();
        long startMem = getUsedMemoryMB();

        // 预热
        for (int i = 0; i < config.getWarmupIterations(); i++) {
            optimizationService.getOptimizationData(testDocId, algorithmType);
        }

        if (config.isEnableGC()) {
            System.gc();
            sleep(100);
        }

        // 正式测试
        long startTime = System.currentTimeMillis();
        for (int i = 0; i < config.getTestIterations(); i++) {
            long opStart = System.nanoTime();
            optimizationService.getOptimizationData(testDocId, algorithmType);
            long opEnd = System.nanoTime();
            latencies.add((opEnd - opStart) / 1_000_000);
        }
        long endTime = System.currentTimeMillis();
        long endMem = getUsedMemoryMB();

        return calculateBenchmarkResult(
                algorithmType,
                "query",
                config.getTestIterations(),
                endTime - startTime,
                latencies,
                endMem - startMem
        );
    }

    /**
     * 基准测试：并发操作性能
     */
    public BenchmarkResult benchmarkConcurrentOperations(String algorithmType, BenchmarkConfig config) {
        log.info("测试并发操作性能: {} ({}线程)", algorithmType, config.getConcurrentThreads());

        ExecutorService executor = Executors.newFixedThreadPool(config.getConcurrentThreads());
        List<Long> latencies = Collections.synchronizedList(new ArrayList<>());
        CountDownLatch latch = new CountDownLatch(config.getTestIterations());

        long startMem = getUsedMemoryMB();
        long startTime = System.currentTimeMillis();

        for (int i = 0; i < config.getTestIterations(); i++) {
            final int index = i;
            executor.submit(() -> {
                try {
                    long opStart = System.nanoTime();
                    performSaveOperation(algorithmType, index);
                    long opEnd = System.nanoTime();
                    latencies.add((opEnd - opStart) / 1_000_000);
                } finally {
                    latch.countDown();
                }
            });
        }

        try {
            latch.await(60, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            log.error("并发测试被中断", e);
        }

        long endTime = System.currentTimeMillis();
        long endMem = getUsedMemoryMB();

        executor.shutdown();

        return calculateBenchmarkResult(
                algorithmType,
                "concurrent_save",
                config.getTestIterations(),
                endTime - startTime,
                latencies,
                endMem - startMem
        );
    }

    /**
     * 基准测试：数据大小对性能的影响
     */
    public List<BenchmarkResult> benchmarkDataSizeImpact(String algorithmType, BenchmarkConfig config) {
        log.info("测试数据大小影响: {}", algorithmType);

        List<BenchmarkResult> results = new ArrayList<>();
        int[] dataSizes = {100, 500, 1000, 5000, 10000}; // 字节

        for (int dataSize : dataSizes) {
            List<Long> latencies = new ArrayList<>();
            long startTime = System.currentTimeMillis();

            for (int i = 0; i < config.getTestIterations() / 10; i++) {
                long opStart = System.nanoTime();
                performSaveOperationWithSize(algorithmType, i, dataSize);
                long opEnd = System.nanoTime();
                latencies.add((opEnd - opStart) / 1_000_000);
            }

            long endTime = System.currentTimeMillis();

            BenchmarkResult result = calculateBenchmarkResult(
                    algorithmType + "_" + dataSize + "B",
                    "save_with_size",
                    config.getTestIterations() / 10,
                    endTime - startTime,
                    latencies,
                    0
            );
            results.add(result);
        }

        return results;
    }

    // ========== 辅助方法 ==========

    /**
     * 执行保存操作
     */
    private void performSaveOperation(String algorithmType, int index) {
        performSaveOperation(algorithmType, index, "benchmark-doc-" + index);
    }

    private void performSaveOperation(String algorithmType, int index, String docId) {
        Map<String, Object> data = generateTestData(algorithmType, 1000);
        optimizationService.saveOptimizationData(docId, algorithmType, data);
    }

    /**
     * 执行指定大小的保存操作
     */
    private void performSaveOperationWithSize(String algorithmType, int index, int dataSize) {
        Map<String, Object> data = generateTestData(algorithmType, dataSize);
        optimizationService.saveOptimizationData(
                "benchmark-size-doc-" + index,
                algorithmType,
                data
        );
    }

    /**
     * 生成测试数据
     */
    private Map<String, Object> generateTestData(String algorithmType, int approximateSize) {
        Map<String, Object> data = new HashMap<>();

        // 生成接近指定大小的测试数据
        StringBuilder sb = new StringBuilder();
        while (sb.length() < approximateSize) {
            sb.append("test_data_");
        }

        data.put("content", sb.toString());
        data.put("timestamp", System.currentTimeMillis());
        data.put("index", new Random().nextInt(10000));

        return data;
    }

    /**
     * 计算基准测试结果
     */
    private BenchmarkResult calculateBenchmarkResult(
            String algorithmType,
            String operation,
            long totalOperations,
            long totalTimeMs,
            List<Long> latencies,
            long memoryUsedMB) {

        BenchmarkResult result = new BenchmarkResult();
        result.setAlgorithmType(algorithmType);
        result.setOperation(operation);
        result.setTotalOperations(totalOperations);
        result.setTotalTimeMs(totalTimeMs);

        // 计算平均延迟
        double avgLatency = latencies.stream()
                .mapToLong(Long::longValue)
                .average()
                .orElse(0.0);
        result.setAvgLatencyMs(avgLatency);

        // 计算吞吐量（ops/sec）
        double throughput = (double) totalOperations / totalTimeMs * 1000;
        result.setThroughput(throughput);

        // 计算百分位延迟
        Collections.sort(latencies);
        int size = latencies.size();
        result.setP50LatencyMs(latencies.get((int) (size * 0.50)));
        result.setP95LatencyMs(latencies.get((int) (size * 0.95)));
        result.setP99LatencyMs(latencies.get((int) (size * 0.99)));
        result.setMinLatencyMs(latencies.get(0));
        result.setMaxLatencyMs(latencies.get(size - 1));

        result.setMemoryUsedMB(memoryUsedMB);

        return result;
    }

    /**
     * 获取已使用内存（MB）
     */
    private long getUsedMemoryMB() {
        Runtime runtime = Runtime.getRuntime();
        return (runtime.totalMemory() - runtime.freeMemory()) / (1024 * 1024);
    }

    /**
     * 休眠
     */
    private void sleep(long millis) {
        try {
            Thread.sleep(millis);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    /**
     * 打印基准测试摘要
     */
    private void printBenchmarkSummary(Map<String, List<BenchmarkResult>> allResults) {
        log.info("\n");
        log.info("==================== 基准测试结果摘要 ====================");

        for (Map.Entry<String, List<BenchmarkResult>> entry : allResults.entrySet()) {
            log.info("\n【{}】", entry.getKey());
            log.info("━".repeat(80));
            log.info(String.format("%-20s %-15s %10s %10s %10s %10s %10s",
                    "Algorithm", "Operation", "Ops", "Avg(ms)", "P95(ms)", "P99(ms)", "TPS"));
            log.info("━".repeat(80));

            for (BenchmarkResult result : entry.getValue()) {
                log.info(String.format("%-20s %-15s %10d %10.2f %10d %10d %10.0f",
                        result.getAlgorithmType(),
                        result.getOperation(),
                        result.getTotalOperations(),
                        result.getAvgLatencyMs(),
                        result.getP95LatencyMs(),
                        result.getP99LatencyMs(),
                        result.getThroughput()
                ));
            }
        }

        log.info("\n" + "=".repeat(80));
    }

    /**
     * 生成性能报告（Markdown格式）
     */
    public String generateMarkdownReport(Map<String, List<BenchmarkResult>> allResults) {
        StringBuilder report = new StringBuilder();
        report.append("# RAG优化算法性能基准测试报告\n\n");
        report.append("**测试时间**: ").append(new Date()).append("\n\n");

        for (Map.Entry<String, List<BenchmarkResult>> entry : allResults.entrySet()) {
            report.append("## ").append(entry.getKey()).append("\n\n");
            report.append("| 算法 | 操作 | 总操作数 | 平均延迟(ms) | P95延迟(ms) | P99延迟(ms) | 吞吐量(ops/s) |\n");
            report.append("|------|------|----------|--------------|-------------|-------------|---------------|\n");

            for (BenchmarkResult result : entry.getValue()) {
                report.append(String.format("| %s | %s | %d | %.2f | %d | %d | %.0f |\n",
                        result.getAlgorithmType(),
                        result.getOperation(),
                        result.getTotalOperations(),
                        result.getAvgLatencyMs(),
                        result.getP95LatencyMs(),
                        result.getP99LatencyMs(),
                        result.getThroughput()
                ));
            }
            report.append("\n");
        }

        return report.toString();
    }
}


