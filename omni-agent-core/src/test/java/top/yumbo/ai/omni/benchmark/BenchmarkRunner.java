package top.yumbo.ai.omni.benchmark;

import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.runner.options.TimeValue;

/**
 * JMH基准测试套件运行器
 * (JMH Benchmark Suite Runner)
 * <p>
 * 运行所有性能基准测试
 * (Runs all performance benchmarks)
 * <p>
 * 使用方式 (Usage):
 * 1. 直接运行main方法 (Run main method directly)
 * 2. 或使用Maven: mvn exec:java -Dexec.mainClass="top.yumbo.ai.omni.benchmark.BenchmarkRunner"
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
public class BenchmarkRunner {

    /**
     * 运行完整基准测试套件
     * (Run full benchmark suite)
     */
    public static void main(String[] args) throws RunnerException {
        System.out.println("=".repeat(80));
        System.out.println("OmniAgent Performance Benchmark Suite");
        System.out.println("Version: 1.0.1");
        System.out.println("Date: 2025-12-15");
        System.out.println("=".repeat(80));
        System.out.println();

        Options opt = new OptionsBuilder()
                // 包含所有基准测试类
                .include(KnowledgeLoaderBenchmark.class.getSimpleName())
                .include(CoreServicesBenchmark.class.getSimpleName())
                
                // 基准测试配置
                .forks(1)                                    // 1个JVM进程
                .warmupIterations(3)                         // 3次预热
                .warmupTime(TimeValue.seconds(1))            // 每次预热1秒
                .measurementIterations(5)                    // 5次测量
                .measurementTime(TimeValue.seconds(1))       // 每次测量1秒
                
                // 输出配置
                .shouldFailOnError(true)                     // 失败时停止
                .shouldDoGC(true)                            // 每次迭代后GC
                
                .build();

        new Runner(opt).run();
        
        System.out.println();
        System.out.println("=".repeat(80));
        System.out.println("Benchmark suite completed!");
        System.out.println("=".repeat(80));
    }

    /**
     * 运行快速基准测试（减少迭代次数）
     * (Run quick benchmark - fewer iterations)
     */
    public static void runQuick() throws RunnerException {
        System.out.println("Running quick benchmark (reduced iterations)...");
        
        Options opt = new OptionsBuilder()
                .include(KnowledgeLoaderBenchmark.class.getSimpleName())
                .include(CoreServicesBenchmark.class.getSimpleName())
                .forks(1)
                .warmupIterations(1)
                .warmupTime(TimeValue.seconds(1))
                .measurementIterations(2)
                .measurementTime(TimeValue.seconds(1))
                .build();

        new Runner(opt).run();
    }

    /**
     * 运行单个基准测试类
     * (Run single benchmark class)
     */
    public static void runSingle(Class<?> benchmarkClass) throws RunnerException {
        System.out.println("Running benchmark: " + benchmarkClass.getSimpleName());
        
        Options opt = new OptionsBuilder()
                .include(benchmarkClass.getSimpleName())
                .forks(1)
                .warmupIterations(2)
                .measurementIterations(3)
                .build();

        new Runner(opt).run();
    }
}

