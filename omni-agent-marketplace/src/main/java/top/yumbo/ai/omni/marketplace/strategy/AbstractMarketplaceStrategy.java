package top.yumbo.ai.omni.marketplace.strategy;

import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

import static top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.*;

/**
 * 抽象策略基类
 *
 * 提供通用功能的默认实现，简化策略开发
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public abstract class AbstractMarketplaceStrategy implements MarketplaceStrategy {

    // 性能指标统计
    private final AtomicLong totalExecutions = new AtomicLong(0);
    private final AtomicLong successCount = new AtomicLong(0);
    private final AtomicLong failureCount = new AtomicLong(0);
    private final List<Long> latencies = Collections.synchronizedList(new ArrayList<>());

    // 资源使用统计
    private long peakMemoryBytes = 0;
    private long totalCpuTimeMs = 0;

    // 初始化状态
    private volatile boolean initialized = false;
    private volatile long lastHealthCheckTime = 0;

    // ========== 必须实现的抽象方法 ==========

    /**
     * 子类必须实现的执行逻辑
     */
    protected abstract <I, O> O doExecute(I input, Map<String, Object> params, ExecutionContext context)
            throws StrategyExecutionException;

    // ========== 基本信息（提供默认实现，可覆盖） ==========

    @Override
    public String getStrategyId() {
        // 默认实现：使用类名生成ID
        return this.getClass().getPackage().getName() + "." + this.getClass().getSimpleName();
    }

    @Override
    public String getVersion() {
        return "1.0.0";  // 默认版本
    }

    @Override
    public AuthorInfo getAuthor() {
        return AuthorInfo.builder()
                .name("Unknown")
                .build();
    }

    @Override
    public String getRequiredFrameworkVersion() {
        return ">=3.0.0 <4.0.0";  // 默认兼容3.x版本
    }

    @Override
    public List<StrategyDependency> getDependencies() {
        return Collections.emptyList();  // 默认无依赖
    }

    @Override
    public String getParameterSchema() {
        // 默认返回空Schema
        return "{}";
    }

    @Override
    public Map<String, Object> getDefaultParameters() {
        return Collections.emptyMap();
    }

    // ========== 兼容性检查（提供默认实现） ==========

    @Override
    public CompatibilityCheck checkCompatibility() {
        return CompatibilityCheck.builder()
                .compatible(true)
                .frameworkVersionMatch(true)
                .dependenciesMet(true)
                .incompatibilityReasons(Collections.emptyList())
                .warnings(Collections.emptyList())
                .missingDependencies(Collections.emptyList())
                .build();
    }

    // ========== 参数验证（提供默认实现） ==========

    @Override
    public ValidationResult validateParameters(Map<String, Object> params) {
        // 默认实现：简单验证
        Map<String, Object> validatedParams = new HashMap<>(getDefaultParameters());
        if (params != null) {
            validatedParams.putAll(params);
        }

        return ValidationResult.builder()
                .valid(true)
                .errors(Collections.emptyList())
                .warnings(Collections.emptyList())
                .validatedParams(validatedParams)
                .build();
    }

    // ========== 执行接口（包装子类实现，添加监控） ==========

    @Override
    public <I, O> ExecutionResult<O> execute(I input, Map<String, Object> params, ExecutionContext context)
            throws StrategyExecutionException {

        long startTime = System.currentTimeMillis();
        totalExecutions.incrementAndGet();

        try {
            // 检查初始化状态
            if (!initialized) {
                throw new StrategyExecutionException(
                        getStrategyId(),
                        StrategyExecutionException.ExecutionErrorCode.INTERNAL_ERROR,
                        "Strategy not initialized"
                );
            }

            // 验证参数
            ValidationResult validation = validateParameters(params);
            if (!validation.isValid()) {
                throw new StrategyExecutionException(
                        getStrategyId(),
                        StrategyExecutionException.ExecutionErrorCode.INVALID_PARAMETERS,
                        "Parameter validation failed: " + validation.getErrors()
                );
            }

            // 执行策略
            O result = doExecute(input, validation.getValidatedParams(), context);

            // 记录成功
            long executionTime = System.currentTimeMillis() - startTime;
            successCount.incrementAndGet();
            recordLatency(executionTime);

            return ExecutionResult.<O>builder()
                    .success(true)
                    .data(result)
                    .executionTimeMs(executionTime)
                    .metadata(Collections.emptyMap())
                    .warnings(Collections.emptyList())
                    .build();

        } catch (StrategyExecutionException e) {
            // 记录失败
            long executionTime = System.currentTimeMillis() - startTime;
            failureCount.incrementAndGet();
            recordLatency(executionTime);

            return ExecutionResult.<O>builder()
                    .success(false)
                    .error(e.getMessage())
                    .executionTimeMs(executionTime)
                    .metadata(Map.of("errorCode", e.getErrorCode()))
                    .build();
        }
    }

    // ========== 生命周期（提供默认实现） ==========

    @Override
    public void initialize(Map<String, Object> config) throws StrategyInitializationException {
        log.info("初始化策略: {}", getStrategyId());
        initialized = true;
    }

    @Override
    public void destroy() {
        log.info("销毁策略: {}", getStrategyId());
        initialized = false;
    }

    @Override
    public HealthStatus checkHealth() {
        lastHealthCheckTime = System.currentTimeMillis();

        if (!initialized) {
            return HealthStatus.builder()
                    .healthy(false)
                    .status(HealthStatus.HealthStatusCode.UNHEALTHY)
                    .details("Strategy not initialized")
                    .lastCheckTime(lastHealthCheckTime)
                    .issues(List.of("Not initialized"))
                    .build();
        }

        return HealthStatus.builder()
                .healthy(true)
                .status(HealthStatus.HealthStatusCode.HEALTHY)
                .details("Strategy is healthy")
                .lastCheckTime(lastHealthCheckTime)
                .issues(Collections.emptyList())
                .build();
    }

    // ========== 性能监控（自动实现） ==========

    @Override
    public PerformanceMetrics getMetrics() {
        long total = totalExecutions.get();
        long success = successCount.get();

        return PerformanceMetrics.builder()
                .totalExecutions(total)
                .successCount(success)
                .failureCount(failureCount.get())
                .averageLatencyMs(calculateAverageLatency())
                .p50LatencyMs(calculatePercentileLatency(0.5))
                .p95LatencyMs(calculatePercentileLatency(0.95))
                .p99LatencyMs(calculatePercentileLatency(0.99))
                .minLatencyMs(latencies.isEmpty() ? 0 : Collections.min(latencies))
                .maxLatencyMs(latencies.isEmpty() ? 0 : Collections.max(latencies))
                .throughput(total > 0 ? (double) success / total : 0)
                .successRate(total > 0 ? (double) success / total : 0)
                .lastUpdateTime(System.currentTimeMillis())
                .build();
    }

    @Override
    public ResourceUsage getResourceUsage() {
        Runtime runtime = Runtime.getRuntime();
        long currentMemory = runtime.totalMemory() - runtime.freeMemory();

        return ResourceUsage.builder()
                .currentMemoryBytes(currentMemory)
                .peakMemoryBytes(Math.max(peakMemoryBytes, currentMemory))
                .cpuTimeMs(totalCpuTimeMs)
                .threadCount(Thread.activeCount())
                .openFileHandles(0)  // 需要JMX支持
                .networkConnections(0)  // 需要JMX支持
                .lastUpdateTime(System.currentTimeMillis())
                .build();
    }

    @Override
    public ExecutionLimits getLimits() {
        return ExecutionLimits.builder()
                .timeoutMs(30000)  // 默认30秒超时
                .maxMemoryBytes(512 * 1024 * 1024)  // 默认512MB
                .maxCpuTimeMs(10000)  // 默认10秒CPU时间
                .maxConcurrentExecutions(10)
                .maxInputSizeBytes(10 * 1024 * 1024)  // 10MB
                .maxOutputSizeBytes(10 * 1024 * 1024)  // 10MB
                .allowNetworkAccess(false)
                .allowFileAccess(false)
                .build();
    }

    // ========== 安全（提供默认实现） ==========

    @Override
    public List<Permission> getRequiredPermissions() {
        return Collections.emptyList();  // 默认不需要权限
    }

    @Override
    public SecurityLevel getSecurityLevel() {
        return SecurityLevel.SAFE;  // 默认安全级别
    }

    // ========== 测试和验证（提供默认实现） ==========

    @Override
    public TestResult runSelfTest() {
        return TestResult.builder()
                .passed(true)
                .testCases(Collections.emptyList())
                .totalTests(0)
                .passedTests(0)
                .failedTests(0)
                .testDurationMs(0)
                .build();
    }

    @Override
    public List<UsageExample> getExamples() {
        return Collections.emptyList();
    }

    // ========== 元数据（提供默认实现） ==========

    @Override
    public StrategyMetadata getMetadata() {
        return StrategyMetadata.builder()
                .strategyId(getStrategyId())
                .name(getStrategyName())
                .category(getCategory())
                .version(getVersion())
                .description(getDescription())
                .author(getAuthor())
                .createdAt(System.currentTimeMillis())
                .updatedAt(System.currentTimeMillis())
                .license("MIT")
                .tags(getTags())
                .price(0.0)
                .currency("USD")
                .build();
    }

    @Override
    public List<String> getTags() {
        return List.of(getCategory().getCode());
    }

    @Override
    public StrategyRating getRating() {
        return StrategyRating.builder()
                .averageRating(0.0)
                .totalRatings(0)
                .fiveStarCount(0)
                .fourStarCount(0)
                .threeStarCount(0)
                .twoStarCount(0)
                .oneStarCount(0)
                .downloadCount(0)
                .usageCount(totalExecutions.get())
                .build();
    }

    // ========== 辅助方法 ==========

    private void recordLatency(long latency) {
        synchronized (latencies) {
            latencies.add(latency);
            // 保持最近1000条记录
            if (latencies.size() > 1000) {
                latencies.remove(0);
            }
        }
    }

    private double calculateAverageLatency() {
        synchronized (latencies) {
            if (latencies.isEmpty()) {
                return 0.0;
            }
            return latencies.stream().mapToLong(Long::longValue).average().orElse(0.0);
        }
    }

    private double calculatePercentileLatency(double percentile) {
        synchronized (latencies) {
            if (latencies.isEmpty()) {
                return 0.0;
            }
            List<Long> sorted = new ArrayList<>(latencies);
            Collections.sort(sorted);
            int index = (int) (sorted.size() * percentile);
            return sorted.get(Math.min(index, sorted.size() - 1));
        }
    }
}

