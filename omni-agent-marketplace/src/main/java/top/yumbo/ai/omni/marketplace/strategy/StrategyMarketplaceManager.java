package top.yumbo.ai.omni.marketplace.strategy;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import static top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.*;

/**
 * 策略市场管理器
 *
 * 负责：
 * 1. 策略注册和发现
 * 2. 策略生命周期管理
 * 3. 策略版本控制
 * 4. 策略安全检查
 * 5. 策略性能监控
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class StrategyMarketplaceManager {

    // 已注册的策略
    private final Map<String, MarketplaceStrategy> strategies = new ConcurrentHashMap<>();

    // 策略元数据索引
    private final Map<StrategyCategory, List<String>> categoryIndex = new ConcurrentHashMap<>();

    // 策略别名映射
    private final Map<String, String> aliases = new ConcurrentHashMap<>();

    public StrategyMarketplaceManager() {
        log.info("初始化策略市场管理器");
    }

    /**
     * 注册策略
     *
     * @param strategy 策略实例
     * @return 注册是否成功
     */
    public boolean registerStrategy(MarketplaceStrategy strategy) {
        try {
            String strategyId = strategy.getStrategyId();

            // 1. 检查兼容性
            CompatibilityCheck compatibility = strategy.checkCompatibility();
            if (!compatibility.isCompatible()) {
                log.error("策略不兼容: {}, 原因: {}", strategyId, compatibility.getIncompatibilityReasons());
                return false;
            }

            // 2. 检查依赖
            for (StrategyDependency dep : strategy.getDependencies()) {
                if (!dep.isOptional() && !strategies.containsKey(dep.getDependencyId())) {
                    log.error("缺少必需依赖: {}", dep.getDependencyId());
                    return false;
                }
            }

            // 3. 初始化策略
            strategy.initialize(Collections.emptyMap());

            // 4. 运行自测试
            TestResult testResult = strategy.runSelfTest();
            if (!testResult.isPassed()) {
                log.warn("策略自测试失败: {}, 但仍然注册", strategyId);
            }

            // 5. 注册策略
            strategies.put(strategyId, strategy);

            // 6. 更新索引
            StrategyCategory category = strategy.getCategory();
            categoryIndex.computeIfAbsent(category, k -> new ArrayList<>()).add(strategyId);

            // 7. 注册别名
            String alias = strategy.getStrategyName().toLowerCase().replace(" ", "_");
            aliases.put(alias, strategyId);

            log.info("✅ 策略注册成功: {} ({})", strategy.getStrategyName(), strategyId);
            return true;

        } catch (Exception e) {
            log.error("策略注册失败: {}", strategy.getStrategyId(), e);
            return false;
        }
    }

    /**
     * 卸载策略
     */
    public boolean unregisterStrategy(String strategyId) {
        MarketplaceStrategy strategy = strategies.remove(strategyId);
        if (strategy != null) {
            try {
                strategy.destroy();

                // 清理索引
                StrategyCategory category = strategy.getCategory();
                List<String> categoryStrategies = categoryIndex.get(category);
                if (categoryStrategies != null) {
                    categoryStrategies.remove(strategyId);
                }

                // 清理别名
                aliases.entrySet().removeIf(entry -> entry.getValue().equals(strategyId));

                log.info("✅ 策略已卸载: {}", strategyId);
                return true;
            } catch (Exception e) {
                log.error("卸载策略失败: {}", strategyId, e);
                return false;
            }
        }
        return false;
    }

    /**
     * 获取策略
     */
    public Optional<MarketplaceStrategy> getStrategy(String strategyIdOrAlias) {
        // 先尝试直接查找
        MarketplaceStrategy strategy = strategies.get(strategyIdOrAlias);

        // 如果找不到，尝试别名
        if (strategy == null) {
            String strategyId = aliases.get(strategyIdOrAlias.toLowerCase());
            if (strategyId != null) {
                strategy = strategies.get(strategyId);
            }
        }

        return Optional.ofNullable(strategy);
    }

    /**
     * 列出所有策略
     */
    public List<StrategyMetadata> listAllStrategies() {
        return strategies.values().stream()
                .map(MarketplaceStrategy::getMetadata)
                .toList();
    }

    /**
     * 按类别列出策略
     */
    public List<StrategyMetadata> listStrategiesByCategory(StrategyCategory category) {
        List<String> strategyIds = categoryIndex.getOrDefault(category, Collections.emptyList());

        return strategyIds.stream()
                .map(strategies::get)
                .filter(Objects::nonNull)
                .map(MarketplaceStrategy::getMetadata)
                .toList();
    }

    /**
     * 搜索策略
     */
    public List<StrategyMetadata> searchStrategies(String keyword) {
        String lowerKeyword = keyword.toLowerCase();

        return strategies.values().stream()
                .filter(strategy -> {
                    String name = strategy.getStrategyName().toLowerCase();
                    String desc = strategy.getDescription().toLowerCase();
                    List<String> tags = strategy.getTags();

                    return name.contains(lowerKeyword) ||
                           desc.contains(lowerKeyword) ||
                           tags.stream().anyMatch(tag -> tag.toLowerCase().contains(lowerKeyword));
                })
                .map(MarketplaceStrategy::getMetadata)
                .toList();
    }

    /**
     * 执行策略
     */
    public <I, O> ExecutionResult<O> executeStrategy(String strategyIdOrAlias, I input,
                                                     Map<String, Object> params) {
        Optional<MarketplaceStrategy> strategyOpt = getStrategy(strategyIdOrAlias);

        if (strategyOpt.isEmpty()) {
            return ExecutionResult.<O>builder()
                    .success(false)
                    .error("Strategy not found: " + strategyIdOrAlias)
                    .executionTimeMs(0)
                    .build();
        }

        MarketplaceStrategy strategy = strategyOpt.get();

        // 创建执行上下文
        ExecutionContext context = ExecutionContext.builder()
                .requestId(UUID.randomUUID().toString())
                .timeoutMs(30000)
                .maxMemoryBytes(512 * 1024 * 1024)
                .environment(Collections.emptyMap())
                .traceContext(Collections.emptyMap())
                .build();

        try {
            return strategy.execute(input, params, context);
        } catch (StrategyExecutionException e) {
            log.error("策略执行失败: {}", strategy.getStrategyId(), e);
            return ExecutionResult.<O>builder()
                    .success(false)
                    .error(e.getMessage())
                    .metadata(Map.of("errorCode", e.getErrorCode()))
                    .build();
        }
    }

    /**
     * 获取策略性能报告
     */
    public Map<String, PerformanceMetrics> getPerformanceReport() {
        Map<String, PerformanceMetrics> report = new HashMap<>();

        strategies.forEach((id, strategy) -> {
            report.put(id, strategy.getMetrics());
        });

        return report;
    }

    /**
     * 健康检查所有策略
     */
    public Map<String, HealthStatus> checkAllHealth() {
        Map<String, HealthStatus> healthReport = new HashMap<>();

        strategies.forEach((id, strategy) -> {
            healthReport.put(id, strategy.checkHealth());
        });

        return healthReport;
    }

    /**
     * 获取统计信息
     */
    public Map<String, Object> getStatistics() {
        long totalStrategies = strategies.size();

        Map<StrategyCategory, Long> categoryCount = new HashMap<>();
        for (StrategyCategory category : StrategyCategory.values()) {
            long count = categoryIndex.getOrDefault(category, Collections.emptyList()).size();
            categoryCount.put(category, count);
        }

        long totalExecutions = strategies.values().stream()
                .mapToLong(s -> s.getMetrics().getTotalExecutions())
                .sum();

        return Map.of(
            "totalStrategies", totalStrategies,
            "categoryDistribution", categoryCount,
            "totalExecutions", totalExecutions,
            "averageSuccessRate", calculateAverageSuccessRate()
        );
    }

    private double calculateAverageSuccessRate() {
        if (strategies.isEmpty()) {
            return 0.0;
        }

        return strategies.values().stream()
                .mapToDouble(s -> s.getMetrics().getSuccessRate())
                .average()
                .orElse(0.0);
    }
}

