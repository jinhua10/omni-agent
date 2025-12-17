package top.yumbo.ai.omni.marketplace;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.marketplace.security.SecureRemoteExecutor;
import top.yumbo.ai.omni.marketplace.security.SecureScriptExecutor;
import top.yumbo.ai.storage.api.model.OptimizationData;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 算法市场服务
 *
 * 支持三种算法类型：
 * 1. Pipeline（配置化）- 最安全，推荐
 * 2. Script（脚本）- 需要沙箱隔离
 * 3. Remote（远程）- 需要网络鉴权
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class AlgorithmMarketService {

    @Autowired(required = false)
    private SecureScriptExecutor scriptExecutor;

    @Autowired(required = false)
    private SecureRemoteExecutor remoteExecutor;

    // 注册的算法组件
    private final Map<String, AlgorithmComponent> components = new ConcurrentHashMap<>();

    // 已发布的算法
    private final Map<String, MarketAlgorithm> publishedAlgorithms = new ConcurrentHashMap<>();

    public AlgorithmMarketService() {
        // 注册内置组件
        registerBuiltinComponents();
    }

    /**
     * 注册内置算法组件
     */
    private void registerBuiltinComponents() {
        // 示例：注册查询扩展组件
        registerComponent("query_expansion", new AlgorithmComponent() {
            @Override
            public Object execute(Object input, Map<String, Object> params) {
                log.info("执行查询扩展: input={}, params={}", input, params);
                // TODO: 实际实现
                return input;
            }

            @Override
            public Map<String, Double> getMetrics() {
                return Map.of("precisionGain", 12.5, "latency", 20.0);
            }
        });

        // 注册其他组件
        registerComponent("semantic_chunking", createSemanticChunkingComponent());
        registerComponent("rerank", createRerankComponent());

        log.info("已注册 {} 个内置算法组件", components.size());
    }

    /**
     * 注册算法组件
     */
    public void registerComponent(String type, AlgorithmComponent component) {
        components.put(type, component);
        log.info("注册算法组件: {}", type);
    }

    /**
     * 发布算法到市场（带安全审核）
     */
    public String publishAlgorithm(MarketAlgorithm algorithm) throws AlgorithmValidationException {
        // 1. 验证算法
        validateAlgorithm(algorithm);

        // 2. 生成ID
        String algorithmId = UUID.randomUUID().toString();
        algorithm.setAlgorithmId(algorithmId);
        algorithm.setCreatedAt(System.currentTimeMillis());
        algorithm.setUpdatedAt(System.currentTimeMillis());
        algorithm.setUsageCount(0L);
        algorithm.setApproved(false);  // 默认需要审核

        // 3. 保存算法
        publishedAlgorithms.put(algorithmId, algorithm);

        log.info("算法已发布（待审核）: id={}, name={}, type={}",
                algorithmId, algorithm.getName(), algorithm.getType());

        return algorithmId;
    }

    /**
     * 审核并批准算法
     */
    public void approveAlgorithm(String algorithmId) {
        MarketAlgorithm algorithm = publishedAlgorithms.get(algorithmId);
        if (algorithm != null) {
            algorithm.setApproved(true);
            log.info("算法已审核通过: id={}, name={}", algorithmId, algorithm.getName());
        }
    }

    /**
     * 执行市场算法（带安全检查）
     */
    public OptimizationData executeMarketAlgorithm(
            String algorithmId,
            String documentId,
            Map<String, Object> context) throws AlgorithmExecutionException {

        // 1. 获取算法
        MarketAlgorithm algorithm = publishedAlgorithms.get(algorithmId);
        if (algorithm == null) {
            throw new AlgorithmExecutionException("算法不存在: " + algorithmId);
        }

        // 2. 检查审核状态
        if (Boolean.FALSE.equals(algorithm.getApproved())) {
            throw new AlgorithmExecutionException("算法未审核通过: " + algorithmId);
        }

        // 3. 增加使用次数
        algorithm.setUsageCount(algorithm.getUsageCount() + 1);

        log.info("执行市场算法: id={}, name={}, type={}",
                algorithmId, algorithm.getName(), algorithm.getType());

        // 4. 根据类型执行
        return switch (algorithm.getType()) {
            case PIPELINE -> executePipeline(algorithm, documentId, context);
            case SCRIPT -> executeScript(algorithm, documentId, context);
            case REMOTE -> executeRemote(algorithm, documentId, context);
        };
    }

    /**
     * 执行配置化算法（Pipeline）- 最安全
     */
    private OptimizationData executePipeline(
            MarketAlgorithm algorithm,
            String documentId,
            Map<String, Object> context) throws AlgorithmExecutionException {

        Object result = documentId;
        Map<String, Double> allMetrics = new HashMap<>();

        long startTime = System.currentTimeMillis();

        try {
            for (MarketAlgorithm.PipelineStep step : algorithm.getPipelineConfig().getSteps()) {
                // 获取组件
                AlgorithmComponent component = components.get(step.getType());
                if (component == null) {
                    log.warn("组件不存在: {}", step.getType());
                    continue;
                }

                // 执行组件
                result = component.execute(result, step.getParams());
                allMetrics.putAll(component.getMetrics());

                // 检查超时
                long elapsed = System.currentTimeMillis() - startTime;
                Integer maxTime = algorithm.getPipelineConfig().getMaxExecutionTimeMs();
                if (maxTime != null && elapsed > maxTime) {
                    throw new AlgorithmExecutionException("Pipeline execution timeout");
                }
            }

            return OptimizationData.builder()
                    .documentId(documentId)
                    .optimizationType(algorithm.getName())
                    .algorithmVersion(algorithm.getVersion())
                    .data(Map.of("result", result))
                    .metadata(Map.of(
                            "algorithmId", algorithm.getAlgorithmId(),
                            "type", "pipeline"
                    ))
                    .metrics(allMetrics)
                    .processedAt(System.currentTimeMillis())
                    .build();

        } catch (Exception e) {
            log.error("Pipeline execution failed", e);
            throw new AlgorithmExecutionException("Pipeline execution failed: " + e.getMessage());
        }
    }

    /**
     * 执行脚本算法（Script）- 需要沙箱隔离
     */
    private OptimizationData executeScript(
            MarketAlgorithm algorithm,
            String documentId,
            Map<String, Object> context) throws AlgorithmExecutionException {

        if (scriptExecutor == null) {
            throw new AlgorithmExecutionException("Script executor not available");
        }

        try {
            // 安全执行脚本（沙箱隔离 + 超时控制）
            Map<String, Object> scriptResult = scriptExecutor.executeSecurely(
                    algorithm.getScript(),
                    context,
                    5000  // 5秒超时
            );

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) scriptResult.get("data");
            @SuppressWarnings("unchecked")
            Map<String, Double> metrics = (Map<String, Double>) scriptResult.get("metrics");

            return OptimizationData.builder()
                    .documentId(documentId)
                    .optimizationType(algorithm.getName())
                    .algorithmVersion(algorithm.getVersion())
                    .data(data)
                    .metrics(metrics)
                    .metadata(Map.of(
                            "algorithmId", algorithm.getAlgorithmId(),
                            "type", "script",
                            "language", algorithm.getScriptLanguage()
                    ))
                    .processedAt(System.currentTimeMillis())
                    .build();

        } catch (SecureScriptExecutor.ScriptExecutionException e) {
            log.error("Script execution failed: algorithmId={}", algorithm.getAlgorithmId(), e);
            throw new AlgorithmExecutionException("Script execution failed: " + e.getMessage());
        }
    }

    /**
     * 执行远程算法（Remote）- 需要网络鉴权
     */
    private OptimizationData executeRemote(
            MarketAlgorithm algorithm,
            String documentId,
            Map<String, Object> context) throws AlgorithmExecutionException {

        if (remoteExecutor == null) {
            throw new AlgorithmExecutionException("Remote executor not available");
        }

        try {
            // 安全调用远程服务
            Map<String, Object> remoteResult = remoteExecutor.executeRemote(
                    algorithm.getRemoteEndpoint(),
                    algorithm.getRemoteAuthToken(),
                    documentId,
                    context
            );

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) remoteResult.get("data");
            @SuppressWarnings("unchecked")
            Map<String, Double> metrics = (Map<String, Double>) remoteResult.get("metrics");

            return OptimizationData.builder()
                    .documentId(documentId)
                    .optimizationType(algorithm.getName())
                    .algorithmVersion(algorithm.getVersion())
                    .data(data)
                    .metrics(metrics)
                    .metadata(Map.of(
                            "algorithmId", algorithm.getAlgorithmId(),
                            "type", "remote",
                            "endpoint", algorithm.getRemoteEndpoint()
                    ))
                    .processedAt(System.currentTimeMillis())
                    .build();

        } catch (SecureRemoteExecutor.RemoteExecutionException e) {
            log.error("Remote execution failed: algorithmId={}", algorithm.getAlgorithmId(), e);
            throw new AlgorithmExecutionException("Remote execution failed: " + e.getMessage());
        }
    }

    /**
     * 获取所有已发布的算法
     */
    public List<MarketAlgorithm> listPublishedAlgorithms(boolean includeUnapproved) {
        return publishedAlgorithms.values().stream()
                .filter(alg -> includeUnapproved || Boolean.TRUE.equals(alg.getApproved()))
                .toList();
    }

    /**
     * 搜索算法
     */
    public List<MarketAlgorithm> searchAlgorithms(String keyword) {
        return publishedAlgorithms.values().stream()
                .filter(alg -> Boolean.TRUE.equals(alg.getApproved()))
                .filter(alg -> alg.getName().contains(keyword) ||
                              (alg.getDescription() != null && alg.getDescription().contains(keyword)) ||
                              (alg.getTags() != null && alg.getTags().contains(keyword)))
                .toList();
    }

    /**
     * 验证算法（安全检查）
     */
    private void validateAlgorithm(MarketAlgorithm algorithm) throws AlgorithmValidationException {
        if (algorithm.getName() == null || algorithm.getName().trim().isEmpty()) {
            throw new AlgorithmValidationException("算法名称不能为空");
        }

        switch (algorithm.getType()) {
            case PIPELINE -> validatePipeline(algorithm);
            case SCRIPT -> validateScript(algorithm);
            case REMOTE -> validateRemote(algorithm);
        }
    }

    private void validatePipeline(MarketAlgorithm algorithm) throws AlgorithmValidationException {
        if (algorithm.getPipelineConfig() == null ||
            algorithm.getPipelineConfig().getSteps() == null ||
            algorithm.getPipelineConfig().getSteps().isEmpty()) {
            throw new AlgorithmValidationException("Pipeline 配置不能为空");
        }

        // 验证所有步骤的组件都存在
        for (MarketAlgorithm.PipelineStep step : algorithm.getPipelineConfig().getSteps()) {
            if (!components.containsKey(step.getType())) {
                throw new AlgorithmValidationException("组件不存在: " + step.getType());
            }
        }
    }

    private void validateScript(MarketAlgorithm algorithm) throws AlgorithmValidationException {
        if (algorithm.getScript() == null || algorithm.getScript().trim().isEmpty()) {
            throw new AlgorithmValidationException("脚本内容不能为空");
        }

        if (algorithm.getScript().length() > 50000) {
            throw new AlgorithmValidationException("脚本过大（最多50KB）");
        }
    }

    private void validateRemote(MarketAlgorithm algorithm) throws AlgorithmValidationException {
        if (algorithm.getRemoteEndpoint() == null || algorithm.getRemoteEndpoint().trim().isEmpty()) {
            throw new AlgorithmValidationException("远程端点不能为空");
        }

        try {
            new java.net.URL(algorithm.getRemoteEndpoint());
        } catch (java.net.MalformedURLException e) {
            throw new AlgorithmValidationException("无效的URL: " + algorithm.getRemoteEndpoint());
        }
    }

    // ========== 辅助方法 ==========

    private AlgorithmComponent createSemanticChunkingComponent() {
        return new AlgorithmComponent() {
            @Override
            public Object execute(Object input, Map<String, Object> params) {
                log.info("执行语义分块");
                // TODO: 实际实现
                return input;
            }

            @Override
            public Map<String, Double> getMetrics() {
                return Map.of("precisionGain", 17.5, "latency", 30.0);
            }
        };
    }

    private AlgorithmComponent createRerankComponent() {
        return new AlgorithmComponent() {
            @Override
            public Object execute(Object input, Map<String, Object> params) {
                log.info("执行重排序");
                // TODO: 实际实现
                return input;
            }

            @Override
            public Map<String, Double> getMetrics() {
                return Map.of("precisionGain", 10.0, "latency", 80.0);
            }
        };
    }

    // ========== 异常类 ==========

    public static class AlgorithmValidationException extends Exception {
        public AlgorithmValidationException(String message) {
            super(message);
        }
    }

    public static class AlgorithmExecutionException extends Exception {
        public AlgorithmExecutionException(String message) {
            super(message);
        }
    }
}

