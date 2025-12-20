package top.yumbo.ai.omni.workflow.agents;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.workflow.Agent;
import top.yumbo.ai.omni.workflow.WorkflowContext;
import top.yumbo.ai.omni.workflow.WorkflowEngine;
import top.yumbo.ai.omni.workflow.WorkflowResult;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

/**
 * 工作流调用 Agent
 * (Workflow Invoker Agent)
 *
 * <p>用于在工作流中调用其他工作流，支持：</p>
 * <ul>
 *   <li>单个工作流调用</li>
 *   <li>批量顺序执行（forEach）</li>
 *   <li>批量并行执行（parallel）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@Component("WorkflowInvoker")
public class WorkflowInvokerAgent implements Agent {

    @Lazy
    @Autowired
    private WorkflowEngine workflowEngine;

    private final ExecutorService executorService = Executors.newFixedThreadPool(10);

    @Override
    public Object execute(Object input, WorkflowContext context) throws Exception {
        if (!(input instanceof Map)) {
            throw new IllegalArgumentException("WorkflowInvoker 输入必须是 Map 类型");
        }

        @SuppressWarnings("unchecked")
        Map<String, Object> config = (Map<String, Object>) input;

        // 获取调用模式
        String mode = (String) config.getOrDefault("mode", "single");

        log.info("🔗 WorkflowInvoker 执行: mode={}", mode);

        return switch (mode) {
            case "single" -> executeSingle(config, context);
            case "forEach" -> executeForEach(config, context);
            case "parallel" -> executeParallel(config, context);
            default -> throw new IllegalArgumentException("不支持的模式: " + mode);
        };
    }

    /**
     * 单个工作流调用
     *
     * @param config 配置
     * @param context 上下文
     * @return 执行结果
     */
    private Object executeSingle(Map<String, Object> config, WorkflowContext context) {
        String workflowName = (String) config.get("workflow");
        String version = (String) config.get("version");
        Object inputData = config.get("input");

        if (workflowName == null) {
            throw new IllegalArgumentException("workflow 参数不能为空");
        }

        log.info("  📌 调用工作流: {}", workflowName);

        WorkflowResult result;
        if (version != null) {
            result = workflowEngine.execute(workflowName, version, inputData);
        } else {
            result = workflowEngine.execute(workflowName, inputData);
        }

        if (!result.isSuccess()) {
            log.error("  ❌ 工作流执行失败: {}", result.getError());
            throw new RuntimeException("工作流执行失败: " + result.getError());
        }

        log.info("  ✅ 工作流执行成功: 耗时={}ms", result.getDuration());

        return Map.of(
                "executionId", result.getExecutionId(),
                "result", result.getFinalResult(),
                "duration", result.getDuration()
        );
    }

    /**
     * 批量顺序执行（forEach）
     *
     * @param config 配置
     * @param context 上下文
     * @return 执行结果列表
     */
    private Object executeForEach(Map<String, Object> config, WorkflowContext context) {
        String workflowName = (String) config.get("workflow");
        String version = (String) config.get("version");

        @SuppressWarnings("unchecked")
        List<Object> items = (List<Object>) config.get("items");

        if (workflowName == null || items == null) {
            throw new IllegalArgumentException("workflow 和 items 参数不能为空");
        }

        log.info("  🔄 批量顺序执行: workflow={}, items={}", workflowName, items.size());

        List<Map<String, Object>> results = new ArrayList<>();
        int successCount = 0;
        int failureCount = 0;

        for (int i = 0; i < items.size(); i++) {
            Object item = items.get(i);
            log.info("    [{}/{}] 执行中...", i + 1, items.size());

            try {
                WorkflowResult result;
                if (version != null) {
                    result = workflowEngine.execute(workflowName, version, item);
                } else {
                    result = workflowEngine.execute(workflowName, item);
                }

                if (result.isSuccess()) {
                    successCount++;
                    results.add(Map.of(
                            "index", i,
                            "success", true,
                            "result", result.getFinalResult(),
                            "executionId", result.getExecutionId()
                    ));
                } else {
                    failureCount++;
                    results.add(Map.of(
                            "index", i,
                            "success", false,
                            "error", result.getError()
                    ));
                }

            } catch (Exception e) {
                failureCount++;
                log.error("    ❌ 执行失败: {}", e.getMessage());
                results.add(Map.of(
                        "index", i,
                        "success", false,
                        "error", e.getMessage()
                ));
            }
        }

        log.info("  ✅ 批量执行完成: 成功={}, 失败={}", successCount, failureCount);

        return Map.of(
                "total", items.size(),
                "success", successCount,
                "failure", failureCount,
                "results", results
        );
    }

    /**
     * 批量并行执行（parallel）
     *
     * @param config 配置
     * @param context 上下文
     * @return 执行结果列表
     */
    private Object executeParallel(Map<String, Object> config, WorkflowContext context) {
        String workflowName = (String) config.get("workflow");
        String version = (String) config.get("version");

        @SuppressWarnings("unchecked")
        List<Object> items = (List<Object>) config.get("items");

        Integer maxParallel = (Integer) config.getOrDefault("maxParallel", 10);

        if (workflowName == null || items == null) {
            throw new IllegalArgumentException("workflow 和 items 参数不能为空");
        }

        log.info("  ⚡ 批量并行执行: workflow={}, items={}, maxParallel={}",
                workflowName, items.size(), maxParallel);

        // 创建并行任务
        List<CompletableFuture<Map<String, Object>>> futures = new ArrayList<>();

        for (int i = 0; i < items.size(); i++) {
            final int index = i;
            final Object item = items.get(i);

            CompletableFuture<Map<String, Object>> future = CompletableFuture.supplyAsync(() -> {
                try {
                    log.debug("    [{}] 开始执行...", index);

                    WorkflowResult result;
                    if (version != null) {
                        result = workflowEngine.execute(workflowName, version, item);
                    } else {
                        result = workflowEngine.execute(workflowName, item);
                    }

                    if (result.isSuccess()) {
                        log.debug("    [{}] ✅ 执行成功", index);
                        return Map.of(
                                "index", index,
                                "success", true,
                                "result", result.getFinalResult(),
                                "executionId", result.getExecutionId()
                        );
                    } else {
                        log.debug("    [{}] ❌ 执行失败: {}", index, result.getError());
                        return Map.of(
                                "index", index,
                                "success", false,
                                "error", result.getError()
                        );
                    }

                } catch (Exception e) {
                    log.error("    [{}] ❌ 执行异常: {}", index, e.getMessage());
                    return Map.of(
                            "index", index,
                            "success", false,
                            "error", e.getMessage()
                    );
                }
            }, executorService);

            futures.add(future);
        }

        // 等待所有任务完成
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();

        // 收集结果
        List<Map<String, Object>> results = futures.stream()
                .map(CompletableFuture::join)
                .collect(Collectors.toList());

        long successCount = results.stream()
                .filter(r -> (Boolean) r.get("success"))
                .count();

        long failureCount = results.size() - successCount;

        log.info("  ✅ 并行执行完成: 成功={}, 失败={}", successCount, failureCount);

        return Map.of(
                "total", items.size(),
                "success", successCount,
                "failure", failureCount,
                "results", results
        );
    }

    @Override
    public String getName() {
        return "WorkflowInvoker";
    }

    @Override
    public String getDescription() {
        return "工作流调用 Agent - 支持单个、批量顺序、批量并行执行";
    }

    @Override
    public String getInputType() {
        return "Map<String, Object>";
    }

    @Override
    public String getOutputType() {
        return "Map<String, Object>";
    }

    @Override
    public Map<String, Object> getConfigSchema() {
        return Map.of(
                "mode", Map.of(
                        "type", "string",
                        "enum", List.of("single", "forEach", "parallel"),
                        "default", "single",
                        "description", "执行模式"
                ),
                "workflow", Map.of(
                        "type", "string",
                        "required", true,
                        "description", "要调用的工作流名称"
                ),
                "version", Map.of(
                        "type", "string",
                        "description", "工作流版本（可选）"
                ),
                "input", Map.of(
                        "type", "any",
                        "description", "单个模式的输入数据"
                ),
                "items", Map.of(
                        "type", "array",
                        "description", "批量模式的输入数据列表"
                ),
                "maxParallel", Map.of(
                        "type", "integer",
                        "default", 10,
                        "description", "并行模式的最大并行数"
                )
        );
    }

    @Override
    public boolean validateInput(Object input) {
        if (!(input instanceof Map)) {
            return false;
        }

        @SuppressWarnings("unchecked")
        Map<String, Object> config = (Map<String, Object>) input;

        // 必须有 workflow 参数
        if (!config.containsKey("workflow")) {
            return false;
        }

        String mode = (String) config.getOrDefault("mode", "single");

        // 批量模式必须有 items
        if (("forEach".equals(mode) || "parallel".equals(mode)) && !config.containsKey("items")) {
            return false;
        }

        return true;
    }
}

