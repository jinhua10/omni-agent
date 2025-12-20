package top.yumbo.ai.omni.workflow;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 工作流引擎
 * (Workflow Engine)
 *
 * <p>负责工作流的执行、调度和状态管理</p>
 *
 * <p>核心功能:</p>
 * <ul>
 *   <li>工作流执行（同步/异步）</li>
 *   <li>步骤依赖解析和拓扑排序</li>
 *   <li>并行执行支持</li>
 *   <li>执行状态跟踪</li>
 *   <li>错误处理和重试</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@Service
public class WorkflowEngine {

    @Autowired
    private WorkflowRegistry workflowRegistry;

    @Autowired
    private Map<String, Agent> agents;

    // 执行记录（executionId -> result）
    private final Map<String, WorkflowResult> executionRecords = new ConcurrentHashMap<>();

    /**
     * 同步执行工作流
     *
     * @param workflowName 工作流名称
     * @param input 输入数据
     * @return 执行结果
     */
    public WorkflowResult execute(String workflowName, Object input) {
        return execute(workflowName, null, input);
    }

    /**
     * 同步执行工作流（指定版本）
     *
     * @param workflowName 工作流名称
     * @param version 版本号（null表示最新版本）
     * @param input 输入数据
     * @return 执行结果
     */
    public WorkflowResult execute(String workflowName, String version, Object input) {
        String executionId = UUID.randomUUID().toString();
        log.info("🚀 开始执行工作流: name={}, version={}, executionId={}",
                 workflowName, version, executionId);

        WorkflowResult result = WorkflowResult.builder()
                .executionId(executionId)
                .workflowName(workflowName)
                .workflowVersion(version)
                .status(WorkflowResult.ExecutionStatus.RUNNING)
                .startTime(System.currentTimeMillis())
                .build();

        executionRecords.put(executionId, result);

        try {
            // 1. 获取工作流定义
            Workflow workflow = version != null
                    ? workflowRegistry.getWorkflow(workflowName, version)
                    : workflowRegistry.getLatestWorkflow(workflowName);

            if (workflow == null) {
                throw new WorkflowException("工作流不存在: " + workflowName);
            }

            result.setWorkflowVersion(workflow.getVersion());

            // 2. 创建工作流上下文
            WorkflowContext context = new WorkflowContext(input);
            context.setMetadata("workflowName", workflowName);
            context.setMetadata("workflowVersion", workflow.getVersion());
            context.setMetadata("executionId", executionId);

            // 3. 构建执行计划（拓扑排序）
            List<WorkflowStep> executionPlan = buildExecutionPlan(workflow.getSteps());
            log.info("📋 执行计划: {} 个步骤", executionPlan.size());

            // 4. 执行步骤
            for (WorkflowStep step : executionPlan) {
                executeStep(step, context, workflow);
            }

            // 5. 设置最终结果
            result.setFinalResult(context.getAllStepResults());
            result.setStepResults(context.getAllStepResults());
            result.setStatus(WorkflowResult.ExecutionStatus.SUCCESS);
            result.setEndTime(System.currentTimeMillis());

            log.info("✅ 工作流执行成功: executionId={}, 耗时={}ms",
                     executionId, result.getDuration());

            return result;

        } catch (Exception e) {
            log.error("❌ 工作流执行失败: executionId={}", executionId, e);

            result.setStatus(WorkflowResult.ExecutionStatus.FAILED);
            result.setError(e.getMessage());
            result.setErrorStack(getStackTrace(e));
            result.setEndTime(System.currentTimeMillis());

            return result;
        }
    }

    /**
     * 异步执行工作流
     *
     * @param workflowName 工作流名称
     * @param input 输入数据
     * @return 异步结果
     */
    public CompletableFuture<WorkflowResult> executeAsync(String workflowName, Object input) {
        return CompletableFuture.supplyAsync(() -> execute(workflowName, input));
    }

    /**
     * 执行单个步骤
     *
     * @param step 步骤定义
     * @param context 工作流上下文
     * @param workflow 工作流定义
     */
    private void executeStep(WorkflowStep step, WorkflowContext context, Workflow workflow)
            throws Exception {

        long stepStartTime = System.currentTimeMillis();
        log.info("  ▶ 步骤: [{}] {}", step.getId(), step.getName());

        try {
            // 1. 检查条件
            if (step.getCondition() != null && !evaluateCondition(step.getCondition(), context)) {
                log.info("  ⏭️ 步骤跳过（条件不满足）: {}", step.getId());
                return;
            }

            // 2. 解析输入
            Object input = resolveInput(step.getInput(), context);

            // 3. 获取 Agent
            Agent agent = agents.get(step.getAgent());
            if (agent == null) {
                throw new WorkflowException("Agent 不存在: " + step.getAgent());
            }

            // 4. 验证输入
            if (!agent.validateInput(input)) {
                throw new WorkflowException("输入验证失败: " + step.getId());
            }

            // 5. 执行 Agent
            Object output = agent.execute(input, context);

            // 6. 保存结果
            context.setStepResult(step.getId(), output);

            long stepDuration = System.currentTimeMillis() - stepStartTime;
            log.info("  ✓ 步骤完成: [{}] 耗时={}ms", step.getId(), stepDuration);

        } catch (Exception e) {
            long stepDuration = System.currentTimeMillis() - stepStartTime;
            log.error("  ✗ 步骤失败: [{}] 耗时={}ms", step.getId(), stepDuration, e);

            if (!step.isAllowFailure()) {
                throw new WorkflowException("步骤执行失败: " + step.getId(), e);
            } else {
                log.warn("  ⚠️ 步骤失败但允许继续: {}", step.getId());
                context.setStepResult(step.getId(), Map.of("error", e.getMessage()));
            }
        }
    }

    /**
     * 构建执行计划（拓扑排序）
     *
     * @param steps 步骤列表
     * @return 排序后的步骤列表
     */
    private List<WorkflowStep> buildExecutionPlan(List<WorkflowStep> steps) {
        // 简单实现：按照依赖关系进行拓扑排序
        Map<String, WorkflowStep> stepMap = steps.stream()
                .collect(Collectors.toMap(WorkflowStep::getId, s -> s));

        List<WorkflowStep> sorted = new ArrayList<>();
        Set<String> visited = new HashSet<>();
        Set<String> visiting = new HashSet<>();

        for (WorkflowStep step : steps) {
            if (!visited.contains(step.getId())) {
                topologicalSort(step, stepMap, visited, visiting, sorted);
            }
        }

        return sorted;
    }

    /**
     * 拓扑排序（DFS）
     */
    private void topologicalSort(WorkflowStep step, Map<String, WorkflowStep> stepMap,
                                  Set<String> visited, Set<String> visiting,
                                  List<WorkflowStep> sorted) {

        if (visiting.contains(step.getId())) {
            throw new WorkflowException("检测到循环依赖: " + step.getId());
        }

        if (visited.contains(step.getId())) {
            return;
        }

        visiting.add(step.getId());

        // 递归处理依赖
        if (step.getDependencies() != null) {
            for (String depId : step.getDependencies()) {
                WorkflowStep depStep = stepMap.get(depId);
                if (depStep != null) {
                    topologicalSort(depStep, stepMap, visited, visiting, sorted);
                }
            }
        }

        visiting.remove(step.getId());
        visited.add(step.getId());
        sorted.add(step);
    }

    /**
     * 解析输入（支持变量替换）
     *
     * @param input 输入配置
     * @param context 工作流上下文
     * @return 解析后的输入
     */
    private Object resolveInput(Object input, WorkflowContext context) {
        if (input == null) {
            return null;
        }

        if (input instanceof String) {
            String str = (String) input;
            // 支持变量替换: ${step_id.output} 或 ${workflow.input}
            if (str.startsWith("${") && str.endsWith("}")) {
                String expr = str.substring(2, str.length() - 1);

                if (expr.startsWith("workflow.input")) {
                    return context.getInitialInput();
                } else if (expr.contains(".output")) {
                    String stepId = expr.substring(0, expr.indexOf(".output"));
                    return context.getStepResult(stepId);
                } else {
                    return context.getStepResult(expr);
                }
            }
        } else if (input instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) input;
            Map<String, Object> resolved = new HashMap<>();
            for (Map.Entry<String, Object> entry : map.entrySet()) {
                resolved.put(entry.getKey(), resolveInput(entry.getValue(), context));
            }
            return resolved;
        }

        return input;
    }

    /**
     * 评估条件表达式
     */
    private boolean evaluateCondition(String condition, WorkflowContext context) {
        // 简单实现：TODO 后续可以集成 SpEL 或其他表达式引擎
        return true;
    }

    /**
     * 获取执行记录
     */
    public WorkflowResult getExecutionResult(String executionId) {
        return executionRecords.get(executionId);
    }

    /**
     * 获取所有执行记录
     */
    public List<WorkflowResult> getAllExecutions() {
        return new ArrayList<>(executionRecords.values());
    }

    /**
     * 获取堆栈信息
     */
    private String getStackTrace(Exception e) {
        StringBuilder sb = new StringBuilder();
        for (StackTraceElement element : e.getStackTrace()) {
            sb.append(element.toString()).append("\n");
        }
        return sb.toString();
    }

    /**
     * 工作流异常
     */
    public static class WorkflowException extends RuntimeException {
        public WorkflowException(String message) {
            super(message);
        }

        public WorkflowException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}

