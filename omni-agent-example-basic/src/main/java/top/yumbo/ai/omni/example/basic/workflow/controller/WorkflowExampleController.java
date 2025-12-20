package top.yumbo.ai.omni.example.basic.workflow.controller;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.workflow.Workflow;
import top.yumbo.ai.omni.workflow.WorkflowEngine;
import top.yumbo.ai.omni.workflow.WorkflowRegistry;
import top.yumbo.ai.omni.workflow.WorkflowResult;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

/**
 * 工作流示例控制器
 *
 * @author OmniAgent Team
 */
@Slf4j
@RestController
@RequestMapping("/api/example/workflow")
@CrossOrigin(origins = "*")
public class WorkflowExampleController {

    @Autowired
    private WorkflowEngine workflowEngine;

    @Autowired
    private WorkflowRegistry workflowRegistry;

    /**
     * 列出所有可用工作流
     */
    @GetMapping("/list")
    public ResponseEntity<Map<String, Object>> listWorkflows() {
        log.info("📋 获取工作流列表");

        List<Map<String, Object>> workflows = workflowRegistry.getAllWorkflows().stream()
                .map(workflow -> Map.of(
                        "name", workflow.getName(),
                        "version", workflow.getVersion(),
                        "description", workflow.getDescription() != null ? workflow.getDescription() : "",
                        "tags", workflow.getTags() != null ? workflow.getTags() : List.of(),
                        "steps", workflow.getSteps().size()
                ))
                .collect(Collectors.toList());

        return ResponseEntity.ok(Map.of(
                "success", true,
                "count", workflows.size(),
                "workflows", workflows
        ));
    }

    /**
     * 执行数据处理工作流
     */
    @PostMapping("/execute/data-processing")
    public ResponseEntity<Map<String, Object>> executeDataProcessing(@RequestBody Map<String, Object> data) {
        log.info("🚀 执行数据处理工作流");

        try {
            WorkflowResult result = workflowEngine.execute("DataProcessingWorkflow", data);

            if (result.isSuccess()) {
                return ResponseEntity.ok(Map.of(
                        "success", true,
                        "executionId", result.getExecutionId(),
                        "duration", result.getDuration() + "ms",
                        "result", result.getFinalResult(),
                        "stepResults", result.getStepResults()
                ));
            } else {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "error", result.getError()
                ));
            }
        } catch (Exception e) {
            log.error("❌ 工作流执行失败", e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "error", e.getMessage()
            ));
        }
    }

    /**
     * 执行批量处理工作流
     */
    @PostMapping("/execute/batch-processing")
    public ResponseEntity<Map<String, Object>> executeBatchProcessing(@RequestBody Map<String, Object> input) {
        log.info("🚀 执行批量处理工作流");

        try {
            WorkflowResult result = workflowEngine.execute("BatchProcessingWorkflow", input);

            if (result.isSuccess()) {
                return ResponseEntity.ok(Map.of(
                        "success", true,
                        "executionId", result.getExecutionId(),
                        "duration", result.getDuration() + "ms",
                        "result", result.getFinalResult()
                ));
            } else {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "error", result.getError()
                ));
            }
        } catch (Exception e) {
            log.error("❌ 工作流执行失败", e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "error", e.getMessage()
            ));
        }
    }

    /**
     * 异步执行工作流
     */
    @PostMapping("/execute-async/{workflowName}")
    public ResponseEntity<Map<String, Object>> executeAsync(
            @PathVariable String workflowName,
            @RequestBody Object input) {

        log.info("🚀 异步执行工作流: {}", workflowName);

        try {
            CompletableFuture<WorkflowResult> future = workflowEngine.executeAsync(workflowName, input);

            return ResponseEntity.ok(Map.of(
                    "success", true,
                    "message", "工作流已提交，正在异步执行",
                    "workflowName", workflowName
            ));
        } catch (Exception e) {
            log.error("❌ 工作流提交失败", e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "error", e.getMessage()
            ));
        }
    }

    /**
     * 通用工作流执行接口
     */
    @PostMapping("/execute/{workflowName}")
    public ResponseEntity<Map<String, Object>> execute(
            @PathVariable String workflowName,
            @RequestParam(required = false) String version,
            @RequestBody Object input) {

        log.info("🚀 执行工作流: {}, version: {}", workflowName, version);

        try {
            WorkflowResult result;
            if (version != null) {
                result = workflowEngine.execute(workflowName, version, input);
            } else {
                result = workflowEngine.execute(workflowName, input);
            }

            if (result.isSuccess()) {
                return ResponseEntity.ok(Map.of(
                        "success", true,
                        "executionId", result.getExecutionId(),
                        "duration", result.getDuration() + "ms",
                        "result", result.getFinalResult(),
                        "stepResults", result.getStepResults()
                ));
            } else {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "error", result.getError()
                ));
            }
        } catch (Exception e) {
            log.error("❌ 工作流执行失败", e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "error", e.getMessage()
            ));
        }
    }

    /**
     * 获取工作流详情
     */
    @GetMapping("/detail/{workflowName}")
    public ResponseEntity<Map<String, Object>> getWorkflowDetail(@PathVariable String workflowName) {
        log.info("📄 获取工作流详情: {}", workflowName);

        Workflow workflow = workflowRegistry.getLatestWorkflow(workflowName);

        if (workflow == null) {
            return ResponseEntity.notFound().build();
        }


        return ResponseEntity.ok(Map.of(
                "success", true,
                "workflow", Map.of(
                        "name", workflow.getName(),
                        "version", workflow.getVersion(),
                        "description", workflow.getDescription() != null ? workflow.getDescription() : "",
                        "author", workflow.getAuthor() != null ? workflow.getAuthor() : "",
                        "tags", workflow.getTags() != null ? workflow.getTags() : List.of(),
                        "status", workflow.getStatus(),
                        "steps", workflow.getSteps().stream()
                                .map(step -> Map.of(
                                        "id", step.getId(),
                                        "name", step.getName() != null ? step.getName() : "",
                                        "agent", step.getAgent(),
                                        "dependencies", step.getDependencies() != null ? step.getDependencies() : List.of()
                                ))
                                .collect(Collectors.toList())
                )
        ));
    }

    /**
     * 测试简单数据
     */
    @GetMapping("/test")
    public ResponseEntity<Map<String, Object>> test() {
        log.info("🧪 测试工作流");

        // 创建测试数据
        Map<String, Object> testData = Map.of(
                "name", "张三",
                "age", 25,
                "email", "zhangsan@example.com",
                "city", "北京"
        );

        try {
            WorkflowResult result = workflowEngine.execute("DataProcessingWorkflow", testData);

            if (result.isSuccess()) {
                return ResponseEntity.ok(Map.of(
                        "success", true,
                        "message", "测试成功",
                        "executionId", result.getExecutionId(),
                        "duration", result.getDuration() + "ms",
                        "result", result.getFinalResult()
                ));
            } else {
                return ResponseEntity.ok(Map.of(
                        "success", false,
                        "message", "测试失败",
                        "error", result.getError()
                ));
            }
        } catch (Exception e) {
            log.error("❌ 测试失败", e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "error", e.getMessage()
            ));
        }
    }

    /**
     * AI 生成工作流
     */
    @PostMapping("/generate")
    public ResponseEntity<Map<String, Object>> generateWorkflow(@RequestBody Map<String, String> request) {
        String description = request.get("description");
        log.info("🤖 AI 生成工作流，描述：{}", description);

        try {
            // TODO: 集成 AI 服务生成工作流
            // 这里先返回一个示例工作流作为演示
            Map<String, Object> workflow = Map.of(
                    "name", "AI_Generated_Workflow_" + System.currentTimeMillis(),
                    "version", "1.0.0",
                    "description", "根据描述生成：" + description,
                    "author", "AI Assistant",
                    "category", "ai-generated",
                    "status", "draft",
                    "steps", List.of(
                            Map.of(
                                    "id", "step_1",
                                    "name", "数据验证",
                                    "description", "验证输入数据格式",
                                    "agent", "DataValidator",
                                    "input", "${workflow.input}",
                                    "config", Map.of(),
                                    "dependencies", List.of(),
                                    "allowFailure", false,
                                    "timeout", 60000,
                                    "retries", 0
                            ),
                            Map.of(
                                    "id", "step_2",
                                    "name", "数据转换",
                                    "description", "转换数据格式",
                                    "agent", "DataTransformer",
                                    "input", "${step_1.output}",
                                    "config", Map.of(),
                                    "dependencies", List.of("step_1"),
                                    "allowFailure", false,
                                    "timeout", 60000,
                                    "retries", 0
                            ),
                            Map.of(
                                    "id", "step_3",
                                    "name", "数据过滤",
                                    "description", "过滤无效数据",
                                    "agent", "DataFilter",
                                    "input", "${step_2.output}",
                                    "config", Map.of(),
                                    "dependencies", List.of("step_2"),
                                    "allowFailure", false,
                                    "timeout", 60000,
                                    "retries", 0
                            )
                    )
            );

            return ResponseEntity.ok(Map.of(
                    "success", true,
                    "message", "工作流生成成功",
                    "workflow", workflow
            ));
        } catch (Exception e) {
            log.error("❌ AI 生成工作流失败", e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "message", "生成失败：" + e.getMessage()
            ));
        }
    }
}

