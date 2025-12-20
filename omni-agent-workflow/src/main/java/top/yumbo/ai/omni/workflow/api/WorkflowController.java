package top.yumbo.ai.omni.workflow.api;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.workflow.Workflow;
import top.yumbo.ai.omni.workflow.WorkflowEngine;
import top.yumbo.ai.omni.workflow.WorkflowRegistry;
import top.yumbo.ai.omni.workflow.WorkflowResult;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

/**
 * 工作流核心 REST API 控制器
 * (Workflow Core REST API Controller)
 *
 * <p>提供工作流的核心管理和执行功能</p>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/workflows")
@CrossOrigin(origins = "*")
public class WorkflowController {

    @Autowired
    private WorkflowEngine workflowEngine;

    @Autowired
    private WorkflowRegistry workflowRegistry;

    /**
     * 列出所有可用工作流
     *
     * @return 工作流列表
     */
    @GetMapping
    public ResponseEntity<Map<String, Object>> listWorkflows() {
        try {
            log.info("📋 获取工作流列表");

            List<Map<String, Object>> workflows = workflowRegistry.getAllWorkflows().stream()
                    .map(workflow -> {
                        Map<String, Object> info = new HashMap<>();
                        info.put("name", workflow.getName());
                        info.put("version", workflow.getVersion() != null ? workflow.getVersion() : "1.0.0");
                        info.put("description", workflow.getDescription() != null ? workflow.getDescription() : "");
                        info.put("author", workflow.getAuthor() != null ? workflow.getAuthor() : "");
                        info.put("category", workflow.getCategory() != null ? workflow.getCategory() : "general");
                        info.put("tags", workflow.getTags() != null ? workflow.getTags() : List.of());
                        info.put("status", workflow.getStatus() != null ? workflow.getStatus() : "active");
                        info.put("steps", workflow.getSteps().size());
                        info.put("createdAt", workflow.getCreatedAt());
                        info.put("updatedAt", workflow.getUpdatedAt());
                        return info;
                    })
                    .collect(Collectors.toList());

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("count", workflows.size());
            response.put("data", workflows);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 获取工作流列表失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "获取失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 获取工作流详情
     *
     * @param workflowName 工作流名称
     * @param version 版本（可选）
     * @return 工作流详情
     */
    @GetMapping("/{workflowName}")
    public ResponseEntity<Map<String, Object>> getWorkflowDetail(
            @PathVariable String workflowName,
            @RequestParam(required = false) String version) {

        try {
            log.info("📄 获取工作流详情: name={}, version={}", workflowName, version);

            Workflow workflow;
            if (version != null) {
                workflow = workflowRegistry.getWorkflow(workflowName, version);
            } else {
                workflow = workflowRegistry.getLatestWorkflow(workflowName);
            }

            if (workflow == null) {
                Map<String, Object> error = new HashMap<>();
                error.put("success", false);
                error.put("message", "工作流不存在");
                return ResponseEntity.notFound().build();
            }

            Map<String, Object> workflowInfo = new HashMap<>();
            workflowInfo.put("name", workflow.getName());
            workflowInfo.put("version", workflow.getVersion() != null ? workflow.getVersion() : "1.0.0");
            workflowInfo.put("description", workflow.getDescription() != null ? workflow.getDescription() : "");
            workflowInfo.put("author", workflow.getAuthor() != null ? workflow.getAuthor() : "");
            workflowInfo.put("category", workflow.getCategory() != null ? workflow.getCategory() : "general");
            workflowInfo.put("tags", workflow.getTags() != null ? workflow.getTags() : List.of());
            workflowInfo.put("status", workflow.getStatus());
            workflowInfo.put("createdAt", workflow.getCreatedAt());
            workflowInfo.put("updatedAt", workflow.getUpdatedAt());
            workflowInfo.put("config", workflow.getConfig() != null ? workflow.getConfig() : Map.of());
            workflowInfo.put("metadata", workflow.getMetadata() != null ? workflow.getMetadata() : Map.of());
            workflowInfo.put("inputSchema", workflow.getInputSchema());
            workflowInfo.put("outputSchema", workflow.getOutputSchema());

            List<Map<String, Object>> steps = workflow.getSteps().stream()
                    .map(step -> {
                        Map<String, Object> stepInfo = new HashMap<>();
                        stepInfo.put("id", step.getId());
                        stepInfo.put("name", step.getName() != null ? step.getName() : "");
                        stepInfo.put("description", step.getDescription() != null ? step.getDescription() : "");
                        stepInfo.put("agent", step.getAgent());
                        stepInfo.put("input", step.getInput());
                        stepInfo.put("config", step.getConfig() != null ? step.getConfig() : Map.of());
                        stepInfo.put("dependencies", step.getDependencies() != null ? step.getDependencies() : List.of());
                        stepInfo.put("allowFailure", step.isAllowFailure());
                        stepInfo.put("timeout", step.getTimeout());
                        stepInfo.put("retries", step.getRetries());
                        stepInfo.put("condition", step.getCondition());
                        return stepInfo;
                    })
                    .collect(Collectors.toList());

            workflowInfo.put("steps", steps);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", workflowInfo);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 获取工作流详情失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "获取失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 执行工作流（同步）
     *
     * @param workflowName 工作流名称
     * @param version 版本（可选）
     * @param input 输入参数
     * @return 执行结果
     */
    @PostMapping("/{workflowName}/execute")
    public ResponseEntity<Map<String, Object>> executeWorkflow(
            @PathVariable String workflowName,
            @RequestParam(required = false) String version,
            @RequestBody Object input) {

        try {
            log.info("🚀 执行工作流: name={}, version={}", workflowName, version);

            WorkflowResult result;
            if (version != null) {
                result = workflowEngine.execute(workflowName, version, input);
            } else {
                result = workflowEngine.execute(workflowName, input);
            }

            Map<String, Object> response = new HashMap<>();
            response.put("success", result.isSuccess());
            response.put("executionId", result.getExecutionId());
            response.put("duration", result.getDuration());
            response.put("result", result.getFinalResult());

            if (result.isSuccess()) {
                response.put("stepResults", result.getStepResults());
                log.info("✅ 工作流执行成功: executionId={}, duration={}ms",
                         result.getExecutionId(), result.getDuration());
            } else {
                response.put("error", result.getError());
                log.error("❌ 工作流执行失败: executionId={}, error={}",
                          result.getExecutionId(), result.getError());
            }

            return result.isSuccess() ?
                    ResponseEntity.ok(response) :
                    ResponseEntity.badRequest().body(response);

        } catch (Exception e) {
            log.error("❌ 工作流执行异常", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "执行失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 异步执行工作流
     *
     * @param workflowName 工作流名称
     * @param version 版本（可选）
     * @param input 输入参数
     * @return 提交结果
     */
    @PostMapping("/{workflowName}/execute-async")
    public ResponseEntity<Map<String, Object>> executeWorkflowAsync(
            @PathVariable String workflowName,
            @RequestParam(required = false) String version,
            @RequestBody Object input) {

        try {
            log.info("🚀 异步执行工作流: name={}, version={}", workflowName, version);

            // 注意：当前 WorkflowEngine.executeAsync 不支持指定版本
            // 如果指定了版本，会记录警告但仍使用最新版本
            if (version != null) {
                log.warn("⚠️ executeAsync 暂不支持指定版本，将使用最新版本");
            }

            CompletableFuture<WorkflowResult> future = workflowEngine.executeAsync(workflowName, input);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "工作流已提交，正在异步执行");
            response.put("workflowName", workflowName);
            response.put("version", version != null ? version : "latest");

            log.info("✅ 工作流已提交异步执行: name={}", workflowName);
            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 工作流提交失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "提交失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 创建工作流
     *
     * @param workflow 工作流定义
     * @return 创建结果
     */
    @PostMapping
    public ResponseEntity<Map<String, Object>> createWorkflow(@RequestBody Workflow workflow) {
        try {
            log.info("📝 创建工作流: name={}, version={}", workflow.getName(), workflow.getVersion());

            // 设置默认值
            if (workflow.getVersion() == null) {
                workflow.setVersion("1.0.0");
            }
            if (workflow.getStatus() == null) {
                workflow.setStatus("active");
            }
            if (workflow.getCreatedAt() == null) {
                workflow.setCreatedAt(System.currentTimeMillis());
            }
            if (workflow.getUpdatedAt() == null) {
                workflow.setUpdatedAt(System.currentTimeMillis());
            }

            workflowRegistry.register(workflow);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "工作流创建成功");
            response.put("name", workflow.getName());
            response.put("version", workflow.getVersion());

            log.info("✅ 工作流创建成功: name={}, version={}", workflow.getName(), workflow.getVersion());
            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 创建工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "创建失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 更新工作流
     *
     * @param workflowName 工作流名称
     * @param workflow 工作流定义
     * @return 更新结果
     */
    @PutMapping("/{workflowName}")
    public ResponseEntity<Map<String, Object>> updateWorkflow(
            @PathVariable String workflowName,
            @RequestBody Workflow workflow) {

        try {
            log.info("📝 更新工作流: name={}", workflowName);

            // 确保名称一致
            workflow.setName(workflowName);
            workflow.setUpdatedAt(System.currentTimeMillis());

            workflowRegistry.register(workflow);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "工作流更新成功");
            response.put("name", workflow.getName());
            response.put("version", workflow.getVersion());

            log.info("✅ 工作流更新成功: name={}, version={}", workflow.getName(), workflow.getVersion());
            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 更新工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "更新失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 删除工作流
     *
     * @param workflowName 工作流名称
     * @param version 版本（可选，不指定则删除所有版本）
     * @return 删除结果
     */
    @DeleteMapping("/{workflowName}")
    public ResponseEntity<Map<String, Object>> deleteWorkflow(
            @PathVariable String workflowName,
            @RequestParam(required = false) String version) {

        try {
            log.info("🗑️ 删除工作流: name={}, version={}", workflowName, version);

            // TODO: 实现删除功能（需要在 WorkflowRegistry 中添加删除方法）
            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("message", "删除功能待实现");

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 删除工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "删除失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 按分类获取工作流
     *
     * @param category 分类
     * @return 工作流列表
     */
    @GetMapping("/category/{category}")
    public ResponseEntity<Map<String, Object>> getWorkflowsByCategory(@PathVariable String category) {
        try {
            log.info("📁 按分类获取工作流: category={}", category);

            List<Map<String, Object>> workflows = workflowRegistry.getAllWorkflows().stream()
                    .filter(workflow -> category.equals(workflow.getCategory()))
                    .map(workflow -> {
                        Map<String, Object> info = new HashMap<>();
                        info.put("name", workflow.getName());
                        info.put("version", workflow.getVersion() != null ? workflow.getVersion() : "1.0.0");
                        info.put("description", workflow.getDescription() != null ? workflow.getDescription() : "");
                        info.put("author", workflow.getAuthor() != null ? workflow.getAuthor() : "");
                        info.put("category", workflow.getCategory());
                        info.put("tags", workflow.getTags() != null ? workflow.getTags() : List.of());
                        info.put("status", workflow.getStatus());
                        return info;
                    })
                    .collect(Collectors.toList());

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("category", category);
            response.put("count", workflows.size());
            response.put("data", workflows);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 获取分类工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "获取失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 搜索工作流
     *
     * @param keyword 关键词
     * @return 搜索结果
     */
    @GetMapping("/search")
    public ResponseEntity<Map<String, Object>> searchWorkflows(@RequestParam String keyword) {
        try {
            log.info("🔍 搜索工作流: keyword={}", keyword);

            String lowerKeyword = keyword.toLowerCase();

            List<Map<String, Object>> workflows = workflowRegistry.getAllWorkflows().stream()
                    .filter(workflow -> {
                        String name = workflow.getName() != null ? workflow.getName().toLowerCase() : "";
                        String desc = workflow.getDescription() != null ? workflow.getDescription().toLowerCase() : "";
                        String author = workflow.getAuthor() != null ? workflow.getAuthor().toLowerCase() : "";

                        boolean matchTags = workflow.getTags() != null &&
                                workflow.getTags().stream()
                                        .anyMatch(tag -> tag.toLowerCase().contains(lowerKeyword));

                        return name.contains(lowerKeyword) ||
                                desc.contains(lowerKeyword) ||
                                author.contains(lowerKeyword) ||
                                matchTags;
                    })
                    .map(workflow -> {
                        Map<String, Object> info = new HashMap<>();
                        info.put("name", workflow.getName());
                        info.put("version", workflow.getVersion() != null ? workflow.getVersion() : "1.0.0");
                        info.put("description", workflow.getDescription() != null ? workflow.getDescription() : "");
                        info.put("author", workflow.getAuthor() != null ? workflow.getAuthor() : "");
                        info.put("category", workflow.getCategory() != null ? workflow.getCategory() : "general");
                        info.put("tags", workflow.getTags() != null ? workflow.getTags() : List.of());
                        return info;
                    })
                    .collect(Collectors.toList());

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("keyword", keyword);
            response.put("count", workflows.size());
            response.put("data", workflows);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 搜索工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "搜索失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 验证工作流定义
     *
     * @param workflow 工作流定义
     * @return 验证结果
     */
    @PostMapping("/validate")
    public ResponseEntity<Map<String, Object>> validateWorkflow(@RequestBody Workflow workflow) {
        try {
            log.info("✅ 验证工作流: name={}", workflow.getName());

            List<String> errors = new java.util.ArrayList<>();

            // 基本验证
            if (workflow.getName() == null || workflow.getName().trim().isEmpty()) {
                errors.add("工作流名称不能为空");
            }
            if (workflow.getSteps() == null || workflow.getSteps().isEmpty()) {
                errors.add("工作流必须包含至少一个步骤");
            }

            // 步骤验证
            if (workflow.getSteps() != null) {
                for (int i = 0; i < workflow.getSteps().size(); i++) {
                    var step = workflow.getSteps().get(i);
                    if (step.getId() == null || step.getId().trim().isEmpty()) {
                        errors.add("步骤 " + (i + 1) + ": ID 不能为空");
                    }
                    if (step.getAgent() == null || step.getAgent().trim().isEmpty()) {
                        errors.add("步骤 " + (i + 1) + ": Agent 不能为空");
                    }
                }
            }

            Map<String, Object> response = new HashMap<>();
            response.put("success", errors.isEmpty());
            response.put("valid", errors.isEmpty());

            if (errors.isEmpty()) {
                response.put("message", "工作流定义有效");
            } else {
                response.put("message", "工作流定义存在错误");
                response.put("errors", errors);
            }

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 验证工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "验证失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 获取工作流统计信息
     *
     * @return 统计信息
     */
    @GetMapping("/stats")
    public ResponseEntity<Map<String, Object>> getWorkflowStats() {
        try {
            log.info("📊 获取工作流统计信息");

            List<Workflow> allWorkflows = workflowRegistry.getAllWorkflows();

            Map<String, Long> categoryStats = allWorkflows.stream()
                    .collect(Collectors.groupingBy(
                            w -> w.getCategory() != null ? w.getCategory() : "general",
                            Collectors.counting()
                    ));

            Map<String, Long> statusStats = allWorkflows.stream()
                    .collect(Collectors.groupingBy(
                            w -> w.getStatus() != null ? w.getStatus() : "active",
                            Collectors.counting()
                    ));

            Map<String, Object> stats = new HashMap<>();
            stats.put("totalWorkflows", allWorkflows.size());
            stats.put("categoryStats", categoryStats);
            stats.put("statusStats", statusStats);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", stats);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 获取统计信息失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "获取失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }
}

