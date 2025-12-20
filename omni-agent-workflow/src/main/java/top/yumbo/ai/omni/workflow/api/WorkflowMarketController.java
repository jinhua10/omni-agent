package top.yumbo.ai.omni.workflow.api;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.workflow.Workflow;
import top.yumbo.ai.omni.workflow.market.MarketWorkflow;
import top.yumbo.ai.omni.workflow.market.WorkflowMarketService;
import top.yumbo.ai.omni.workflow.market.WorkflowRating;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 工作流市场 REST API 控制器
 * (Workflow Market REST API Controller)
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/workflows/market")
@CrossOrigin(origins = "*")
public class WorkflowMarketController {

    @Autowired
    private WorkflowMarketService marketService;

    /**
     * 发布工作流到市场
     *
     * @param request 发布请求
     * @param userId 用户ID（从请求头获取）
     * @param userName 用户名称（从请求头获取）
     * @return 发布结果
     */
    @PostMapping("/publish")
    public ResponseEntity<Map<String, Object>> publishWorkflow(
            @RequestBody PublishWorkflowRequest request,
            @RequestHeader(value = "X-User-Id", defaultValue = "anonymous") String userId,
            @RequestHeader(value = "X-User-Name", defaultValue = "Anonymous") String userName) {

        try {
            log.info("📤 发布工作流请求: name={}, version={}, author={}",
                     request.getName(), request.getVersion(), userName);

            // 构建工作流定义
            Workflow workflow = Workflow.builder()
                    .name(request.getName())
                    .version(request.getVersion())
                    .description(request.getDescription())
                    .tags(request.getTags() != null ? List.of(request.getTags()) : null)
                    .build();

            // 发布到市场
            String marketId = marketService.publishWorkflow(workflow, userId, userName);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("marketId", marketId);
            response.put("message", "工作流发布成功");

            log.info("✅ 工作流发布成功: marketId={}", marketId);
            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 工作流发布失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "发布失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 搜索工作流
     *
     * @param keyword 搜索关键词
     * @param page 页码
     * @param size 每页大小
     * @return 搜索结果
     */
    @GetMapping("/search")
    public ResponseEntity<Map<String, Object>> searchWorkflows(
            @RequestParam String keyword,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {

        try {
            log.info("🔍 搜索工作流: keyword={}, page={}, size={}", keyword, page, size);

            List<MarketWorkflow> workflows = marketService.searchWorkflows(keyword, page, size);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", workflows);
            response.put("page", page);
            response.put("size", size);
            response.put("total", workflows.size());

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 搜索失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "搜索失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 获取热门工作流
     *
     * @param limit 数量限制
     * @return 热门工作流列表
     */
    @GetMapping("/popular")
    public ResponseEntity<Map<String, Object>> getPopularWorkflows(
            @RequestParam(defaultValue = "10") int limit) {

        try {
            log.info("🔥 获取热门工作流: limit={}", limit);

            List<MarketWorkflow> workflows = marketService.getPopularWorkflows(limit);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", workflows);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 获取热门工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "获取失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 获取最新工作流
     *
     * @param limit 数量限制
     * @return 最新工作流列表
     */
    @GetMapping("/recent")
    public ResponseEntity<Map<String, Object>> getRecentWorkflows(
            @RequestParam(defaultValue = "10") int limit) {

        try {
            log.info("🆕 获取最新工作流: limit={}", limit);

            List<MarketWorkflow> workflows = marketService.getRecentWorkflows(limit);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", workflows);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 获取最新工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "获取失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 获取高评分工作流
     *
     * @param limit 数量限制
     * @return 高评分工作流列表
     */
    @GetMapping("/top-rated")
    public ResponseEntity<Map<String, Object>> getTopRatedWorkflows(
            @RequestParam(defaultValue = "10") int limit) {

        try {
            log.info("⭐ 获取高评分工作流: limit={}", limit);

            List<MarketWorkflow> workflows = marketService.getTopRatedWorkflows(limit);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", workflows);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 获取高评分工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "获取失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 下载工作流
     *
     * @param workflowId 工作流ID
     * @param userId 用户ID
     * @return 工作流定义
     */
    @GetMapping("/{workflowId}/download")
    public ResponseEntity<Map<String, Object>> downloadWorkflow(
            @PathVariable String workflowId,
            @RequestHeader(value = "X-User-Id", defaultValue = "anonymous") String userId) {

        try {
            log.info("⬇️ 下载工作流: workflowId={}, userId={}", workflowId, userId);

            Workflow workflow = marketService.downloadWorkflow(workflowId, userId);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", workflow);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 下载工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "下载失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 安装工作流
     *
     * @param workflowId 工作流ID
     * @param userId 用户ID
     * @return 安装结果
     */
    @PostMapping("/{workflowId}/install")
    public ResponseEntity<Map<String, Object>> installWorkflow(
            @PathVariable String workflowId,
            @RequestHeader(value = "X-User-Id", defaultValue = "anonymous") String userId) {

        try {
            log.info("📦 安装工作流: workflowId={}, userId={}", workflowId, userId);

            boolean success = marketService.installWorkflow(workflowId, userId);

            Map<String, Object> response = new HashMap<>();
            response.put("success", success);
            response.put("message", success ? "安装成功" : "安装失败");

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 安装工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "安装失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 评分工作流
     *
     * @param workflowId 工作流ID
     * @param request 评分请求
     * @param userId 用户ID
     * @param userName 用户名称
     * @return 评分结果
     */
    @PostMapping("/{workflowId}/rate")
    public ResponseEntity<Map<String, Object>> rateWorkflow(
            @PathVariable String workflowId,
            @RequestBody RatingRequest request,
            @RequestHeader(value = "X-User-Id", defaultValue = "anonymous") String userId,
            @RequestHeader(value = "X-User-Name", defaultValue = "Anonymous") String userName) {

        try {
            log.info("⭐ 评分工作流: workflowId={}, userId={}, rating={}",
                     workflowId, userId, request.getRating());

            // 验证评分范围
            if (request.getRating() < 1 || request.getRating() > 5) {
                Map<String, Object> error = new HashMap<>();
                error.put("success", false);
                error.put("message", "评分必须在 1-5 之间");
                return ResponseEntity.badRequest().body(error);
            }

            boolean success = marketService.rateWorkflow(
                    workflowId, userId, userName,
                    request.getRating(), request.getComment());

            Map<String, Object> response = new HashMap<>();
            response.put("success", success);
            response.put("message", success ? "评分成功" : "评分失败");

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 评分失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "评分失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 获取工作流的评分列表
     *
     * @param workflowId 工作流ID
     * @param page 页码
     * @param size 每页大小
     * @return 评分列表
     */
    @GetMapping("/{workflowId}/ratings")
    public ResponseEntity<Map<String, Object>> getWorkflowRatings(
            @PathVariable String workflowId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {

        try {
            log.info("📊 获取工作流评分: workflowId={}, page={}, size={}", workflowId, page, size);

            List<WorkflowRating> ratings = marketService.getWorkflowRatings(workflowId, page, size);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", ratings);
            response.put("page", page);
            response.put("size", size);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 获取评分失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "获取失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }

    /**
     * 获取工作流详情
     *
     * @param workflowId 工作流ID
     * @return 工作流详情
     */
    @GetMapping("/{workflowId}")
    public ResponseEntity<Map<String, Object>> getWorkflowDetail(
            @PathVariable String workflowId) {

        try {
            log.info("📄 获取工作流详情: workflowId={}", workflowId);

            var workflowOpt = marketService.getWorkflowDetail(workflowId);

            if (workflowOpt.isEmpty()) {
                Map<String, Object> error = new HashMap<>();
                error.put("success", false);
                error.put("message", "工作流不存在");
                return ResponseEntity.notFound().build();
            }

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", workflowOpt.get());

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
     * 按分类获取工作流
     *
     * @param category 分类
     * @param page 页码
     * @param size 每页大小
     * @return 工作流列表
     */
    @GetMapping("/category/{category}")
    public ResponseEntity<Map<String, Object>> getWorkflowsByCategory(
            @PathVariable String category,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {

        try {
            log.info("📁 获取分类工作流: category={}, page={}, size={}", category, page, size);

            List<MarketWorkflow> workflows = marketService.getWorkflowsByCategory(category, page, size);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", workflows);
            response.put("category", category);
            response.put("page", page);
            response.put("size", size);

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
     * 获取用户的工作流
     *
     * @param authorId 作者ID
     * @param page 页码
     * @param size 每页大小
     * @return 工作流列表
     */
    @GetMapping("/author/{authorId}")
    public ResponseEntity<Map<String, Object>> getUserWorkflows(
            @PathVariable String authorId,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {

        try {
            log.info("👤 获取用户工作流: authorId={}, page={}, size={}", authorId, page, size);

            List<MarketWorkflow> workflows = marketService.getUserWorkflows(authorId, page, size);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("data", workflows);
            response.put("page", page);
            response.put("size", size);

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("❌ 获取用户工作流失败", e);
            Map<String, Object> error = new HashMap<>();
            error.put("success", false);
            error.put("message", "获取失败: " + e.getMessage());
            return ResponseEntity.badRequest().body(error);
        }
    }
}

