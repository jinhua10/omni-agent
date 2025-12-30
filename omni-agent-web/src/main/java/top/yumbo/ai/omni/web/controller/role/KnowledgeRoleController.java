package top.yumbo.ai.omni.web.controller.role;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.knowledge.registry.dto.role.CreateRoleRequest;
import top.yumbo.ai.omni.knowledge.registry.dto.role.LearnFromDomainsRequest;
import top.yumbo.ai.omni.knowledge.registry.dto.role.UpdateRoleRequest;
import top.yumbo.ai.omni.knowledge.registry.model.role.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.role.RoleStatus;
import top.yumbo.ai.omni.knowledge.registry.role.service.KnowledgeRoleService;
import top.yumbo.ai.omni.knowledge.registry.role.service.RoleLearningService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 知识角色管理控制器
 *
 * <p>提供角色的 CRUD 和学习 API</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/knowledge-roles")
@RequiredArgsConstructor
public class KnowledgeRoleController {

    private final KnowledgeRoleService roleService;
    private final RoleLearningService learningService;

    /**
     * 创建知识角色
     *
     * @param request 创建请求
     * @return 创建的角色
     */
    @PostMapping
    public ResponseEntity<KnowledgeRole> createRole(@RequestBody CreateRoleRequest request) {
        try {
            KnowledgeRole role = roleService.createRole(request);
            return ResponseEntity.ok(role);
        } catch (Exception e) {
            log.error("创建知识角色失败", e);
            throw e;
        }
    }

    /**
     * 获取知识角色详情
     *
     * @param roleId 角色ID
     * @return 角色详情
     */
    @GetMapping("/{roleId}")
    public ResponseEntity<KnowledgeRole> getRole(@PathVariable String roleId) {
        try {
            KnowledgeRole role = roleService.getRole(roleId);
            return ResponseEntity.ok(role);
        } catch (Exception e) {
            log.error("获取知识角色失败: {}", roleId, e);
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * 列出所有知识角色
     *
     * @param status 角色状态（可选）
     * @return 角色列表
     */
    @GetMapping
    public ResponseEntity<List<KnowledgeRole>> listRoles(
            @RequestParam(required = false) RoleStatus status) {
        try {
            List<KnowledgeRole> roles;

            if (status != null) {
                roles = roleService.listRolesByStatus(status);
            } else {
                roles = roleService.listAllRoles();
            }

            return ResponseEntity.ok(roles);
        } catch (Exception e) {
            log.error("列出知识角色失败", e);
            throw e;
        }
    }

    /**
     * 更新知识角色
     *
     * @param roleId 角色ID
     * @param request 更新请求
     * @return 更新后的角色
     */
    @PutMapping("/{roleId}")
    public ResponseEntity<KnowledgeRole> updateRole(
            @PathVariable String roleId,
            @RequestBody UpdateRoleRequest request) {
        try {
            KnowledgeRole role = roleService.updateRole(roleId, request);
            return ResponseEntity.ok(role);
        } catch (Exception e) {
            log.error("更新知识角色失败: {}", roleId, e);
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * 删除知识角色
     *
     * @param roleId 角色ID
     * @return 删除结果
     */
    @DeleteMapping("/{roleId}")
    public ResponseEntity<Map<String, Object>> deleteRole(@PathVariable String roleId) {
        try {
            roleService.deleteRole(roleId);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "Role deleted successfully");
            response.put("roleId", roleId);

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            log.error("删除知识角色失败: {}", roleId, e);

            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("message", e.getMessage());

            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * 角色学习 - 从指定域学习知识
     *
     * @param roleId 角色ID
     * @param request 学习请求
     * @return 学习结果
     */
    @PostMapping("/{roleId}/learn")
    public ResponseEntity<Map<String, Object>> learnFromDomains(
            @PathVariable String roleId,
            @RequestBody LearnFromDomainsRequest request) {
        try {
            // 异步执行学习任务
            new Thread(() -> {
                try {
                    learningService.learnFromDomains(roleId, request);
                } catch (Exception e) {
                    log.error("角色学习任务失败: {}", roleId, e);
                }
            }).start();

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "Learning task started");
            response.put("roleId", roleId);

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            log.error("启动学习任务失败: {}", roleId, e);

            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("message", e.getMessage());

            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * 停止学习
     *
     * @param roleId 角色ID
     * @return 停止结果
     */
    @PostMapping("/{roleId}/stop-learning")
    public ResponseEntity<Map<String, Object>> stopLearning(@PathVariable String roleId) {
        try {
            learningService.stopLearning(roleId);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "Learning stopped");
            response.put("roleId", roleId);

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            log.error("停止学习失败: {}", roleId, e);

            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("message", e.getMessage());

            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * 获取统计信息
     *
     * @return 统计信息
     */
    @GetMapping("/statistics")
    public ResponseEntity<Map<String, Object>> getStatistics() {
        try {
            Map<String, Object> stats = new HashMap<>();
            stats.put("totalRoles", roleService.countRoles());
            stats.put("activeRoles", roleService.listRolesByStatus(RoleStatus.ACTIVE).size());
            stats.put("learningRoles", roleService.listRolesByStatus(RoleStatus.LEARNING).size());

            return ResponseEntity.ok(stats);
        } catch (Exception e) {
            log.error("获取统计信息失败", e);
            throw e;
        }
    }
}






