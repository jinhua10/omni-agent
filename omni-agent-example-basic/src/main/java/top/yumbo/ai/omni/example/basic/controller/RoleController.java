package top.yumbo.ai.omni.example.basic.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.core.role.RoleService;
import top.yumbo.ai.omni.core.role.Role;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 角色管理控制器
 * (Role Management Controller)
 *
 * <p>提供角色的增删改查、分页、搜索功能</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/roles")
@CrossOrigin(origins = "*")
@RequiredArgsConstructor
public class RoleController {

    private final RoleService roleService;

    /**
     * 获取角色列表（支持分页和搜索）
     * GET /api/roles
     */
    @GetMapping
    public ResponseEntity<RolePageResponse> getRoleList(
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "10") int pageSize,
            @RequestParam(required = false) String keyword,
            @RequestParam(required = false) Boolean enabled) {

        try {
            log.debug("获取角色列表: page={}, pageSize={}, keyword={}, enabled={}",
                page, pageSize, keyword, enabled);

            // 获取所有角色
            List<Role> allRoles = roleService.getAllRoles();

            // 过滤
            List<Role> filtered = allRoles.stream()
                .filter(role -> keyword == null || keyword.isEmpty() ||
                    role.getName().contains(keyword) ||
                    role.getDescription().contains(keyword))
                .filter(role -> enabled == null || role.isEnabled() == enabled)
                .sorted((r1, r2) -> Integer.compare(r2.getPriority(), r1.getPriority()))
                .collect(Collectors.toList());

            // 分页
            int start = (page - 1) * pageSize;
            int end = Math.min(start + pageSize, filtered.size());

            List<Role> pageRoles = start < filtered.size()
                ? filtered.subList(start, end)
                : List.of();

            RolePageResponse response = new RolePageResponse();
            response.setList(pageRoles);
            response.setTotal(filtered.size());
            response.setPage(page);
            response.setPageSize(pageSize);
            response.setTotalPages((int) Math.ceil((double) filtered.size() / pageSize));

            return ResponseEntity.ok(response);

        } catch (Exception e) {
            log.error("获取角色列表失败", e);
            return ResponseEntity.internalServerError().build();
        }
    }

    /**
     * 获取角色详情
     * GET /api/roles/{roleName}
     */
    @GetMapping("/{roleName}")
    public ResponseEntity<Role> getRole(@PathVariable String roleName) {
        try {
            log.debug("获取角色详情: {}", roleName);

            Role role = roleService.getRole(roleName);

            return ResponseEntity.ok(role);

        } catch (Exception e) {
            log.error("获取角色详情失败: {}", roleName, e);
            return ResponseEntity.internalServerError().build();
        }
    }

    /**
     * 创建角色
     * POST /api/roles
     */
    @PostMapping
    public ResponseEntity<Map<String, Object>> createRole(@RequestBody Role role) {
        Map<String, Object> result = new HashMap<>();

        try {
            log.info("创建角色: {}", role.getName());

            roleService.registerRole(role);

            result.put("status", "success");
            result.put("message", "角色创建成功");
            result.put("roleName", role.getName());
            return ResponseEntity.ok(result);

        } catch (Exception e) {
            log.error("创建角色失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
            return ResponseEntity.internalServerError().body(result);
        }
    }

    /**
     * 更新角色
     * PUT /api/roles/{roleName}
     */
    @PutMapping("/{roleName}")
    public ResponseEntity<Map<String, Object>> updateRole(
            @PathVariable String roleName,
            @RequestBody Role role) {

        Map<String, Object> result = new HashMap<>();

        try {
            log.info("更新角色: {}", roleName);

            // 确保名称一致
            role.setName(roleName);

            roleService.updateRole(role);

            result.put("status", "success");
            result.put("message", "角色更新成功");
            result.put("roleName", roleName);
            return ResponseEntity.ok(result);

        } catch (Exception e) {
            log.error("更新角色失败: {}", roleName, e);
            result.put("status", "error");
            result.put("error", e.getMessage());
            return ResponseEntity.internalServerError().body(result);
        }
    }

    /**
     * 删除角色
     * DELETE /api/roles/{roleName}
     */
    @DeleteMapping("/{roleName}")
    public ResponseEntity<Map<String, Object>> deleteRole(@PathVariable String roleName) {
        Map<String, Object> result = new HashMap<>();

        try {
            log.info("删除角色: {}", roleName);

            boolean deleted = roleService.deleteRole(roleName);

            if (deleted) {
                result.put("status", "success");
                result.put("message", "角色删除成功");
                result.put("roleName", roleName);
                return ResponseEntity.ok(result);
            } else {
                result.put("status", "error");
                result.put("message", "角色不存在");
                return ResponseEntity.notFound().build();
            }

        } catch (Exception e) {
            log.error("删除角色失败: {}", roleName, e);
            result.put("status", "error");
            result.put("error", e.getMessage());
            return ResponseEntity.internalServerError().body(result);
        }
    }

    /**
     * 启用/禁用角色
     * PATCH /api/roles/{roleName}/toggle
     */
    @PatchMapping("/{roleName}/toggle")
    public ResponseEntity<Map<String, Object>> toggleRole(@PathVariable String roleName) {
        Map<String, Object> result = new HashMap<>();

        try {
            log.info("切换角色状态: {}", roleName);

            Role role = roleService.getRole(roleName);
            role.setEnabled(!role.isEnabled());

            roleService.updateRole(role);

            result.put("status", "success");
            result.put("message", "角色状态已更新");
            result.put("roleName", roleName);
            result.put("enabled", role.isEnabled());
            return ResponseEntity.ok(result);

        } catch (Exception e) {
            log.error("切换角色状态失败: {}", roleName, e);
            result.put("status", "error");
            result.put("error", e.getMessage());
            return ResponseEntity.internalServerError().body(result);
        }
    }

    /**
     * 获取默认角色
     * GET /api/roles/default
     */
    @GetMapping("/default")
    public ResponseEntity<Role> getDefaultRole() {
        try {
            Role defaultRole = roleService.getDefaultRole();
            return ResponseEntity.ok(defaultRole);
        } catch (Exception e) {
            log.error("获取默认角色失败", e);
            return ResponseEntity.internalServerError().build();
        }
    }

    /**
     * 批量创建角色
     * POST /api/roles/batch
     */
    @PostMapping("/batch")
    public ResponseEntity<Map<String, Object>> createRoles(@RequestBody BatchCreateRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            int successCount = 0;
            int failCount = 0;

            for (Role role : request.getRoles()) {
                try {
                    roleService.registerRole(role);
                    successCount++;
                } catch (Exception e) {
                    failCount++;
                    log.warn("创建角色失败: {}", role.getName(), e);
                }
            }

            result.put("status", "success");
            result.put("totalCount", request.getRoles().size());
            result.put("successCount", successCount);
            result.put("failCount", failCount);
            result.put("message", String.format("批量创建完成: 成功 %d, 失败 %d", successCount, failCount));

            return ResponseEntity.ok(result);

        } catch (Exception e) {
            log.error("批量创建角色失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
            return ResponseEntity.internalServerError().body(result);
        }
    }

    // ========== DTO 类 ==========

    @Data
    public static class RolePageResponse {
        private List<Role> list;
        private long total;
        private int page;
        private int pageSize;
        private int totalPages;
    }

    @Data
    public static class BatchCreateRequest {
        private List<Role> roles;
    }
}

