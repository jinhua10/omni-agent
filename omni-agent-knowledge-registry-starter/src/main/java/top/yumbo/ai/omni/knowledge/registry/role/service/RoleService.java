package top.yumbo.ai.omni.knowledge.registry.role.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import jakarta.annotation.PostConstruct;
import top.yumbo.ai.omni.knowledge.registry.model.role.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.role.RoleStatus;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 角色服务 (Role Service)
 * <p>
 * 提供角色管理和查询功能
 * (Provides role management and query functions)
 * <p>
 * 核心功能 (Core Features):
 * - 角色注册和管理 (Role registration and management)
 * - 角色查询 (Role query)
 * - 角色使用统计 (Role usage statistics)
 * - 关键词匹配 (Keyword matching)
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@Slf4j
@Service
public class RoleService {

    /**
     * 角色存储 (Role storage)
     * Key: roleId, Value: Role
     */
    private final Map<String, KnowledgeRole> roles = new ConcurrentHashMap<>();

    /**
     * 角色使用统计 (Role usage statistics)
     * Key: roleId, Value: usage count
     */
    private final Map<String, Integer> usageStats = new ConcurrentHashMap<>();

    /**
     * 默认角色ID (Default role ID)
     */
    private static final String DEFAULT_ROLE_ID = "default";

    @PostConstruct
    public void init() {
        // 初始化默认角色 (Initialize default role)
        KnowledgeRole defaultRole = KnowledgeRole.builder()
                .roleId(DEFAULT_ROLE_ID)
                .roleName("Default Role")
                .description("Default role for general questions")
                .keywords(Arrays.asList("general", "common", "default"))
                .status(RoleStatus.ACTIVE)
                .priority(0)
                .config(new HashMap<>())
                .build();

        roles.put(DEFAULT_ROLE_ID, defaultRole);
        log.info("RoleService initialized with default role");
    }

    /**
     * 注册角色 (Register role)
     *
     * @param role 角色对象 (Role object)
     */
    public void registerRole(KnowledgeRole role) {
        if (role == null || role.getRoleId() == null) {
            throw new IllegalArgumentException("Role or role ID cannot be null");
        }

        roles.put(role.getRoleId(), role);
        log.info("Role registered: {} - {}", role.getRoleId(), role.getRoleName());
    }

    /**
     * 获取角色 (Get role by ID)
     *
     * @param roleId 角色ID (Role ID)
     * @return 角色对象，不存在则返回默认角色 (Role object, or default role if not exists)
     */
    public KnowledgeRole getRole(String roleId) {
        KnowledgeRole role = roles.get(roleId);
        if (role == null) {
            log.warn("Role not found: {}, returning default role", roleId);
            return roles.get(DEFAULT_ROLE_ID);
        }
        return role;
    }

    /**
     * 获取所有启用的角色 (Get all enabled roles)
     *
     * @return 启用的角色列表 (List of enabled roles)
     */
    public List<KnowledgeRole> getEnabledRoles() {
        return roles.values().stream()
                .filter(r -> r.getStatus() == RoleStatus.ACTIVE)
                .sorted(Comparator.comparingInt(KnowledgeRole::getPriority).reversed())
                .collect(Collectors.toList());
    }

    /**
     * 检查角色是否存在 (Check if role exists)
     *
     * @param roleId 角色ID (Role ID)
     * @return 是否存在 (Whether exists)
     */
    public boolean hasRole(String roleId) {
        return roles.containsKey(roleId);
    }

    /**
     * 根据关键词匹配角色 (Match roles by keywords)
     *
     * @param keywords 关键词列表 (Keywords list)
     * @return 匹配的角色列表，按优先级排序 (Matched roles list, sorted by priority)
     */
    public List<KnowledgeRole> matchRolesByKeywords(List<String> keywords) {
        if (keywords == null || keywords.isEmpty()) {
            return Collections.singletonList(getDefaultRole());
        }

        return getEnabledRoles().stream()
                .filter(role -> {
                    // 检查角色关键词是否包含任何输入关键词
                    for (String keyword : keywords) {
                        if (role.getKeywords() != null &&
                                role.getKeywords().stream()
                                        .anyMatch(k -> k.equalsIgnoreCase(keyword))) {
                            return true;
                        }
                    }
                    return false;
                })
                .sorted(Comparator.comparingInt(KnowledgeRole::getPriority).reversed())
                .collect(Collectors.toList());
    }

    /**
     * 记录角色使用 (Record role usage)
     *
     * @param roleId 角色ID (Role ID)
     */
    public void recordUsage(String roleId) {
        usageStats.merge(roleId, 1, Integer::sum);
        log.debug("Role usage recorded: {} (total: {})", roleId, usageStats.get(roleId));
    }

    /**
     * 获取角色使用统计 (Get role usage statistics)
     *
     * @return 使用统计 Map (Usage statistics map)
     */
    public Map<String, Integer> getUsageStats() {
        return new HashMap<>(usageStats);
    }

    /**
     * 获取默认角色 (Get default role)
     *
     * @return 默认角色 (Default role)
     */
    public KnowledgeRole getDefaultRole() {
        return roles.get(DEFAULT_ROLE_ID);
    }

    /**
     * 获取所有角色 (Get all roles)
     *
     * @return 所有角色列表 (List of all roles)
     */
    public List<KnowledgeRole> getAllRoles() {
        return new ArrayList<>(roles.values());
    }

    /**
     * 删除角色 (Delete role)
     *
     * @param roleId 角色ID (Role ID)
     * @return 是否删除成功 (Whether deletion succeeded)
     */
    public boolean deleteRole(String roleId) {
        if (DEFAULT_ROLE_ID.equals(roleId)) {
            log.warn("Cannot delete default role");
            return false;
        }

        KnowledgeRole removed = roles.remove(roleId);
        if (removed != null) {
            usageStats.remove(roleId);
            log.info("Role deleted: {}", roleId);
            return true;
        }
        return false;
    }

    /**
     * 更新角色 (Update role)
     *
     * @param role 角色对象 (Role object)
     */
    public void updateRole(KnowledgeRole role) {
        if (role == null || role.getRoleId() == null) {
            throw new IllegalArgumentException("Role or role ID cannot be null");
        }

        if (!roles.containsKey(role.getRoleId())) {
            throw new IllegalArgumentException("Role not found: " + role.getRoleId());
        }

        roles.put(role.getRoleId(), role);
        log.info("Role updated: {} - {}", role.getRoleId(), role.getRoleName());
    }

    /**
     * 清除所有使用统计 (Clear all usage statistics)
     */
    public void clearUsageStats() {
        usageStats.clear();
        log.info("Usage statistics cleared");
    }
}


