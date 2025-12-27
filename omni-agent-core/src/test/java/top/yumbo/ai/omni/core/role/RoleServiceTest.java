package top.yumbo.ai.omni.core.role;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * RoleService 单元测试
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@DisplayName("RoleService 单元测试")
class RoleServiceTest {

    private RoleService roleService;

    @BeforeEach
    void setUp() {
        roleService = new RoleService();
        roleService.init();
    }

    @Test
    @DisplayName("测试默认角色初始化")
    void testDefaultRoleInitialization() {
        // 验证默认角色存在
        Role defaultRole = roleService.getDefaultRole();
        assertNotNull(defaultRole, "默认角色应该存在");
        assertEquals("default", defaultRole.getId(), "默认角色ID应该是default");
        assertTrue(defaultRole.isEnabled(), "默认角色应该是启用状态");
    }

    @Test
    @DisplayName("测试角色注册")
    void testRegisterRole() {
        // 创建新角色
        Role testRole = Role.builder()
                .id("test-role")
                .name("Test Role")
                .description("A test role")
                .keywords(Arrays.asList("test", "example"))
                .enabled(true)
                .priority(10)
                .build();

        // 注册角色
        roleService.registerRole(testRole);

        // 验证角色已注册
        assertTrue(roleService.hasRole("test-role"), "角色应该已注册");
        Role retrieved = roleService.getRole("test-role");
        assertEquals("Test Role", retrieved.getName(), "角色名称应该匹配");
    }

    @Test
    @DisplayName("测试获取不存在的角色返回默认角色")
    void testGetNonExistentRoleReturnsDefault() {
        Role role = roleService.getRole("non-existent");
        assertNotNull(role, "应该返回默认角色");
        assertEquals("default", role.getId(), "应该返回默认角色ID");
    }

    @Test
    @DisplayName("测试关键词匹配")
    void testMatchRolesByKeywords() {
        // 注册测试角色
        Role javaRole = Role.builder()
                .id("java-developer")
                .name("Java Developer")
                .keywords(Arrays.asList("java", "spring", "backend"))
                .enabled(true)
                .priority(20)
                .build();

        Role pythonRole = Role.builder()
                .id("python-developer")
                .name("Python Developer")
                .keywords(Arrays.asList("python", "django", "ml"))
                .enabled(true)
                .priority(15)
                .build();

        roleService.registerRole(javaRole);
        roleService.registerRole(pythonRole);

        // 测试关键词匹配
        List<Role> matched = roleService.matchRolesByKeywords(Arrays.asList("java"));
        assertFalse(matched.isEmpty(), "应该匹配到角色");
        assertEquals("java-developer", matched.get(0).getId(), "应该匹配到Java开发者角色");

        // 测试优先级排序
        List<Role> allMatched = roleService.matchRolesByKeywords(Arrays.asList("java", "python"));
        assertTrue(allMatched.size() >= 2, "应该匹配到多个角色");
        // 验证按优先级排序（高优先级在前）
        assertTrue(allMatched.get(0).getPriority() >= allMatched.get(1).getPriority(),
                "应该按优先级降序排列");
    }

    @Test
    @DisplayName("测试使用统计")
    void testUsageStatistics() {
        String roleId = "test-role";
        Role testRole = Role.builder()
                .id(roleId)
                .name("Test Role")
                .enabled(true)
                .build();

        roleService.registerRole(testRole);

        // 记录使用
        roleService.recordUsage(roleId);
        roleService.recordUsage(roleId);
        roleService.recordUsage(roleId);

        // 验证统计
        Map<String, Integer> stats = roleService.getUsageStats();
        assertEquals(3, stats.get(roleId), "使用次数应该是3");
    }

    @Test
    @DisplayName("测试获取所有启用的角色")
    void testGetEnabledRoles() {
        // 注册一个启用的角色
        Role enabledRole = Role.builder()
                .id("enabled-role")
                .name("Enabled Role")
                .enabled(true)
                .priority(10)
                .build();

        // 注册一个禁用的角色
        Role disabledRole = Role.builder()
                .id("disabled-role")
                .name("Disabled Role")
                .enabled(false)
                .priority(5)
                .build();

        roleService.registerRole(enabledRole);
        roleService.registerRole(disabledRole);

        // 获取所有启用的角色
        List<Role> enabledRoles = roleService.getEnabledRoles();

        // 验证只返回启用的角色
        assertTrue(enabledRoles.stream().allMatch(Role::isEnabled),
                "应该只返回启用的角色");
        assertTrue(enabledRoles.stream().anyMatch(r -> r.getId().equals("enabled-role")),
                "应该包含启用的角色");
        assertFalse(enabledRoles.stream().anyMatch(r -> r.getId().equals("disabled-role")),
                "不应该包含禁用的角色");
    }

    @Test
    @DisplayName("测试删除角色")
    void testDeleteRole() {
        String roleId = "deletable-role";
        Role role = Role.builder()
                .id(roleId)
                .name("Deletable Role")
                .enabled(true)
                .build();

        roleService.registerRole(role);
        assertTrue(roleService.hasRole(roleId), "角色应该存在");

        // 删除角色
        boolean deleted = roleService.deleteRole(roleId);
        assertTrue(deleted, "应该成功删除");
        assertFalse(roleService.hasRole(roleId), "角色应该已被删除");
    }

    @Test
    @DisplayName("测试不能删除默认角色")
    void testCannotDeleteDefaultRole() {
        boolean deleted = roleService.deleteRole("default");
        assertFalse(deleted, "不应该能删除默认角色");
        assertTrue(roleService.hasRole("default"), "默认角色应该仍然存在");
    }

    @Test
    @DisplayName("测试更新角色")
    void testUpdateRole() {
        String roleId = "updatable-role";
        Role originalRole = Role.builder()
                .id(roleId)
                .name("Original Name")
                .description("Original Description")
                .enabled(true)
                .priority(5)
                .build();

        roleService.registerRole(originalRole);

        // 更新角色
        Role updatedRole = Role.builder()
                .id(roleId)
                .name("Updated Name")
                .description("Updated Description")
                .enabled(true)
                .priority(10)
                .build();

        roleService.updateRole(updatedRole);

        // 验证更新
        Role retrieved = roleService.getRole(roleId);
        assertEquals("Updated Name", retrieved.getName(), "名称应该已更新");
        assertEquals("Updated Description", retrieved.getDescription(), "描述应该已更新");
        assertEquals(10, retrieved.getPriority(), "优先级应该已更新");
    }

    @Test
    @DisplayName("测试清除使用统计")
    void testClearUsageStats() {
        String roleId = "test-role";
        Role role = Role.builder()
                .id(roleId)
                .name("Test Role")
                .enabled(true)
                .build();

        roleService.registerRole(role);
        roleService.recordUsage(roleId);
        roleService.recordUsage(roleId);

        Map<String, Integer> stats = roleService.getUsageStats();
        assertTrue(stats.containsKey(roleId), "应该有使用统计");

        // 清除统计
        roleService.clearUsageStats();
        stats = roleService.getUsageStats();
        assertFalse(stats.containsKey(roleId), "统计应该已清除");
    }

    @Test
    @DisplayName("测试角色列表为空时关键词匹配返回默认角色")
    void testMatchWithEmptyKeywordsReturnsDefault() {
        List<Role> matched = roleService.matchRolesByKeywords(Arrays.asList());
        assertFalse(matched.isEmpty(), "应该返回默认角色");
        assertEquals("default", matched.get(0).getId(), "应该返回默认角色");
    }

    @Test
    @DisplayName("测试注册空角色抛出异常")
    void testRegisterNullRoleThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> {
            roleService.registerRole(null);
        }, "注册null角色应该抛出异常");
    }

    @Test
    @DisplayName("测试注册ID为null的角色抛出异常")
    void testRegisterRoleWithNullIdThrowsException() {
        Role roleWithNullId = Role.builder()
                .id(null)
                .name("Test Role")
                .enabled(true)
                .build();

        assertThrows(IllegalArgumentException.class, () -> {
            roleService.registerRole(roleWithNullId);
        }, "注册ID为null的角色应该抛出异常");
    }
}


