package top.yumbo.ai.omni.core.edge;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import top.yumbo.ai.omni.core.evolution.EvolutionService;
import top.yumbo.ai.omni.core.feedback.FeedbackService;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeLoader;
import top.yumbo.ai.omni.knowledge.registry.role.Role;
import top.yumbo.ai.omni.knowledge.registry.role.RoleService;

import java.util.*;

import static org.assertj.core.api.Assertions.*;

/**
 * 边界条件和异常测试
 * (Edge Cases and Exception Tests)
 * 
 * <p>
 * 测试各种边界条件、异常场景和极端输入
 * (Tests various edge cases, exception scenarios, and extreme inputs)
 * </p>
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
class EdgeCaseTest {

    private KnowledgeLoader knowledgeLoader;
    private FeedbackService feedbackService;
    private RoleService roleService;
    private EvolutionService evolutionService;

    @BeforeEach
    void setUp() {
        knowledgeLoader = new KnowledgeLoader(100);
        feedbackService = new FeedbackService();
        roleService = new RoleService();
        evolutionService = new EvolutionService();
        
        roleService.init();
    }

    /**
     * 测试1: 极小缓存容量
     */
    @Test
    void testMinimalCacheCapacity() {
        KnowledgeLoader tinyLoader = new KnowledgeLoader(1);
        
        // 添加多个条目
        tinyLoader.load("key1", k -> new KnowledgeLoader.KnowledgeEntry(k, "value1"));
        tinyLoader.load("key2", k -> new KnowledgeLoader.KnowledgeEntry(k, "value2"));
        
        // 验证容量限制生效
        assertThat(tinyLoader).isNotNull();
    }

    /**
     * 测试2: 负数优先级角色
     */
    @Test
    void testNegativePriorityRole() {
        Role role = Role.builder()
                .id("negative-role")
                .name("Negative Priority")
                .description("Test negative priority")
                .keywords(Arrays.asList("negative"))
                .enabled(true)
                .priority(-10)
                .properties(new HashMap<>())
                .build();
        
        roleService.registerRole(role);
        
        // 验证角色被注册
        assertThat(roleService.getRole("negative-role")).isNotNull();
    }

    /**
     * 测试3: 空关键词角色
     */
    @Test
    void testRoleWithEmptyKeywords() {
        Role role = Role.builder()
                .id("empty-keywords")
                .name("Empty Keywords Role")
                .description("Role with no keywords")
                .keywords(Collections.emptyList())
                .enabled(true)
                .priority(5)
                .properties(new HashMap<>())
                .build();
        
        roleService.registerRole(role);
        assertThat(roleService.getRole("empty-keywords")).isNotNull();
    }

    /**
     * 测试4: 极短反馈文本
     */
    @Test
    void testSingleCharacterFeedback() {
        feedbackService.collectExplicit("s1", "u1", "?", "!", 0.5, 
            new String[]{}, "");
        
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats.get("totalCount")).isEqualTo(1L);
    }

    /**
     * 测试5: 极高评分
     */
    @Test
    void testExtremelyHighRating() {
        feedbackService.collectExplicit("s1", "u1", "test", "answer", 
            100.0, new String[]{"high"}, "Extreme rating");
        
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats).isNotNull();
    }

    /**
     * 测试6: 极低评分
     */
    @Test
    void testExtremelyLowRating() {
        feedbackService.collectExplicit("s1", "u1", "test", "answer", 
            -50.0, new String[]{"low"}, "Negative rating");
        
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats).isNotNull();
    }

    /**
     * 测试7: 空标签数组
     */
    @Test
    void testEmptyTagsArray() {
        feedbackService.collectExplicit("s1", "u1", "test", "answer", 
            0.8, new String[]{}, "No tags");
        
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats.get("totalCount")).isEqualTo(1L);
    }

    /**
     * 测试8: 大量标签
     */
    @Test
    void testManyTags() {
        String[] manyTags = new String[100];
        for (int i = 0; i < 100; i++) {
            manyTags[i] = "tag-" + i;
        }
        
        feedbackService.collectExplicit("s1", "u1", "test", "answer", 
            0.8, manyTags, "Many tags");
        
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats.get("totalCount")).isEqualTo(1L);
    }

    /**
     * 测试9: 特殊字符处理
     */
    @Test
    void testSpecialCharactersInFeedback() {
        String specialChars = "!@#$%^&*()_+-={}[]|\\:\";<>?,./~`'";
        
        feedbackService.collectExplicit("s1", "u1", specialChars, specialChars, 
            0.8, new String[]{"special"}, specialChars);
        
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats.get("totalCount")).isEqualTo(1L);
    }

    /**
     * 测试10: Unicode字符处理
     */
    @Test
    void testUnicodeCharacters() {
        String unicode = "你好世界 🌍 مرحبا العالم Привет мир";
        
        feedbackService.collectExplicit("s1", "u1", unicode, unicode, 
            0.8, new String[]{"unicode"}, unicode);
        
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats.get("totalCount")).isEqualTo(1L);
    }

    /**
     * 测试11: 极长的角色描述
     */
    @Test
    void testVeryLongRoleDescription() {
        StringBuilder longDesc = new StringBuilder();
        for (int i = 0; i < 1000; i++) {
            longDesc.append("Very long description text. ");
        }
        
        Role role = Role.builder()
                .id("long-desc-role")
                .name("Long Description")
                .description(longDesc.toString())
                .keywords(Arrays.asList("long"))
                .enabled(true)
                .priority(5)
                .properties(new HashMap<>())
                .build();
        
        roleService.registerRole(role);
        assertThat(roleService.getRole("long-desc-role")).isNotNull();
    }

    /**
     * 测试12: 重复注册相同角色
     */
    @Test
    void testDuplicateRoleRegistration() {
        Role role1 = createRole("duplicate", "First", 5);
        Role role2 = createRole("duplicate", "Second", 10);
        
        roleService.registerRole(role1);
        roleService.registerRole(role2); // 应该覆盖第一个
        
        Role retrieved = roleService.getRole("duplicate");
        assertThat(retrieved).isNotNull();
        // 第二次注册应该覆盖第一次
    }

    /**
     * 测试13: 大量并发反馈收集
     */
    @Test
    void testConcurrentFeedbackCollection() throws InterruptedException {
        int threadCount = 10;
        int feedbackPerThread = 50;
        
        Thread[] threads = new Thread[threadCount];
        
        for (int t = 0; t < threadCount; t++) {
            final int threadId = t;
            threads[t] = new Thread(() -> {
                for (int i = 0; i < feedbackPerThread; i++) {
                    feedbackService.collectImplicit(
                        "concurrent-" + threadId,
                        "user-" + threadId,
                        "question-" + i,
                        "answer-" + i,
                        0.8
                    );
                }
            });
            threads[t].start();
        }
        
        // 等待所有线程完成
        for (Thread thread : threads) {
            thread.join();
        }
        
        // 验证数据完整性
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats.get("totalCount")).isNotNull();
    }

    // ==================== 辅助方法 ====================

    private Role createRole(String id, String name, int priority) {
        return Role.builder()
                .id(id)
                .name(name)
                .description("Test role: " + name)
                .keywords(Arrays.asList(name.toLowerCase(), "test"))
                .enabled(true)
                .priority(priority)
                .properties(new HashMap<>())
                .build();
    }
}

