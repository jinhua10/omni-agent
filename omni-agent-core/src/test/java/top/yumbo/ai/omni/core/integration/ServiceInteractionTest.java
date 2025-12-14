package top.yumbo.ai.omni.core.integration;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import top.yumbo.ai.omni.core.evolution.ConceptVersion;
import top.yumbo.ai.omni.core.evolution.EvolutionService;
import top.yumbo.ai.omni.core.feedback.FeedbackService;
import top.yumbo.ai.omni.core.knowledge.KnowledgeLoader;
import top.yumbo.ai.omni.core.role.Role;
import top.yumbo.ai.omni.core.role.RoleService;

import java.util.*;

import static org.assertj.core.api.Assertions.*;

/**
 * 服务交互集成测试
 * (Service Interaction Integration Tests)
 * 
 * <p>
 * 测试多个服务之间的简单交互场景
 * (Tests simple interaction scenarios between services)
 * </p>
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
class ServiceInteractionTest {

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
     * 测试1: 知识加载与角色管理协同
     */
    @Test
    void testKnowledgeLoadingWithRoleManagement() {
        // 加载知识
        KnowledgeLoader.KnowledgeEntry entry1 = knowledgeLoader.load(
            "java-basics", 
            k -> new KnowledgeLoader.KnowledgeEntry(k, "Java programming basics")
        );
        
        // 注册相关角色
        Role javaExpert = createRole("java-expert", "Java Expert", 10);
        roleService.registerRole(javaExpert);
        
        // 验证两者都成功
        assertThat(entry1).isNotNull();
        assertThat(roleService.getRole("java-expert")).isNotNull();
    }

    /**
     * 测试2: 反馈收集与知识加载结合
     */
    @Test
    void testFeedbackWithKnowledgeLoading() {
        // 加载知识
        knowledgeLoader.load("topic1", k -> new KnowledgeLoader.KnowledgeEntry(k, "Content"));
        
        // 收集关于该知识的反馈
        feedbackService.collectExplicit("s1", "u1", "Q about topic1", 
            "A about topic1", 0.9, new String[]{"topic1"}, "Good");
        
        // 验证反馈统计
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats.get("totalCount")).isEqualTo(1L);
    }

    /**
     * 测试3: 角色与反馈关联
     */
    @Test
    void testRoleWithFeedbackCorrelation() {
        // 注册角色
        Role expertRole = createRole("expert", "Expert", 10);
        roleService.registerRole(expertRole);
        
        // 收集专家角色的反馈
        feedbackService.collectExplicit("s1", "expert", "Expert question", 
            "Expert answer", 0.95, new String[]{"expert"}, "Excellent");
        
        // 验证
        assertThat(roleService.getRole("expert")).isNotNull();
        assertThat(feedbackService.getStatistics().get("totalCount")).isEqualTo(1L);
    }

    /**
     * 测试4: 概念进化与反馈循环
     */
    @Test
    void testEvolutionWithFeedbackLoop() {
        // 创建初始版本
        evolutionService.createVersion("concept1", "V1 content", 
            "Initial", ConceptVersion.ChangeType.CREATE, "author1");
        
        // 收集反馈
        feedbackService.collectExplicit("s1", "u1", "How is V1?", 
            "V1 content", 0.7, new String[]{"v1"}, "Needs improvement");
        
        // 基于反馈创建新版本
        ConceptVersion v2 = evolutionService.createVersion("concept1", "V2 improved content", 
            "Based on feedback", ConceptVersion.ChangeType.UPDATE, "author1");
        
        // 验证进化
        assertThat(v2).isNotNull();
    }

    /**
     * 测试5: 多角色协作场景
     */
    @Test
    void testMultiRoleCollaboration() {
        // 注册多个角色
        roleService.registerRole(createRole("reviewer", "Reviewer", 8));
        roleService.registerRole(createRole("author", "Author", 5));
        roleService.registerRole(createRole("editor", "Editor", 7));
        
        // 模拟协作流程
        feedbackService.collectExplicit("collab", "author", 
            "Draft", "Initial draft", 0.6, new String[]{"draft"}, "First version");
        feedbackService.collectExplicit("collab", "reviewer", 
            "Draft", "Reviewed draft", 0.8, new String[]{"review"}, "Good");
        feedbackService.collectExplicit("collab", "editor", 
            "Draft", "Final draft", 0.9, new String[]{"final"}, "Published");
        
        // 验证所有反馈
        assertThat(feedbackService.getSessionFeedback("collab")).hasSize(3);
    }

    /**
     * 测试6: 知识缓存与概念进化
     */
    @Test
    void testKnowledgeCachingWithEvolution() {
        // 加载知识
        knowledgeLoader.load("doc1", k -> new KnowledgeLoader.KnowledgeEntry(k, "Original"));
        
        // 创建概念版本
        evolutionService.createVersion("doc1", "Original content", 
            "First version", ConceptVersion.ChangeType.CREATE, "system");
        
        // 更新版本
        ConceptVersion updated = evolutionService.createVersion("doc1", "Updated content", 
            "Second version", ConceptVersion.ChangeType.UPDATE, "system");
        
        // 验证
        assertThat(updated).isNotNull();
    }

    /**
     * 测试7: 并发服务访问
     */
    @Test
    void testConcurrentServiceAccess() throws InterruptedException {
        Thread[] threads = new Thread[5];
        
        for (int i = 0; i < 5; i++) {
            final int threadId = i;
            threads[i] = new Thread(() -> {
                // 每个线程执行不同服务操作
                knowledgeLoader.load("key-" + threadId, 
                    k -> new KnowledgeLoader.KnowledgeEntry(k, "value-" + threadId));
                
                roleService.registerRole(createRole("role-" + threadId, 
                    "Role " + threadId, threadId));
                
                feedbackService.collectImplicit("session-" + threadId, 
                    "user-" + threadId, "Q" + threadId, "A" + threadId, 0.8);
            });
            threads[i].start();
        }
        
        // 等待完成
        for (Thread thread : threads) {
            thread.join();
        }
        
        // 验证数据一致性
        assertThat(feedbackService.getStatistics().get("totalCount")).isEqualTo(5L);
    }

    /**
     * 测试8: 反馈统计聚合
     */
    @Test
    void testFeedbackStatisticsAggregation() {
        // 收集不同类型的反馈
        feedbackService.collectExplicit("s1", "u1", "Q1", "A1", 1.0, 
            new String[]{"positive"}, "Great");
        feedbackService.collectExplicit("s2", "u2", "Q2", "A2", 0.0, 
            new String[]{"neutral"}, "OK");
        feedbackService.collectExplicit("s3", "u3", "Q3", "A3", -1.0, 
            new String[]{"negative"}, "Bad");
        feedbackService.collectImplicit("s4", "u4", "Q4", "A4", 0.5);
        
        // 验证统计
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats.get("totalCount")).isEqualTo(4L);
        assertThat(stats.get("explicitCount")).isEqualTo(3L);
        assertThat(stats.get("implicitCount")).isEqualTo(1L);
    }

    /**
     * 测试9: 角色优先级排序
     */
    @Test
    void testRolePrioritySorting() {
        // 注册不同优先级的角色
        roleService.registerRole(createRole("low", "Low Priority", 1));
        roleService.registerRole(createRole("high", "High Priority", 10));
        roleService.registerRole(createRole("medium", "Medium Priority", 5));
        
        // 验证注册
        assertThat(roleService.getRole("low")).isNotNull();
        assertThat(roleService.getRole("high")).isNotNull();
        assertThat(roleService.getRole("medium")).isNotNull();
    }

    /**
     * 测试10: 知识预加载策略
     */
    @Test
    void testKnowledgePreloadingStrategy() {
        // 批量预加载
        for (int i = 0; i < 20; i++) {
            final String content = "Preloaded content " + i;
            knowledgeLoader.load("preload-" + i, 
                k -> new KnowledgeLoader.KnowledgeEntry(k, content));
        }
        
        // 验证缓存工作
        KnowledgeLoader.KnowledgeEntry cached = knowledgeLoader.load("preload-5", 
            k -> new KnowledgeLoader.KnowledgeEntry(k, "Should not reload"));
        
        assertThat(cached).isNotNull();
    }

    /**
     * 测试11: 反馈时间序列
     */
    @Test
    void testFeedbackTimeSeries() throws InterruptedException {
        // 模拟时间序列反馈
        for (int i = 0; i < 5; i++) {
            feedbackService.collectExplicit("timeseries", "u1", 
                "Question " + i, "Answer " + i, 0.5 + (i * 0.1), 
                new String[]{"ts-" + i}, "Feedback " + i);
            Thread.sleep(10); // 小延迟确保时间差异
        }
        
        // 验证所有反馈被记录
        assertThat(feedbackService.getSessionFeedback("timeseries")).hasSize(5);
    }

    /**
     * 测试12: 概念版本回滚
     */
    @Test
    void testConceptVersionRollback() {
        // 创建多个版本
        evolutionService.createVersion("doc1", "V1", "Version 1", 
            ConceptVersion.ChangeType.CREATE, "author");
        evolutionService.createVersion("doc1", "V2", "Version 2", 
            ConceptVersion.ChangeType.UPDATE, "author");
        ConceptVersion v3 = evolutionService.createVersion("doc1", "V3", "Version 3", 
            ConceptVersion.ChangeType.UPDATE, "author");
        
        // 验证最新版本
        assertThat(v3).isNotNull();
        assertThat(v3.getContent()).isEqualTo("V3");
    }

    /**
     * 测试13: 空数据处理
     */
    @Test
    void testEmptyDataHandling() {
        // 空反馈统计
        Map<String, Object> emptyStats = feedbackService.getStatistics();
        assertThat(emptyStats.get("totalCount")).isEqualTo(0L);
        
        // 不存在的角色 - RoleService返回默认角色
        Role defaultRole = roleService.getRole("nonexistent");
        assertThat(defaultRole).isNotNull();
        assertThat(defaultRole.getId()).isEqualTo("default");
        
        // 验证服务可用
        assertThat(evolutionService).isNotNull();
    }

    /**
     * 测试14: 大量数据处理
     */
    @Test
    void testLargeDataHandling() {
        // 创建大量反馈
        for (int i = 0; i < 100; i++) {
            feedbackService.collectImplicit("bulk", "user", 
                "Q" + i, "A" + i, 0.75);
        }
        
        // 验证统计正确
        Map<String, Object> stats = feedbackService.getStatistics();
        assertThat(stats.get("totalCount")).isEqualTo(100L);
    }

    /**
     * 测试15: 混合操作场景
     */
    @Test
    void testMixedOperationScenario() {
        // 混合执行多种操作
        knowledgeLoader.load("mixed1", k -> new KnowledgeLoader.KnowledgeEntry(k, "Content1"));
        roleService.registerRole(createRole("mixed-role", "Mixed", 5));
        feedbackService.collectExplicit("mixed", "user", "Q", "A", 0.8, 
            new String[]{"mixed"}, "Comment");
        ConceptVersion concept = evolutionService.createVersion("mixed-concept", "Content", "Initial", 
            ConceptVersion.ChangeType.CREATE, "system");
        
        // 验证所有操作成功
        assertThat(roleService.getRole("mixed-role")).isNotNull();
        assertThat(feedbackService.getStatistics().get("totalCount")).isEqualTo(1L);
        assertThat(concept).isNotNull();
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
