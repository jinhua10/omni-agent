package top.yumbo.ai.omni.integration;

import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import top.yumbo.ai.omni.core.evolution.ConceptVersion;
import top.yumbo.ai.omni.core.evolution.EvolutionService;
import top.yumbo.ai.omni.core.feedback.Feedback;
import top.yumbo.ai.omni.core.feedback.FeedbackService;
import top.yumbo.ai.omni.core.knowledge.KnowledgeLoader;
import top.yumbo.ai.omni.core.role.Role;
import top.yumbo.ai.omni.core.role.RoleService;

import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 核心模块集成测试
 * (Core Modules Integration Test)
 * <p>
 * 测试核心模块之间的协作和集成
 * (Tests collaboration and integration between core modules)
 * <p>
 * 测试场景 (Test Scenarios):
 * 1. 知识查询 + 反馈 + 角色管理
 * 2. 知识演化 + 缓存更新
 * 3. 批量处理 + 多角色协作
 * 4. 概念合并 + 反馈驱动
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@Slf4j
public class CoreModulesIntegrationTest {

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

        // Initialize RoleService default role
        roleService.init();

        // Populate test data
        knowledgeLoader.load("test-key", key ->
                new KnowledgeLoader.KnowledgeEntry(key, "Test Content"));
    }

    /**
     * 测试1: 知识查询 + 反馈收集 + 角色管理
     * (Test 1: Knowledge query + Feedback collection + Role management)
     * <p>
     * 场景: 用户查询知识 → 系统响应 → 收集反馈 → 记录角色使用
     */
    @Test
    void testKnowledgeQueryWithFeedback() {
        log.info("=== Test 1: Knowledge Query with Feedback ===");

        // 1. Register a custom role
        Role customRole = Role.builder()
                .id("expert")
                .name("Expert Role")
                .description("Expert in specific domain")
                .keywords(Arrays.asList("technical", "expert", "advanced"))
                .enabled(true)
                .priority(10)
                .properties(new HashMap<>())
                .build();
        roleService.registerRole(customRole);

        // 2. Load knowledge entry
        String questionKey = "java-concurrency";
        KnowledgeLoader.KnowledgeEntry entry = knowledgeLoader.load(
                questionKey,
                key -> new KnowledgeLoader.KnowledgeEntry(key, "Java concurrency best practices")
        );

        assertNotNull(entry, "Knowledge entry should be loaded");
        assertEquals("Java concurrency best practices", entry.getContent());

        // 3. Simulate user feedback
        String sessionId = "session-001";
        String userId = "user-001";
        Feedback feedback = feedbackService.collectExplicit(
                sessionId,
                userId,
                "How to use Java concurrency?",
                entry.getContent(),
                1.0, // Positive feedback
                new String[]{"helpful", "clear"},
                "Very helpful explanation!"
        );

        assertNotNull(feedback, "Feedback should be collected");
        assertEquals(1.0, feedback.getValue());

        // 4. Record role usage
        roleService.recordUsage("expert");

        // 5. Verify statistics
        List<Feedback> sessionFeedback = feedbackService.getSessionFeedback(sessionId);
        assertEquals(1, sessionFeedback.size(), "Should have 1 feedback for session");

        Map<String, Integer> roleStats = roleService.getUsageStats();
        assertTrue(roleStats.get("expert") > 0, "Expert role should have usage count");

        // 6. Verify cache hit
        KnowledgeLoader.KnowledgeEntry cached = knowledgeLoader.load(
                questionKey,
                key -> null // Should not be called
        );
        assertNotNull(cached, "Should get from cache");

        log.info("✓ Test 1 completed: Knowledge query with feedback integration working");
    }

    /**
     * 测试2: 知识演化 + 缓存更新
     * (Test 2: Knowledge evolution + Cache update)
     * <p>
     * 场景: 概念创建 → 版本更新 → 缓存刷新
     */
    @Test
    void testKnowledgeEvolutionWithCache() {
        log.info("=== Test 2: Knowledge Evolution with Cache ===");

        String conceptId = "algorithm-sorting";
        String key = "sorting-v1";

        // 1. Load initial knowledge
        KnowledgeLoader.KnowledgeEntry v1 = knowledgeLoader.load(
                key,
                k -> new KnowledgeLoader.KnowledgeEntry(k, "Bubble sort is O(n^2)")
        );
        assertEquals("Bubble sort is O(n^2)", v1.getContent());

        // 2. Create initial concept version
        ConceptVersion version1 = evolutionService.createVersion(
                conceptId,
                v1.getContent(),
                "Initial version of sorting algorithms",
                ConceptVersion.ChangeType.CREATE,
                "system"
        );

        assertEquals(1, version1.getVersion());
        assertTrue(version1.isCurrent());

        // 3. Update concept (simulate learning)
        ConceptVersion version2 = evolutionService.createVersion(
                conceptId,
                "Merge sort is O(n log n) - better than bubble sort",
                "Updated with more efficient algorithm",
                ConceptVersion.ChangeType.UPDATE,
                "user-001"
        );

        assertEquals(2, version2.getVersion());
        assertTrue(version2.isCurrent());
        assertFalse(version1.isCurrent(), "Previous version should not be current");

        // 4. Refresh cache with new content
        knowledgeLoader.refresh(key);
        KnowledgeLoader.KnowledgeEntry v2 = knowledgeLoader.load(
                key,
                k -> new KnowledgeLoader.KnowledgeEntry(k, version2.getContent())
        );

        assertEquals(version2.getContent(), v2.getContent());

        // 5. Verify version history
        List<ConceptVersion> history = evolutionService.getVersionHistory(conceptId);
        assertEquals(2, history.size(), "Should have 2 versions");

        log.info("✓ Test 2 completed: Knowledge evolution with cache working");
    }

    /**
     * 测试3: 批量处理 + 多角色反馈聚合
     * (Test 3: Batch processing + Multi-role feedback aggregation)
     */
    @Test
    void testBatchProcessingWithRoles() {
        log.info("=== Test 3: Batch Processing with Roles ===");

        // 1. Register multiple roles
        Role beginner = Role.builder()
                .id("beginner")
                .name("Beginner")
                .description("Beginner level")
                .keywords(Arrays.asList("basic", "simple"))
                .enabled(true)
                .priority(1)
                .properties(new HashMap<>())
                .build();

        Role intermediate = Role.builder()
                .id("intermediate")
                .name("Intermediate")
                .description("Intermediate level")
                .keywords(Arrays.asList("moderate", "intermediate"))
                .enabled(true)
                .priority(5)
                .properties(new HashMap<>())
                .build();

        Role advanced = Role.builder()
                .id("advanced")
                .name("Advanced")
                .description("Advanced level")
                .keywords(Arrays.asList("advanced", "complex"))
                .enabled(true)
                .priority(10)
                .properties(new HashMap<>())
                .build();

        roleService.registerRole(beginner);
        roleService.registerRole(intermediate);
        roleService.registerRole(advanced);

        // 2. Batch load knowledge
        List<String> keys = Arrays.asList("concept-1", "concept-2", "concept-3");
        Map<String, KnowledgeLoader.KnowledgeEntry> entries = knowledgeLoader.batchLoad(
                keys,
                key -> new KnowledgeLoader.KnowledgeEntry(key, "Content for " + key)
        );

        assertEquals(3, entries.size(), "Should load 3 entries");

        // 3. Collect feedback from different roles
        String sessionId = "batch-session";
        for (String key : keys) {
            feedbackService.collectExplicit(
                    sessionId,
                    "user-beginner",
                    "Question about " + key,
                    entries.get(key).getContent(),
                    0.5,
                    new String[]{"basic"},
                    "Okay"
            );
        }

        // 4. Verify aggregated stats
        List<Feedback> sessionFeedback = feedbackService.getSessionFeedback(sessionId);
        assertEquals(3, sessionFeedback.size());

        List<Role> allRoles = roleService.getAllRoles();
        assertTrue(allRoles.size() >= 4, "Should have at least 4 roles (including default)");

        // 5. Cache stats
        KnowledgeLoader.LoadStatistics stats = knowledgeLoader.getStatistics();
        assertTrue(stats.getHitRate() >= 0.0 && stats.getHitRate() <= 1.0);

        log.info("✓ Test 3 completed: Batch processing with multi-role working");
    }

    /**
     * 测试4: 概念合并 + 反馈驱动演化
     * (Test 4: Concept merge + Feedback-driven evolution)
     */
    @Test
    void testConceptMergeWithFeedback() {
        log.info("=== Test 4: Concept Merge with Feedback ===");

        // 1. Create two related concepts
        String conceptA = "concept-a";
        String conceptB = "concept-b";

        ConceptVersion versionA = evolutionService.createVersion(
                conceptA,
                "Concept A: Database normalization",
                "Initial concept A",
                ConceptVersion.ChangeType.CREATE,
                "user-001"
        );

        ConceptVersion versionB = evolutionService.createVersion(
                conceptB,
                "Concept B: Database denormalization",
                "Initial concept B",
                ConceptVersion.ChangeType.CREATE,
                "user-002"
        );

        // 2. Collect feedback suggesting merge
        feedbackService.collectExplicit(
                "merge-session",
                "user-003",
                "What's the difference between A and B?",
                "They are related",
                -0.5, // Negative feedback suggests confusion
                new String[]{"confusing", "duplicate"},
                "These concepts are too similar and confusing"
        );

        // 3. Merge concepts based on feedback
        String mergedConceptId = "concept-ab-merged";
        ConceptVersion merged = evolutionService.createVersion(
                mergedConceptId,
                "Normalization vs Denormalization: Both are database design strategies",
                "Merged concepts A and B based on user feedback",
                ConceptVersion.ChangeType.MERGE,
                "system"
        );

        // 4. Update cache with merged content
        knowledgeLoader.load(
                mergedConceptId,
                key -> new KnowledgeLoader.KnowledgeEntry(key, merged.getContent())
        );

        // 5. Verify merge
        assertEquals(ConceptVersion.ChangeType.MERGE, merged.getChangeType());
        assertEquals(1, merged.getVersion());

        // 6. Get feedback stats
        Map<String, Object> feedbackStats = feedbackService.getStatistics();
        long totalFeedback = (long) feedbackStats.get("totalCount");
        assertTrue(totalFeedback > 0);

        log.info("✓ Test 4 completed: Concept merge with feedback working");
    }

    /**
     * 测试5: 高频访问 + 缓存优化 + 反馈趋势
     * (Test 5: High-frequency access + Cache optimization + Feedback trends)
     */
    @Test
    void testHotKnowledgeWithFeedbackTrends() {
        log.info("=== Test 5: Hot Knowledge with Feedback Trends ===");

        String hotKey = "hot-topic";

        // 1. Simulate high-frequency access
        for (int i = 0; i < 20; i++) {
            knowledgeLoader.load(
                    hotKey,
                    key -> new KnowledgeLoader.KnowledgeEntry(key, "Popular topic content")
            );
        }

        // 2. Collect multiple feedback entries
        for (int i = 0; i < 10; i++) {
            feedbackService.collectExplicit(
                    "hot-session-" + i,
                    "user-" + i,
                    "Question about hot topic",
                    "Popular topic content",
                    i % 2 == 0 ? 1.0 : -1.0, // Mixed feedback
                    new String[]{"hot"},
                    "Feedback " + i
            );
        }

        // 3. Check cache hit rate
        KnowledgeLoader.LoadStatistics stats = knowledgeLoader.getStatistics();
        double hitRate = stats.getHitRate();
        assertTrue(hitRate > 0.9, "Hit rate should be > 90% for hot knowledge: " + hitRate);

        // 4. Analyze feedback trends
        Map<String, Object> feedbackStats = feedbackService.getStatistics();
        long positiveCount = (long) feedbackStats.get("positiveCount");
        long negativeCount = (long) feedbackStats.get("negativeCount");

        assertTrue(positiveCount + negativeCount >= 10, "Should have collected feedback");

        log.info("✓ Test 5 completed: Hot knowledge with high cache hit rate");
    }

    /**
     * 测试6: 多角色协作 + 版本演进
     * (Test 6: Multi-role collaboration + Version evolution)
     */
    @Test
    void testMultiRoleCollaboration() {
        log.info("=== Test 6: Multi-Role Collaboration ===");

        // 1. Register roles: Author, Editor, Approver
        Role author = Role.builder()
                .id("author")
                .name("Author")
                .description("Content author")
                .keywords(Arrays.asList("create", "write"))
                .enabled(true)
                .priority(5)
                .properties(new HashMap<>())
                .build();

        Role editor = Role.builder()
                .id("editor")
                .name("Editor")
                .description("Content editor")
                .keywords(Arrays.asList("edit", "improve"))
                .enabled(true)
                .priority(7)
                .properties(new HashMap<>())
                .build();

        Role approver = Role.builder()
                .id("approver")
                .name("Approver")
                .description("Content approver")
                .keywords(Arrays.asList("approve", "review"))
                .enabled(true)
                .priority(10)
                .properties(new HashMap<>())
                .build();

        roleService.registerRole(author);
        roleService.registerRole(editor);
        roleService.registerRole(approver);

        // 2. Workflow: Author creates
        String conceptId = "collaborative-doc";
        ConceptVersion v1 = evolutionService.createVersion(
                conceptId,
                "Initial draft",
                "Author created initial version",
                ConceptVersion.ChangeType.CREATE,
                "author-001"
        );
        roleService.recordUsage("author");

        // 3. Editor improves
        ConceptVersion v2 = evolutionService.createVersion(
                conceptId,
                "Improved draft with better structure",
                "Editor improved content",
                ConceptVersion.ChangeType.UPDATE,
                "editor-001"
        );
        roleService.recordUsage("editor");

        // 4. Approver reviews and provides feedback
        feedbackService.collectExplicit(
                "collab-session",
                "approver-001",
                "Review of collaborative doc",
                v2.getContent(),
                1.0,
                new String[]{"approved", "quality"},
                "Approved for publication"
        );
        roleService.recordUsage("approver");

        // 5. Final version
        ConceptVersion v3 = evolutionService.createVersion(
                conceptId,
                "Final approved version ready for publication",
                "Approved by reviewer",
                ConceptVersion.ChangeType.UPDATE,
                "approver-001"
        );

        // 6. Verify collaboration
        List<ConceptVersion> history = evolutionService.getVersionHistory(conceptId);
        assertEquals(3, history.size(), "Should have 3 versions from collaboration");

        Map<String, Integer> roleStats = roleService.getUsageStats();
        assertTrue(roleStats.get("author") > 0);
        assertTrue(roleStats.get("editor") > 0);
        assertTrue(roleStats.get("approver") > 0);

        log.info("✓ Test 6 completed: Multi-role collaboration workflow working");
    }

    /**
     * 测试7: 并发访问 + 实时反馈 + 缓存一致性
     * (Test 7: Concurrent access + Real-time feedback + Cache consistency)
     */
    @Test
    void testConcurrentAccessWithRealTimeFeedback() throws InterruptedException {
        log.info("=== Test 7: Concurrent Access with Real-time Feedback ===");

        String sharedKey = "concurrent-topic";
        int threadCount = 10;
        CountDownLatch latch = new CountDownLatch(threadCount);
        AtomicInteger successCount = new AtomicInteger(0);
        AtomicInteger feedbackCount = new AtomicInteger(0);

        // Concurrent threads accessing same knowledge
        for (int i = 0; i < threadCount; i++) {
            final int threadId = i;
            new Thread(() -> {
                try {
                    // 1. Load knowledge
                    KnowledgeLoader.KnowledgeEntry entry = knowledgeLoader.load(
                            sharedKey,
                            key -> new KnowledgeLoader.KnowledgeEntry(key, "Shared content")
                    );

                    if (entry != null) {
                        successCount.incrementAndGet();

                        // 2. Provide feedback
                        feedbackService.collectExplicit(
                                "concurrent-session-" + threadId,
                                "user-" + threadId,
                                "Concurrent question",
                                entry.getContent(),
                                threadId % 2 == 0 ? 1.0 : 0.0,
                                new String[]{"concurrent"},
                                "Feedback from thread " + threadId
                        );
                        feedbackCount.incrementAndGet();
                    }
                } catch (Exception e) {
                    log.error("Thread {} failed", threadId, e);
                } finally {
                    latch.countDown();
                }
            }).start();
        }

        // Wait for all threads
        latch.await();

        // Verify results
        assertEquals(threadCount, successCount.get(), "All threads should succeed");
        assertEquals(threadCount, feedbackCount.get(), "All threads should provide feedback");

        // Check cache consistency
        KnowledgeLoader.LoadStatistics stats = knowledgeLoader.getStatistics();
        assertTrue(stats.getCacheHits().get() + stats.getCacheMisses().get() >= threadCount);

        log.info("✓ Test 7 completed: Concurrent access working correctly");
    }

    /**
     * 测试8: 综合统计 + 所有模块指标
     * (Test 8: Comprehensive statistics + All module metrics)
     */
    @Test
    void testComprehensiveStatistics() {
        log.info("=== Test 8: Comprehensive Statistics ===");

        // 1. Generate diverse activity
        Role testRole = Role.builder()
                .id("stats-role")
                .name("Statistics Role")
                .description("For stats test")
                .keywords(Arrays.asList("stats"))
                .enabled(true)
                .priority(5)
                .properties(new HashMap<>())
                .build();
        roleService.registerRole(testRole);

        // Load some knowledge
        for (int i = 0; i < 5; i++) {
            final int index = i;  // Make effectively final for lambda
            knowledgeLoader.load(
                    "stats-key-" + index,
                    key -> new KnowledgeLoader.KnowledgeEntry(key, "Stats content " + index)
            );
        }

        // Create versions
        ConceptVersion statsVersion = evolutionService.createVersion(
                "stats-concept",
                "Stats concept content",
                "For statistics test",
                ConceptVersion.ChangeType.CREATE,
                "system"
        );

        // Collect feedback
        feedbackService.collectExplicit(
                "stats-session",
                "stats-user",
                "Stats question",
                "Stats answer",
                0.8,
                new String[]{"stats"},
                "Good"
        );

        roleService.recordUsage("stats-role");

        // 2. Collect all statistics
        KnowledgeLoader.LoadStatistics knowledgeStats = knowledgeLoader.getStatistics();
        Map<String, Object> feedbackStats = feedbackService.getStatistics();
        Map<String, Integer> roleStats = roleService.getUsageStats();
        List<Role> allRoles = roleService.getAllRoles();

        // 3. Verify comprehensive metrics
        assertNotNull(knowledgeStats);
        assertTrue(knowledgeStats.getCacheHits().get() + knowledgeStats.getCacheMisses().get() > 0);

        assertNotNull(feedbackStats);
        assertTrue((long) feedbackStats.get("totalCount") > 0);

        assertNotNull(roleStats);
        assertTrue(roleStats.size() > 0);

        assertTrue(allRoles.size() > 0);

        // 4. Log comprehensive report
        log.info("=== Comprehensive Statistics Report ===");
        log.info("Knowledge Stats: {}", knowledgeStats);
        log.info("Feedback Stats: {}", feedbackStats);
        log.info("Role Stats: {}", roleStats);
        log.info("Total Roles: {}", allRoles.size());
        log.info("=====================================");

        log.info("✓ Test 8 completed: All module statistics working");
    }
}
