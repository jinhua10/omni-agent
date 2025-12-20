package top.yumbo.ai.omni.core.resilience;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import top.yumbo.ai.omni.core.chunking.DocumentChunkingService;
import top.yumbo.ai.omni.core.chunking.ChunkingStrategyManager;
import top.yumbo.ai.omni.core.evolution.EvolutionService;
import top.yumbo.ai.omni.core.evolution.ConceptVersion;
import top.yumbo.ai.omni.core.feedback.FeedbackService;
import top.yumbo.ai.omni.core.knowledge.KnowledgeLoader;
import top.yumbo.ai.omni.core.role.RoleService;
import top.yumbo.ai.storage.api.DocumentStorageService;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * 弹性与恢复测试 - 测试文档切分服务的容错和错误恢复机制
 * (Resilience and Recovery Test - Test document chunking service fault tolerance and error recovery)
 *
 * <p>覆盖场景 (Coverage Scenarios):
 * - 存储层失败恢复 (Storage layer failure recovery)
 * - 并发异常处理 (Concurrent exception handling)
 * - 部分失败容忍 (Partial failure tolerance)
 * - 降级处理 (Graceful degradation)
 * - 资源清理 (Resource cleanup)
 * - 数据一致性 (Data consistency under failures)
 * - 大数据处理 (Large data handling)
 * - 空数据处理 (Null/empty data handling)
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class ResilienceAndRecoveryTest {

    @Mock
    private DocumentStorageService storageService;

    @Mock
    private ChunkingStrategyManager strategyManager;

    private DocumentChunkingService chunkingService;
    private EvolutionService evolutionService;
    private FeedbackService feedbackService;
    private KnowledgeLoader knowledgeLoader;
    private RoleService roleService;

    @BeforeEach
    void setUp() {
        chunkingService = new DocumentChunkingService(storageService, strategyManager);
        evolutionService = new EvolutionService();
        feedbackService = new FeedbackService();
        knowledgeLoader = new KnowledgeLoader();
        roleService = new RoleService();
    }

    /**
     * 测试存储服务失败时的错误处理
     * (Test error handling when storage service fails)
     */
    @Test
    void testStorageServiceFailureHandling() {
        // 模拟存储服务抛出异常
        when(storageService.saveChunks(anyString(), anyList()))
            .thenThrow(new RuntimeException("Storage service unavailable"));

        // 调用切分服务 - 应该捕获异常并返回空列表
        List<String> result = chunkingService.chunkAndStore("doc1", "Some content");

        // 验证返回空列表而不是抛出异常
        assertThat(result).isEmpty();
        verify(storageService).saveChunks(eq("doc1"), anyList());
    }

    /**
     * 测试部分存储失败的容错处理
     * (Test partial storage failure tolerance)
     */
    @Test
    void testPartialStorageFailureTolerance() {
        // 第一次调用失败，第二次成功
        when(storageService.saveChunks(anyString(), anyList()))
            .thenThrow(new RuntimeException("Temporary failure"))
            .thenReturn(Arrays.asList("chunk1", "chunk2"));

        // 第一次调用 - 失败
        List<String> result1 = chunkingService.chunkAndStore("doc1", "Content 1");
        assertThat(result1).isEmpty();

        // 第二次调用 - 成功
        List<String> result2 = chunkingService.chunkAndStore("doc2", "Content 2");
        assertThat(result2).hasSize(2);

        verify(storageService, times(2)).saveChunks(anyString(), anyList());
    }

    /**
     * 测试并发操作中的异常隔离
     * (Test exception isolation in concurrent operations)
     */
    @Test
    void testConcurrentExceptionIsolation() throws InterruptedException {
        // 设置：偶数次调用成功，奇数次调用失败
        AtomicInteger callCount = new AtomicInteger(0);
        when(storageService.saveChunks(anyString(), anyList())).thenAnswer(invocation -> {
            int count = callCount.incrementAndGet();
            if (count % 2 == 0) {
                return Collections.singletonList("chunk-" + count);
            } else {
                throw new RuntimeException("Simulated failure " + count);
            }
        });

        // 并发执行10个任务
        ExecutorService executor = Executors.newFixedThreadPool(5);
        List<Future<List<String>>> futures = new ArrayList<>();

        for (int i = 0; i < 10; i++) {
            final int index = i;
            futures.add(executor.submit(() -> 
                chunkingService.chunkAndStore("doc-" + index, "Content " + index)
            ));
        }

        // 收集结果
        int successCount = 0;
        int failureCount = 0;
        for (Future<List<String>> future : futures) {
            try {
                List<String> result = future.get(5, TimeUnit.SECONDS);
                if (result.isEmpty()) {
                    failureCount++;
                } else {
                    successCount++;
                }
            } catch (Exception e) {
                failureCount++;
            }
        }

        executor.shutdown();
        assertThat(executor.awaitTermination(5, TimeUnit.SECONDS)).isTrue();

        // 验证：应该有成功和失败的混合，失败不影响成功的操作
        assertThat(successCount).isGreaterThan(0);
        assertThat(failureCount).isGreaterThan(0);
        assertThat(successCount + failureCount).isEqualTo(10);
    }

    /**
     * 测试空数据和无效输入的容错
     * (Test null and invalid input tolerance)
     */
    @Test
    void testNullAndInvalidInputTolerance() {
        // 测试null内容
        List<String> result1 = chunkingService.chunkAndStore("doc1", null);
        assertThat(result1).isEmpty();

        // 测试空字符串
        List<String> result2 = chunkingService.chunkAndStore("doc2", "");
        assertThat(result2).isEmpty();

        // 测试只有空格
        List<String> result3 = chunkingService.chunkAndStore("doc3", "   ");
        assertThat(result3).isEmpty();

        // 验证没有调用存储服务
        verify(storageService, never()).saveChunks(anyString(), anyList());
    }

    /**
     * 测试角色服务的基本操作
     * (Test role service basic operations)
     */
    @Test
    void testRoleServiceBasicOperations() {
        // 手动初始化默认角色
        roleService.init();

        // 获取默认角色
        var role = roleService.getRole("default");

        // 验证返回了默认角色
        assertThat(role).isNotNull();
        assertThat(role.getId()).isEqualTo("default");
        assertThat(role.getName()).isEqualTo("Default Role");
    }

    /**
     * 测试反馈服务的正常操作
     * (Test feedback service normal operation)
     */
    @Test
    void testFeedbackServiceNormalOperation() {
        // 收集反馈 - 应该不抛出异常
        assertThatCode(() -> 
            feedbackService.collectExplicit("session1", "user1", "question", "answer", 
                1.0, new String[]{"helpful"}, "Good answer")
        ).doesNotThrowAnyException();
    }

    /**
     * 测试知识加载器的缓存机制
     * (Test knowledge loader caching mechanism)
     */
    @Test
    void testKnowledgeLoaderCachingMechanism() {
        // 第一次加载
        var entry1 = knowledgeLoader.load("concept1", 
            k -> new KnowledgeLoader.KnowledgeEntry(k, "Initial content"));
        assertThat(entry1.getContent()).isEqualTo("Initial content");

        // 第二次加载同一个概念 - 应该从缓存返回相同内容
        var entry2 = knowledgeLoader.load("concept1",
            k -> new KnowledgeLoader.KnowledgeEntry(k, "Should not be called"));
        assertThat(entry2.getContent()).isEqualTo("Initial content");
    }

    /**
     * 测试进化服务的多版本管理
     * (Test evolution service multi-version management)
     */
    @Test
    void testEvolutionServiceMultiVersionManagement() {
        // 创建多个版本
        var v1 = evolutionService.createVersion("concept1", "Content 1", 
            "Initial version", ConceptVersion.ChangeType.CREATE, "creator1");
        var v2 = evolutionService.createVersion("concept1", "Content 2",
            "Updated content", ConceptVersion.ChangeType.UPDATE, "creator1");
        var v3 = evolutionService.createVersion("concept1", "Content 3",
            "Further update", ConceptVersion.ChangeType.UPDATE, "creator1");

        // 验证所有版本都成功创建
        assertThat(v1).isNotNull();
        assertThat(v2).isNotNull();
        assertThat(v3).isNotNull();
        assertThat(v1.getConceptId()).isEqualTo("concept1");
        assertThat(v2.getConceptId()).isEqualTo("concept1");
        assertThat(v3.getConceptId()).isEqualTo("concept1");
    }

    /**
     * 测试大文档切分时的内存管理
     * (Test memory management when chunking large documents)
     */
    @Test
    void testLargeDocumentMemoryManagement() {
        // 创建一个大文档 (1MB)
        StringBuilder largeContent = new StringBuilder();
        for (int i = 0; i < 10000; i++) {
            largeContent.append("This is line ").append(i).append(" with some content. ");
        }

        when(storageService.saveChunks(anyString(), anyList()))
            .thenReturn(Arrays.asList("chunk1", "chunk2", "chunk3"));

        // 应该能够处理大文档而不抛出OOM
        assertThatCode(() -> 
            chunkingService.chunkAndStore("large-doc", largeContent.toString())
        ).doesNotThrowAnyException();

        verify(storageService).saveChunks(eq("large-doc"), anyList());
    }

    /**
     * 测试服务初始化
     * (Test service initialization)
     */
    @Test
    void testServiceInitialization() {
        // 使用mock存储服务创建 - 应该成功
        assertThatCode(() -> {
            DocumentChunkingService service = new DocumentChunkingService(storageService, strategyManager);
            assertThat(service).isNotNull();
        }).doesNotThrowAnyException();
    }

    /**
     * 测试多次连续失败后的恢复
     * (Test recovery after multiple consecutive failures)
     */
    @Test
    void testRecoveryAfterMultipleFailures() {
        // 前3次失败，第4次成功
        when(storageService.saveChunks(anyString(), anyList()))
            .thenThrow(new RuntimeException("Failure 1"))
            .thenThrow(new RuntimeException("Failure 2"))
            .thenThrow(new RuntimeException("Failure 3"))
            .thenReturn(Collections.singletonList("chunk1"));

        // 执行4次操作
        assertThat(chunkingService.chunkAndStore("doc1", "Content")).isEmpty();
        assertThat(chunkingService.chunkAndStore("doc2", "Content")).isEmpty();
        assertThat(chunkingService.chunkAndStore("doc3", "Content")).isEmpty();
        assertThat(chunkingService.chunkAndStore("doc4", "Content")).hasSize(1);

        verify(storageService, times(4)).saveChunks(anyString(), anyList());
    }

    /**
     * 测试资源清理 - 确保异常不影响资源释放
     * (Test resource cleanup - ensure exceptions don't prevent resource release)
     */
    @Test
    void testResourceCleanupOnException() {
        when(storageService.saveChunks(anyString(), anyList()))
            .thenThrow(new RuntimeException("Storage error"));

        // 执行多次失败操作
        for (int i = 0; i < 5; i++) {
            chunkingService.chunkAndStore("doc" + i, "Content " + i);
        }

        // 验证每次都尝试了存储（没有因为前面的失败而放弃）
        verify(storageService, times(5)).saveChunks(anyString(), anyList());
    }

    /**
     * 测试并发写入的数据一致性
     * (Test data consistency under concurrent writes)
     */
    @Test
    void testConcurrentWriteConsistency() throws InterruptedException {
        AtomicInteger successCount = new AtomicInteger(0);
        
        when(storageService.saveChunks(anyString(), anyList())).thenAnswer(invocation -> {
            successCount.incrementAndGet();
            return Collections.singletonList("chunk-" + successCount.get());
        });

        // 并发写入20个文档
        ExecutorService executor = Executors.newFixedThreadPool(10);
        CountDownLatch latch = new CountDownLatch(20);

        for (int i = 0; i < 20; i++) {
            final int index = i;
            executor.submit(() -> {
                try {
                    chunkingService.chunkAndStore("doc-" + index, "Content " + index);
                } finally {
                    latch.countDown();
                }
            });
        }

        // 等待所有任务完成
        assertThat(latch.await(10, TimeUnit.SECONDS)).isTrue();
        executor.shutdown();

        // 验证所有写入都被处理
        assertThat(successCount.get()).isEqualTo(20);
        verify(storageService, times(20)).saveChunks(anyString(), anyList());
    }

    /**
     * 测试级联失败的隔离
     * (Test cascading failure isolation)
     */
    @Test
    void testCascadingFailureIsolation() {
        // 存储失败不应影响其他服务
        when(storageService.saveChunks(anyString(), anyList()))
            .thenThrow(new RuntimeException("Storage failure"));

        // 切分服务失败
        assertThat(chunkingService.chunkAndStore("doc1", "Content")).isEmpty();

        // 但角色服务应该独立运行（初始化后返回默认角色）
        roleService.init();
        var role = roleService.getRole("default");
        assertThat(role).isNotNull();
        assertThat(role.getId()).isEqualTo("default");

        // 知识加载器应该正常工作
        var entry = knowledgeLoader.load("concept1",
            k -> new KnowledgeLoader.KnowledgeEntry(k, "Content"));
        assertThat(entry).isNotNull();

        // 进化服务应该正常工作
        var version = evolutionService.createVersion("concept1", "Content",
            "Initial version", ConceptVersion.ChangeType.CREATE, "creator1");
        assertThat(version).isNotNull();
    }

    /**
     * 测试并发知识加载
     * (Test concurrent knowledge loading)
     */
    @Test
    void testConcurrentKnowledgeLoading() throws InterruptedException {
        AtomicInteger loadCount = new AtomicInteger(0);

        ExecutorService executor = Executors.newFixedThreadPool(5);
        CountDownLatch latch = new CountDownLatch(10);

        for (int i = 0; i < 10; i++) {
            final int index = i;
            executor.submit(() -> {
                try {
                    knowledgeLoader.load("concept-" + index, k -> {
                        loadCount.incrementAndGet();
                        return new KnowledgeLoader.KnowledgeEntry(k, "Content " + index);
                    });
                } finally {
                    latch.countDown();
                }
            });
        }

        assertThat(latch.await(5, TimeUnit.SECONDS)).isTrue();
        executor.shutdown();

        // 每个概念应该只加载一次
        assertThat(loadCount.get()).isEqualTo(10);
    }

    /**
     * 测试进化服务的并发版本创建
     * (Test evolution service concurrent version creation)
     */
    @Test
    void testEvolutionServiceConcurrentVersionCreation() throws InterruptedException {
        ExecutorService executor = Executors.newFixedThreadPool(3);
        CountDownLatch latch = new CountDownLatch(3);
        List<ConceptVersion> versions = new ArrayList<>();

        for (int i = 1; i <= 3; i++) {
            final int versionNum = i;
            executor.submit(() -> {
                try {
                    ConceptVersion v = evolutionService.createVersion(
                        "concept" + versionNum, "Content " + versionNum,
                        "Create " + versionNum, 
                        ConceptVersion.ChangeType.CREATE, "creator" + versionNum
                    );
                    synchronized (versions) {
                        versions.add(v);
                    }
                } finally {
                    latch.countDown();
                }
            });
        }

        assertThat(latch.await(5, TimeUnit.SECONDS)).isTrue();
        executor.shutdown();

        // 所有版本都应该创建成功
        assertThat(versions).hasSize(3);
    }
}
