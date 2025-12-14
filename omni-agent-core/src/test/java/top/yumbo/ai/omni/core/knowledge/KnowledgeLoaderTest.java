package top.yumbo.ai.omni.core.knowledge;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import top.yumbo.ai.omni.core.knowledge.KnowledgeLoader.KnowledgeEntry;
import top.yumbo.ai.omni.core.knowledge.KnowledgeLoader.LoadStatistics;

import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.*;

/**
 * KnowledgeLoader 单元测试
 * (KnowledgeLoader Unit Test)
 *
 * @author OmniAgent Team
 * @since 1.0.1
 */
class KnowledgeLoaderTest {

    private KnowledgeLoader loader;
    private int loadCallCount;

    @BeforeEach
    void setUp() {
        loader = new KnowledgeLoader(10); // 小缓存用于测试
        loadCallCount = 0;
    }

    @Test
    void testCacheHit() {
        // 模拟加载函数
        Function<String, KnowledgeEntry> mockLoader = key -> {
            loadCallCount++;
            return new KnowledgeEntry(key, "Content for " + key);
        };

        // 第一次加载 - 缓存未命中
        KnowledgeEntry entry1 = loader.load("key1", mockLoader);
        assertNotNull(entry1);
        assertEquals("key1", entry1.getKey());
        assertEquals(1, loadCallCount);

        // 第二次加载相同键 - 缓存命中
        KnowledgeEntry entry2 = loader.load("key1", mockLoader);
        assertNotNull(entry2);
        assertEquals("key1", entry2.getKey());
        assertEquals(1, loadCallCount); // 加载次数不变

        // 验证统计
        LoadStatistics stats = loader.getStatistics();
        assertEquals(1, stats.getCacheHits().get());
        assertEquals(1, stats.getCacheMisses().get());
        assertEquals(1.0 / 2, stats.getHitRate(), 0.01);
    }

    @Test
    void testCacheMiss() {
        Function<String, KnowledgeEntry> mockLoader = key -> {
            loadCallCount++;
            return new KnowledgeEntry(key, "Content for " + key);
        };

        // 加载不同的键
        loader.load("key1", mockLoader);
        loader.load("key2", mockLoader);
        loader.load("key3", mockLoader);

        assertEquals(3, loadCallCount);
        assertEquals(3, loader.getStatistics().getCacheMisses().get());
        assertEquals(0, loader.getStatistics().getCacheHits().get());
    }

    @Test
    void testLRUEviction() {
        Function<String, KnowledgeEntry> mockLoader = key -> 
            new KnowledgeEntry(key, "Content for " + key);

        // 填满缓存（容量10）
        for (int i = 0; i < 10; i++) {
            loader.load("key" + i, mockLoader);
        }

        assertEquals(10, loader.getCacheSize());

        // 再加载一个，应该驱逐最老的
        loader.load("key10", mockLoader);
        assertEquals(10, loader.getCacheSize());

        // key0 应该被驱逐了，重新加载会缓存未命中
        LoadStatistics statsBefore = loader.getStatistics();
        int missesBefore = statsBefore.getCacheMisses().get();

        loader.load("key0", mockLoader);

        LoadStatistics statsAfter = loader.getStatistics();
        assertEquals(missesBefore + 1, statsAfter.getCacheMisses().get());
    }

    @Test
    void testBatchLoad() {
        Function<String, KnowledgeEntry> mockLoader = key -> 
            new KnowledgeEntry(key, "Content for " + key);

        Iterable<String> keys = Arrays.asList("key1", "key2", "key3");
        Map<String, KnowledgeEntry> results = loader.batchLoad(keys, mockLoader);

        assertEquals(3, results.size());
        assertTrue(results.containsKey("key1"));
        assertTrue(results.containsKey("key2"));
        assertTrue(results.containsKey("key3"));

        // 再次批量加载相同的键，应该从缓存获取
        loadCallCount = 0;
        loader.batchLoad(keys, key -> {
            loadCallCount++;
            return new KnowledgeEntry(key, "Content for " + key);
        });

        assertEquals(0, loadCallCount); // 全部命中缓存
    }

    @Test
    void testRefresh() {
        Function<String, KnowledgeEntry> mockLoader = key -> {
            loadCallCount++;
            return new KnowledgeEntry(key, "Content for " + key + " v" + loadCallCount);
        };

        // 第一次加载
        KnowledgeEntry entry1 = loader.load("key1", mockLoader);
        assertEquals("Content for key1 v1", entry1.getContent());

        // 刷新缓存
        loader.refresh("key1");

        // 再次加载，应该重新执行加载函数
        KnowledgeEntry entry2 = loader.load("key1", mockLoader);
        assertEquals("Content for key1 v2", entry2.getContent());
        assertEquals(2, loadCallCount);
    }

    @Test
    void testClearCache() {
        Function<String, KnowledgeEntry> mockLoader = key -> 
            new KnowledgeEntry(key, "Content for " + key);

        // 加载一些数据
        loader.load("key1", mockLoader);
        loader.load("key2", mockLoader);
        loader.load("key3", mockLoader);

        assertEquals(3, loader.getCacheSize());

        // 清空缓存
        loader.clearCache();
        assertEquals(0, loader.getCacheSize());
    }

    @Test
    void testStatistics() {
        Function<String, KnowledgeEntry> mockLoader = key -> 
            new KnowledgeEntry(key, "Content for " + key);

        // 执行一些操作
        loader.load("key1", mockLoader); // miss
        loader.load("key1", mockLoader); // hit
        loader.load("key2", mockLoader); // miss
        loader.load("key1", mockLoader); // hit

        LoadStatistics stats = loader.getStatistics();

        assertEquals(2, stats.getCacheHits().get());
        assertEquals(2, stats.getCacheMisses().get());
        assertEquals(2, stats.getLoads().get());
        assertEquals(0.5, stats.getHitRate(), 0.01);
        assertTrue(stats.getAverageLoadTimeMs() >= 0);
    }

    @Test
    void testKnowledgeEntry() {
        KnowledgeEntry entry = new KnowledgeEntry("test-key", "test-content");

        assertEquals("test-key", entry.getKey());
        assertEquals("test-content", entry.getContent());
        assertEquals(0, entry.getAccessCount());
        assertNotNull(entry.getCreatedAt());
        assertNotNull(entry.getLastAccessedAt());

        entry.recordAccess();
        assertEquals(1, entry.getAccessCount());

        entry.recordAccess();
        assertEquals(2, entry.getAccessCount());
    }

    @Test
    void testStatisticsReset() {
        Function<String, KnowledgeEntry> mockLoader = key -> 
            new KnowledgeEntry(key, "Content for " + key);

        loader.load("key1", mockLoader);
        loader.load("key1", mockLoader);

        LoadStatistics stats = loader.getStatistics();
        assertTrue(stats.getCacheHits().get() > 0);

        stats.reset();

        assertEquals(0, stats.getCacheHits().get());
        assertEquals(0, stats.getCacheMisses().get());
        assertEquals(0, stats.getLoads().get());
        assertEquals(0, stats.getPreloads().get());
        assertEquals(0.0, stats.getHitRate());
    }

    @Test
    void testHotspotPreloadStrategy() {
        KnowledgeLoader.HotspotPreloadStrategy strategy = 
            new KnowledgeLoader.HotspotPreloadStrategy();

        strategy.updateScore("key1", 100);
        strategy.updateScore("key2", 50);
        strategy.updateScore("key3", 200);
        strategy.updateScore("key4", 75);

        Iterable<String> keysToPreload = strategy.getKeysToPreload(2);
        
        // 应该返回得分最高的2个键
        var keys = new java.util.ArrayList<String>();
        keysToPreload.forEach(keys::add);

        assertEquals(2, keys.size());
        assertTrue(keys.contains("key3")); // 得分200
        assertTrue(keys.contains("key1")); // 得分100
    }
}
