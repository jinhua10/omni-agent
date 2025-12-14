package top.yumbo.ai.omni.core.knowledge;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.time.Instant;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;

/**
 * 知识库加载器 - 带LRU缓存和预加载策略
 * (Knowledge Loader - With LRU cache and preload strategy)
 *
 * <p>
 * 性能优化特性:
 * - LRU缓存机制，减少重复加载
 * - 预加载策略，提前加载热点知识
 * - 加载统计，监控性能指标
 * - 线程安全，支持并发访问
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.1
 */
@Slf4j
@Component
public class KnowledgeLoader {

    /** 默认缓存容量 */
    private static final int DEFAULT_CACHE_SIZE = 1000;

    /** 默认预加载数量 */
    private static final int DEFAULT_PRELOAD_SIZE = 100;

    /** LRU缓存 */
    private final LRUCache<String, KnowledgeEntry> cache;

    /** 统计信息 */
    private final LoadStatistics statistics;

    /** 预加载策略 */
    private PreloadStrategy preloadStrategy;

    /**
     * 构造函数
     */
    public KnowledgeLoader() {
        this(DEFAULT_CACHE_SIZE);
    }

    /**
     * 构造函数
     *
     * @param cacheSize 缓存容量
     */
    public KnowledgeLoader(int cacheSize) {
        this.cache = new LRUCache<>(cacheSize);
        this.statistics = new LoadStatistics();
        this.preloadStrategy = new HotspotPreloadStrategy();
        log.info("KnowledgeLoader initialized with cache size: {}", cacheSize);
    }

    /**
     * 加载知识条目（带缓存）
     * (Load knowledge entry with cache)
     *
     * @param key 知识键
     * @param loader 加载函数
     * @return 知识条目
     */
    public KnowledgeEntry load(String key, Function<String, KnowledgeEntry> loader) {
        long startTime = System.nanoTime();

        try {
            // 1. 尝试从缓存获取
            KnowledgeEntry cached = cache.get(key);
            if (cached != null) {
                statistics.recordCacheHit();
                log.debug("Cache hit for key: {}", key);
                return cached;
            }

            // 2. 缓存未命中，执行加载
            statistics.recordCacheMiss();
            log.debug("Cache miss for key: {}, loading from source", key);

            KnowledgeEntry entry = loader.apply(key);
            if (entry != null) {
                // 3. 加载成功，放入缓存
                cache.put(key, entry);
                statistics.recordLoad();
                log.debug("Knowledge loaded and cached: {}", key);
            }

            return entry;

        } finally {
            long duration = System.nanoTime() - startTime;
            statistics.recordLoadTime(duration);
        }
    }

    /**
     * 批量加载知识条目
     * (Batch load knowledge entries)
     *
     * @param keys 知识键列表
     * @param loader 加载函数
     * @return 知识条目映射
     */
    public Map<String, KnowledgeEntry> batchLoad(
            Iterable<String> keys,
            Function<String, KnowledgeEntry> loader) {

        Map<String, KnowledgeEntry> results = new ConcurrentHashMap<>();

        for (String key : keys) {
            KnowledgeEntry entry = load(key, loader);
            if (entry != null) {
                results.put(key, entry);
            }
        }

        return results;
    }

    /**
     * 预加载热点知识
     * (Preload hot knowledge)
     *
     * @param loader 加载函数
     */
    public void preload(Function<String, KnowledgeEntry> loader) {
        log.info("Starting knowledge preload...");
        long startTime = System.currentTimeMillis();

        try {
            // 获取需要预加载的键列表
            Iterable<String> keysToPreload = preloadStrategy.getKeysToPreload(DEFAULT_PRELOAD_SIZE);

            int count = 0;
            for (String key : keysToPreload) {
                load(key, loader);
                count++;
            }

            long duration = System.currentTimeMillis() - startTime;
            log.info("Preload completed: {} entries in {}ms", count, duration);
            statistics.recordPreload(count);

        } catch (Exception e) {
            log.error("Preload failed", e);
        }
    }

    /**
     * 手动刷新缓存
     * (Manually refresh cache)
     *
     * @param key 知识键
     */
    public void refresh(String key) {
        cache.remove(key);
        log.debug("Cache refreshed for key: {}", key);
    }

    /**
     * 清空缓存
     * (Clear cache)
     */
    public void clearCache() {
        cache.clear();
        log.info("Cache cleared");
    }

    /**
     * 获取缓存大小
     * (Get cache size)
     *
     * @return 缓存大小
     */
    public int getCacheSize() {
        return cache.size();
    }

    /**
     * 获取统计信息
     * (Get statistics)
     *
     * @return 统计信息
     */
    public LoadStatistics getStatistics() {
        return statistics;
    }

    /**
     * 设置预加载策略
     * (Set preload strategy)
     *
     * @param strategy 预加载策略
     */
    public void setPreloadStrategy(PreloadStrategy strategy) {
        this.preloadStrategy = strategy;
        log.info("Preload strategy updated: {}", strategy.getClass().getSimpleName());
    }

    /**
     * LRU缓存实现
     * (LRU Cache Implementation)
     *
     * @param <K> 键类型
     * @param <V> 值类型
     */
    private static class LRUCache<K, V> {
        private final int capacity;
        private final Map<K, V> cache;

        public LRUCache(int capacity) {
            this.capacity = capacity;
            this.cache = new LinkedHashMap<K, V>(capacity, 0.75f, true) {
                @Override
                protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
                    return size() > LRUCache.this.capacity;
                }
            };
        }

        public synchronized V get(K key) {
            return cache.get(key);
        }

        public synchronized void put(K key, V value) {
            cache.put(key, value);
        }

        public synchronized void remove(K key) {
            cache.remove(key);
        }

        public synchronized void clear() {
            cache.clear();
        }

        public synchronized int size() {
            return cache.size();
        }
    }

    /**
     * 知识条目
     * (Knowledge Entry)
     */
    @Data
    public static class KnowledgeEntry {
        /** 键 */
        private String key;

        /** 内容 */
        private String content;

        /** 元数据 */
        private Map<String, Object> metadata;

        /** 创建时间 */
        private Instant createdAt;

        /** 最后访问时间 */
        private Instant lastAccessedAt;

        /** 访问次数 */
        private int accessCount;

        public KnowledgeEntry(String key, String content) {
            this.key = key;
            this.content = content;
            this.createdAt = Instant.now();
            this.lastAccessedAt = Instant.now();
            this.accessCount = 0;
            this.metadata = new ConcurrentHashMap<>();
        }

        /**
         * 记录访问
         */
        public void recordAccess() {
            this.lastAccessedAt = Instant.now();
            this.accessCount++;
        }
    }

    /**
     * 加载统计
     * (Load Statistics)
     */
    @Data
    public static class LoadStatistics {
        /** 缓存命中次数 */
        private final AtomicInteger cacheHits = new AtomicInteger(0);

        /** 缓存未命中次数 */
        private final AtomicInteger cacheMisses = new AtomicInteger(0);

        /** 加载次数 */
        private final AtomicInteger loads = new AtomicInteger(0);

        /** 预加载次数 */
        private final AtomicInteger preloads = new AtomicInteger(0);

        /** 总加载时间（纳秒） */
        private final AtomicLong totalLoadTime = new AtomicLong(0);

        /**
         * 记录缓存命中
         */
        public void recordCacheHit() {
            cacheHits.incrementAndGet();
        }

        /**
         * 记录缓存未命中
         */
        public void recordCacheMiss() {
            cacheMisses.incrementAndGet();
        }

        /**
         * 记录加载
         */
        public void recordLoad() {
            loads.incrementAndGet();
        }

        /**
         * 记录预加载
         */
        public void recordPreload(int count) {
            preloads.addAndGet(count);
        }

        /**
         * 记录加载时间
         */
        public void recordLoadTime(long nanos) {
            totalLoadTime.addAndGet(nanos);
        }

        /**
         * 获取缓存命中率
         * (Get cache hit rate)
         *
         * @return 命中率 (0.0 - 1.0)
         */
        public double getHitRate() {
            int total = cacheHits.get() + cacheMisses.get();
            return total == 0 ? 0.0 : (double) cacheHits.get() / total;
        }

        /**
         * 获取平均加载时间（毫秒）
         * (Get average load time in milliseconds)
         *
         * @return 平均加载时间
         */
        public double getAverageLoadTimeMs() {
            int loadCount = loads.get();
            return loadCount == 0 ? 0.0 : totalLoadTime.get() / 1_000_000.0 / loadCount;
        }

        /**
         * 重置统计
         */
        public void reset() {
            cacheHits.set(0);
            cacheMisses.set(0);
            loads.set(0);
            preloads.set(0);
            totalLoadTime.set(0);
        }

        @Override
        public String toString() {
            return String.format(
                "LoadStatistics{hits=%d, misses=%d, loads=%d, preloads=%d, hitRate=%.2f%%, avgLoadTime=%.2fms}",
                cacheHits.get(),
                cacheMisses.get(),
                loads.get(),
                preloads.get(),
                getHitRate() * 100,
                getAverageLoadTimeMs()
            );
        }
    }

    /**
     * 预加载策略接口
     * (Preload Strategy Interface)
     */
    public interface PreloadStrategy {
        /**
         * 获取需要预加载的键列表
         *
         * @param maxCount 最大数量
         * @return 键列表
         */
        Iterable<String> getKeysToPreload(int maxCount);
    }

    /**
     * 热点预加载策略
     * (Hotspot Preload Strategy)
     */
    public static class HotspotPreloadStrategy implements PreloadStrategy {
        private final Map<String, Integer> hotspotScores = new ConcurrentHashMap<>();

        @Override
        public Iterable<String> getKeysToPreload(int maxCount) {
            // 返回得分最高的键
            return hotspotScores.entrySet().stream()
                .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
                .limit(maxCount)
                .map(Map.Entry::getKey)
                .toList();
        }

        /**
         * 更新热点得分
         *
         * @param key 键
         * @param score 得分
         */
        public void updateScore(String key, int score) {
            hotspotScores.put(key, score);
        }
    }
}
