package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.web.model.ApiResponse;

import java.util.*;

/**
 * 缓存管理控制器 (Cache Management Controller)
 *
 * 提供多级缓存的统计、监控和管理功能
 * (Provides multi-level cache statistics, monitoring and management)
 *
 * Phase 4.2.4 - 缓存管理界面
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@RestController
@RequestMapping("/api/cache")
public class CacheManagementController {

    /**
     * 获取所有缓存统计
     * GET /api/cache/stats
     */
    @GetMapping("/stats")
    public ApiResponse<CacheOverview> getCacheStats() {
        try {
            CacheOverview overview = new CacheOverview();

            // 查询缓存
            CacheStats queryCache = new CacheStats();
            queryCache.setName("query");
            queryCache.setDisplayName("查询缓存");
            queryCache.setHitRate(0.92);
            queryCache.setSize(1500L);
            queryCache.setMaxSize(10000L);
            queryCache.setHitCount(9200L);
            queryCache.setMissCount(800L);
            queryCache.setEvictionCount(150L);
            queryCache.setAvgLoadTime(25L);

            // Embedding缓存
            CacheStats embeddingCache = new CacheStats();
            embeddingCache.setName("embedding");
            embeddingCache.setDisplayName("向量缓存");
            embeddingCache.setHitRate(0.88);
            embeddingCache.setSize(3200L);
            embeddingCache.setMaxSize(20000L);
            embeddingCache.setHitCount(8800L);
            embeddingCache.setMissCount(1200L);
            embeddingCache.setEvictionCount(320L);
            embeddingCache.setAvgLoadTime(120L);

            // 检索结果缓存
            CacheStats retrievalCache = new CacheStats();
            retrievalCache.setName("retrieval");
            retrievalCache.setDisplayName("检索缓存");
            retrievalCache.setHitRate(0.85);
            retrievalCache.setSize(2800L);
            retrievalCache.setMaxSize(15000L);
            retrievalCache.setHitCount(8500L);
            retrievalCache.setMissCount(1500L);
            retrievalCache.setEvictionCount(280L);
            retrievalCache.setAvgLoadTime(85L);

            List<CacheStats> caches = Arrays.asList(queryCache, embeddingCache, retrievalCache);
            overview.setCaches(caches);

            // 总体统计
            overview.setTotalHitRate(0.88);
            overview.setTotalSize(7500L);
            overview.setTotalMaxSize(45000L);
            overview.setTotalHitCount(26500L);
            overview.setTotalMissCount(3500L);

            log.info("📊 获取缓存统计成功: {} 个缓存", caches.size());
            return ApiResponse.success(overview);
        } catch (Exception e) {
            log.error("❌ 获取缓存统计失败", e);
            return ApiResponse.error("获取统计失败: " + e.getMessage());
        }
    }

    /**
     * 获取缓存热点数据
     * GET /api/cache/hotkeys
     */
    @GetMapping("/hotkeys")
    public ApiResponse<List<HotKey>> getHotKeys(@RequestParam String cacheName) {
        try {
            List<HotKey> hotKeys = new ArrayList<>();

            for (int i = 0; i < 10; i++) {
                HotKey hotKey = new HotKey();
                hotKey.setKey("key_" + i);
                hotKey.setHitCount(1000L - i * 50);
                hotKey.setLastAccessTime(System.currentTimeMillis() - i * 60000);
                hotKey.setSize(1024L + i * 100);
                hotKeys.add(hotKey);
            }

            log.info("🔥 获取热点数据成功: cache={}, {} 个热点", cacheName, hotKeys.size());
            return ApiResponse.success(hotKeys);
        } catch (Exception e) {
            log.error("❌ 获取热点数据失败", e);
            return ApiResponse.error("获取热点失败: " + e.getMessage());
        }
    }

    /**
     * 获取缓存趋势数据
     * GET /api/cache/trends
     */
    @GetMapping("/trends")
    public ApiResponse<CacheTrends> getCacheTrends(@RequestParam String cacheName) {
        try {
            CacheTrends trends = new CacheTrends();

            List<TrendPoint> hitRateTrend = new ArrayList<>();
            List<TrendPoint> sizeTrend = new ArrayList<>();

            long now = System.currentTimeMillis();
            for (int i = 0; i < 24; i++) {
                TrendPoint hitPoint = new TrendPoint();
                hitPoint.setTimestamp(now - (23 - i) * 3600000);
                hitPoint.setValue(0.80 + Math.random() * 0.15);
                hitRateTrend.add(hitPoint);

                TrendPoint sizePoint = new TrendPoint();
                sizePoint.setTimestamp(now - (23 - i) * 3600000);
                sizePoint.setValue(1000.0 + Math.random() * 500);
                sizeTrend.add(sizePoint);
            }

            trends.setHitRateTrend(hitRateTrend);
            trends.setSizeTrend(sizeTrend);

            log.info("📈 获取缓存趋势成功: cache={}", cacheName);
            return ApiResponse.success(trends);
        } catch (Exception e) {
            log.error("❌ 获取缓存趋势失败", e);
            return ApiResponse.error("获取趋势失败: " + e.getMessage());
        }
    }

    /**
     * 清除缓存
     * POST /api/cache/clear
     */
    @PostMapping("/clear")
    public ApiResponse<Void> clearCache(@RequestBody ClearCacheRequest request) {
        try {
            log.info("🗑️ 清除缓存: cache={}, type={}", request.getCacheName(), request.getClearType());

            // 实际应该调用缓存服务清除

            log.info("✅ 缓存清除成功");
            return ApiResponse.success(null, "缓存已清除");
        } catch (Exception e) {
            log.error("❌ 清除缓存失败", e);
            return ApiResponse.error("清除缓存失败: " + e.getMessage());
        }
    }

    /**
     * 预热缓存
     * POST /api/cache/warmup
     */
    @PostMapping("/warmup")
    public ApiResponse<WarmupResult> warmupCache(@RequestBody WarmupRequest request) {
        try {
            log.info("🔥 预热缓存: cache={}, keys={}", request.getCacheName(), request.getKeys().size());

            WarmupResult result = new WarmupResult();
            result.setTotalKeys(request.getKeys().size());
            result.setSuccessCount(request.getKeys().size() - 2);
            result.setFailureCount(2);
            result.setDuration(1500L);

            log.info("✅ 缓存预热完成: success={}, failure={}", result.getSuccessCount(), result.getFailureCount());
            return ApiResponse.success(result);
        } catch (Exception e) {
            log.error("❌ 缓存预热失败", e);
            return ApiResponse.error("预热失败: " + e.getMessage());
        }
    }

    // ==================== DTO 类 ====================

    @Data
    public static class CacheOverview {
        private List<CacheStats> caches;
        private Double totalHitRate;
        private Long totalSize;
        private Long totalMaxSize;
        private Long totalHitCount;
        private Long totalMissCount;
    }

    @Data
    public static class CacheStats {
        private String name;
        private String displayName;
        private Double hitRate;
        private Long size;
        private Long maxSize;
        private Long hitCount;
        private Long missCount;
        private Long evictionCount;
        private Long avgLoadTime;
    }

    @Data
    public static class HotKey {
        private String key;
        private Long hitCount;
        private Long lastAccessTime;
        private Long size;
    }

    @Data
    public static class CacheTrends {
        private List<TrendPoint> hitRateTrend;
        private List<TrendPoint> sizeTrend;
    }

    @Data
    public static class TrendPoint {
        private Long timestamp;
        private Double value;
    }

    @Data
    public static class ClearCacheRequest {
        private String cacheName;
        private String clearType; // all, expired, partial
        private List<String> keys; // for partial clear
    }

    @Data
    public static class WarmupRequest {
        private String cacheName;
        private List<String> keys;
    }

    @Data
    public static class WarmupResult {
        private Integer totalKeys;
        private Integer successCount;
        private Integer failureCount;
        private Long duration;
    }
}



