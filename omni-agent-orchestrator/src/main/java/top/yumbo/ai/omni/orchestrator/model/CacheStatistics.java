package top.yumbo.ai.omni.orchestrator.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 缓存统计信息
 * (Cache Statistics)
 *
 * <p>
 * 记录查询缓存的统计信息
 * (Records query cache statistics)
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CacheStatistics {

    /**
     * 查询缓存大小
     * (Query cache size)
     */
    private long queryCacheSize;

    /**
     * 查询缓存命中次数
     * (Query cache hit count)
     */
    private long queryCacheHits;

    /**
     * 查询缓存未命中次数
     * (Query cache miss count)
     */
    private long queryCacheMisses;

    /**
     * 扩展缓存大小
     * (Expansion cache size)
     */
    private long expansionCacheSize;

    /**
     * 扩展缓存命中次数
     * (Expansion cache hit count)
     */
    private long expansionCacheHits;

    /**
     * 扩展缓存未命中次数
     * (Expansion cache miss count)
     */
    private long expansionCacheMisses;

    /**
     * 总命中次数
     * (Total hit count)
     */
    private long totalHits;

    /**
     * 总未命中次数
     * (Total miss count)
     */
    private long totalMisses;

    /**
     * 整体命中率
     * (Overall hit rate)
     */
    private double overallHitRate;

    /**
     * 获取查询缓存命中率
     * (Get query cache hit rate)
     *
     * @return 查询缓存命中率 (Query cache hit rate)
     */
    public double getQueryCacheHitRate() {
        long total = queryCacheHits + queryCacheMisses;
        return total == 0 ? 0.0 : (double) queryCacheHits / total;
    }

    /**
     * 获取扩展缓存命中率
     * (Get expansion cache hit rate)
     *
     * @return 扩展缓存命中率 (Expansion cache hit rate)
     */
    public double getExpansionCacheHitRate() {
        long total = expansionCacheHits + expansionCacheMisses;
        return total == 0 ? 0.0 : (double) expansionCacheHits / total;
    }
}


