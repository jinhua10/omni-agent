package top.yumbo.ai.omni.core.query.cache;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.core.query.model.CacheStatistics;
import top.yumbo.ai.rag.api.model.SearchResult;

import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * 查询扩展缓存服务
 * (Query Expansion Cache Service)
 *
 * <p>
 * 使用 Caffeine 实现高性能缓存
 * (High-performance caching using Caffeine)
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
@ConfigurationProperties(prefix = "omni-agent.query-expansion.cache")
@ConditionalOnProperty(
    prefix = "omni-agent.query-expansion.cache",
    name = "enabled",
    havingValue = "true",
    matchIfMissing = true
)
public class QueryExpansionCacheService {

    /**
     * 缓存最大条目数
     */
    private int maxSize = 1000;

    /**
     * 缓存过期时间（分钟）
     */
    private int expireMinutes = 60;

    /**
     * 查询扩展缓存
     * (Query expansion cache)
     */
    private Cache<String, List<String>> expansionCache;

    /**
     * 查询结果缓存
     * (Query result cache)
     */
    private Cache<String, List<SearchResult>> resultCache;

    /**
     * 统计信息
     * (Statistics)
     */
    private long expansionCacheHits = 0;
    private long expansionCacheMisses = 0;
    private long resultCacheHits = 0;
    private long resultCacheMisses = 0;

    @jakarta.annotation.PostConstruct
    public void init() {
        // 初始化扩展缓存
        this.expansionCache = Caffeine.newBuilder()
                .maximumSize(maxSize)
                .expireAfterWrite(expireMinutes, TimeUnit.MINUTES)
                .recordStats()
                .build();

        // 初始化结果缓存
        this.resultCache = Caffeine.newBuilder()
                .maximumSize(maxSize)
                .expireAfterWrite(expireMinutes, TimeUnit.MINUTES)
                .recordStats()
                .build();

        log.info("✅ 查询扩展缓存服务初始化完成: maxSize={}, expireMinutes={}",
                maxSize, expireMinutes);
    }

    // Setters for Spring Boot configuration properties
    public void setMaxSize(int maxSize) {
        this.maxSize = maxSize;
    }

    public void setExpireMinutes(int expireMinutes) {
        this.expireMinutes = expireMinutes;
    }

    /**
     * 获取扩展查询缓存
     * (Get expansion cache)
     *
     * @param query 原始查询 (Original query)
     * @return 扩展查询列表，不存在返回 null (Expansion list, null if not found)
     */
    public List<String> getExpansion(String query) {
        List<String> result = expansionCache.getIfPresent(query);
        if (result != null) {
            expansionCacheHits++;
            log.debug("🎯 查询扩展缓存命中: query={}", query);
        } else {
            expansionCacheMisses++;
            log.debug("❌ 查询扩展缓存未命中: query={}", query);
        }
        return result;
    }

    /**
     * 缓存扩展查询
     * (Put expansion to cache)
     *
     * @param query 原始查询 (Original query)
     * @param expansions 扩展查询列表 (Expansion list)
     */
    public void putExpansion(String query, List<String> expansions) {
        expansionCache.put(query, expansions);
        log.debug("💾 缓存查询扩展: query={}, count={}", query, expansions.size());
    }

    /**
     * 获取查询结果缓存
     * (Get query result cache)
     *
     * @param cacheKey 缓存键 (Cache key)
     * @return 查询结果列表，不存在返回 null (Result list, null if not found)
     */
    public List<SearchResult> getResult(String cacheKey) {
        List<SearchResult> result = resultCache.getIfPresent(cacheKey);
        if (result != null) {
            resultCacheHits++;
            log.debug("🎯 查询结果缓存命中: key={}", cacheKey);
        } else {
            resultCacheMisses++;
            log.debug("❌ 查询结果缓存未命中: key={}", cacheKey);
        }
        return result;
    }

    /**
     * 缓存查询结果
     * (Put query result to cache)
     *
     * @param cacheKey 缓存键 (Cache key)
     * @param results 查询结果列表 (Result list)
     */
    public void putResult(String cacheKey, List<SearchResult> results) {
        resultCache.put(cacheKey, results);
        log.debug("💾 缓存查询结果: key={}, count={}", cacheKey, results.size());
    }

    /**
     * 清除所有缓存
     * (Clear all caches)
     */
    public void clearAll() {
        expansionCache.invalidateAll();
        resultCache.invalidateAll();
        log.info("🧹 已清除所有查询扩展缓存");
    }

    /**
     * 清除扩展缓存
     * (Clear expansion cache)
     */
    public void clearExpansionCache() {
        expansionCache.invalidateAll();
        log.info("🧹 已清除查询扩展缓存");
    }

    /**
     * 清除结果缓存
     * (Clear result cache)
     */
    public void clearResultCache() {
        resultCache.invalidateAll();
        log.info("🧹 已清除查询结果缓存");
    }

    /**
     * 获取缓存统计信息
     * (Get cache statistics)
     *
     * @return 缓存统计信息 (Cache statistics)
     */
    public CacheStatistics getStatistics() {
        long totalHits = expansionCacheHits + resultCacheHits;
        long totalMisses = expansionCacheMisses + resultCacheMisses;
        double hitRate = totalHits + totalMisses == 0 ? 0.0 :
                (double) totalHits / (totalHits + totalMisses);

        return CacheStatistics.builder()
                .queryCacheSize(resultCache.estimatedSize())
                .queryCacheHits(resultCacheHits)
                .queryCacheMisses(resultCacheMisses)
                .expansionCacheSize(expansionCache.estimatedSize())
                .expansionCacheHits(expansionCacheHits)
                .expansionCacheMisses(expansionCacheMisses)
                .totalHits(totalHits)
                .totalMisses(totalMisses)
                .overallHitRate(hitRate)
                .build();
    }
}

