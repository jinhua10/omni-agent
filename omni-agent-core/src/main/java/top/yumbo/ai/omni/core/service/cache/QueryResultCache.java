package top.yumbo.ai.omni.core.service.cache;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.rag.model.Document;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 查询结果缓存服务
 * (Query Result Cache Service)
 *
 * <p>缓存热门查询结果，提升响应速度</p>
 *
 * <p>缓存策略：</p>
 * <ul>
 *     <li>LRU淘汰策略 - 最近最少使用</li>
 *     <li>TTL过期机制 - 时间到期自动清除</li>
 *     <li>热度统计 - 记录查询频率</li>
 *     <li>智能预热 - 预加载热门查询</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
@ConfigurationProperties(prefix = "omni-agent.query-cache")
@Data
public class QueryResultCache {

    /**
     * 是否启用缓存
     */
    private boolean enabled = true;

    /**
     * 最大缓存条目数
     */
    private int maxSize = 1000;

    /**
     * 缓存过期时间（分钟）
     */
    private int ttlMinutes = 30;

    /**
     * 缓存数据
     */
    private final Map<String, CacheEntry> cache = new ConcurrentHashMap<>();

    /**
     * 访问顺序记录（用于LRU）
     */
    private final LinkedList<String> accessOrder = new LinkedList<>();

    /**
     * 查询频率统计
     */
    private final Map<String, Long> queryFrequency = new ConcurrentHashMap<>();

    /**
     * 获取缓存结果
     *
     * @param query 查询文本
     * @param domainIds 域ID列表
     * @return 缓存的结果，如果不存在或已过期返回null
     */
    public List<Document> get(String query, List<String> domainIds) {
        if (!enabled) {
            return null;
        }

        String cacheKey = generateCacheKey(query, domainIds);
        CacheEntry entry = cache.get(cacheKey);

        if (entry == null) {
            log.debug("缓存未命中: {}", cacheKey);
            return null;
        }

        // 检查是否过期
        if (isExpired(entry)) {
            log.debug("缓存已过期: {}", cacheKey);
            cache.remove(cacheKey);
            accessOrder.remove(cacheKey);
            return null;
        }

        // 更新访问时间和顺序
        entry.setLastAccessTime(LocalDateTime.now());
        entry.incrementHitCount();
        updateAccessOrder(cacheKey);

        // 记录查询频率
        queryFrequency.merge(cacheKey, 1L, Long::sum);

        log.debug("✅ 缓存命中: {} (命中次数: {})", cacheKey, entry.getHitCount());

        return new ArrayList<>(entry.getResults());
    }

    /**
     * 存入缓存
     *
     * @param query 查询文本
     * @param domainIds 域ID列表
     * @param results 查询结果
     */
    public void put(String query, List<String> domainIds, List<Document> results) {
        if (!enabled || results == null || results.isEmpty()) {
            return;
        }

        String cacheKey = generateCacheKey(query, domainIds);

        // 如果缓存已满，移除最少使用的条目
        if (cache.size() >= maxSize) {
            evictLRU();
        }

        CacheEntry entry = CacheEntry.builder()
                .cacheKey(cacheKey)
                .query(query)
                .domainIds(new ArrayList<>(domainIds))
                .results(new ArrayList<>(results))
                .createdTime(LocalDateTime.now())
                .lastAccessTime(LocalDateTime.now())
                .hitCount(0)
                .build();

        cache.put(cacheKey, entry);
        accessOrder.addFirst(cacheKey);

        log.debug("💾 加入缓存: {} (结果数: {})", cacheKey, results.size());
    }

    /**
     * 清除所有缓存
     */
    public void clear() {
        cache.clear();
        accessOrder.clear();
        queryFrequency.clear();
        log.info("已清空所有缓存");
    }

    /**
     * 清除过期缓存
     */
    public void evictExpired() {
        long before = cache.size();

        Iterator<Map.Entry<String, CacheEntry>> iterator = cache.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<String, CacheEntry> entry = iterator.next();
            if (isExpired(entry.getValue())) {
                iterator.remove();
                accessOrder.remove(entry.getKey());
            }
        }

        long after = cache.size();
        if (before > after) {
            log.info("清除 {} 个过期缓存条目", before - after);
        }
    }

    /**
     * 获取缓存统计信息
     */
    public CacheStatistics getStatistics() {
        long totalHits = cache.values().stream()
                .mapToLong(CacheEntry::getHitCount)
                .sum();

        return CacheStatistics.builder()
                .enabled(enabled)
                .size(cache.size())
                .maxSize(maxSize)
                .totalHits(totalHits)
                .ttlMinutes(ttlMinutes)
                .build();
    }

    /**
     * 获取热门查询
     */
    public List<String> getHotQueries(int topK) {
        return queryFrequency.entrySet().stream()
                .sorted((e1, e2) -> e2.getValue().compareTo(e1.getValue()))
                .limit(topK)
                .map(Map.Entry::getKey)
                .collect(java.util.stream.Collectors.toList());
    }

    /**
     * 生成缓存键
     */
    private String generateCacheKey(String query, List<String> domainIds) {
        // 标准化查询文本
        String normalizedQuery = query.trim().toLowerCase();

        // 排序域ID列表确保一致性
        List<String> sortedDomains = new ArrayList<>(domainIds);
        Collections.sort(sortedDomains);

        return normalizedQuery + "|" + String.join(",", sortedDomains);
    }

    /**
     * 检查缓存条目是否过期
     */
    private boolean isExpired(CacheEntry entry) {
        LocalDateTime expireTime = entry.getCreatedTime().plusMinutes(ttlMinutes);
        return LocalDateTime.now().isAfter(expireTime);
    }

    /**
     * 更新访问顺序（LRU）
     */
    private void updateAccessOrder(String cacheKey) {
        accessOrder.remove(cacheKey);
        accessOrder.addFirst(cacheKey);
    }

    /**
     * 移除最少使用的条目（LRU淘汰）
     */
    private void evictLRU() {
        if (!accessOrder.isEmpty()) {
            String lruKey = accessOrder.removeLast();
            cache.remove(lruKey);
            log.debug("淘汰LRU缓存: {}", lruKey);
        }
    }


    /**
     * 缓存条目
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CacheEntry {
        /** 缓存键 */
        private String cacheKey;

        /** 原始查询 */
        private String query;

        /** 域ID列表 */
        private List<String> domainIds;

        /** 缓存的结果 */
        private List<Document> results;

        /** 创建时间 */
        private LocalDateTime createdTime;

        /** 最后访问时间 */
        private LocalDateTime lastAccessTime;

        /** 命中次数 */
        private long hitCount;

        public void incrementHitCount() {
            this.hitCount++;
        }
    }

    /**
     * 缓存统计信息
     */
    @Data
    @Builder
    public static class CacheStatistics {
        /** 是否启用 */
        private boolean enabled;

        /** 当前缓存大小 */
        private int size;

        /** 最大缓存大小 */
        private int maxSize;

        /** 总命中次数 */
        private long totalHits;

        /** TTL（分钟） */
        private int ttlMinutes;

        /**
         * 计算缓存使用率
         */
        public double getUsageRate() {
            return maxSize > 0 ? (double) size / maxSize : 0.0;
        }
    }
}

