package top.yumbo.ai.omni.core.service.cache;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
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
     * 是否启用持久化
     */
    private boolean persistenceEnabled = true;

    /**
     * 持久化存储路径前缀
     */
    private String persistencePrefix = "query-cache";

    /**
     * 缓存索引文件ID（记录所有缓存键）
     */
    private static final String CACHE_INDEX_ID = "query-cache-index";

    /**
     * 缓存数据（L1 内存缓存）
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
     * 缓存键索引（记录所有缓存的 storageId）
     */
    private final Set<String> cacheIndex = ConcurrentHashMap.newKeySet();

    /**
     * 文档存储服务（用于持久化）
     */
    @Autowired(required = false)
    private DocumentStorageService storageService;

    /**
     * JSON序列化工具
     */
    private final ObjectMapper objectMapper = new ObjectMapper()
            .registerModule(new JavaTimeModule());

    /**
     * 是否启用预热
     */
    private boolean warmupEnabled = true;

    /**
     * 预热查询数量（加载最热门的N个查询）
     */
    private int warmupSize = 50;

    /**
     * 启动时加载持久化缓存
     */
    @PostConstruct
    public void init() {
        if (enabled && persistenceEnabled && storageService != null) {
            loadPersistedCache();

            // 预热缓存
            if (warmupEnabled) {
                warmupCache();
            }
        }
        log.info("✅ 查询缓存已初始化 (启用: {}, 持久化: {}, 预热: {}, 最大: {})",
                enabled, persistenceEnabled, warmupEnabled, maxSize);
    }

    /**
     * 关闭时保存缓存到持久化存储
     */
    @PreDestroy
    public void destroy() {
        if (enabled && persistenceEnabled && storageService != null) {
            persistCache();
        }
    }

    /**
     * 获取缓存结果（两级缓存：内存 + 持久化）
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

        // 1. 先从 L1 内存缓存获取
        CacheEntry entry = cache.get(cacheKey);

        if (entry == null && persistenceEnabled && storageService != null) {
            // 2. L1 未命中，尝试从 L2 持久化存储加载
            entry = loadFromPersistence(cacheKey);
            if (entry != null) {
                // 加载到内存缓存
                cache.put(cacheKey, entry);
                accessOrder.addFirst(cacheKey);
                log.debug("📀 从持久化加载缓存: {}", cacheKey);
            }
        }

        if (entry == null) {
            log.debug("缓存未命中: {}", cacheKey);
            return null;
        }

        // 检查是否过期
        if (isExpired(entry)) {
            log.debug("缓存已过期: {}", cacheKey);
            cache.remove(cacheKey);
            accessOrder.remove(cacheKey);
            // 删除持久化缓存
            if (persistenceEnabled && storageService != null) {
                deleteFromPersistence(cacheKey);
            }
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
     * 存入缓存（同时写入内存和持久化存储）
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

        // 写入内存缓存
        cache.put(cacheKey, entry);
        accessOrder.addFirst(cacheKey);

        // 异步写入持久化存储
        if (persistenceEnabled && storageService != null) {
            saveToPersistence(cacheKey, entry);
        }

        log.debug("💾 加入缓存: {} (结果数: {}, 持久化: {})",
                cacheKey, results.size(), persistenceEnabled);
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
            // 同时删除持久化缓存
            if (persistenceEnabled && storageService != null) {
                deleteFromPersistence(lruKey);
            }
            log.debug("淘汰LRU缓存: {}", lruKey);
        }
    }

    /**
     * 缓存预热
     * 在系统启动后，预先执行热门查询以填充缓存
     */
    public void warmupCache() {
        try {
            log.info("🔥 开始缓存预热...");

            // 获取热门查询（基于历史查询频率）
            List<String> hotQueries = getHotQueries(warmupSize);

            if (hotQueries.isEmpty()) {
                log.info("   无热门查询可预热");
                return;
            }

            int warmedUp = 0;
            for (String cacheKey : hotQueries) {
                try {
                    // 检查缓存是否已经在内存中
                    if (cache.containsKey(cacheKey)) {
                        warmedUp++;
                        continue;
                    }

                    // 从持久化存储加载
                    String storageId = getStorageId(cacheKey);
                    CacheEntry entry = loadFromPersistence(cacheKey);

                    if (entry != null && !isExpired(entry)) {
                        cache.put(cacheKey, entry);
                        accessOrder.addFirst(cacheKey);
                        warmedUp++;
                    }
                } catch (Exception e) {
                    log.debug("预热缓存失败: {} - {}", cacheKey, e.getMessage());
                }
            }

            log.info("✅ 缓存预热完成: {} 个热门查询已加载", warmedUp);

        } catch (Exception e) {
            log.error("缓存预热失败: {}", e.getMessage());
        }
    }

    /**
     * 手动触发预热（用于定时任务）
     */
    public void triggerWarmup() {
        if (enabled && persistenceEnabled && warmupEnabled) {
            warmupCache();
        } else {
            log.warn("预热未启用，跳过");
        }
    }

    // ========== 持久化相关方法 ==========

    /**
     * 从持久化存储加载缓存条目
     */
    private CacheEntry loadFromPersistence(String cacheKey) {
        try {
            String storageId = getStorageId(cacheKey);
            Optional<String> jsonOpt = storageService.getExtractedText(storageId);

            if (jsonOpt.isPresent()) {
                String json = jsonOpt.get();
                CacheEntry entry = objectMapper.readValue(json, CacheEntry.class);
                log.debug("📀 从持久化加载: {} (结果数: {})", cacheKey, entry.getResults().size());
                return entry;
            }
        } catch (Exception e) {
            log.warn("从持久化加载缓存失败: {} - {}", cacheKey, e.getMessage());
        }
        return null;
    }

    /**
     * 保存缓存条目到持久化存储
     */
    private void saveToPersistence(String cacheKey, CacheEntry entry) {
        try {
            String storageId = getStorageId(cacheKey);
            String json = objectMapper.writeValueAsString(entry);
            storageService.saveExtractedText(storageId, json);

            // 添加到索引
            cacheIndex.add(storageId);

            log.debug("💾 持久化保存: {}", cacheKey);
        } catch (Exception e) {
            log.warn("持久化保存缓存失败: {} - {}", cacheKey, e.getMessage());
        }
    }

    /**
     * 从持久化存储删除缓存条目
     */
    private void deleteFromPersistence(String cacheKey) {
        try {
            String storageId = getStorageId(cacheKey);
            storageService.deleteDocument(storageId);

            // 从索引移除
            cacheIndex.remove(storageId);

            log.debug("🗑️ 删除持久化缓存: {}", cacheKey);
        } catch (Exception e) {
            log.debug("删除持久化缓存失败: {} - {}", cacheKey, e.getMessage());
        }
    }

    /**
     * 生成存储ID
     */
    private String getStorageId(String cacheKey) {
        // 使用缓存键的哈希值作为存储ID，避免特殊字符
        int hash = cacheKey.hashCode();
        return persistencePrefix + "-" + Math.abs(hash);
    }

    /**
     * 加载所有持久化缓存到内存（系统启动时）
     */
    private void loadPersistedCache() {
        try {
            log.info("🔄 开始加载持久化缓存...");

            // 1. 先加载缓存索引文件
            Set<String> persistedKeys = loadCacheIndex();
            if (persistedKeys.isEmpty()) {
                log.info("📋 缓存索引为空，无缓存需要加载");
                return;
            }

            log.debug("📋 缓存索引包含 {} 个条目", persistedKeys.size());

            // 2. 根据索引加载每个缓存条目
            int loaded = 0;
            int expired = 0;
            int failed = 0;

            for (String storageId : persistedKeys) {
                try {
                    Optional<String> jsonOpt = storageService.getExtractedText(storageId);
                    if (jsonOpt.isPresent()) {
                        CacheEntry entry = objectMapper.readValue(
                                jsonOpt.get(), CacheEntry.class);

                        // 检查是否过期
                        if (!isExpired(entry)) {
                            cache.put(entry.getCacheKey(), entry);
                            accessOrder.addFirst(entry.getCacheKey());
                            cacheIndex.add(storageId);
                            loaded++;
                        } else {
                            // 删除过期的持久化缓存
                            storageService.deleteExtractedText(storageId);
                            expired++;
                        }
                    }
                } catch (Exception e) {
                    log.warn("加载缓存条目失败: {} - {}", storageId, e.getMessage());
                    failed++;
                }
            }

            log.info("✅ 持久化缓存加载完成: {} 个加载, {} 个过期, {} 个失败",
                    loaded, expired, failed);

            // 3. 更新索引文件（移除过期的）
            if (expired > 0 || failed > 0) {
                saveCacheIndex();
            }

        } catch (Exception e) {
            log.error("加载持久化缓存失败: {}", e.getMessage());
        }
    }

    /**
     * 加载缓存索引
     */
    private Set<String> loadCacheIndex() {
        try {
            Optional<String> indexJson = storageService.getExtractedText(CACHE_INDEX_ID);
            if (indexJson.isPresent()) {
                // 解析索引文件（JSON数组）
                List<String> indexList = objectMapper.readValue(
                        indexJson.get(),
                        objectMapper.getTypeFactory().constructCollectionType(List.class, String.class)
                );
                return new java.util.HashSet<>(indexList);
            }
        } catch (Exception e) {
            log.warn("加载缓存索引失败: {}", e.getMessage());
        }
        return new java.util.HashSet<>();
    }

    /**
     * 保存缓存索引
     */
    private void saveCacheIndex() {
        try {
            // 将索引集合转换为JSON数组
            String indexJson = objectMapper.writeValueAsString(new ArrayList<>(cacheIndex));
            storageService.saveExtractedText(CACHE_INDEX_ID, indexJson);
            log.debug("📋 缓存索引已更新: {} 个条目", cacheIndex.size());
        } catch (Exception e) {
            log.warn("保存缓存索引失败: {}", e.getMessage());
        }
    }

    /**
     * 保存所有内存缓存到持久化存储（系统关闭时）
     */
    private void persistCache() {
        try {
            log.info("💾 开始持久化缓存...");
            int saved = 0;

            for (Map.Entry<String, CacheEntry> entry : cache.entrySet()) {
                try {
                    saveToPersistence(entry.getKey(), entry.getValue());
                    saved++;
                } catch (Exception e) {
                    log.warn("持久化缓存条目失败: {} - {}", entry.getKey(), e.getMessage());
                }
            }

            // 保存缓存索引
            saveCacheIndex();

            log.info("✅ 缓存持久化完成: {} 个条目", saved);
        } catch (Exception e) {
            log.error("持久化缓存失败: {}", e.getMessage());
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

