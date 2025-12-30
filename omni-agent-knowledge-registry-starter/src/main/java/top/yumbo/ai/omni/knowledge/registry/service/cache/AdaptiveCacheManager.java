package top.yumbo.ai.omni.knowledge.registry.service.cache;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

/**
 * 自适应缓存管理器
 * (Adaptive Cache Manager)
 *
 * <p>根据系统内存使用情况动态调整缓存大小</p>
 *
 * <p>策略：</p>
 * <ul>
 *     <li>监控JVM内存使用率</li>
 *     <li>内存紧张时自动缩减缓存</li>
 *     <li>内存充足时适当扩大缓存</li>
 *     <li>支持手动触发调整</li>
 * </ul>
 *
 * <p>内存阈值：</p>
 * <ul>
 *     <li>高压阈值（>85%）：缩减缓存至50%</li>
 *     <li>中压阈值（70%-85%）：缩减至75%</li>
 *     <li>正常（50%-70%）：保持当前</li>
 *     <li>充裕（<50%）：扩大至150%（不超过配置上限）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class AdaptiveCacheManager {

    /**
     * 查询结果缓存
     */
    @Autowired
    private QueryResultCache queryResultCache;

    /**
     * 初始最大缓存大小（用于计算调整范围）
     */
    private int initialMaxSize;

    /**
     * 最小缓存大小（保底值）
     */
    private static final int MIN_CACHE_SIZE = 100;

    /**
     * 最大扩展倍数
     */
    private static final double MAX_EXPANSION_RATIO = 2.0;

    /**
     * 内存使用率阈值
     */
    private static final double HIGH_PRESSURE_THRESHOLD = 0.85;  // 85%
    private static final double MEDIUM_PRESSURE_THRESHOLD = 0.70; // 70%
    private static final double LOW_PRESSURE_THRESHOLD = 0.50;   // 50%

    /**
     * 定时任务：每5分钟检查一次内存并调整缓存
     */
    @Scheduled(fixedRate = 300000) // 5分钟
    public void adaptCacheSize() {
        if (!queryResultCache.isEnabled()) {
            return;
        }

        // 初始化初始大小
        if (initialMaxSize == 0) {
            initialMaxSize = queryResultCache.getMaxSize();
        }

        // 获取当前内存使用率
        double memoryUsage = getMemoryUsageRatio();
        int currentMaxSize = queryResultCache.getMaxSize();
        int newMaxSize = calculateNewCacheSize(memoryUsage, currentMaxSize);

        if (newMaxSize != currentMaxSize) {
            log.info("🔄 自适应调整缓存大小: 内存使用率={:.1f}%, {} -> {}",
                    memoryUsage * 100, currentMaxSize, newMaxSize);

            queryResultCache.setMaxSize(newMaxSize);

            // 如果缓存需要缩减，清理超出部分
            if (newMaxSize < currentMaxSize) {
                queryResultCache.evictToSize(newMaxSize);
            }
        }
    }

    /**
     * 计算新的缓存大小
     *
     * @param memoryUsage 当前内存使用率（0.0-1.0）
     * @param currentSize 当前缓存大小
     * @return 新的缓存大小
     */
    private int calculateNewCacheSize(double memoryUsage, int currentSize) {
        int newSize;

        if (memoryUsage >= HIGH_PRESSURE_THRESHOLD) {
            // 高压：缩减至50%
            newSize = Math.max(MIN_CACHE_SIZE, currentSize / 2);
            log.warn("⚠️ 内存高压 ({}%)，缩减缓存至50%", String.format("%.1f", memoryUsage * 100));
        } else if (memoryUsage >= MEDIUM_PRESSURE_THRESHOLD) {
            // 中压：缩减至75%
            newSize = Math.max(MIN_CACHE_SIZE, (int) (currentSize * 0.75));
            log.info("ℹ️ 内存中压 ({}%)，缩减缓存至75%", String.format("%.1f", memoryUsage * 100));
        } else if (memoryUsage <= LOW_PRESSURE_THRESHOLD) {
            // 内存充裕：扩大至150%（不超过上限）
            int maxAllowed = (int) (initialMaxSize * MAX_EXPANSION_RATIO);
            newSize = Math.min(maxAllowed, (int) (currentSize * 1.5));
            log.info("✨ 内存充裕 ({}%)，扩大缓存至150%", String.format("%.1f", memoryUsage * 100));
        } else {
            // 正常：保持当前大小
            newSize = currentSize;
        }

        return newSize;
    }

    /**
     * 获取当前JVM内存使用率
     *
     * @return 内存使用率（0.0-1.0）
     */
    private double getMemoryUsageRatio() {
        Runtime runtime = Runtime.getRuntime();
        long maxMemory = runtime.maxMemory();      // 最大可用内存
        long totalMemory = runtime.totalMemory();  // 已申请内存
        long freeMemory = runtime.freeMemory();    // 空闲内存

        long usedMemory = totalMemory - freeMemory;

        return (double) usedMemory / maxMemory;
    }

    /**
     * 手动触发缓存调整
     */
    public void manualAdapt() {
        log.info("🔧 手动触发缓存自适应调整");
        adaptCacheSize();
    }

    /**
     * 获取当前内存状态
     *
     * @return 内存状态信息
     */
    public MemoryStatus getMemoryStatus() {
        Runtime runtime = Runtime.getRuntime();
        long maxMemory = runtime.maxMemory();
        long totalMemory = runtime.totalMemory();
        long freeMemory = runtime.freeMemory();
        long usedMemory = totalMemory - freeMemory;

        return MemoryStatus.builder()
                .maxMemory(maxMemory)
                .totalMemory(totalMemory)
                .usedMemory(usedMemory)
                .freeMemory(freeMemory)
                .usageRatio(getMemoryUsageRatio())
                .cacheSize(queryResultCache.getCache().size())
                .cacheMaxSize(queryResultCache.getMaxSize())
                .build();
    }

    /**
     * 内存状态
     */
    @lombok.Data
    @lombok.Builder
    @lombok.NoArgsConstructor
    @lombok.AllArgsConstructor
    public static class MemoryStatus {
        /** 最大内存（字节） */
        private long maxMemory;

        /** 已分配内存（字节） */
        private long totalMemory;

        /** 已使用内存（字节） */
        private long usedMemory;

        /** 空闲内存（字节） */
        private long freeMemory;

        /** 使用率（0.0-1.0） */
        private double usageRatio;

        /** 当前缓存条目数 */
        private int cacheSize;

        /** 最大缓存条目数 */
        private int cacheMaxSize;

        /**
         * 格式化输出
         */
        @Override
        public String toString() {
            return String.format(
                    "Memory[used=%dMB, max=%dMB, usage=%.1f%%, cache=%d/%d]",
                    usedMemory / 1024 / 1024,
                    maxMemory / 1024 / 1024,
                    usageRatio * 100,
                    cacheSize,
                    cacheMaxSize
            );
        }
    }
}

