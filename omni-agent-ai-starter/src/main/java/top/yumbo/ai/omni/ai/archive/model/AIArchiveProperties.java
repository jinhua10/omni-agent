package top.yumbo.ai.omni.ai.archive.model;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * AI归档配置属性
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.ai.archive")
public class AIArchiveProperties {

    /**
     * 是否启用AI调用归档
     */
    private Boolean enabled = true;

    /**
     * 归档存储路径
     */
    private String storagePath = "data/ai-archives";

    /**
     * 是否启用内存索引
     */
    private Boolean enableMemoryIndex = true;

    /**
     * 内存索引最大数量
     */
    private Integer maxMemoryIndex = 1000;

    /**
     * 自动清理旧归档的天数（0表示不清理）
     */
    private Integer autoCleanDays = 30;

    /**
     * 线程池配置
     */
    private ThreadPool threadPool = new ThreadPool();

    @Data
    public static class ThreadPool {
        /** 核心线程数 */
        private Integer coreSize = 2;
        /** 最大线程数 */
        private Integer maxSize = 5;
        /** 队列容量 */
        private Integer queueCapacity = 1000;
    }
}

