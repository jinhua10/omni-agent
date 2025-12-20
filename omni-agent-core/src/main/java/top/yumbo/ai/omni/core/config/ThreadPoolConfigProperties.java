package top.yumbo.ai.omni.core.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * 线程池配置属性
 * (Thread Pool Configuration Properties)
 *
 * <p>统一管理所有线程池的配置</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "omni-agent.thread-pool")
public class ThreadPoolConfigProperties {

    /**
     * Vision LLM 处理线程池配置
     */
    private ThreadPoolConfig visionLlm = new ThreadPoolConfig();

    /**
     * 文件监听器线程池配置
     */
    private ThreadPoolConfig fileWatcher = new ThreadPoolConfig();

    /**
     * 单个线程池的配置
     */
    @Data
    public static class ThreadPoolConfig {
        /**
         * 核心线程数
         */
        private int corePoolSize = 2;

        /**
         * 最大线程数
         */
        private int maxPoolSize = 4;

        /**
         * 队列容量
         */
        private int queueCapacity = 100;

        /**
         * 空闲线程存活时间（秒）
         */
        private int keepAliveSeconds = 60;

        /**
         * 线程名称前缀
         */
        private String threadNamePrefix = "async-";

        /**
         * 是否允许核心线程超时
         */
        private boolean allowCoreThreadTimeout = true;

        /**
         * 关闭时是否等待任务完成
         */
        private boolean waitForTasksToCompleteOnShutdown = true;

        /**
         * 等待终止的超时时间（秒）
         */
        private int awaitTerminationSeconds = 60;
    }
}

