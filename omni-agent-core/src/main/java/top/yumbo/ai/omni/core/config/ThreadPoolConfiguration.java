package top.yumbo.ai.omni.core.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * 线程池配置
 * (Thread Pool Configuration)
 *
 * <p>统一配置所有线程池</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Configuration
public class ThreadPoolConfiguration {

    private final ThreadPoolConfigProperties threadPoolConfig;

    public ThreadPoolConfiguration(ThreadPoolConfigProperties threadPoolConfig) {
        this.threadPoolConfig = threadPoolConfig;
    }

    /**
     * Vision LLM 处理线程池
     * <p>
     * 用于并行处理 PPT/PDF 的不同页面/批次
     * </p>
     */
    @Bean(name = "visionLlmExecutor")
    @ConditionalOnProperty(prefix = "omni-agent.vision-llm", name = "enabled", havingValue = "true")
    public Executor visionLlmExecutor() {
        ThreadPoolConfigProperties.ThreadPoolConfig config = threadPoolConfig.getVisionLlm();

        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(config.getCorePoolSize());
        executor.setMaxPoolSize(config.getMaxPoolSize());
        executor.setQueueCapacity(config.getQueueCapacity());
        executor.setKeepAliveSeconds(config.getKeepAliveSeconds());
        executor.setThreadNamePrefix(config.getThreadNamePrefix());
        executor.setAllowCoreThreadTimeOut(config.isAllowCoreThreadTimeout());
        executor.setWaitForTasksToCompleteOnShutdown(config.isWaitForTasksToCompleteOnShutdown());
        executor.setAwaitTerminationSeconds(config.getAwaitTerminationSeconds());

        // 拒绝策略：调用者运行
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());

        executor.initialize();

        log.info("✅ Vision LLM 线程池已初始化 - core: {}, max: {}, queue: {}",
            config.getCorePoolSize(), config.getMaxPoolSize(), config.getQueueCapacity());

        return executor;
    }

    /**
     * 文件监听器线程池
     * <p>
     * 用于异步处理文件变更事件
     * </p>
     */
    @Bean(name = "fileWatcherExecutor")
    public Executor fileWatcherExecutor() {
        ThreadPoolConfigProperties.ThreadPoolConfig config = threadPoolConfig.getFileWatcher();

        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(config.getCorePoolSize());
        executor.setMaxPoolSize(config.getMaxPoolSize());
        executor.setQueueCapacity(config.getQueueCapacity());
        executor.setKeepAliveSeconds(config.getKeepAliveSeconds());
        executor.setThreadNamePrefix(config.getThreadNamePrefix());
        executor.setAllowCoreThreadTimeOut(config.isAllowCoreThreadTimeout());
        executor.setWaitForTasksToCompleteOnShutdown(config.isWaitForTasksToCompleteOnShutdown());
        executor.setAwaitTerminationSeconds(config.getAwaitTerminationSeconds());

        // 拒绝策略：丢弃最旧的任务
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.DiscardOldestPolicy());

        executor.initialize();

        log.info("✅ 文件监听器线程池已初始化 - core: {}, max: {}, queue: {}",
            config.getCorePoolSize(), config.getMaxPoolSize(), config.getQueueCapacity());

        return executor;
    }
}


