package top.yumbo.ai.omni.core.config;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * 跨域查询线程池配置
 * (Cross-Domain Query Thread Pool Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConfigurationProperties(prefix = "omni-agent.cross-domain-query")
@Data
public class CrossDomainQueryConfig {

    /**
     * 核心线程数
     */
    private int corePoolSize = 5;

    /**
     * 最大线程数
     */
    private int maxPoolSize = 10;

    /**
     * 队列容量
     */
    private int queueCapacity = 100;

    /**
     * 线程名称前缀
     */
    private String threadNamePrefix = "CrossDomain-";

    /**
     * 超时时间（秒）
     */
    private int queryTimeout = 30;

    /**
     * 创建跨域查询专用线程池
     */
    @Bean("crossDomainQueryExecutor")
    public Executor crossDomainQueryExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();

        // 核心线程数
        executor.setCorePoolSize(corePoolSize);

        // 最大线程数
        executor.setMaxPoolSize(maxPoolSize);

        // 队列容量
        executor.setQueueCapacity(queueCapacity);

        // 线程名称前缀
        executor.setThreadNamePrefix(threadNamePrefix);

        // 拒绝策略：调用者运行
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());

        // 线程空闲时间（秒）
        executor.setKeepAliveSeconds(60);

        // 允许核心线程超时
        executor.setAllowCoreThreadTimeOut(true);

        // 等待所有任务完成后再关闭
        executor.setWaitForTasksToCompleteOnShutdown(true);

        // 等待时间（秒）
        executor.setAwaitTerminationSeconds(60);

        executor.initialize();

        log.info("✅ 跨域查询线程池已初始化:");
        log.info("   核心线程数: {}", corePoolSize);
        log.info("   最大线程数: {}", maxPoolSize);
        log.info("   队列容量: {}", queueCapacity);

        return executor;
    }

}

