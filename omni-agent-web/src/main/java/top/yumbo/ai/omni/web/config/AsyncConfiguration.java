package top.yumbo.ai.omni.web.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * 异步配置
 *
 * <p>为问答系统提供专用的线程池，提升性能和吞吐量</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Configuration
@EnableAsync
public class AsyncConfiguration {

    /**
     * 问答任务线程池
     *
     * <p>配置说明：</p>
     * <ul>
     *   <li>核心线程数：10 - 保持常驻线程处理并发请求</li>
     *   <li>最大线程数：50 - 高峰期最多支持50个并发</li>
     *   <li>队列容量：200 - 缓冲等待的任务</li>
     *   <li>拒绝策略：CallerRunsPolicy - 调用者线程执行，避免任务丢失</li>
     * </ul>
     */
    @Bean(name = "qaTaskExecutor")
    public Executor qaTaskExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();

        // 核心线程数（常驻线程）
        executor.setCorePoolSize(10);

        // 最大线程数
        executor.setMaxPoolSize(50);

        // 队列容量
        executor.setQueueCapacity(200);

        // 线程名称前缀
        executor.setThreadNamePrefix("QA-Async-");

        // 拒绝策略：由调用者线程执行
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());

        // 线程空闲时间（秒）
        executor.setKeepAliveSeconds(60);

        // 允许核心线程超时
        executor.setAllowCoreThreadTimeOut(true);

        // 等待所有任务完成后关闭
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(60);

        executor.initialize();

        log.info("✅ 问答任务线程池初始化完成: core={}, max={}, queue={}",
                10, 50, 200);

        return executor;
    }
}

