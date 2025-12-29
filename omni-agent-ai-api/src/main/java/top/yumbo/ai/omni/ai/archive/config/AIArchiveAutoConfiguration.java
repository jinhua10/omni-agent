package top.yumbo.ai.omni.ai.archive.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.EnableAspectJAutoProxy;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import top.yumbo.ai.omni.ai.archive.AICallArchiveService;
import top.yumbo.ai.omni.ai.archive.aspect.AICallArchiveAspect;
import top.yumbo.ai.omni.ai.archive.impl.FileAICallArchiveService;

import java.util.concurrent.Executor;

/**
 * AI调用归档自动配置
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Configuration
@EnableAsync
@EnableAspectJAutoProxy
@EnableConfigurationProperties(AIArchiveProperties.class)
@ConditionalOnProperty(name = "omni-agent.ai.archive.enabled", havingValue = "true", matchIfMissing = true)
public class AIArchiveAutoConfiguration {

    /**
     * AI归档服务
     */
    @Bean
    @ConditionalOnMissingBean
    public AICallArchiveService aiCallArchiveService() {
        log.info("✅ 创建AI调用归档服务 (FileAICallArchiveService)");
        return new FileAICallArchiveService();
    }

    /**
     * AI归档切面
     */
    @Bean
    @ConditionalOnMissingBean
    public AICallArchiveAspect aiCallArchiveAspect(AICallArchiveService archiveService) {
        log.info("✅ 创建AI调用归档切面");
        return new AICallArchiveAspect(archiveService);
    }

    /**
     * AI归档专用线程池
     */
    @Bean(name = "aiArchiveExecutor")
    public Executor aiArchiveExecutor(AIArchiveProperties properties) {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        executor.setCorePoolSize(properties.getThreadPool().getCoreSize());
        executor.setMaxPoolSize(properties.getThreadPool().getMaxSize());
        executor.setQueueCapacity(properties.getThreadPool().getQueueCapacity());
        executor.setThreadNamePrefix("ai-archive-");
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.setAwaitTerminationSeconds(60);
        executor.initialize();

        log.info("✅ AI归档线程池已初始化: core={}, max={}, queue={}",
                properties.getThreadPool().getCoreSize(),
                properties.getThreadPool().getMaxSize(),
                properties.getThreadPool().getQueueCapacity());

        return executor;
    }
}

