package top.yumbo.ai.omni.behavior.starter.config;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.behavior.starter.properties.BehaviorProperties;

/**
 * Behavior 自动配置
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(BehaviorProperties.class)
@RequiredArgsConstructor
public class BehaviorAutoConfiguration {

    private final BehaviorProperties properties;

    /**
     * 初始化日志
     */
    @Bean
    @ConditionalOnMissingBean
    public BehaviorConfigLogger behaviorConfigLogger() {
        log.info("✅ Behavior Starter 已初始化");
        log.info("   存储类型: {}", properties.getType());

        switch (properties.getType().toLowerCase()) {
            case "memory":
                log.info("   Memory 配置 - maxSize: {}, ttl: {}s",
                        properties.getMemory().getMaxSize(),
                        properties.getMemory().getTtl());
                break;
            case "mongodb":
                log.info("   MongoDB 配置 - database: {}, collection: {}",
                        properties.getMongodb().getDatabase(),
                        properties.getMongodb().getCollection());
                break;
            case "redis":
                log.info("   Redis 配置 - keyPrefix: {}, ttl: {}s",
                        properties.getRedis().getKeyPrefix(),
                        properties.getRedis().getTtl());
                break;
        }

        return new BehaviorConfigLogger();
    }

    /**
     * 配置日志记录器（标记 Bean）
     */
    public static class BehaviorConfigLogger {
        // 空类，仅用于标记配置已加载
    }
}

