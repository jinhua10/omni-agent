package top.yumbo.ai.behavior.starter.redis;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.data.redis.core.RedisTemplate;
import top.yumbo.ai.behavior.api.BehaviorAnalysisService;

/**
 * Redis 行为分析服务自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@ConditionalOnClass(RedisTemplate.class)
public class RedisBehaviorAnalysisAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(BehaviorAnalysisService.class)
    public BehaviorAnalysisService redisBehaviorAnalysisService(RedisTemplate<String, Object> redisTemplate) {
        log.info("🚀 Auto-configuring RedisBehaviorAnalysisService (Distributed Mode)");
        return new RedisBehaviorAnalysisService(redisTemplate);
    }
}
