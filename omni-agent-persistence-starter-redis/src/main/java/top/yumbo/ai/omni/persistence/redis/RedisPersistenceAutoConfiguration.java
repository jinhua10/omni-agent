package top.yumbo.ai.omni.persistence.redis;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;

/**
 * Redis 持久化自动配置
 * (Redis Persistence Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(RedisPersistenceProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.persistence.type",
    havingValue = "redis"
)
public class RedisPersistenceAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(name = "persistenceRedisTemplate")
    public RedisTemplate<String, Object> persistenceRedisTemplate(
            RedisConnectionFactory connectionFactory) {
        RedisTemplate<String, Object> template = new RedisTemplate<>();
        template.setConnectionFactory(connectionFactory);

        // 使用 String 序列化 key
        template.setKeySerializer(new StringRedisSerializer());
        template.setHashKeySerializer(new StringRedisSerializer());

        // 使用 JSON 序列化 value
        GenericJackson2JsonRedisSerializer jsonSerializer = new GenericJackson2JsonRedisSerializer();
        template.setValueSerializer(jsonSerializer);
        template.setHashValueSerializer(jsonSerializer);

        template.afterPropertiesSet();
        return template;
    }

    @Bean
    @ConditionalOnMissingBean(QuestionClassifierPersistence.class)
    public QuestionClassifierPersistence questionClassifierPersistence(
            RedisTemplate<String, Object> persistenceRedisTemplate,
            RedisPersistenceProperties properties) {
        log.info("Auto-configuring RedisPersistence: {}:{}", properties.getHost(), properties.getPort());
        return new RedisPersistence(persistenceRedisTemplate, properties);
    }
}

