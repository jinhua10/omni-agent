package top.yumbo.ai.omni.storage.redis;

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
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

/**
 * Redis 文档存储自动配置
 * (Redis Document Storage Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(RedisStorageProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.document-storage.type",
    havingValue = "redis"
)
public class RedisDocumentStorageAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(name = "documentStorageRedisTemplate")
    public RedisTemplate<String, Object> documentStorageRedisTemplate(
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
    @ConditionalOnMissingBean
    public DocumentStorageService documentStorageService(
            RedisTemplate<String, Object> documentStorageRedisTemplate,
            RedisStorageProperties properties) {
        log.info("Auto-configuring RedisDocumentStorage: {}:{}",
                properties.getHost(), properties.getPort());
        return new RedisDocumentStorage(documentStorageRedisTemplate, properties);
    }
}

