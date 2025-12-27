package top.yumbo.ai.omni.rag.redis;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import top.yumbo.ai.omni.rag.RagService;

/**
 * Redis RAG 自动配置类
 * (Redis RAG Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@ConditionalOnClass(RedisConnectionFactory.class)
@org.springframework.boot.autoconfigure.condition.ConditionalOnProperty(
    name = "omni-agent.rag.type",
    havingValue = "redis"
)
@EnableConfigurationProperties(RedisRAGProperties.class)
public class RedisRAGAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(name = "ragRedisTemplate")
    public RedisTemplate<String, Object> ragRedisTemplate(RedisConnectionFactory redisConnectionFactory) {
        RedisTemplate<String, Object> template = new RedisTemplate<>();
        template.setConnectionFactory(redisConnectionFactory);

        // 设置序列化器
        StringRedisSerializer stringSerializer = new StringRedisSerializer();
        GenericJackson2JsonRedisSerializer jsonSerializer = new GenericJackson2JsonRedisSerializer();

        // Key使用String序列化
        template.setKeySerializer(stringSerializer);
        template.setHashKeySerializer(stringSerializer);

        // Value使用JSON序列化
        template.setValueSerializer(jsonSerializer);
        template.setHashValueSerializer(jsonSerializer);

        template.afterPropertiesSet();

        log.info("Redis RAG Template 配置完成");
        return template;
    }

    @Bean
    @ConditionalOnMissingBean(RagService.class)
    public RagService redisRAGService(RedisTemplate<String, Object> ragRedisTemplate,
                                          RedisRAGProperties properties) {
        log.info("创建 Redis RAG Service");
        log.info("  KeyPrefix: {}", properties.getKeyPrefix());
        log.info("  启用文本索引: {}", properties.isEnableTextIndex());
        log.info("  文档TTL: {} 秒", properties.getDocumentTtl());

        return new RedisRAGService(ragRedisTemplate, properties);
    }
}
