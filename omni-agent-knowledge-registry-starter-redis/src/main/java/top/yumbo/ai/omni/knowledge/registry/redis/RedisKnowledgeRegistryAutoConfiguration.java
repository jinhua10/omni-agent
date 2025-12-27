package top.yumbo.ai.omni.knowledge.registry.redis;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;

/**
 * Redis 知识注册表自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnClass(RedisTemplate.class)
@ConditionalOnProperty(
        prefix = "omni-agent.knowledge-registry",
        name = "type",
        havingValue = "redis"
)
@EnableConfigurationProperties(RedisKnowledgeRegistryProperties.class)
public class RedisKnowledgeRegistryAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(KnowledgeRegistry.class)
    public KnowledgeRegistry knowledgeRegistry(
            RedisTemplate<String, String> redisTemplate,
            RedisKnowledgeRegistryProperties properties) {

        log.info("🚀 初始化 Redis 知识注册表");
        log.info("   - Key 前缀: {}", properties.getKeyPrefix());
        log.info("   - 域列表 Key: {}", properties.getDomainListKey());

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

        return new RedisKnowledgeRegistry(
                redisTemplate,
                objectMapper,
                properties.getKeyPrefix(),
                properties.getDomainListKey()
        );
    }
}

