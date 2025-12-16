package top.yumbo.ai.voting.starter.redis;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.Jackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;
import top.yumbo.ai.voting.api.VotingService;

/**
 * Redis投票自动配置
 * (Redis Voting Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Configuration
@ConditionalOnClass(RedisTemplate.class)
@org.springframework.boot.autoconfigure.condition.ConditionalOnProperty(
    name = "omni-agent.voting.type",
    havingValue = "redis"
)
@EnableConfigurationProperties(RedisVotingProperties.class)
public class RedisVotingAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(name = "votingRedisTemplate")
    public RedisTemplate<String, Object> votingRedisTemplate(RedisConnectionFactory connectionFactory) {
        RedisTemplate<String, Object> template = new RedisTemplate<>();
        template.setConnectionFactory(connectionFactory);

        // 使用Jackson序列化
        Jackson2JsonRedisSerializer<Object> serializer = new Jackson2JsonRedisSerializer<>(Object.class);
        
        // Key使用String序列化
        template.setKeySerializer(new StringRedisSerializer());
        template.setHashKeySerializer(new StringRedisSerializer());
        
        // Value使用JSON序列化
        template.setValueSerializer(serializer);
        template.setHashValueSerializer(serializer);
        
        template.afterPropertiesSet();
        return template;
    }

    @Bean
    @ConditionalOnMissingBean(VotingService.class)
    public VotingService votingService(
            RedisTemplate<String, Object> votingRedisTemplate,
            RedisVotingProperties properties) {
        return new RedisVotingService(votingRedisTemplate, properties);
    }
}
