package top.yumbo.ai.voting.starter.mongodb;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.core.MongoTemplate;
import top.yumbo.ai.voting.api.VotingService;

/**
 * MongoDB投票自动配置
 * (MongoDB Voting Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Configuration
@ConditionalOnClass(MongoTemplate.class)
@EnableConfigurationProperties(MongoVotingProperties.class)
public class MongoVotingAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(VotingService.class)
    public VotingService votingService(
            MongoTemplate mongoTemplate,
            MongoVotingProperties properties) {
        return new MongoVotingService(mongoTemplate, properties);
    }
}
