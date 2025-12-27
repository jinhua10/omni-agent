package top.yumbo.ai.omni.voting.starter.elasticsearch;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import top.yumbo.ai.omni.voting.api.VotingService;

@Configuration
@ConditionalOnClass(ElasticsearchOperations.class)
@org.springframework.boot.autoconfigure.condition.ConditionalOnProperty(
    name = "omni-agent.voting.type",
    havingValue = "elasticsearch"
)
@EnableConfigurationProperties(ElasticsearchVotingProperties.class)
public class ElasticsearchVotingAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(VotingService.class)
    public VotingService votingService(
            ElasticsearchOperations elasticsearchOperations,
            ElasticsearchVotingProperties properties) {
        return new ElasticsearchVotingService(elasticsearchOperations, properties);
    }
}
