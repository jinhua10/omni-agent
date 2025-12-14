package top.yumbo.ai.p2p.starter.elasticsearch;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import top.yumbo.ai.p2p.api.P2PCollaborationService;

@Configuration
@ConditionalOnClass(ElasticsearchOperations.class)
@EnableConfigurationProperties(ElasticsearchP2PProperties.class)
public class ElasticsearchP2PAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(P2PCollaborationService.class)
    public P2PCollaborationService p2pCollaborationService(
            ElasticsearchOperations elasticsearchOperations,
            ElasticsearchP2PProperties properties) {
        return new ElasticsearchP2PCollaborationService(elasticsearchOperations, properties);
    }
}
