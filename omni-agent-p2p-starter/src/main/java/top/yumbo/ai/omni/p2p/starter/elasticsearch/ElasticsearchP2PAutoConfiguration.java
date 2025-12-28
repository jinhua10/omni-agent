package top.yumbo.ai.omni.p2p.starter.elasticsearch;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;

/**
 * Elasticsearch P2P数据传输自动配置
 * (Elasticsearch P2P Data Transfer Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Configuration
@ConditionalOnClass(ElasticsearchOperations.class)
@org.springframework.boot.autoconfigure.condition.ConditionalOnProperty(
    name = "omni-agent.p2p.type",
    havingValue = "elasticsearch"
)
@EnableConfigurationProperties(ElasticsearchP2PProperties.class)
public class ElasticsearchP2PAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(P2PDataTransferService.class)
    public P2PDataTransferService p2pDataTransferService(
            ElasticsearchOperations elasticsearchOperations,
            ElasticsearchP2PProperties properties) {
        return new ElasticsearchP2PDataTransferService(elasticsearchOperations, properties);
    }
}
