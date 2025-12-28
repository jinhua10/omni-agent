package top.yumbo.ai.omni.p2p.starter.mongodb;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.core.MongoTemplate;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;

/**
 * MongoDB P2P数据传输自动配置
 * (MongoDB P2P Data Transfer Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Configuration
@ConditionalOnClass(MongoTemplate.class)
@org.springframework.boot.autoconfigure.condition.ConditionalOnProperty(
    name = "omni-agent.p2p.type",
    havingValue = "mongodb"
)
@EnableConfigurationProperties(MongoP2PProperties.class)
public class MongoP2PAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(P2PDataTransferService.class)
    public P2PDataTransferService p2pDataTransferService(
            MongoTemplate mongoTemplate,
            MongoP2PProperties properties) {
        return new MongoP2PDataTransferService(mongoTemplate, properties);
    }
}
