package top.yumbo.ai.p2p.starter.mongodb;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.core.MongoTemplate;
import top.yumbo.ai.p2p.api.P2PCollaborationService;

/**
 * MongoDB P2P协作自动配置
 * (MongoDB P2P Collaboration Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Configuration
@ConditionalOnClass(MongoTemplate.class)
@EnableConfigurationProperties(MongoP2PProperties.class)
public class MongoP2PAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(P2PCollaborationService.class)
    public P2PCollaborationService p2pCollaborationService(
            MongoTemplate mongoTemplate,
            MongoP2PProperties properties) {
        return new MongoP2PCollaborationService(mongoTemplate, properties);
    }
}
