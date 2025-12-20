package top.yumbo.ai.persistence.mongodb;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.core.MongoTemplate;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;

/**
 * MongoDB 持久化自动配置
 * (MongoDB Persistence Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(MongoDBPersistenceProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.persistence.type",
    havingValue = "mongodb"
)
public class MongoDBPersistenceAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(QuestionClassifierPersistence.class)
    public QuestionClassifierPersistence questionClassifierPersistence(
            MongoTemplate mongoTemplate,
            MongoDBPersistenceProperties properties) {
        log.info("Auto-configuring MongoDBPersistence: {}", properties.getDatabase());
        return new MongoDBPersistence(mongoTemplate, properties);
    }
}

