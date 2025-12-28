package top.yumbo.ai.omni.storage.impl.mongodb;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.core.MongoTemplate;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

/**
 * MongoDB 文档存储自动配置
 * (MongoDB Document Storage Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(MongoDBStorageProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.document-storage.type",
    havingValue = "mongodb"
)
public class MongoDBDocumentStorageAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public DocumentStorageService documentStorageService(
            MongoTemplate mongoTemplate,
            MongoDBStorageProperties properties) {
        log.info("Auto-configuring MongoDBDocumentStorage: {}", properties.getDatabase());
        return new MongoDBDocumentStorage(mongoTemplate, properties.getBucketName());
    }
}

