package top.yumbo.ai.omni.knowledge.registry.mongodb;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.core.MongoTemplate;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;

/**
 * MongoDB 知识注册表自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnClass(MongoTemplate.class)
@ConditionalOnProperty(
        prefix = "omni-agent.knowledge-registry",
        name = "type",
        havingValue = "mongodb"
)
@EnableConfigurationProperties(MongoKnowledgeRegistryProperties.class)
public class MongoKnowledgeRegistryAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(KnowledgeRegistry.class)
    public KnowledgeRegistry knowledgeRegistry(
            MongoTemplate mongoTemplate,
            MongoKnowledgeRegistryProperties properties) {

        log.info("🚀 初始化 MongoDB 知识注册表");
        log.info("   - 集合名称: {}", properties.getCollectionName());

        return new MongoKnowledgeRegistry(
                mongoTemplate,
                properties.getCollectionName()
        );
    }
}

