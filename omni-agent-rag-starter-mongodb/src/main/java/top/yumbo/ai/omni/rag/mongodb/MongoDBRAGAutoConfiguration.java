package top.yumbo.ai.omni.rag.mongodb;

import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.core.MongoTemplate;
import top.yumbo.ai.omni.rag.RagService;

import java.util.concurrent.TimeUnit;

/**
 * MongoDB RAG 自动配置
 * (MongoDB RAG Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnClass({MongoClient.class, MongoTemplate.class})
@ConditionalOnProperty(name = "omni-agent.rag.type", havingValue = "mongodb")
@EnableConfigurationProperties(MongoDBRAGProperties.class)
public class MongoDBRAGAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public MongoClient mongoClient(MongoDBRAGProperties properties) {
        log.info("创建MongoDB客户端: {}", properties.getUri());

        ConnectionString connectionString = new ConnectionString(properties.getUri());

        MongoClientSettings settings = MongoClientSettings.builder()
                .applyConnectionString(connectionString)
                .applyToConnectionPoolSettings(builder -> builder
                        .maxSize(properties.getMaxPoolSize())
                        .minSize(properties.getMinPoolSize())
                        .maxConnectionIdleTime(60000, TimeUnit.MILLISECONDS)
                )
                .applyToSocketSettings(builder -> builder
                        .connectTimeout(properties.getConnectionTimeout(), TimeUnit.MILLISECONDS)
                        .readTimeout(properties.getSocketTimeout(), TimeUnit.MILLISECONDS)
                )
                .applyToClusterSettings(builder -> builder
                        .serverSelectionTimeout(properties.getServerSelectionTimeout(), TimeUnit.MILLISECONDS)
                )
                .build();

        return MongoClients.create(settings);
    }

    @Bean
    @ConditionalOnMissingBean
    public MongoTemplate mongoTemplate(MongoClient mongoClient, MongoDBRAGProperties properties) {
        log.info("创建MongoTemplate: 数据库={}", properties.getDatabase());
        return new MongoTemplate(mongoClient, properties.getDatabase());
    }

    @Bean
    @ConditionalOnMissingBean(RagService.class)
    public RagService ragService(MongoTemplate mongoTemplate, MongoDBRAGProperties properties) {
        log.info("创建MongoDB RAG Service: 集合={}", properties.getCollectionName());
        return new MongoDBRAGService(mongoTemplate, properties);
    }
}
