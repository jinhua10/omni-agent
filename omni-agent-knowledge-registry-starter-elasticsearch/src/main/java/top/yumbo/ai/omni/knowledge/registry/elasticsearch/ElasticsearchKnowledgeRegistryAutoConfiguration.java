package top.yumbo.ai.omni.knowledge.registry.elasticsearch;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;

/**
 * Elasticsearch 知识注册表自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnClass(ElasticsearchOperations.class)
@ConditionalOnProperty(
        prefix = "omni-agent.knowledge-registry",
        name = "type",
        havingValue = "elasticsearch"
)
@EnableConfigurationProperties(ElasticsearchKnowledgeRegistryProperties.class)
public class ElasticsearchKnowledgeRegistryAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(KnowledgeRegistry.class)
    public KnowledgeRegistry knowledgeRegistry(
            ElasticsearchOperations elasticsearchOperations,
            ElasticsearchKnowledgeRegistryProperties properties) {

        log.info("🚀 初始化 Elasticsearch 知识注册表");
        log.info("   - 索引名称: {}", properties.getIndexName());

        return new ElasticsearchKnowledgeRegistry(
                elasticsearchOperations,
                properties.getIndexName()
        );
    }
}

