package top.yumbo.ai.omni.knowledge.registry.elasticsearch;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Elasticsearch 知识注册表配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.knowledge-registry.elasticsearch")
public class ElasticsearchKnowledgeRegistryProperties {

    /**
     * 索引名称
     */
    private String indexName = "knowledge_domains";
}

