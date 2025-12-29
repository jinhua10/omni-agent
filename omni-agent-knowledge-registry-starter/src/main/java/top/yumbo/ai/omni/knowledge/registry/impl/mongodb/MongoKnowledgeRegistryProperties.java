package top.yumbo.ai.omni.knowledge.registry.impl.mongodb;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * MongoDB 知识注册表配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.knowledge-registry.mongodb")
public class MongoKnowledgeRegistryProperties {

    /**
     * MongoDB 集合名称
     */
    private String collectionName = "knowledge_domains";
}

