package top.yumbo.ai.omni.knowledge.registry.impl.h2;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * H2 知识注册表配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.knowledge-registry.h2")
public class H2KnowledgeRegistryProperties {

    /**
     * 表名
     */
    private String tableName = "knowledge_domains";
}

