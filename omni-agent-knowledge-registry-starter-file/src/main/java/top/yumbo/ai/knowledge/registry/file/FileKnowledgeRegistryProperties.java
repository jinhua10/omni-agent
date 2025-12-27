package top.yumbo.ai.knowledge.registry.file;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * 文件知识注册表配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.knowledge-registry.file")
public class FileKnowledgeRegistryProperties {

    /**
     * 注册表文件存储路径
     */
    private String basePath = "data/knowledge-network/registry";

    /**
     * 是否在启动时自动创建目录
     */
    private boolean autoCreateDirectories = true;

    /**
     * JSON 文件是否格式化输出（便于阅读）
     */
    private boolean prettyPrint = true;
}

