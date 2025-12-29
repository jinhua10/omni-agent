package top.yumbo.ai.omni.knowledge.registry.impl.sqlite;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * SQLite 知识注册表配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.knowledge-registry.sqlite")
public class SQLiteKnowledgeRegistryProperties {

    /**
     * 表名
     */
    private String tableName = "knowledge_domains";

    /**
     * 数据库文件路径
     */
    private String dbPath = "data/omni-agent.db";
}

