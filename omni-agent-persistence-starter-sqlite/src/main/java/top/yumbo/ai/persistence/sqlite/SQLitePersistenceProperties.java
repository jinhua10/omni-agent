package top.yumbo.ai.persistence.sqlite;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * SQLite 持久化配置属性
 * (SQLite Persistence Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.persistence.sqlite")
public class SQLitePersistenceProperties {

    /**
     * 数据库文件路径
     * 默认: ./data/omni-agent.db
     */
    private String dbPath = "./data/omni-agent.db";

    /**
     * 是否自动创建表
     */
    private boolean autoCreateTables = true;

    /**
     * 是否启用 SQL 日志
     */
    private boolean showSql = false;

    /**
     * 连接超时（毫秒）
     */
    private int connectionTimeout = 30000;
}

