package top.yumbo.ai.omni.p2p.starter.sqlite;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * SQLite P2P数据传输配置
 * (SQLite P2P Data Transfer Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.p2p.sqlite")
public class SqliteP2PProperties {

    /**
     * SQLite数据库文件路径
     * (SQLite database file path)
     */
    private String databasePath = "data/p2p-transfer.db";

    /**
     * 源表名称
     * (Source table name)
     */
    private String sourceTable = "knowledge_base";

    /**
     * 批次大小
     * (Batch size)
     */
    private int batchSize = 1000;

    /**
     * 是否自动创建表
     * (Auto create table)
     */
    private boolean autoCreateTable = true;
}
