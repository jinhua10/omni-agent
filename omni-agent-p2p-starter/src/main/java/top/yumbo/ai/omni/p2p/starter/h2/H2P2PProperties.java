package top.yumbo.ai.omni.p2p.starter.h2;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * H2 P2P数据传输配置
 * (H2 P2P Data Transfer Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.p2p.h2")
public class H2P2PProperties {

    /**
     * H2数据库文件路径
     * (H2 database file path)
     */
    private String databasePath = "data/p2p-transfer";

    /**
     * 数据库用户名
     * (Database username)
     */
    private String username = "sa";

    /**
     * 数据库密码
     * (Database password)
     */
    private String password = "";

    /**
     * H2数据库模式 (embedded/file/mem)
     * (H2 database mode: embedded/file/mem)
     */
    private String mode = "file";

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

    /**
     * 获取完整的JDBC URL
     * (Get full JDBC URL)
     */
    public String getJdbcUrl() {
        return switch (mode.toLowerCase()) {
            case "mem" -> "jdbc:h2:mem:" + databasePath;
            case "embedded", "file" -> "jdbc:h2:file:" + databasePath;
            default -> "jdbc:h2:file:" + databasePath;
        };
    }
}
