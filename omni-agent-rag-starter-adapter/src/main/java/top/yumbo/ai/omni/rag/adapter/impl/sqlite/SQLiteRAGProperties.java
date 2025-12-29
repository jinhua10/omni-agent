package top.yumbo.ai.omni.rag.adapter.impl.sqlite;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * SQLite RAG 配置属性
 * (SQLite RAG Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.rag.sqlite")
public class SQLiteRAGProperties {

    /**
     * SQLite 数据库文件路径
     */
    private String databasePath = "./data/rag.db";

    /**
     * 是否在启动时初始化数据库表
     */
    private boolean initDatabase = true;

    /**
     * 是否在启动时重建索引
     */
    private boolean rebuildOnStartup = false;

    /**
     * 最大搜索结果数
     */
    private int maxResults = 100;

    /**
     * 默认返回结果数
     */
    private int defaultTopK = 10;

    /**
     * 最小相似度阈值
     */
    private float minScore = 0.0f;

    /**
     * 是否启用全文搜索（FTS5）
     */
    private boolean enableFts = true;

    /**
     * 连接池最大连接数
     */
    private int maxPoolSize = 10;

    /**
     * 连接池最小空闲连接数
     */
    private int minIdle = 2;

    /**
     * 连接超时时间（毫秒）
     */
    private long connectionTimeout = 30000;
}
