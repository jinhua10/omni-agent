package top.yumbo.ai.omni.p2p.starter.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * P2P 配置属性
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.p2p")
public class P2PProperties {

    /**
     * 是否启用 P2P 功能
     */
    private boolean enabled = true;

    /**
     * 存储类型
     * 可选值: memory, h2, sqlite, redis, mongodb, elasticsearch
     */
    private String storageType = "memory";

    /**
     * H2 数据库配置
     */
    private H2Config h2 = new H2Config();

    /**
     * SQLite 数据库配置
     */
    private SqliteConfig sqlite = new SqliteConfig();

    /**
     * Redis 配置
     */
    private RedisConfig redis = new RedisConfig();

    /**
     * MongoDB 配置
     */
    private MongodbConfig mongodb = new MongodbConfig();

    /**
     * Elasticsearch 配置
     */
    private ElasticsearchConfig elasticsearch = new ElasticsearchConfig();

    @Data
    public static class H2Config {
        /**
         * 数据库文件路径
         */
        private String dbPath = "./data/p2p/h2";

        /**
         * 数据库名称
         */
        private String dbName = "p2p";
    }

    @Data
    public static class SqliteConfig {
        /**
         * 数据库文件路径
         */
        private String dbPath = "./data/p2p/sqlite/p2p.db";
    }

    @Data
    public static class RedisConfig {
        /**
         * 键前缀
         */
        private String keyPrefix = "p2p:";

        /**
         * TTL（秒）
         */
        private long ttl = 0; // 0 表示永不过期
    }

    @Data
    public static class MongodbConfig {
        /**
         * 数据库名称
         */
        private String database = "omni-agent";

        /**
         * 集合名称前缀
         */
        private String collectionPrefix = "p2p_";
    }

    @Data
    public static class ElasticsearchConfig {
        /**
         * 索引前缀
         */
        private String indexPrefix = "p2p-";
    }
}

