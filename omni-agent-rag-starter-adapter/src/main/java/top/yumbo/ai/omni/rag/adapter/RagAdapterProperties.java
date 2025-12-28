package top.yumbo.ai.omni.rag.adapter;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * RAG 适配器配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.rag")
public class RagAdapterProperties {

    /**
     * RAG 类型：file, sqlite, mongodb, redis, h2, elasticsearch, mock
     */
    private String type = "mock";

    /**
     * 向量维度
     */
    private Integer vectorDimension = 768;

    /**
     * File/Lucene 配置
     */
    private FileConfig file = new FileConfig();

    /**
     * SQLite 配置
     */
    private SQLiteConfig sqlite = new SQLiteConfig();

    /**
     * MongoDB 配置
     */
    private MongoDBConfig mongodb = new MongoDBConfig();

    /**
     * Redis 配置
     */
    private RedisConfig redis = new RedisConfig();

    /**
     * H2 配置
     */
    private H2Config h2 = new H2Config();

    /**
     * Elasticsearch 配置
     */
    private ElasticsearchConfig elasticsearch = new ElasticsearchConfig();

    @Data
    public static class FileConfig {
        private String indexPath = "data/rag-index/file";
        private Double ramBufferSizeMb = 16.0;
    }

    @Data
    public static class SQLiteConfig {
        private String databasePath = "data/rag-index/sqlite/rag.db";
        private Boolean initDatabase = true;
        private Boolean enableFts = true;
    }

    @Data
    public static class MongoDBConfig {
        private String collectionName = "rag_documents";
        private Boolean enableTextSearch = true;
    }

    @Data
    public static class RedisConfig {
        private String keyPrefix = "omni-rag:";
        private Long documentTtl = 86400L; // 24小时
        private Boolean enableTextIndex = true;
    }

    @Data
    public static class H2Config {
        private String databasePath = "data/rag-index/h2/rag";
        private Boolean initDatabase = true;
        private Boolean enableFullText = true;
    }

    @Data
    public static class ElasticsearchConfig {
        private String host = "localhost";
        private Integer port = 9200;
        private String indexPrefix = "omni-rag-";
    }
}

