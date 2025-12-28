package top.yumbo.ai.omni.voting.starter;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Voting 服务配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.voting")
public class VotingProperties {

    /**
     * Voting 类型：memory, mongodb, redis, elasticsearch
     */
    private String type = "memory";

    /**
     * Memory 配置
     */
    private MemoryConfig memory = new MemoryConfig();

    /**
     * MongoDB 配置
     */
    private MongoDBConfig mongodb = new MongoDBConfig();

    /**
     * Redis 配置
     */
    private RedisConfig redis = new RedisConfig();

    /**
     * Elasticsearch 配置
     */
    private ElasticsearchConfig elasticsearch = new ElasticsearchConfig();

    @Data
    public static class MemoryConfig {
        private Long maxSize = 10000L;
        private Long ttl = 3600000L; // 1小时
    }

    @Data
    public static class MongoDBConfig {
        private String collectionName = "voting_records";
        private Boolean enableIndexes = true;
    }

    @Data
    public static class RedisConfig {
        private String keyPrefix = "omni-voting:";
        private Long ttl = 86400L; // 24小时
    }

    @Data
    public static class ElasticsearchConfig {
        private String indexName = "omni-voting";
        private Integer shards = 1;
        private Integer replicas = 0;
    }
}

