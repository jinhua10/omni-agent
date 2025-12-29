package top.yumbo.ai.omni.behavior.starter.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Behavior 配置属性
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.behavior")
public class BehaviorProperties {

    /**
     * 存储类型: memory, mongodb, redis
     */
    private String type = "memory";

    /**
     * Memory 配置
     */
    private MemoryProperties memory = new MemoryProperties();

    /**
     * MongoDB 配置
     */
    private MongoDBProperties mongodb = new MongoDBProperties();

    /**
     * Redis 配置
     */
    private RedisProperties redis = new RedisProperties();

    @Data
    public static class MemoryProperties {
        /**
         * 最大存储数量
         */
        private Integer maxSize = 10000;

        /**
         * 过期时间（秒）
         */
        private Integer ttl = 3600;
    }

    @Data
    public static class MongoDBProperties {
        /**
         * 数据库名称
         */
        private String database = "omni-agent-behavior";

        /**
         * 集合名称
         */
        private String collection = "behaviors";
    }

    @Data
    public static class RedisProperties {
        /**
         * Key 前缀
         */
        private String keyPrefix = "behavior:";

        /**
         * 过期时间（秒）
         */
        private Integer ttl = 3600;
    }
}

