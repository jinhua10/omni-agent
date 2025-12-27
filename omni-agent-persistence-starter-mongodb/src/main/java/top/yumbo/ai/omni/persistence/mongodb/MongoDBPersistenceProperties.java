package top.yumbo.ai.omni.persistence.mongodb;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * MongoDB 持久化配置属性
 * (MongoDB Persistence Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.persistence.mongodb")
public class MongoDBPersistenceProperties {

    /**
     * MongoDB URI
     * 默认: mongodb://localhost:27017
     */
    private String uri = "mongodb://localhost:27017";

    /**
     * 数据库名称
     * 默认: omni-agent
     */
    private String database = "omni-agent";

    /**
     * 集合名称前缀
     * 默认: persistence_
     */
    private String collectionPrefix = "persistence_";
}

