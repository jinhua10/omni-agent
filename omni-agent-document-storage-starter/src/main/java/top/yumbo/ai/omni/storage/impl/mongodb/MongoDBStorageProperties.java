package top.yumbo.ai.omni.storage.impl.mongodb;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * MongoDB 文档存储配置属性
 * (MongoDB Document Storage Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.document-storage.mongodb")
public class MongoDBStorageProperties {

    /**
     * MongoDB URI
     * 默认: mongodb://localhost:27017
     */
    private String uri = "mongodb://localhost:27017";

    /**
     * 数据库名称
     */
    private String database = "omni-agent";

    /**
     * GridFS bucket 名称
     */
    private String bucketName = "documents";
}

