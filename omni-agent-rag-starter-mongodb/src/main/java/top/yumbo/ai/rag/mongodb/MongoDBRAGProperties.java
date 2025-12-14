package top.yumbo.ai.rag.mongodb;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * MongoDB RAG 配置属性
 * (MongoDB RAG Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.rag.mongodb")
public class MongoDBRAGProperties {

    /**
     * MongoDB连接URI
     * 默认: mongodb://localhost:27017
     */
    private String uri = "mongodb://localhost:27017";

    /**
     * 数据库名称
     * 默认: omni_rag
     */
    private String database = "omni_rag";

    /**
     * 集合名称
     * 默认: rag_documents
     */
    private String collectionName = "rag_documents";

    /**
     * 启用文本搜索
     * 默认: true
     */
    private boolean enableTextSearch = true;

    /**
     * 连接超时(毫秒)
     * 默认: 10000
     */
    private int connectionTimeout = 10000;

    /**
     * Socket超时(毫秒)
     * 默认: 30000
     */
    private int socketTimeout = 30000;

    /**
     * 最大连接池大小
     * 默认: 100
     */
    private int maxPoolSize = 100;

    /**
     * 最小连接池大小
     * 默认: 10
     */
    private int minPoolSize = 10;

    /**
     * 服务器选择超时(毫秒)
     * 默认: 30000
     */
    private int serverSelectionTimeout = 30000;
}
