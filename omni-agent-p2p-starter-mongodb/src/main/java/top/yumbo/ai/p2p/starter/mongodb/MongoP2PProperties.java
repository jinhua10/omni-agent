package top.yumbo.ai.p2p.starter.mongodb;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * MongoDB P2P配置属性
 * (MongoDB P2P Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.p2p.mongodb")
public class MongoP2PProperties {

    /**
     * 连接码集合名称
     * (Connection code collection name)
     */
    private String codeCollectionName = "p2p_connection_codes";

    /**
     * 连接集合名称
     * (Connection collection name)
     */
    private String connectionCollectionName = "p2p_connections";

    /**
     * 共享知识集合名称
     * (Shared knowledge collection name)
     */
    private String knowledgeCollectionName = "p2p_shared_knowledge";

    /**
     * 连接码有效期（分钟）
     * (Connection code expiration time in minutes)
     */
    private int codeExpirationMinutes = 10;

    /**
     * 连接超时时间（分钟）
     * (Connection timeout in minutes)
     */
    private int connectionTimeoutMinutes = 1440; // 24小时
}
