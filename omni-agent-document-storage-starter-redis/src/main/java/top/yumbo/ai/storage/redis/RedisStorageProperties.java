package top.yumbo.ai.storage.redis;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Redis 文档存储配置属性
 * (Redis Document Storage Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.document-storage.redis")
public class RedisStorageProperties {

    /**
     * Redis 主机
     * 默认: localhost
     */
    private String host = "localhost";

    /**
     * Redis 端口
     * 默认: 6379
     */
    private int port = 6379;

    /**
     * Redis 密码
     */
    private String password;

    /**
     * Redis 数据库索引
     * 默认: 0
     */
    private int database = 0;

    /**
     * Key 前缀
     * 默认: omni-agent:documents:
     */
    private String keyPrefix = "omni-agent:documents:";

    /**
     * 数据过期时间（秒）
     * 0 表示不过期，默认: 0
     */
    private long ttl = 0;
}

