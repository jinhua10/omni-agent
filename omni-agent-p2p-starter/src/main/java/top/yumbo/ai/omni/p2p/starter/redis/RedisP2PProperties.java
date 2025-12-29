package top.yumbo.ai.omni.p2p.starter.redis;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Redis P2P协作配置属性
 * (Redis P2P Collaboration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.p2p.redis")
public class RedisP2PProperties {

    /**
     * Redis连接配置
     */
    private String host = "localhost";
    private int port = 6379;
    private String password;
    private int database = 0;

    /**
     * 连接码过期时间（分钟）
     */
    private int codeExpirationMinutes = 10;

    /**
     * 连接超时时间（分钟）
     */
    private int connectionTimeoutMinutes = 60;

    /**
     * Key前缀
     */
    private String keyPrefix = "omni:p2p:";

    /**
     * 数据TTL（小时），0表示不过期
     */
    private int dataTtlHours = 0;
}
