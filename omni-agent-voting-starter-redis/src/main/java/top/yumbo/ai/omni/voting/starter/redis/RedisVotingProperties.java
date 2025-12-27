package top.yumbo.ai.omni.voting.starter.redis;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Redis投票配置属性
 * (Redis Voting Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.voting.redis")
public class RedisVotingProperties {

    /**
     * Redis主机地址
     * (Redis host address)
     */
    private String host = "localhost";

    /**
     * Redis端口
     * (Redis port)
     */
    private int port = 6379;

    /**
     * Redis密码
     * (Redis password)
     */
    private String password;

    /**
     * Redis数据库索引
     * (Redis database index)
     */
    private int database = 0;

    /**
     * 投票会话过期时间（分钟）
     * (Voting session expiration time in minutes)
     */
    private int sessionExpirationMinutes = 60;

    /**
     * Redis键前缀
     * (Redis key prefix)
     */
    private String keyPrefix = "omni:voting:";
}
