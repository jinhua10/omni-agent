package top.yumbo.ai.omni.voting.starter.impl.mongodb;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * MongoDB投票配置属性
 * (MongoDB Voting Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.voting.mongodb")
public class MongoVotingProperties {

    /**
     * 会话集合名称
     * (Session collection name)
     */
    private String sessionCollectionName = "voting_sessions";

    /**
     * 投票集合名称
     * (Vote collection name)
     */
    private String voteCollectionName = "votes";

    /**
     * 投票会话过期时间（分钟）
     * (Voting session expiration time in minutes)
     */
    private int sessionExpirationMinutes = 60;
}
