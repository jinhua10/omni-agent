package top.yumbo.ai.voting.starter.elasticsearch;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

@Data
@ConfigurationProperties(prefix = "omni-agent.voting.elasticsearch")
public class ElasticsearchVotingProperties {
    private String sessionIndexName = "voting_sessions";
    private String voteIndexName = "votes";
    private int sessionExpirationMinutes = 60;
}
