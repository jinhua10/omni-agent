package top.yumbo.ai.p2p.starter.elasticsearch;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

@Data
@ConfigurationProperties(prefix = "omni-agent.p2p.elasticsearch")
public class ElasticsearchP2PProperties {
    private String codeIndexName = "p2p_connection_codes";
    private String connectionIndexName = "p2p_connections";
    private String knowledgeIndexName = "p2p_shared_knowledge";
    private int codeExpirationMinutes = 10;
    private int connectionTimeoutMinutes = 1440;
}
