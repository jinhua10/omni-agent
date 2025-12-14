package top.yumbo.ai.storage.elasticsearch;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Elasticsearch 文档存储配置属性
 * (Elasticsearch Document Storage Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.document-storage.elasticsearch")
public class ElasticsearchStorageProperties {

    /**
     * Elasticsearch 主机地址
     * 默认: localhost:9200
     */
    private String host = "localhost:9200";

    /**
     * 用户名
     */
    private String username;

    /**
     * 密码
     */
    private String password;

    /**
     * 索引前缀
     * 默认: omni-agent-docs
     */
    private String indexPrefix = "omni-agent-docs";

    /**
     * 分片数量
     * 默认: 1
     */
    private int numberOfShards = 1;

    /**
     * 副本数量
     * 默认: 1
     */
    private int numberOfReplicas = 1;
}

