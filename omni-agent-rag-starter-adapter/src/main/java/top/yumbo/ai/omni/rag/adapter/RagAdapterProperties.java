package top.yumbo.ai.omni.rag.adapter;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * RAG 适配器配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.rag")
public class RagAdapterProperties {

    /**
     * RAG 类型：file, elasticsearch, mock
     */
    private String type = "mock";

    /**
     * 索引基础路径（用于 file 类型）
     */
    private String indexBasePath = "data/rag-indexes";

    /**
     * 向量维度
     */
    private Integer vectorDimension = 768;

    /**
     * Elasticsearch 配置
     */
    private ElasticsearchConfig elasticsearch = new ElasticsearchConfig();

    @Data
    public static class ElasticsearchConfig {
        private String host = "localhost";
        private Integer port = 9200;
        private String indexPrefix = "omni-rag-";
    }
}

