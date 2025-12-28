package top.yumbo.ai.omni.rag.adapter.impl.elasticsearch;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Elasticsearch RAG 配置属性
 * (Elasticsearch RAG Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.rag.elasticsearch")
public class ElasticsearchRAGProperties {

    /**
     * 索引名称
     */
    private String indexName = "omni-agent-rag";

    /**
     * 分片数量
     */
    private int numberOfShards = 3;

    /**
     * 副本数量
     */
    private int numberOfReplicas = 1;

    /**
     * 向量维度
     */
    private int vectorDimension = 768;

    /**
     * 写入后是否立即刷新
     */
    private boolean refreshAfterWrite = false;

    /**
     * 连接超时（毫秒）
     */
    private int connectionTimeout = 5000;

    /**
     * Socket超时（毫秒）
     */
    private int socketTimeout = 60000;
}
