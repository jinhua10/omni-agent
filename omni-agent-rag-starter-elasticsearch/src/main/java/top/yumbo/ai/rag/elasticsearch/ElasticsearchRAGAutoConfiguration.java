package top.yumbo.ai.rag.elasticsearch;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.json.jackson.JacksonJsonpMapper;
import co.elastic.clients.transport.rest_client.RestClientTransport;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpHost;
import org.elasticsearch.client.RestClient;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.rag.api.RAGService;

/**
 * Elasticsearch RAG 自动配置类
 * (Elasticsearch RAG Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@ConditionalOnClass(ElasticsearchClient.class)
@org.springframework.boot.autoconfigure.condition.ConditionalOnProperty(
    name = "omni-agent.rag.type",
    havingValue = "elasticsearch"
)
@EnableConfigurationProperties(ElasticsearchRAGProperties.class)
public class ElasticsearchRAGAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(name = "ragElasticsearchClient")
    public ElasticsearchClient ragElasticsearchClient(
            org.springframework.boot.autoconfigure.elasticsearch.ElasticsearchProperties esProperties) {
        
        // 解析 Elasticsearch 地址
        String[] uris = esProperties.getUris().toArray(new String[0]);
        HttpHost[] hosts = new HttpHost[uris.length];
        
        for (int i = 0; i < uris.length; i++) {
            String uri = uris[i];
            // 简单解析（生产环境建议使用更完善的解析）
            if (uri.startsWith("http://")) {
                uri = uri.substring(7);
            } else if (uri.startsWith("https://")) {
                uri = uri.substring(8);
            }
            
            String[] parts = uri.split(":");
            String host = parts[0];
            int port = parts.length > 1 ? Integer.parseInt(parts[1]) : 9200;
            
            hosts[i] = new HttpHost(host, port, "http");
        }

        // 创建 RestClient
        RestClient restClient = RestClient.builder(hosts).build();

        // 创建 Transport
        RestClientTransport transport = new RestClientTransport(
            restClient, 
            new JacksonJsonpMapper()
        );

        // 创建 ElasticsearchClient
        ElasticsearchClient client = new ElasticsearchClient(transport);

        log.info("Elasticsearch RAG Client 配置完成");
        log.info("  连接地址: {}", String.join(", ", esProperties.getUris()));
        
        return client;
    }

    @Bean
    @ConditionalOnMissingBean(RAGService.class)
    public ElasticsearchRAGService elasticsearchRAGService(
            ElasticsearchClient ragElasticsearchClient,
            ElasticsearchRAGProperties properties) {
        
        log.info("创建 Elasticsearch RAG Service");
        log.info("  索引名称: {}", properties.getIndexName());
        log.info("  分片数量: {}", properties.getNumberOfShards());
        log.info("  副本数量: {}", properties.getNumberOfReplicas());
        log.info("  向量维度: {}", properties.getVectorDimension());

        return new ElasticsearchRAGService(ragElasticsearchClient, properties);
    }
}
