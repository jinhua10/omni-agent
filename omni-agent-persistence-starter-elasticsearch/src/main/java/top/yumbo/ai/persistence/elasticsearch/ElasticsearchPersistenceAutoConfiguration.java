package top.yumbo.ai.persistence.elasticsearch;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.json.jackson.JacksonJsonpMapper;
import co.elastic.clients.transport.rest_client.RestClientTransport;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpHost;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.elasticsearch.client.RestClient;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.StringUtils;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;

/**
 * Elasticsearch 持久化自动配置
 * (Elasticsearch Persistence Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(ElasticsearchPersistenceProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.persistence.type",
    havingValue = "elasticsearch"
)
public class ElasticsearchPersistenceAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(name = "persistenceElasticsearchClient")
    public ElasticsearchClient persistenceElasticsearchClient(ElasticsearchPersistenceProperties properties) {
        try {
            String[] hostParts = properties.getHost().split(":");
            String hostname = hostParts[0];
            int port = hostParts.length > 1 ? Integer.parseInt(hostParts[1]) : 9200;

            // 创建 RestClient
            RestClient restClient;
            if (StringUtils.hasText(properties.getUsername()) && StringUtils.hasText(properties.getPassword())) {
                BasicCredentialsProvider credentialsProvider = new BasicCredentialsProvider();
                credentialsProvider.setCredentials(
                    AuthScope.ANY,
                    new UsernamePasswordCredentials(properties.getUsername(), properties.getPassword())
                );

                restClient = RestClient.builder(new HttpHost(hostname, port, "http"))
                    .setHttpClientConfigCallback(httpClientBuilder ->
                        httpClientBuilder.setDefaultCredentialsProvider(credentialsProvider)
                    )
                    .build();
            } else {
                restClient = RestClient.builder(new HttpHost(hostname, port, "http")).build();
            }

            RestClientTransport transport = new RestClientTransport(restClient, new JacksonJsonpMapper());
            return new ElasticsearchClient(transport);
        } catch (Exception e) {
            log.error("Failed to create ElasticsearchClient", e);
            throw new RuntimeException("Failed to initialize Elasticsearch client", e);
        }
    }

    @Bean
    @ConditionalOnMissingBean
    public QuestionClassifierPersistence questionClassifierPersistence(
            ElasticsearchClient persistenceElasticsearchClient,
            ElasticsearchPersistenceProperties properties) {
        log.info("Auto-configuring ElasticsearchPersistence: {}", properties.getHost());
        return new ElasticsearchPersistence(persistenceElasticsearchClient, properties);
    }
}

