package top.yumbo.ai.ai.ollama;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;
import top.yumbo.ai.ai.api.AIService;

import java.time.Duration;

/**
 * Ollama AI 自动配置
 * (Ollama AI Auto Configuration)
 *
 * <p>支持本地和远程 Ollama 服务</p>
 * <p>通过 omni-agent.ai.ollama.base-url 配置服务地址</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(OllamaProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.ai.type",
    havingValue = "ollama",
    matchIfMissing = true // 默认使用 Ollama
)
public class OllamaAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(name = "ollamaRestTemplate")
    public RestTemplate ollamaRestTemplate(OllamaProperties properties) {
        // 配置请求工厂以设置超时
        SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
        requestFactory.setConnectTimeout(Duration.ofMillis(properties.getTimeout()));
        requestFactory.setReadTimeout(Duration.ofMillis(properties.getTimeout()));

        return new RestTemplateBuilder()
                .requestFactory(() -> requestFactory)
                .build();
    }

    @Bean
    @ConditionalOnMissingBean
    public AIService aiService(RestTemplate ollamaRestTemplate, OllamaProperties properties) {
        String mode = properties.getBaseUrl().contains("localhost") ? "local" : "remote";
        log.info("Auto-configuring OllamaAIService: mode={}, baseUrl={}", mode, properties.getBaseUrl());
        return new OllamaAIService(ollamaRestTemplate, properties);
    }
}

