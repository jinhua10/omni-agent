package top.yumbo.ai.ai.online;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.client.RestTemplate;
import top.yumbo.ai.ai.api.AIService;

import java.time.Duration;

/**
 * Online API AI 自动配置
 * (Online API AI Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(OnlineAPIProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.ai.type",
    havingValue = "online-api"
)
public class OnlineAPIAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(name = "onlineApiRestTemplate")
    public RestTemplate onlineApiRestTemplate(OnlineAPIProperties properties) {
        return new RestTemplateBuilder()
                .setConnectTimeout(Duration.ofMillis(properties.getTimeout()))
                .setReadTimeout(Duration.ofMillis(properties.getTimeout()))
                .build();
    }

    @Bean
    @ConditionalOnMissingBean
    public AIService aiService(RestTemplate onlineApiRestTemplate, OnlineAPIProperties properties) {
        log.info("Auto-configuring OnlineAPIAIService: provider={}, model={}",
                properties.getProvider(), properties.getDefaultModel());
        return new OnlineAPIAIService(onlineApiRestTemplate, properties);
    }
}

