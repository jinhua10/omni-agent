package top.yumbo.ai.omni.marketplace.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.common.http.HttpClientAdapter;
import top.yumbo.ai.omni.common.http.RestTemplateAdapter;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.web.client.RestTemplate;

import java.time.Duration;

/**
 * 算法市场自动配置
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Configuration
@ComponentScan("top.yumbo.ai.omni.marketplace")
public class MarketplaceAutoConfiguration {

    /**
     * 配置 HttpClientAdapter（如果用户没有提供）
     */
    @Bean
    @ConditionalOnMissingBean(HttpClientAdapter.class)
    public HttpClientAdapter httpClientAdapter() {
        log.info("Creating default RestTemplate-based HttpClientAdapter for marketplace");

        RestTemplate restTemplate = new RestTemplateBuilder()
                .setConnectTimeout(Duration.ofSeconds(30))
                .setReadTimeout(Duration.ofSeconds(30))
                .build();

        return new RestTemplateAdapter(restTemplate);
    }
}

