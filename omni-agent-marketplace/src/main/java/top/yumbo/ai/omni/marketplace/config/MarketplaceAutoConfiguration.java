package top.yumbo.ai.omni.marketplace.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.common.http.HttpClientAdapter;
import top.yumbo.ai.omni.common.http.RestTemplateAdapter;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
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

        // 配置请求工厂以设置超时
        SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
        requestFactory.setConnectTimeout(Duration.ofSeconds(30));
        requestFactory.setReadTimeout(Duration.ofSeconds(30));

        RestTemplate restTemplate = new RestTemplateBuilder()
                .requestFactory(() -> requestFactory)
                .build();

        return new RestTemplateAdapter(restTemplate);
    }
}

