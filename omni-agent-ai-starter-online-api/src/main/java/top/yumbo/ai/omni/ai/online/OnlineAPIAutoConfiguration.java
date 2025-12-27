package top.yumbo.ai.omni.ai.online;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
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
        // 配置请求工厂以设置超时
        SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
        requestFactory.setConnectTimeout(Duration.ofMillis(properties.getTimeout()));
        requestFactory.setReadTimeout(Duration.ofMillis(properties.getTimeout()));

        return new RestTemplateBuilder()
                .requestFactory(() -> requestFactory)
                .build();
    }

    /**
     * 通用 AI Service（用于普通对话）
     */
    @Bean
    @ConditionalOnMissingBean
    public AIService aiService(RestTemplate onlineApiRestTemplate, OnlineAPIProperties properties) {
        log.info("Auto-configuring OnlineAPIAIService: provider={}, model={}",
                properties.getProvider(), properties.getDefaultModel());
        return new OnlineAPIAIService(onlineApiRestTemplate, properties);
    }

    /**
     * Vision AI Service（专门用于图像识别）⭐
     * 使用 Spring IoC 注入 VisionLLMProperties 配置
     */
    @Bean(name = "visionAIService")
    @ConditionalOnProperty(prefix = "omni-agent.vision-llm", name = "enabled", havingValue = "true")
    public AIService visionAIService(
            RestTemplate onlineApiRestTemplate,
            @Autowired(required = false) top.yumbo.ai.ai.api.config.VisionLLMProperties visionLLMProperties) {

        if (visionLLMProperties == null) {
            log.warn("⚠️ Vision LLM 配置未找到，Vision 功能可能无法正常工作");
            return null;
        }

        // 创建专门的 Vision Properties（使用 Spring 注入的配置）
        OnlineAPIProperties visionProps = new OnlineAPIProperties();
        visionProps.setProvider("qianwen"); // Vision 通常使用千问
        visionProps.setDefaultModel(visionLLMProperties.getModel());
        visionProps.setEndpoint(visionLLMProperties.getEndpoint());
        visionProps.setApiKey(visionLLMProperties.getApiKey());
        visionProps.setTimeout(60000); // Vision 处理时间较长

        log.info("✅ Auto-configuring Vision AIService: model={}, endpoint={}",
                visionProps.getDefaultModel(), visionProps.getEndpoint());

        return new OnlineAPIAIService(onlineApiRestTemplate, visionProps);
    }
}

