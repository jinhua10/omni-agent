package top.yumbo.ai.omni.ai.api.config;

import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * AI API 自动配置
 * (AI API Auto Configuration)
 *
 * <p>启用 Vision LLM 相关的配置属性</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Configuration
@EnableConfigurationProperties({
        VisionLLMProperties.class,
        VisionLLMBatchProcessingProperties.class
})
public class AIAPIAutoConfiguration {
    // 这个类用于启用配置属性的自动注册
}

