package top.yumbo.ai.omni.web.config;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.ai.api.config.VisionLLMProperties;
import top.yumbo.ai.omni.web.util.DocumentParser;
import top.yumbo.ai.omni.web.util.parser.SimpleDocumentParser;
import top.yumbo.ai.omni.web.util.parser.image.SmartImageExtractor;
import top.yumbo.ai.omni.web.util.parser.image.VisionLLMStrategy;

/**
 * 文档解析器配置
 * (Document Parser Configuration)
 *
 * <p>根据配置自动创建文档解析器 Bean，支持图片提取功能</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@RequiredArgsConstructor
public class DocumentParserConfig {

    private final VisionLLMProperties visionLLMProperties;

    /**
     * 创建文档解析器 Bean（启用 Vision LLM）
     */
    @Bean
    @ConditionalOnProperty(prefix = "omni-agent.vision-llm", name = "enabled", havingValue = "true")
    public DocumentParser documentParserWithVision() {
        log.info("📷 创建文档解析器（启用图片提取功能）");

        // 创建 Vision LLM 策略
        VisionLLMStrategy visionStrategy = new VisionLLMStrategy(
                visionLLMProperties.getApiKey(),
                visionLLMProperties.getModel(),
                visionLLMProperties.getEndpoint(),
                visionLLMProperties.getSystemPrompt()
        );

        // 创建智能图片提取器
        SmartImageExtractor imageExtractor = new SmartImageExtractor();
        imageExtractor.addStrategy(visionStrategy);

        // 创建文档解析器
        DocumentParser parser = new SimpleDocumentParser(imageExtractor);

        log.info("✅ 文档解析器已创建（图片提取: 启用）");
        log.info("   Vision LLM 模型: {}", visionLLMProperties.getModel());
        log.info("   Vision LLM 端点: {}", visionLLMProperties.getEndpoint());

        return parser;
    }

    /**
     * 创建文档解析器 Bean（不启用 Vision LLM）
     */
    @Bean
    @ConditionalOnProperty(prefix = "omni-agent.vision-llm", name = "enabled", havingValue = "false", matchIfMissing = true)
    public DocumentParser documentParserDefault() {
        log.info("📄 创建文档解析器（图片提取: 禁用）");

        DocumentParser parser = new SimpleDocumentParser(false);

        log.info("✅ 文档解析器已创建（使用占位符模式）");
        log.info("💡 提示：设置 omni-agent.vision-llm.enabled=true 以启用图片内容提取");

        return parser;
    }
}



