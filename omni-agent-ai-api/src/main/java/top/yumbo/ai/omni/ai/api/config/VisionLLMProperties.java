package top.yumbo.ai.omni.ai.api.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * Vision LLM 配置属性
 * (Vision LLM Configuration Properties)
 *
 * <p>用于图片转文字功能的配置</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "omni-agent.vision-llm")
public class VisionLLMProperties {

    /**
     * 是否启用 Vision LLM 功能
     */
    private boolean enabled = false;

    /**
     * API 密钥
     */
    private String apiKey;

    /**
     * 模型名称
     * 如: qwen-vl-plus, qwen-vl-max, deepseek-vl, gpt-4o
     */
    private String model = "qwen-vl-plus";

    /**
     * API 端点
     */
    private String endpoint = "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions";

    /**
     * 系统提示词
     */
    private String systemPrompt =
            "请分析这张图片并提取其中的关键信息。" +
            "如果图片包含文字，请完整准确地提取所有文字内容。" +
            "如果是图表或示意图，请描述其主要内容和含义。" +
            "保持输出简洁，只提取核心信息。";
}

