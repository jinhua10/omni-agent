package top.yumbo.ai.ai.online;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Online API AI 配置属性
 * (Online API AI Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.ai.online")
public class OnlineAPIProperties {

    /**
     * API 提供商
     * 支持: openai, claude, qianwen, etc.
     * 默认: openai
     */
    private String provider = "openai";

    /**
     * API Base URL
     * OpenAI: https://api.openai.com/v1
     * Claude: https://api.anthropic.com/v1
     */
    private String baseUrl = "https://api.openai.com/v1";

    /**
     * API Key
     */
    private String apiKey;

    /**
     * 默认模型
     * OpenAI: gpt-3.5-turbo, gpt-4
     * Claude: claude-3-opus, claude-3-sonnet
     */
    private String defaultModel = "gpt-3.5-turbo";

    /**
     * 请求超时时间（毫秒）
     * 默认: 60000 (60秒)
     */
    private int timeout = 60000;

    /**
     * 最大重试次数
     * 默认: 3
     */
    private int maxRetries = 3;

    /**
     * 温度参数 (0.0-2.0)
     * 默认: 0.7
     */
    private double temperature = 0.7;

    /**
     * Top P 参数 (0.0-1.0)
     * 默认: 0.9
     */
    private double topP = 0.9;

    /**
     * 最大生成 Token 数
     * 默认: 2048
     */
    private int maxTokens = 2048;

    /**
     * 是否启用流式响应
     * 默认: false
     */
    private boolean streamEnabled = false;
}

