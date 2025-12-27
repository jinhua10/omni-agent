package top.yumbo.ai.omni.ai.online;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Online API AI 配置属性
 * (Online API AI Configuration Properties)
 *
 * <p>支持 LLM 问答和 Embedding 向量化</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.ai.online")
public class OnlineAPIProperties {

    /**
     * API 提供商
     * 支持: openai, claude, qianwen (tongyi), zhipu, dashscope, azure, etc.
     * 默认: qianwen
     */
    private String provider = "qianwen";

    /**
     * API 端点 URL（完整的 API 地址，直接使用）
     * <p>
     * 这是推荐的配置方式，endpoint 就是具体的 API 接口地址
     * <p>
     * 示例:
     * - DeepSeek: <a href="https://api.deepseek.com/v1/chat/completions">...</a>
     * - 千问: <a href="https://dashscope.aliyuncs.com/api/v1/chat/completions">...</a>
     * - OpenAI: <a href="https://api.openai.com/v1/chat/completions">...</a>
     * - Claude: <a href="https://api.anthropic.com/v1/messages">...</a>
     * - 智谱: <a href="https://open.bigmodel.cn/api/paas/v4/chat/completions">...</a>
     * - 企业网关: <a href="https://your-gateway.com/ai/chat/completions">...</a>
     */
    private String endpoint;

    /**
     * API 基础 URL（向后兼容的配置方式，会自动拼接 /chat/completions）
     * <p>
     * 注意：推荐使用 endpoint 配置完整 URL
     *
     * @deprecated 推荐使用 endpoint 配置完整 API 地址
     */
    @Deprecated
    private String baseUrl;

    /**
     * API Key / Access Token
     * 千问使用 API Key (从阿里云DashScope获取)
     * OpenAI使用 API Key
     * Claude使用 API Key
     */
    private String apiKey;

    /**
     * 默认模型（用于问答）
     * 千问 (Qianwen): qwen-plus, qwen-turbo, qwen-max
     * OpenAI: gpt-3.5-turbo, gpt-4
     * Claude: claude-3-opus, claude-3-sonnet
     * 智谱AI: glm-4, glm-3-turbo
     */
    private String defaultModel = "qwen-plus";

    /**
     * Embedding 模型名称（用于向量化）⭐
     * <p>
     * OpenAI 推荐模型:
     * - text-embedding-3-small (1536维，性价比高，推荐)
     * - text-embedding-3-large (3072维，最强)
     * - text-embedding-ada-002 (1536维，经典)
     * <p>
     * 阿里云 DashScope 推荐模型:
     * - text-embedding-v1 (1536维)
     * - text-embedding-v2 (1536维)
     * <p>
     * 默认: text-embedding-3-small
     */
    private String embeddingModel = "text-embedding-3-small";

    /**
     * 是否启用 Embedding 功能 ⭐
     * 默认: true
     */
    private boolean enableEmbedding = true;

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

