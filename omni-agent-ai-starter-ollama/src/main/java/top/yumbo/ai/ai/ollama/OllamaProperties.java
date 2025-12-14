package top.yumbo.ai.ai.ollama;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Ollama AI 配置属性
 * (Ollama AI Configuration Properties)
 *
 * <p>支持本地和远程 Ollama 服务</p>
 * <p>本地: http://localhost:11434</p>
 * <p>远程: http://your-server-ip:11434</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.ai.ollama")
public class OllamaProperties {

    /**
     * Ollama 服务地址
     * 本地: http://localhost:11434
     * 远程: http://your-server-ip:11434
     * 默认: http://localhost:11434
     */
    private String baseUrl = "http://localhost:11434";

    /**
     * 默认模型名称
     * 默认: llama2
     */
    private String defaultModel = "llama2";

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
}

