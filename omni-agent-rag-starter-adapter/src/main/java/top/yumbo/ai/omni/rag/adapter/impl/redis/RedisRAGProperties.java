package top.yumbo.ai.omni.rag.adapter.impl.redis;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Redis RAG 配置属性
 * (Redis RAG Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.rag.redis")
public class RedisRAGProperties {

    /**
     * Redis Key前缀
     */
    private String keyPrefix = "rag:";

    /**
     * 是否启用文本索引
     */
    private boolean enableTextIndex = true;

    /**
     * 文档TTL（秒），0表示永不过期
     */
    private long documentTtl = 0;

    /**
     * Redis连接超时（毫秒）
     */
    private int connectionTimeout = 2000;

    /**
     * Redis读取超时（毫秒）
     */
    private int readTimeout = 5000;
}
