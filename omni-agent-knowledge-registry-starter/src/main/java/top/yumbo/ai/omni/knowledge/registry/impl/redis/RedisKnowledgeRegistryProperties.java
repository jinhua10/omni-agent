package top.yumbo.ai.omni.knowledge.registry.impl.redis;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Redis 知识注册表配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.knowledge-registry.redis")
public class RedisKnowledgeRegistryProperties {

    /**
     * Redis key 前缀
     */
    private String keyPrefix = "knowledge:domain:";

    /**
     * 域列表 key
     */
    private String domainListKey = "knowledge:domains:all";
}

