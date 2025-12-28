package top.yumbo.ai.omni.knowledge.registry.impl.memory;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRegistry;

/**
 * Memory 知识注册表自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnProperty(
        prefix = "omni-agent.knowledge-registry",
        name = "type",
        havingValue = "memory"
)
public class MemoryKnowledgeRegistryAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(KnowledgeRegistry.class)
    public KnowledgeRegistry knowledgeRegistry() {
        log.info("🚀 初始化内存知识注册表（开发/测试模式）");
        log.warn("⚠️  注意：内存模式数据不持久化！");

        return new MemoryKnowledgeRegistry();
    }
}

