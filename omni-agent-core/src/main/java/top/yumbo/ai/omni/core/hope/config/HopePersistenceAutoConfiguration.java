package top.yumbo.ai.omni.core.hope.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.omni.core.hope.persistence.HopePersistence;
import top.yumbo.ai.omni.core.hope.persistence.impl.KnowledgeRegistryHopePersistence;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;

/**
 * HOPE 持久化自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
public class HopePersistenceAutoConfiguration {

    /**
     * 基于 KnowledgeRegistry 的 HOPE 持久化实现
     */
    @Bean
    @ConditionalOnMissingBean(HopePersistence.class)
    public HopePersistence hopePersistence(KnowledgeRegistry knowledgeRegistry) {
        log.info("✅ Creating KnowledgeRegistryHopePersistence");
        return new KnowledgeRegistryHopePersistence(knowledgeRegistry);
    }
}

