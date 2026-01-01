package top.yumbo.ai.omni.hope.starter.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.omni.hope.api.persistence.HopePersistence;
import top.yumbo.ai.omni.hope.starter.impl.HOPEKnowledgeManager;
import top.yumbo.ai.omni.hope.starter.impl.QuestionClassifier;
import top.yumbo.ai.omni.hope.starter.persistence.InMemoryHopePersistence;
import top.yumbo.ai.omni.hope.starter.persistence.KnowledgeRegistryHopePersistence;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRegistry;
import top.yumbo.ai.omni.rag.RagService;

/**
 * HOPE 持久化自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@AutoConfigureAfter(name = {
        "top.yumbo.ai.omni.rag.adapter.config.RagAdapterAutoConfiguration",
        "top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistryAutoConfiguration"
})
public class HopePersistenceAutoConfiguration {

    /**
     * 基于 KnowledgeRegistry 的 HOPE 持久化实现
     * 仅当 KnowledgeRegistry 存在时创建
     */
    @Bean
    @ConditionalOnBean(KnowledgeRegistry.class)
    @ConditionalOnMissingBean(HopePersistence.class)
    public HopePersistence knowledgeRegistryHopePersistence(@Autowired(required = false) KnowledgeRegistry knowledgeRegistry) {
        log.info("✅ Creating KnowledgeRegistryHopePersistence");
        return new KnowledgeRegistryHopePersistence(knowledgeRegistry);
    }

    /**
     * 内存实现 - 当没有 KnowledgeRegistry 时作为后备方案
     */
    @Bean
    @ConditionalOnMissingBean({HopePersistence.class, KnowledgeRegistry.class})
    public HopePersistence inMemoryHopePersistence() {
        log.info("✅ Creating InMemoryHopePersistence (fallback)");
        return new InMemoryHopePersistence();
    }

    /**
     * 问题分类器
     */
    @Bean
    @ConditionalOnMissingBean(QuestionClassifier.class)
    public QuestionClassifier questionClassifier(HopePersistence persistence) {
        log.info("✅ Creating QuestionClassifier");
        return new QuestionClassifier(persistence);
    }

    /**
     * HOPE 知识管理器
     */
    @Bean
    @ConditionalOnMissingBean(HOPEKnowledgeManager.class)
    public HOPEKnowledgeManager hopeKnowledgeManager(
            QuestionClassifier questionClassifier,
            RagService ragService) {
        log.info("✅ Creating HOPEKnowledgeManager");
        return new HOPEKnowledgeManager(questionClassifier, ragService);
    }
}

