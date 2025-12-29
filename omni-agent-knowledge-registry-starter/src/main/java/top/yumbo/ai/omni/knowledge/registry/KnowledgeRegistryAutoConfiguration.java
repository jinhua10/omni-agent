package top.yumbo.ai.omni.knowledge.registry;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.AutoConfigureOrder;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.core.Ordered;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeAssociationService;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeExtractionService;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRefinementService;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeStorageService;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

/**
 * 知识注册表核心服务自动配置
 *
 * <p>提供知识存储和关联服务的默认实现</p>
 * <p>使用 HIGHEST_PRECEDENCE+10 确保在DocumentStorage之后，但在业务服务之前初始化</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@AutoConfigureOrder(Ordered.HIGHEST_PRECEDENCE + 10)
public class KnowledgeRegistryAutoConfiguration {

    public KnowledgeRegistryAutoConfiguration() {
        log.info("🚀 知识注册表核心服务自动配置已加载");
    }

    /**
     * 创建默认的知识存储服务
     */
    @Bean
    @ConditionalOnMissingBean(KnowledgeStorageService.class)
    public KnowledgeStorageService knowledgeStorageService(DocumentStorageService documentStorage) {
        log.info("📦 创建默认知识存储服务（基于 DocumentStorageService）");
        return new top.yumbo.ai.omni.knowledge.registry.network.DefaultKnowledgeStorageService(documentStorage);
    }

    /**
     * 创建默认的知识关联服务
     */
    @Bean
    @ConditionalOnMissingBean(KnowledgeAssociationService.class)
    public KnowledgeAssociationService knowledgeAssociationService(KnowledgeStorageService storageService) {
        log.info("🔗 创建默认知识关联服务（基于 KnowledgeStorageService）");
        return new top.yumbo.ai.omni.knowledge.registry.network.DefaultKnowledgeAssociationService(storageService);
    }

    /**
     * 创建默认的知识提取服务
     */
    @Bean
    @ConditionalOnMissingBean(KnowledgeExtractionService.class)
    public KnowledgeExtractionService knowledgeExtractionService(KnowledgeStorageService storageService) {
        log.info("🔍 创建默认知识提取服务（基于 KnowledgeStorageService）");
        return new top.yumbo.ai.omni.knowledge.registry.network.DefaultKnowledgeExtractionService(storageService);
    }

    /**
     * 创建默认的知识提炼服务
     */
    @Bean
    @ConditionalOnMissingBean(KnowledgeRefinementService.class)
    public KnowledgeRefinementService knowledgeRefinementService(
            @Autowired(required = false) AIService aiService) {
        log.info("🎨 创建默认知识提炼服务（基于 AIService）");
        if (aiService == null) {
            log.warn("⚠️ AIService 不可用，知识提炼将仅使用规则方式");
        }
        return new top.yumbo.ai.omni.knowledge.registry.network.DefaultKnowledgeRefinementService(aiService);
    }
}

