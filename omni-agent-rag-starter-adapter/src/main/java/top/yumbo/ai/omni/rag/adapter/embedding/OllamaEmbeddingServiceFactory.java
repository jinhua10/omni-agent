package top.yumbo.ai.omni.rag.adapter.embedding;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.ai.starter.impl.OllamaAIService;
import top.yumbo.ai.omni.ai.starter.properties.OllamaProperties;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.adapter.config.RagAdapterProperties;

/**
 * Ollama 嵌入服务工厂
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class OllamaEmbeddingServiceFactory {

    public static RagService create(
            RagAdapterProperties.EmbeddingConfig config,
            String domainId) {

        RagAdapterProperties.OllamaConfig ollamaConfig = config.getOllama();
        if (ollamaConfig == null) {
            log.error("Ollama 配置为空");
            return null;
        }

        try {
            OllamaProperties properties = new OllamaProperties();
            properties.setBaseUrl(ollamaConfig.getBaseUrl());
            properties.setDefaultModel(ollamaConfig.getModel());
            properties.setTimeout(ollamaConfig.getTimeout() != null ?
                    ollamaConfig.getTimeout() : 30000);

            // 创建 RestTemplate
            org.springframework.web.client.RestTemplate restTemplate =
                    new org.springframework.web.client.RestTemplate();

            OllamaAIService aiService = new OllamaAIService(restTemplate, properties);
            RagService ragService = new EmbeddingServiceAdapter(aiService, domainId);

            log.info("✅ Ollama 嵌入服务创建成功");
            log.info("  - Base URL: {}", ollamaConfig.getBaseUrl());
            log.info("  - 模型: {}", ollamaConfig.getModel());
            log.info("  - 超时: {}ms", properties.getTimeout());

            return ragService;

        } catch (Exception e) {
            log.error("创建 Ollama 嵌入服务失败", e);
            return null;
        }
    }
}

