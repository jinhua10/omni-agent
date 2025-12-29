package top.yumbo.ai.omni.rag.adapter.embedding;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.client.RestTemplate;
import top.yumbo.ai.omni.ai.starter.impl.OnlineAPIAIService;
import top.yumbo.ai.omni.ai.starter.properties.OnlineAPIProperties;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.adapter.config.RagAdapterProperties;

/**
 * Online API 嵌入服务工厂
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class OnlineEmbeddingServiceFactory {

    public static RagService create(
            RagAdapterProperties.EmbeddingConfig config,
            String domainId) {

        RagAdapterProperties.OnlineConfig onlineConfig = config.getOnline();
        if (onlineConfig == null) {
            log.error("Online API 配置为空");
            return null;
        }

        try {
            OnlineAPIProperties properties = new OnlineAPIProperties();
            properties.setEndpoint(onlineConfig.getEndpoint());
            properties.setApiKey(onlineConfig.getApiKey());
            properties.setEmbeddingModel(onlineConfig.getModel());
            properties.setTimeout(onlineConfig.getTimeout() != null ?
                    onlineConfig.getTimeout() : 30000);

            // 创建 RestTemplate
            RestTemplate restTemplate = new RestTemplate();

            OnlineAPIAIService aiService = new OnlineAPIAIService(restTemplate, properties);
            RagService ragService = new EmbeddingServiceAdapter(aiService, domainId);

            log.info("✅ Online API 嵌入服务创建成功");
            log.info("  - Endpoint: {}", onlineConfig.getEndpoint());
            log.info("  - 模型: {}", onlineConfig.getModel());
            log.info("  - 超时: {}ms", properties.getTimeout());

            return ragService;

        } catch (Exception e) {
            log.error("创建 Online API 嵌入服务失败", e);
            return null;
        }
    }
}

