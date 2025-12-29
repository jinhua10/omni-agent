package top.yumbo.ai.omni.rag.adapter.embedding;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.ai.api.EmbeddingService;
import top.yumbo.ai.omni.ai.starter.impl.OnnxEmbeddingService;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.adapter.config.RagAdapterProperties;

/**
 * ONNX 嵌入服务工厂
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class OnnxEmbeddingServiceFactory {

    public static RagService create(
            RagAdapterProperties.EmbeddingConfig config,
            String domainId) {

        RagAdapterProperties.OnnxConfig onnxConfig = config.getOnnx();
        if (onnxConfig == null) {
            log.error("ONNX 配置为空");
            return null;
        }

        try {
            String modelPath = onnxConfig.getModelPath();
            int maxLength = onnxConfig.getMaxLength() != null ?
                    onnxConfig.getMaxLength() : 512;

            // 使用模型路径构造
            EmbeddingService embeddingService = new OnnxEmbeddingService(modelPath);
            RagService ragService = new EmbeddingServiceAdapter(embeddingService, domainId);

            log.info("✅ ONNX 嵌入服务创建成功");
            log.info("  - 模型路径: {}", modelPath);
            log.info("  - 最大长度: {}", maxLength);

            return ragService;

        } catch (Exception e) {
            log.error("创建 ONNX 嵌入服务失败", e);
            return null;
        }
    }
}

