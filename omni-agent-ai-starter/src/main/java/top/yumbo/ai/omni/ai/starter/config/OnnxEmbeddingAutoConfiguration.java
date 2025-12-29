package top.yumbo.ai.omni.ai.starter.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.ai.api.EmbeddingService;
import top.yumbo.ai.omni.ai.starter.impl.OnnxEmbeddingService;
import top.yumbo.ai.omni.ai.starter.impl.SharedOnnxModelManager;
import top.yumbo.ai.omni.ai.starter.properties.OnnxEmbeddingProperties;

/**
 * ONNX Embedding 自动配置
 * (ONNX Embedding Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnClass(ai.onnxruntime.OrtEnvironment.class)
@ConditionalOnProperty(prefix = "omni-agent.embedding.onnx", name = "enabled", havingValue = "true", matchIfMissing = false)
@EnableConfigurationProperties(OnnxEmbeddingProperties.class)
public class OnnxEmbeddingAutoConfiguration {

    @Autowired(required = false)
    private SharedOnnxModelManager sharedModelManager;

    /**
     * 创建共享模型管理器（如果不存在）
     */
    @Bean
    @ConditionalOnMissingBean(SharedOnnxModelManager.class)
    public SharedOnnxModelManager sharedOnnxModelManager() {
        return new SharedOnnxModelManager();
    }

    /**
     * 创建 ONNX Embedding 服务 Bean
     */
    @Bean
    @ConditionalOnMissingBean(EmbeddingService.class)
    public EmbeddingService onnxEmbeddingService(OnnxEmbeddingProperties properties) {
        try {
            log.info("正在初始化 ONNX Embedding 服务...");
            log.info("模型路径: {}", properties.getModelPath());
            log.info("最大序列长度: {}", properties.getMaxSequenceLength());

            // 使用共享模型管理器（如果可用）
            SharedOnnxModelManager modelManager = sharedModelManager != null ?
                    sharedModelManager : new SharedOnnxModelManager();

            OnnxEmbeddingService service = new OnnxEmbeddingService(
                    modelManager,
                    properties.getModelPath(),
                    properties.getMaxSequenceLength()
            );

            log.info("✅ ONNX Embedding 服务初始化成功");
            log.info("   模型: {}", service.getEmbeddingModel());
            log.info("   维度: {}", service.getDimension());
            log.info("   共享模式: {}", sharedModelManager != null ? "已启用" : "未启用");

            return service;

        } catch (Exception e) {
            log.error("❌ ONNX Embedding 服务初始化失败", e);
            log.error("请检查:");
            log.error("  1. 模型文件是否存在: {}", properties.getModelPath());
            log.error("  2. ONNX Runtime 依赖是否已添加");
            log.error("  3. 模型格式是否正确（ONNX 格式）");
            throw new RuntimeException("ONNX Embedding 服务初始化失败", e);
        }
    }
}

