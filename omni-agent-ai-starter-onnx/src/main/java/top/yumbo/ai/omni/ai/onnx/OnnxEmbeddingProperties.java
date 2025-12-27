package top.yumbo.ai.omni.ai.onnx;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * ONNX Embedding 配置属性
 * (ONNX Embedding Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.embedding.onnx")
public class OnnxEmbeddingProperties {

    /**
     * 是否启用 ONNX Embedding 服务
     */
    private boolean enabled = true;

    /**
     * ONNX 模型文件路径
     *
     * 支持的路径格式：
     * 1. 相对路径：./models/bge-base-zh/model.onnx
     * 2. 绝对路径：/data/models/bge-base-zh/model.onnx
     * 3. classpath：models/bge-base-zh/model.onnx
     */
    private String modelPath = "./models/bge-base-zh/model.onnx";

    /**
     * 最大序列长度
     *
     * 不同模型的推荐值：
     * - bge-base-zh-v1.5: 512
     * - bge-m3: 8192
     * - text2vec-base-chinese: 512
     */
    private int maxSequenceLength = 512;

    /**
     * 批处理大小
     * 用于批量处理时的优化
     */
    private int batchSize = 32;
}

