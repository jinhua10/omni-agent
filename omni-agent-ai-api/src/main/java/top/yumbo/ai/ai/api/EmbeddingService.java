package top.yumbo.ai.ai.api;

/**
 * Embedding 服务接口
 * (Embedding Service Interface)
 *
 * <p>用于生成文本的向量表示</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface EmbeddingService {

    /**
     * 生成单个文本的向量
     * @param text 文本
     * @return 向量数组
     */
    float[] embed(String text);

    /**
     * 批量生成向量
     * @param texts 文本列表
     * @return 向量数组列表
     */
    java.util.List<float[]> embedBatch(java.util.List<String> texts);

    /**
     * 获取向量维度
     * @return 维度
     */
    int getDimension();

    /**
     * 获取当前使用的 Embedding 模型
     * @return 模型名称
     */
    String getEmbeddingModel();
}

