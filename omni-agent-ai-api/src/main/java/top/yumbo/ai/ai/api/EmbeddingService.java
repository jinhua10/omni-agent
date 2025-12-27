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

    /**
     * 动态检测模型维度（通过实际调用）⭐
     *
     * <p>用于未注册模型的维度自动检测</p>
     *
     * @return 实际向量维度
     */
    default int detectDimension() {
        try {
            float[] testVector = embed("test");
            return testVector.length;
        } catch (Exception e) {
            return getDimension(); // 降级到配置的维度
        }
    }

    /**
     * 验证模型兼容性（检查维度是否匹配）⭐
     *
     * @param expectedDimension 期望的维度
     * @return 是否兼容
     */
    default boolean isCompatible(int expectedDimension) {
        int actualDimension = getDimension();
        return actualDimension == expectedDimension;
    }
}

