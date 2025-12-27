package top.yumbo.ai.ai.api;

import lombok.Builder;
import lombok.Data;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Embedding 模型注册表
 * (Embedding Model Registry)
 *
 * <p>动态管理 Embedding 模型的元数据，避免硬编码</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
public class EmbeddingModelRegistry {

    private static final Map<String, ModelMetadata> MODELS = new ConcurrentHashMap<>();

    /**
     * 模型元数据
     */
    @Data
    @Builder
    public static class ModelMetadata {
        private String modelName;      // 模型名称
        private int dimension;          // 向量维度
        private String provider;        // 提供商
        private String description;     // 描述
        private boolean verified;       // 是否已验证
    }

    static {
        // 注册常见模型（初始数据）
        registerCommonModels();
    }

    /**
     * 注册常见模型
     */
    private static void registerCommonModels() {
        // OpenAI 模型
        register("text-embedding-3-small", 1536, "openai", "OpenAI Embedding v3 Small");
        register("text-embedding-3-large", 3072, "openai", "OpenAI Embedding v3 Large");
        register("text-embedding-ada-002", 1536, "openai", "OpenAI Ada v2");

        // Ollama 模型
        register("nomic-embed-text", 768, "ollama", "Nomic Embed Text");
        register("mxbai-embed-large", 1024, "ollama", "MxBai Embed Large");
        register("all-minilm", 384, "ollama", "All MiniLM");
        register("snowflake-arctic-embed", 1024, "ollama", "Snowflake Arctic Embed");

        // ONNX 本地模型
        register("bge-base-zh-v1.5", 768, "onnx", "BGE Base Chinese v1.5");
        register("bge-large-zh", 1024, "onnx", "BGE Large Chinese");
        register("bge-m3", 1024, "onnx", "BGE M3 Multilingual");
        register("text2vec-base-chinese", 768, "onnx", "Text2Vec Base Chinese");

        // 阿里云 DashScope
        register("text-embedding-v1", 1536, "dashscope", "DashScope Embedding v1");
        register("text-embedding-v2", 1536, "dashscope", "DashScope Embedding v2");
    }

    /**
     * 注册模型
     */
    public static void register(String modelName, int dimension, String provider, String description) {
        MODELS.put(modelName, ModelMetadata.builder()
                .modelName(modelName)
                .dimension(dimension)
                .provider(provider)
                .description(description)
                .verified(true)
                .build());
    }

    /**
     * 获取模型维度
     *
     * @param modelName 模型名称
     * @return 维度，如果未注册则返回 null
     */
    public static Integer getDimension(String modelName) {
        ModelMetadata metadata = MODELS.get(modelName);
        return metadata != null ? metadata.getDimension() : null;
    }

    /**
     * 获取模型元数据
     */
    public static ModelMetadata getMetadata(String modelName) {
        return MODELS.get(modelName);
    }

    /**
     * 检查模型是否已注册
     */
    public static boolean isRegistered(String modelName) {
        return MODELS.containsKey(modelName);
    }

    /**
     * 获取所有已注册模型
     */
    public static Map<String, ModelMetadata> getAllModels() {
        return new ConcurrentHashMap<>(MODELS);
    }

    /**
     * 移除模型
     */
    public static void unregister(String modelName) {
        MODELS.remove(modelName);
    }
}

