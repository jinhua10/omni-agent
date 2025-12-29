package top.yumbo.ai.omni.rag.adapter.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.ArrayList;
import java.util.List;

/**
 * RAG 适配器配置属性（统一使用数组配置）
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.rag")
public class RagAdapterProperties {

    /**
     * 向量维度（全局默认值）
     */
    private Integer vectorDimension = 768;

    /**
     * RAG 实例列表
     *
     * <p>如果列表为空，系统会自动创建一个默认的 File 实例</p>
     */
    private List<RagInstanceConfig> instances = new ArrayList<>();

    /**
     * RAG 实例配置
     */
    @Data
    public static class RagInstanceConfig {
        /**
         * 实例 ID（唯一标识）
         * 如果不设置，将自动生成
         */
        private String id;

        /**
         * 实例名称（描述性）
         */
        private String name;

        /**
         * 实例类型：file, sqlite, mongodb, redis, h2, elasticsearch, mock
         */
        private String type = "file";

        /**
         * 是否为主实例（primary）
         * 主实例会被自动注入到 @Autowired RagService
         */
        private boolean primary = false;

        /**
         * 向量维度（每个实例可以使用不同的维度）
         * 如果不设置则使用 embedding.dimension 或全局配置
         */
        private Integer vectorDimension;

        /**
         * 嵌入模型配置（每个实例可以使用不同的模型）
         */
        private EmbeddingConfig embedding;

        /**
         * File/Lucene 配置
         */
        private FileConfig file;

        /**
         * SQLite 配置
         */
        private SQLiteConfig sqlite;

        /**
         * MongoDB 配置
         */
        private MongoDBConfig mongodb;

        /**
         * Redis 配置
         */
        private RedisConfig redis;

        /**
         * H2 配置
         */
        private H2Config h2;

        /**
         * Elasticsearch 配置
         */
        private ElasticsearchConfig elasticsearch;

        /**
         * 获取实际的向量维度
         * 优先级：实例配置 > 嵌入模型配置 > 全局配置
         */
        public int getEffectiveVectorDimension(int globalDimension) {
            if (vectorDimension != null) {
                return vectorDimension;
            }
            if (embedding != null && embedding.getDimension() != null) {
                return embedding.getDimension();
            }
            return globalDimension;
        }

        /**
         * 获取或生成实例 ID
         */
        public String getOrGenerateId() {
            if (id == null || id.isEmpty()) {
                return type + "-" + System.currentTimeMillis();
            }
            return id;
        }
    }

    /**
     * 嵌入模型配置
     */
    @Data
    public static class EmbeddingConfig {
        /**
         * 嵌入模型类型：onnx, online, ollama
         */
        private String provider = "onnx";

        /**
         * 模型名称或路径
         */
        private String model;

        /**
         * 向量维度（根据模型自动确定）
         */
        private Integer dimension;

        /**
         * ONNX 配置
         */
        private OnnxConfig onnx;

        /**
         * Online API 配置
         */
        private OnlineConfig online;

        /**
         * Ollama 配置
         */
        private OllamaConfig ollama;
    }

    /**
     * ONNX 嵌入模型配置
     */
    @Data
    public static class OnnxConfig {
        /**
         * 模型文件路径
         */
        private String modelPath = "models/bge-base-zh/model.onnx";

        /**
         * 词汇表路径
         */
        private String vocabPath = "models/bge-base-zh/vocab.txt";

        /**
         * 最大序列长度
         */
        private Integer maxLength = 512;

        /**
         * 是否使用池化
         */
        private Boolean pooling = true;
    }

    /**
     * Online API 嵌入模型配置
     */
    @Data
    public static class OnlineConfig {
        /**
         * API 端点
         */
        private String endpoint;

        /**
         * API Key
         */
        private String apiKey;

        /**
         * 模型名称
         */
        private String model = "text-embedding-ada-002";

        /**
         * 超时时间（毫秒）
         */
        private Integer timeout = 30000;
    }

    /**
     * Ollama 嵌入模型配置
     */
    @Data
    public static class OllamaConfig {
        /**
         * Ollama 服务地址
         */
        private String baseUrl = "http://localhost:11434";

        /**
         * 模型名称
         */
        private String model = "nomic-embed-text";

        /**
         * 超时时间（毫秒）
         */
        private Integer timeout = 30000;
    }

    @Data
    public static class FileConfig {
        private String indexPath = "data/rag-index/file";
        private Double ramBufferSizeMb = 16.0;
    }

    @Data
    public static class SQLiteConfig {
        private String databasePath = "data/rag-index/sqlite/rag.db";
        private Boolean initDatabase = true;
        private Boolean enableFts = true;
    }

    @Data
    public static class MongoDBConfig {
        private String collectionName = "rag_documents";
        private Boolean enableTextSearch = true;
    }

    @Data
    public static class RedisConfig {
        private String keyPrefix = "omni-rag:";
        private Long documentTtl = 86400L; // 24小时
        private Boolean enableTextIndex = true;
    }

    @Data
    public static class H2Config {
        private String databasePath = "data/rag-index/h2/rag";
        private Boolean initDatabase = true;
        private Boolean enableFullText = true;
    }

    @Data
    public static class ElasticsearchConfig {
        private String host = "localhost";
        private Integer port = 9200;
        private String indexPrefix = "omni-rag-";
    }

    /**
     * 检查是否配置了多个实例
     */
    public boolean hasMultipleInstances() {
        return instances.size() > 1;
    }

    /**
     * 获取主实例配置
     */
    public RagInstanceConfig getPrimaryInstance() {
        return instances.stream()
                .filter(RagInstanceConfig::isPrimary)
                .findFirst()
                .orElseGet(() -> instances.isEmpty() ? null : instances.get(0));
    }
}

