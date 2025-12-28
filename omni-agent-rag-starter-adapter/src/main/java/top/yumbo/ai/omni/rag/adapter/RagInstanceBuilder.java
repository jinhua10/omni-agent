package top.yumbo.ai.omni.rag.adapter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.adapter.impl.elasticsearch.ElasticsearchRAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.elasticsearch.ElasticsearchRAGService;
import top.yumbo.ai.omni.rag.adapter.impl.file.FileRAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.file.LuceneRAGService;
import top.yumbo.ai.omni.rag.adapter.impl.h2.H2RAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.h2.H2RAGService;
import top.yumbo.ai.omni.rag.adapter.impl.mongodb.MongoDBRAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.mongodb.MongoDBRAGService;
import top.yumbo.ai.omni.rag.adapter.impl.redis.RedisRAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.redis.RedisRAGService;
import top.yumbo.ai.omni.rag.adapter.impl.sqlite.SQLiteRAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.sqlite.SQLiteRAGService;

/**
 * RAG 实例构建器
 *
 * <p>根据配置创建不同类型的 RAG 服务实例</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class RagInstanceBuilder {

    private final RagAdapterProperties.RagInstanceConfig config;
    private final int globalVectorDimension;
    private JdbcTemplate jdbcTemplate;
    private org.springframework.data.mongodb.core.MongoTemplate mongoTemplate;
    private org.springframework.data.redis.core.RedisTemplate<String, Object> redisTemplate;
    private co.elastic.clients.elasticsearch.ElasticsearchClient elasticsearchClient;

    public RagInstanceBuilder(RagAdapterProperties.RagInstanceConfig config, int globalVectorDimension) {
        this.config = config;
        this.globalVectorDimension = globalVectorDimension;
    }

    public RagInstanceBuilder withJdbcTemplate(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
        return this;
    }

    public RagInstanceBuilder withMongoTemplate(org.springframework.data.mongodb.core.MongoTemplate mongoTemplate) {
        this.mongoTemplate = mongoTemplate;
        return this;
    }

    public RagInstanceBuilder withRedisTemplate(org.springframework.data.redis.core.RedisTemplate<String, Object> redisTemplate) {
        this.redisTemplate = redisTemplate;
        return this;
    }

    public RagInstanceBuilder withElasticsearchClient(co.elastic.clients.elasticsearch.ElasticsearchClient client) {
        this.elasticsearchClient = client;
        return this;
    }

    /**
     * 构建 RAG 服务实例
     */
    public RagService build() {
        String instanceId = config.getId();
        String type = config.getType().toLowerCase();

        log.info("🔨 构建 RAG 实例: id={}, type={}", instanceId, type);

        try {
            // 1. 创建存储服务
            RagService storageService = switch (type) {
                case "file", "lucene" -> buildFileRAG(instanceId);
                case "sqlite" -> buildSQLiteRAG(instanceId);
                case "mongodb", "mongo" -> buildMongoDBRAG(instanceId);
                case "redis" -> buildRedisRAG(instanceId);
                case "h2" -> buildH2RAG(instanceId);
                case "elasticsearch", "es" -> buildElasticsearchRAG(instanceId);
                case "mock" -> {
                    log.info("✅ 创建 Mock RAG 实例: {}", instanceId);
                    yield new MockRagService(instanceId);
                }
                default -> {
                    log.warn("⚠️ 未知的 RAG 类型: {}, 使用 Mock 服务", type);
                    yield new MockRagService(instanceId);
                }
            };

            // 2. 如果配置了嵌入模型，包装嵌入功能
            if (config.getEmbedding() != null && needsEmbeddingService(type)) {
                RagService embeddingService = buildEmbeddingService(instanceId);
                if (embeddingService != null) {
                    log.info("🎨 为实例 {} 添加嵌入服务装饰器", instanceId);
                    return new EmbeddingRagServiceDecorator(storageService, embeddingService, instanceId);
                } else {
                    log.warn("⚠️ 嵌入服务创建失败，使用纯存储服务（语义搜索不可用）");
                }
            }

            return storageService;

        } catch (Exception e) {
            log.error("❌ 创建 RAG 实例失败: id={}, type={}", instanceId, type, e);
            return new MockRagService(instanceId);
        }
    }

    /**
     * 判断是否需要嵌入服务
     *
     * <p>File/Lucene 可能内置了嵌入功能，其他都需要</p>
     */
    private boolean needsEmbeddingService(String type) {
        return !type.equals("mock");
    }

    /**
     * 构建嵌入服务
     */
    private RagService buildEmbeddingService(String instanceId) {
        RagAdapterProperties.EmbeddingConfig embeddingConfig = config.getEmbedding();
        String provider = embeddingConfig.getProvider().toLowerCase();

        log.info("🧠 创建嵌入服务: provider={}, model={}", provider, embeddingConfig.getModel());

        try {
            return switch (provider) {
                case "onnx" -> buildOnnxEmbeddingService(instanceId, embeddingConfig);
                case "online" -> buildOnlineEmbeddingService(instanceId, embeddingConfig);
                case "ollama" -> buildOllamaEmbeddingService(instanceId, embeddingConfig);
                default -> {
                    log.warn("⚠️ 未知的嵌入服务提供者: {}", provider);
                    yield null;
                }
            };
        } catch (Exception e) {
            log.error("❌ 创建嵌入服务失败: provider={}", provider, e);
            return null;
        }
    }

    /**
     * 创建 ONNX 嵌入服务
     */
    private RagService buildOnnxEmbeddingService(String instanceId, RagAdapterProperties.EmbeddingConfig config) {
        log.info("✅ ONNX 嵌入服务: model={}, dimension={}",
                config.getModel(), config.getDimension());

        // TODO: 实现 ONNX 嵌入服务
        // return new OnnxEmbeddingService(config.getOnnx());

        log.warn("⚠️ ONNX 嵌入服务��未实现");
        return null;
    }

    /**
     * 创建 Online API 嵌入服务
     */
    private RagService buildOnlineEmbeddingService(String instanceId, RagAdapterProperties.EmbeddingConfig config) {
        log.info("✅ Online API 嵌入服务: model={}, endpoint={}",
                config.getModel(), config.getOnline().getEndpoint());

        // TODO: 实现 Online API 嵌入服务
        // return new OnlineEmbeddingService(config.getOnline());

        log.warn("⚠️ Online API 嵌入服务尚未实现");
        return null;
    }

    /**
     * 创建 Ollama 嵌入服务
     */
    private RagService buildOllamaEmbeddingService(String instanceId, RagAdapterProperties.EmbeddingConfig config) {
        return top.yumbo.ai.omni.rag.adapter.embedding.OllamaEmbeddingServiceFactory
                .create(config, instanceId);
    }

    private RagService buildFileRAG(String instanceId) {
        if (config.getFile() == null) {
            throw new IllegalArgumentException("File 配置不能为空");
        }

        FileRAGProperties props = new FileRAGProperties();
        props.setIndexPath(config.getFile().getIndexPath());
        props.setRamBufferSizeMb(config.getFile().getRamBufferSizeMb());

        LuceneRAGService service = new LuceneRAGService(props, instanceId);
        service.init();

        log.info("✅ 创建 File/Lucene RAG 实例成功: {}", instanceId);
        return service;
    }

    private RagService buildSQLiteRAG(String instanceId) {
        if (jdbcTemplate == null) {
            throw new IllegalStateException("JdbcTemplate 未配置，无法创建 SQLite RAG 实例");
        }
        if (config.getSqlite() == null) {
            throw new IllegalArgumentException("SQLite 配置不能为空");
        }

        SQLiteRAGProperties props = new SQLiteRAGProperties();
        props.setDatabasePath(config.getSqlite().getDatabasePath());
        props.setInitDatabase(config.getSqlite().getInitDatabase());
        props.setEnableFts(config.getSqlite().getEnableFts());

        SQLiteRAGService service = new SQLiteRAGService(jdbcTemplate, props, instanceId);
        service.init();

        log.info("✅ 创建 SQLite RAG 实例成功: {}", instanceId);
        return service;
    }

    private RagService buildMongoDBRAG(String instanceId) {
        if (mongoTemplate == null) {
            throw new IllegalStateException("MongoTemplate 未配置，无法创建 MongoDB RAG 实例");
        }
        if (config.getMongodb() == null) {
            throw new IllegalArgumentException("MongoDB 配置不能为空");
        }

        MongoDBRAGProperties props = new MongoDBRAGProperties();
        props.setCollectionName(config.getMongodb().getCollectionName());
        props.setEnableTextSearch(config.getMongodb().getEnableTextSearch());

        MongoDBRAGService service = new MongoDBRAGService(mongoTemplate, props, instanceId);
        service.init();

        log.info("✅ 创建 MongoDB RAG 实例成功: {}", instanceId);
        return service;
    }

    private RagService buildRedisRAG(String instanceId) {
        if (redisTemplate == null) {
            throw new IllegalStateException("RedisTemplate 未配置，无法创建 Redis RAG 实例");
        }
        if (config.getRedis() == null) {
            throw new IllegalArgumentException("Redis 配置不能为空");
        }

        RedisRAGProperties props = new RedisRAGProperties();
        props.setKeyPrefix(config.getRedis().getKeyPrefix());
        props.setDocumentTtl(config.getRedis().getDocumentTtl());
        props.setEnableTextIndex(config.getRedis().getEnableTextIndex());

        RedisRAGService service = new RedisRAGService(redisTemplate, props, instanceId);
        service.init();

        log.info("✅ 创建 Redis RAG 实例成功: {}", instanceId);
        return service;
    }

    private RagService buildH2RAG(String instanceId) {
        if (config.getH2() == null) {
            throw new IllegalArgumentException("H2 配置不能为空");
        }

        H2RAGProperties props = new H2RAGProperties();
        String dbPath = config.getH2().getDatabasePath();
        props.setUrl("jdbc:h2:" + dbPath + ";AUTO_SERVER=TRUE");

        H2RAGService service = new H2RAGService(props, instanceId);
        service.init();

        log.info("✅ 创建 H2 RAG 实例成功: {}", instanceId);
        return service;
    }

    private RagService buildElasticsearchRAG(String instanceId) {
        if (elasticsearchClient == null) {
            throw new IllegalStateException("ElasticsearchClient 未配置，无法创建 Elasticsearch RAG 实例");
        }
        if (config.getElasticsearch() == null) {
            throw new IllegalArgumentException("Elasticsearch 配置不能为空");
        }

        ElasticsearchRAGProperties props = new ElasticsearchRAGProperties();
        props.setIndexName(config.getElasticsearch().getIndexPrefix() + instanceId);
        props.setNumberOfShards(3);
        props.setNumberOfReplicas(1);
        props.setVectorDimension(config.getEffectiveVectorDimension(globalVectorDimension));
        props.setRefreshAfterWrite(false);

        ElasticsearchRAGService service = new ElasticsearchRAGService(
                elasticsearchClient,
                props,
                instanceId
        );
        service.init();

        log.info("✅ 创建 Elasticsearch RAG 实例成功: {}", instanceId);
        return service;
    }
}

