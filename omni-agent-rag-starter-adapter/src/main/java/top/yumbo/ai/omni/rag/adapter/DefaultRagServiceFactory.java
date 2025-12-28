package top.yumbo.ai.omni.rag.adapter;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.RagServiceFactory;
import top.yumbo.ai.omni.rag.adapter.impl.file.FileRAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.file.LuceneRAGService;
import top.yumbo.ai.omni.rag.adapter.impl.sqlite.SQLiteRAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.sqlite.SQLiteRAGService;
import top.yumbo.ai.omni.rag.adapter.impl.mongodb.MongoDBRAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.mongodb.MongoDBRAGService;
import top.yumbo.ai.omni.rag.adapter.impl.redis.RedisRAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.redis.RedisRAGService;
import top.yumbo.ai.omni.rag.adapter.impl.h2.H2RAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.h2.H2RAGService;
import top.yumbo.ai.omni.rag.adapter.impl.elasticsearch.ElasticsearchRAGProperties;
import top.yumbo.ai.omni.rag.adapter.impl.elasticsearch.ElasticsearchRAGService;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * RAG 服务工厂实现
 *
 * <p>管理不同知识域的 RAG 服务实例</p>
 * <p>支持多种数据源：File, SQLite, MongoDB, Redis, H2, Elasticsearch</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class DefaultRagServiceFactory implements RagServiceFactory {

    /**
     * 域 RAG 服务缓存
     * key: domainId, value: RagService
     */
    private final Map<String, RagService> ragServiceCache = new ConcurrentHashMap<>();

    /**
     * RAG 配置
     */
    private final RagAdapterProperties properties;

    /**
     * RAG 服务提供者（从 Spring 容器中获取）
     */
    private final ObjectProvider<RagService> ragServiceProvider;

    /**
     * JDBC Template（可选，用于 SQLite/H2）
     */
    @Autowired(required = false)
    private JdbcTemplate jdbcTemplate;

    /**
     * MongoDB Template（可选）
     */
    @Autowired(required = false)
    private MongoTemplate mongoTemplate;

    /**
     * Redis Template（可选）
     */
    @Autowired(required = false)
    private RedisTemplate<String, Object> redisTemplate;

    /**
     * Elasticsearch Client（可选）
     */
    @Autowired(required = false)
    private ElasticsearchClient elasticsearchClient;

    /**
     * 主构造函数（只接收必需参数）
     */
    public DefaultRagServiceFactory(
            RagAdapterProperties properties,
            ObjectProvider<RagService> ragServiceProvider,
            JdbcTemplate jdbcTemplate) {
        this.properties = properties;
        this.ragServiceProvider = ragServiceProvider;
        this.jdbcTemplate = jdbcTemplate;

        log.info("✅ RAG 服务工厂初始化完成（兼容模式）");
        log.info("  - 实例数量: {}", properties.getInstances().size());
        log.info("  - 可用实现: {}", ragServiceProvider.stream().count());
        log.info("  - JDBC 可用: {}", jdbcTemplate != null);
    }

    @Override
    public RagService getOrCreateRagService(String domainId) {
        return ragServiceCache.computeIfAbsent(domainId, this::createRagService);
    }

    @Override
    public boolean hasRagService(String domainId) {
        return ragServiceCache.containsKey(domainId);
    }

    @Override
    public void removeRagService(String domainId) {
        RagService removed = ragServiceCache.remove(domainId);
        if (removed != null) {
            log.info("✅ 移除域 {} 的 RAG 服务", domainId);
        }
    }

    /**
     * 创建 RAG 服务实例（兼容旧版 API）
     *
     * <p>注意：新版本建议直接使用 RagServiceRegistry 或注入 Map&lt;String, RagService&gt;</p>
     */
    private RagService createRagService(String domainId) {
        log.info("📋 为域 {} 创建 RAG 服务（兼容模式）", domainId);

        // 优先从 Spring 容器获取
        RagService ragService = ragServiceProvider.getIfAvailable();
        if (ragService != null) {
            log.info("✅ 使用容器中的 RAG 服务: {} (域: {})",
                    ragService.getClass().getSimpleName(), domainId);
            return ragService;
        }

        // 如果有配置实例，使用第一个配置创建
        if (!properties.getInstances().isEmpty()) {
            RagAdapterProperties.RagInstanceConfig config = properties.getInstances().get(0);
            try {
                RagInstanceBuilder builder = new RagInstanceBuilder(config, properties.getVectorDimension())
                        .withJdbcTemplate(jdbcTemplate)
                        .withMongoTemplate(mongoTemplate)
                        .withRedisTemplate(redisTemplate)
                        .withElasticsearchClient(elasticsearchClient);
                return builder.build();
            } catch (Exception e) {
                log.error("创建 RAG 服务失败", e);
            }
        }

        // 降级为 Mock
        return createMockRagService(domainId);
    }


    /**
     * 创建 Mock RAG 服务（用于开发和测试）
     */
    private RagService createMockRagService(String domainId) {
        log.info("🔧 创建 Mock RAG 服务 (域: {})", domainId);
        return new MockRagService(domainId);
    }
}

