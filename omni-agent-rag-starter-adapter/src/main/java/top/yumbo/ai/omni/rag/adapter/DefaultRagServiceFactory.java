package top.yumbo.ai.omni.rag.adapter;

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

    public DefaultRagServiceFactory(
            RagAdapterProperties properties,
            ObjectProvider<RagService> ragServiceProvider) {
        this.properties = properties;
        this.ragServiceProvider = ragServiceProvider;

        log.info("✅ RAG 服务工厂初始化完成");
        log.info("  - 配置类型: {}", properties.getType());
        log.info("  - 可用实现: {}", ragServiceProvider.stream().count());
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
     * 创建 RAG 服务实例
     *
     * <p>策略：</p>
     * <ul>
     *   <li>优先使用 Spring 容器中已注册的 RagService Bean</li>
     *   <li>如果没有，根据配置的 type 创建对应实现</li>
     *   <li>如果没有可用实现，返回 Mock 实现（开发阶段）</li>
     * </ul>
     */
    private RagService createRagService(String domainId) {
        log.info("📋 为域 {} 创建 RAG 服务，类型: {}", domainId, properties.getType());

        // 优先从 Spring 容器获取
        RagService ragService = ragServiceProvider.getIfAvailable();
        if (ragService != null) {
            log.info("✅ 使用容器中的 RAG 服务: {} (域: {})",
                    ragService.getClass().getSimpleName(), domainId);
            return ragService;
        }

        // 根据配置类型创建
        String type = properties.getType().toLowerCase();
        return switch (type) {
            case "file", "lucene" -> createFileRAGService(domainId);
            case "sqlite" -> createSQLiteRAGService(domainId);
            case "mongodb", "mongo" -> createMongoDBRAGService(domainId);
            case "redis" -> createRedisRAGService(domainId);
            case "h2" -> createH2RAGService(domainId);
            case "elasticsearch", "es" -> {
                log.warn("Elasticsearch RAG 实现尚未迁移，使用 Mock 服务");
                yield createMockRagService(domainId);
            }
            default -> createMockRagService(domainId);
        };
    }

    /**
     * 创建 File/Lucene RAG 服务
     */
    private RagService createFileRAGService(String domainId) {
        try {
            FileRAGProperties fileProps = new FileRAGProperties();
            fileProps.setIndexPath(properties.getFile().getIndexPath());
            fileProps.setRamBufferSizeMb(properties.getFile().getRamBufferSizeMb());

            LuceneRAGService service = new LuceneRAGService(fileProps, domainId);
            service.init();

            log.info("✅ 创建 File/Lucene RAG 服务成功 (域: {})", domainId);
            return service;

        } catch (Exception e) {
            log.error("创建 File/Lucene RAG 服务失败", e);
            return createMockRagService(domainId);
        }
    }

    /**
     * 创建 SQLite RAG 服务
     */
    private RagService createSQLiteRAGService(String domainId) {
        if (jdbcTemplate == null) {
            log.warn("⚠️ JdbcTemplate 未配置，无法创建 SQLite RAG 服务，使用 Mock 服务");
            return createMockRagService(domainId);
        }

        try {
            SQLiteRAGProperties sqliteProps = new SQLiteRAGProperties();
            sqliteProps.setDatabasePath(properties.getSqlite().getDatabasePath());
            sqliteProps.setInitDatabase(properties.getSqlite().getInitDatabase());
            sqliteProps.setEnableFts(properties.getSqlite().getEnableFts());

            SQLiteRAGService service = new SQLiteRAGService(jdbcTemplate, sqliteProps, domainId);
            service.init();

            log.info("✅ 创建 SQLite RAG 服务成功 (域: {})", domainId);
            return service;

        } catch (Exception e) {
            log.error("创建 SQLite RAG 服务失败", e);
            return createMockRagService(domainId);
        }
    }

    /**
     * 创建 MongoDB RAG 服务
     */
    private RagService createMongoDBRAGService(String domainId) {
        if (mongoTemplate == null) {
            log.warn("⚠️ MongoTemplate 未配置，无法创建 MongoDB RAG 服务，使用 Mock 服务");
            return createMockRagService(domainId);
        }

        try {
            MongoDBRAGProperties mongoProps = new MongoDBRAGProperties();
            mongoProps.setCollectionName(properties.getMongodb().getCollectionName());
            mongoProps.setEnableTextSearch(properties.getMongodb().getEnableTextSearch());

            MongoDBRAGService service = new MongoDBRAGService(mongoTemplate, mongoProps, domainId);
            service.init();

            log.info("✅ 创建 MongoDB RAG 服务成功 (域: {})", domainId);
            return service;

        } catch (Exception e) {
            log.error("创建 MongoDB RAG 服务失败", e);
            return createMockRagService(domainId);
        }
    }

    /**
     * 创建 Redis RAG 服务
     */
    private RagService createRedisRAGService(String domainId) {
        if (redisTemplate == null) {
            log.warn("⚠️ RedisTemplate 未配置，无法创建 Redis RAG 服务，使用 Mock 服务");
            return createMockRagService(domainId);
        }

        try {
            RedisRAGProperties redisProps = new RedisRAGProperties();
            redisProps.setKeyPrefix(properties.getRedis().getKeyPrefix());
            redisProps.setDocumentTtl(properties.getRedis().getDocumentTtl());
            redisProps.setEnableTextIndex(properties.getRedis().getEnableTextIndex());

            RedisRAGService service = new RedisRAGService(redisTemplate, redisProps, domainId);
            service.init();

            log.info("✅ 创建 Redis RAG 服务成功 (域: {})", domainId);
            return service;

        } catch (Exception e) {
            log.error("创建 Redis RAG 服务失败", e);
            return createMockRagService(domainId);
        }
    }

    /**
     * 创建 H2 RAG 服务
     */
    private RagService createH2RAGService(String domainId) {
        try {
            H2RAGProperties h2Props = new H2RAGProperties();
            // 使用配置中的数据库路径构建 URL
            String dbPath = properties.getH2().getDatabasePath();
            h2Props.setUrl("jdbc:h2:" + dbPath + ";AUTO_SERVER=TRUE");

            H2RAGService service = new H2RAGService(h2Props, domainId);
            service.init();

            log.info("✅ 创建 H2 RAG 服务成功 (域: {})", domainId);
            return service;

        } catch (Exception e) {
            log.error("创建 H2 RAG 服务失败", e);
            return createMockRagService(domainId);
        }
    }

    /**
     * 创建 Mock RAG 服务（用于开发和测试）
     */
    private RagService createMockRagService(String domainId) {
        log.info("🔧 创建 Mock RAG 服务 (域: {})", domainId);
        return new MockRagService(domainId);
    }
}

