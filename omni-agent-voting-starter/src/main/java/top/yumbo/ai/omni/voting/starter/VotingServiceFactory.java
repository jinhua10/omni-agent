package top.yumbo.ai.omni.voting.starter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.voting.VotingService;
import top.yumbo.ai.omni.voting.starter.impl.memory.MemoryVotingService;
import top.yumbo.ai.omni.voting.starter.impl.mongodb.MongoVotingService;
import top.yumbo.ai.omni.voting.starter.impl.mongodb.MongoVotingProperties;
import top.yumbo.ai.omni.voting.starter.impl.redis.RedisVotingService;
import top.yumbo.ai.omni.voting.starter.impl.redis.RedisVotingProperties;

/**
 * Voting 服务工厂
 *
 * <p>根据配置动态创建对应的 Voting 服务实现</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class VotingServiceFactory {

    private final VotingProperties properties;
    private final ObjectProvider<VotingService> votingServiceProvider;

    @Autowired(required = false)
    private MongoTemplate mongoTemplate;

    @Autowired(required = false)
    private RedisTemplate<String, Object> redisTemplate;

    private VotingService votingService;

    public VotingServiceFactory(
            VotingProperties properties,
            ObjectProvider<VotingService> votingServiceProvider) {
        this.properties = properties;
        this.votingServiceProvider = votingServiceProvider;
    }

    /**
     * 获取或创建 Voting 服务
     */
    public VotingService getVotingService() {
        if (votingService == null) {
            votingService = createVotingService();
        }
        return votingService;
    }

    /**
     * 创建 Voting 服务实例
     */
    private VotingService createVotingService() {
        log.info("📋 创建 Voting 服务，类型: {}", properties.getType());

        // 优先从 Spring 容器获取
        VotingService service = votingServiceProvider.getIfAvailable();
        if (service != null) {
            log.info("✅ 使用容器中的 Voting 服务: {}", service.getClass().getSimpleName());
            return service;
        }

        // 根据配置类型创建
        String type = properties.getType().toLowerCase();
        switch (type) {
            case "mongodb":
            case "mongo":
                return createMongoDBVotingService();

            case "redis":
                return createRedisVotingService();

            case "elasticsearch":
            case "es":
                log.warn("Elasticsearch Voting 实现尚未完全迁移，使用 Memory 服务");
                return createMemoryVotingService();

            case "memory":
            default:
                return createMemoryVotingService();
        }
    }

    /**
     * 创建 Memory Voting 服务
     */
    private VotingService createMemoryVotingService() {
        try {
            MemoryVotingService service = new MemoryVotingService();
            log.info("✅ 创建 Memory Voting 服务成功");
            return service;
        } catch (Exception e) {
            log.error("创建 Memory Voting 服务失败", e);
            throw new RuntimeException("创建 Memory Voting 服务失败", e);
        }
    }

    /**
     * 创建 MongoDB Voting 服务
     */
    private VotingService createMongoDBVotingService() {
        if (mongoTemplate == null) {
            log.warn("⚠️ MongoTemplate 未配置，无法创建 MongoDB Voting 服务，使用 Memory 服务");
            return createMemoryVotingService();
        }

        try {
            MongoVotingProperties mongoProps = new MongoVotingProperties();
            mongoProps.setCollectionName(properties.getMongodb().getCollectionName());
            mongoProps.setEnableIndexes(properties.getMongodb().getEnableIndexes());

            MongoVotingService service = new MongoVotingService(mongoTemplate, mongoProps);
            log.info("✅ 创建 MongoDB Voting 服务成功");
            return service;
        } catch (Exception e) {
            log.error("创建 MongoDB Voting 服务失败", e);
            return createMemoryVotingService();
        }
    }

    /**
     * 创建 Redis Voting 服务
     */
    private VotingService createRedisVotingService() {
        if (redisTemplate == null) {
            log.warn("⚠️ RedisTemplate 未配置，无法创建 Redis Voting 服务，使用 Memory 服务");
            return createMemoryVotingService();
        }

        try {
            RedisVotingProperties redisProps = new RedisVotingProperties();
            redisProps.setKeyPrefix(properties.getRedis().getKeyPrefix());
            redisProps.setTtl(properties.getRedis().getTtl());

            RedisVotingService service = new RedisVotingService(redisTemplate, redisProps);
            log.info("✅ 创建 Redis Voting 服务成功");
            return service;
        } catch (Exception e) {
            log.error("创建 Redis Voting 服务失败", e);
            return createMemoryVotingService();
        }
    }
}

