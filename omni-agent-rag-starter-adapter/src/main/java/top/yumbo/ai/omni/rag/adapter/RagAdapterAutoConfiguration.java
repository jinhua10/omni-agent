package top.yumbo.ai.omni.rag.adapter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.core.JdbcTemplate;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.RagServiceFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * RAG 适配器自动配置（统一配置方式）
 *
 * <p>支持单实例和多实例配置，统一使用数组方式</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@AutoConfiguration
@EnableConfigurationProperties(RagAdapterProperties.class)
public class RagAdapterAutoConfiguration {

    // 可选依赖使用字段注入，避免类型加载问题
    private Object mongoTemplate;
    private Object redisTemplate;
    private Object elasticsearchClient;

    @Autowired(required = false)
    public void setMongoTemplate(Object mongoTemplate) {
        this.mongoTemplate = mongoTemplate;
    }

    @Autowired(required = false)
    public void setRedisTemplate(Object redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    @Autowired(required = false)
    public void setElasticsearchClient(Object elasticsearchClient) {
        this.elasticsearchClient = elasticsearchClient;
    }

    /**
     * 创建 RAG 服务实例（支持单实例和多实例）
     */
    @Bean
    public Map<String, RagService> ragServices(
            RagAdapterProperties properties,
            ObjectProvider<JdbcTemplate> jdbcTemplate) {

        Map<String, RagService> services = new HashMap<>();
        List<RagAdapterProperties.RagInstanceConfig> instances = properties.getInstances();

        // 如果没有配置任何实例，创建默认实例
        if (instances.isEmpty()) {
            log.info("📋 未配置 RAG 实例，创建默认 File 实例");

            RagAdapterProperties.RagInstanceConfig defaultConfig = new RagAdapterProperties.RagInstanceConfig();
            defaultConfig.setId("default");
            defaultConfig.setName("默认 File 实例");
            defaultConfig.setType("file");
            defaultConfig.setPrimary(true);

            RagAdapterProperties.FileConfig fileConfig = new RagAdapterProperties.FileConfig();
            defaultConfig.setFile(fileConfig);

            instances.add(defaultConfig);
        }

        log.info("🔧 配置 RAG 服务");
        log.info("  - 实例数量: {}", instances.size());
        log.info("  - 全局向量维度: {}", properties.getVectorDimension());

        // 创建每个实例
        for (RagAdapterProperties.RagInstanceConfig instanceConfig : instances) {
            String instanceId = instanceConfig.getOrGenerateId();

            log.info("📋 创建 RAG 实例: id={}, type={}, primary={}",
                    instanceId, instanceConfig.getType(), instanceConfig.isPrimary());

            try {
                RagInstanceBuilder builder = new RagInstanceBuilder(instanceConfig, properties.getVectorDimension())
                        .withJdbcTemplate(jdbcTemplate.getIfAvailable())
                        .withMongoTemplate(mongoTemplate)
                        .withRedisTemplate(redisTemplate)
                        .withElasticsearchClient(elasticsearchClient);

                RagService service = builder.build();
                services.put(instanceId, service);

                log.info("✅ RAG 实例创建成功: {}", instanceId);
            } catch (Exception e) {
                log.error("❌ 创建 RAG 实例失败: {}", instanceId, e);
                // 降级为 Mock 服务
                services.put(instanceId, new MockRagService(instanceId));
            }
        }

        log.info("✅ 所有 RAG 实例创建完成，共 {} 个", services.size());
        return services;
    }

    /**
     * 创建主 RAG 服务（自动选择 primary 实例）
     */
    @Bean
    @Primary
    @ConditionalOnMissingBean(RagService.class)
    public RagService ragService(
            RagAdapterProperties properties,
            Map<String, RagService> ragServices) {

        log.info("🎯 选择主 RAG 服务实例");

        // 查找标记为 primary 的实例
        RagAdapterProperties.RagInstanceConfig primaryConfig = properties.getPrimaryInstance();
        if (primaryConfig != null) {
            String primaryId = primaryConfig.getOrGenerateId();
            RagService service = ragServices.get(primaryId);
            if (service != null) {
                log.info("✅ 主 RAG 服务: {} (id={})", primaryConfig.getName(), primaryId);
                return service;
            }
        }

        // 如果没有标记为 primary 的，使用第一个
        if (!ragServices.isEmpty()) {
            String firstId = ragServices.keySet().iterator().next();
            log.info("⚠️ 未找到标记为 primary 的实例，使用第一个: {}", firstId);
            return ragServices.get(firstId);
        }

        // 降级为 Mock
        log.warn("⚠️ 未找到任何 RAG 实例，使用 Mock 服务");
        return new MockRagService("default");
    }

    /**
     * RAG 服务注册表
     */
    @Bean
    public RagServiceRegistry ragServiceRegistry(Map<String, RagService> ragServices) {
        return new RagServiceRegistry(ragServices);
    }

    /**
     * RAG 服务工厂（兼容旧版 API）
     */
    @Bean
    @ConditionalOnMissingBean
    public RagServiceFactory ragServiceFactory(
            RagAdapterProperties properties,
            ObjectProvider<RagService> ragServiceProvider) {

        log.info("🔧 配置 RAG 服务工厂（兼容模式）");

        return new DefaultRagServiceFactory(
                properties,
                ragServiceProvider
        );
    }
}

