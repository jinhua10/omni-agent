package top.yumbo.ai.omni.rag.adapter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.core.JdbcTemplate;
import top.yumbo.ai.omni.rag.RagService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * RAG 适配器自动配置
 *
 * <p>根据配置动态创建多个 RAG 服务实例，支持向量化操作</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@AutoConfiguration
@EnableConfigurationProperties(RagAdapterProperties.class)
public class RagAdapterAutoConfiguration {

    /**
     * 创建所有 RAG 服务实例
     */
    @Bean
    public Map<String, RagService> ragServices(
            RagAdapterProperties properties,
            ObjectProvider<JdbcTemplate> jdbcTemplate,
            ObjectProvider<Object> mongoTemplate,
            ObjectProvider<Object> redisTemplate,
            ObjectProvider<Object> elasticsearchClient) {

        Map<String, RagService> services = new HashMap<>();
        List<RagAdapterProperties.RagInstanceConfig> instances = properties.getInstances();

        // 如果没有配置实例，创建默认 File 实例
        if (instances.isEmpty()) {
            instances = createDefaultInstance();
        }

        log.info("🚀 开始创建 RAG 实例，共 {} 个", instances.size());

        // 创建每个实例
        for (RagAdapterProperties.RagInstanceConfig config : instances) {
            String instanceId = config.getOrGenerateId();

            try {
                RagService service = new RagInstanceBuilder(config, properties.getVectorDimension())
                        .withJdbcTemplate(jdbcTemplate.getIfAvailable())
                        .withMongoTemplate(mongoTemplate.getIfAvailable())
                        .withRedisTemplate(redisTemplate.getIfAvailable())
                        .withElasticsearchClient(elasticsearchClient.getIfAvailable())
                        .build();

                services.put(instanceId, service);
                log.info("✅ 实例创建成功: id={}, type={}", instanceId, config.getType());

            } catch (Exception e) {
                log.error("❌ 实例创建失败: id={}, 使用 Mock 降级", instanceId, e);
                services.put(instanceId, new MockRagService(instanceId));
            }
        }

        log.info("✅ RAG 实例创建完成，共 {} 个", services.size());
        return services;
    }

    /**
     * 主 RAG 服务（自动选择 primary 实例）
     */
    @Bean
    @Primary
    @ConditionalOnMissingBean(RagService.class)
    public RagService primaryRagService(
            RagAdapterProperties properties,
            Map<String, RagService> ragServices) {

        // 查找 primary 实例
        RagAdapterProperties.RagInstanceConfig primaryConfig = properties.getPrimaryInstance();
        if (primaryConfig != null) {
            String primaryId = primaryConfig.getOrGenerateId();
            RagService service = ragServices.get(primaryId);
            if (service != null) {
                log.info("🎯 主 RAG 服务: {}", primaryId);
                return service;
            }
        }

        // 使用第一个实例
        if (!ragServices.isEmpty()) {
            String firstId = ragServices.keySet().iterator().next();
            log.info("🎯 主 RAG 服务（默认）: {}", firstId);
            return ragServices.get(firstId);
        }

        // 降级为 Mock
        log.warn("⚠️ 未找到任何实例，使用 Mock 服务");
        return new MockRagService("default");
    }

    /**
     * RAG 服务注册表（用于管理多实例）
     */
    @Bean
    @ConditionalOnMissingBean
    public RagServiceRegistry ragServiceRegistry(Map<String, RagService> ragServices) {
        return new RagServiceRegistry(ragServices);
    }

    /**
     * 创建默认实例配置
     */
    private List<RagAdapterProperties.RagInstanceConfig> createDefaultInstance() {
        log.info("📋 未配置实例，创建默认 File 实例");

        RagAdapterProperties.RagInstanceConfig config = new RagAdapterProperties.RagInstanceConfig();
        config.setId("default");
        config.setName("默认 File 实例");
        config.setType("file");
        config.setPrimary(true);

        RagAdapterProperties.FileConfig fileConfig = new RagAdapterProperties.FileConfig();
        config.setFile(fileConfig);

        return List.of(config);
    }
}

