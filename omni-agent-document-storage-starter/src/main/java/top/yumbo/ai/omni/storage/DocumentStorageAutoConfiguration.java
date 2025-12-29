package top.yumbo.ai.omni.storage;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.AutoConfigureOrder;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.core.Ordered;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 文档存储统一自动配置
 *
 * <p>根据配置动态创建多个文档存储服务实例</p>
 * <p>如果没有配置，默认使用 File 存储作为兜底</p>
 *
 * <p>使用 HIGHEST_PRECEDENCE 确保在其他服务（如知识网络）之前初始化</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@AutoConfiguration
@AutoConfigureOrder(Ordered.HIGHEST_PRECEDENCE)
@EnableConfigurationProperties(DocumentStorageProperties.class)
public class DocumentStorageAutoConfiguration {

    /**
     * 创建所有文档存储服务实例
     *
     * <p>注意：使用 Object 类型避免可选依赖的 ClassNotFoundException</p>
     */
    @Bean
    public Map<String, DocumentStorageService> documentStorageServices(
            DocumentStorageProperties properties,
            ObjectProvider<Object> mongoTemplate,
            ObjectProvider<Object> redisTemplate,
            ObjectProvider<Object> s3Client,
            ObjectProvider<Object> minioClient,
            ObjectProvider<Object> elasticsearchClient) {

        Map<String, DocumentStorageService> services = new HashMap<>();
        List<DocumentStorageProperties.StorageInstanceConfig> instances = properties.getInstances();

        // 如果没有配置实例，创建默认 File 实例
        if (instances.isEmpty()) {
            instances = createDefaultInstance();
        }

        log.info("🚀 开始创建文档存储实例，共 {} 个", instances.size());

        // 创建每个实例
        for (DocumentStorageProperties.StorageInstanceConfig config : instances) {
            String instanceId = config.getOrGenerateId();

            try {
                DocumentStorageService service = new DocumentStorageInstanceBuilder(config)
                        .withMongoTemplate(mongoTemplate.getIfAvailable())
                        .withRedisTemplate(redisTemplate.getIfAvailable())
                        .withS3Client(s3Client.getIfAvailable())
                        .withMinioClient(minioClient.getIfAvailable())
                        .withElasticsearchClient(elasticsearchClient.getIfAvailable())
                        .build();

                services.put(instanceId, service);
                log.info("✅ 实例创建成功: id={}, type={}", instanceId, config.getType());

            } catch (Exception e) {
                log.error("❌ 实例创建失败: id={}, 使用 File 存储降级", instanceId, e);
                // 降级为 File 存储
                DocumentStorageProperties.StorageInstanceConfig fallbackConfig =
                        new DocumentStorageProperties.StorageInstanceConfig();
                fallbackConfig.setId(instanceId);
                fallbackConfig.setType("file");
                services.put(instanceId, new DocumentStorageInstanceBuilder(fallbackConfig).build());
            }
        }

        log.info("✅ 文档存储实例创建完成，共 {} 个", services.size());
        return services;
    }

    /**
     * 主文档存储服务（自动选择 primary 实例）
     */
    @Bean
    @Primary
    public DocumentStorageService documentStorageService(
            DocumentStorageProperties properties,
            Map<String, DocumentStorageService> documentStorageServices) {

        // 查找 primary 实例
        DocumentStorageProperties.StorageInstanceConfig primaryConfig = properties.getPrimaryInstance();
        if (primaryConfig != null) {
            String primaryId = primaryConfig.getOrGenerateId();
            DocumentStorageService service = documentStorageServices.get(primaryId);
            if (service != null) {
                log.info("🎯 主文档存储服务: {}", primaryId);
                return service;
            }
        }

        // 使用第一个实例
        if (!documentStorageServices.isEmpty()) {
            String firstId = documentStorageServices.keySet().iterator().next();
            log.info("🎯 主文档存储服务（默认）: {}", firstId);
            return documentStorageServices.get(firstId);
        }

        // 降级为 File 存储
        log.warn("⚠️ 未找到任何实例，创建默认 File 存储");
        return new DocumentStorageInstanceBuilder(createDefaultConfig()).build();
    }

    /**
     * 文档存储注册表（用于管理多实例）
     */
    @Bean
    @ConditionalOnMissingBean
    public DocumentStorageRegistry documentStorageRegistry(
            Map<String, DocumentStorageService> documentStorageServices) {
        return new DocumentStorageRegistry(documentStorageServices);
    }

    /**
     * 创建默认实例配置
     */
    private List<DocumentStorageProperties.StorageInstanceConfig> createDefaultInstance() {
        log.info("📋 未配置实例，创建默认 File 实例");
        return List.of(createDefaultConfig());
    }

    /**
     * 创建默认配置
     */
    private DocumentStorageProperties.StorageInstanceConfig createDefaultConfig() {
        DocumentStorageProperties.StorageInstanceConfig config =
                new DocumentStorageProperties.StorageInstanceConfig();
        config.setId("default");
        config.setName("默认 File 存储");
        config.setType("file");
        config.setPrimary(true);

        DocumentStorageProperties.FileConfig fileConfig =
                new DocumentStorageProperties.FileConfig();
        config.setFile(fileConfig);

        return config;
    }
}

