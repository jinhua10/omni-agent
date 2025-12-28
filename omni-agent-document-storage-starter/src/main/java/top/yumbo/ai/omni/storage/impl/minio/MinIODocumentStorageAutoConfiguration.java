package top.yumbo.ai.omni.storage.impl.minio;

import io.minio.MinioClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

/**
 * MinIO 文档存储自动配置
 * (MinIO Document Storage Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(MinIOStorageProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.document-storage.type",
    havingValue = "minio"
)
public class MinIODocumentStorageAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public MinioClient minioClient(MinIOStorageProperties properties) {
        log.info("Creating MinioClient: {}", properties.getEndpoint());

        MinioClient.Builder builder = MinioClient.builder()
                .endpoint(properties.getEndpoint())
                .credentials(properties.getAccessKey(), properties.getSecretKey());

        if (properties.getRegion() != null && !properties.getRegion().isEmpty()) {
            builder.region(properties.getRegion());
        }

        return builder.build();
    }

    @Bean
    @ConditionalOnMissingBean
    public DocumentStorageService documentStorageService(
            MinioClient minioClient,
            MinIOStorageProperties properties) {
        log.info("Auto-configuring MinIODocumentStorage: {}", properties.getBucketName());
        return new MinIODocumentStorage(minioClient, properties);
    }
}

