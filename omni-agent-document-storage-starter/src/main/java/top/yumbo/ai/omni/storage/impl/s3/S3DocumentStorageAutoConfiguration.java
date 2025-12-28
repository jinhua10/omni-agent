package top.yumbo.ai.omni.storage.impl.s3;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.StringUtils;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.S3Configuration;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import java.net.URI;

/**
 * S3 文档存储自动配置
 * (S3 Document Storage Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(S3StorageProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.document-storage.type",
    havingValue = "s3"
)
public class S3DocumentStorageAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public S3Client s3Client(S3StorageProperties properties) {
        log.info("Creating S3Client: region={}, bucket={}", properties.getRegion(), properties.getBucketName());

        AwsBasicCredentials credentials = AwsBasicCredentials.create(
                properties.getAccessKeyId(),
                properties.getSecretAccessKey()
        );

        software.amazon.awssdk.services.s3.S3ClientBuilder builder = S3Client.builder()
                .region(Region.of(properties.getRegion()))
                .credentialsProvider(StaticCredentialsProvider.create(credentials));

        // 配置自定义 endpoint（用于兼容 S3 的服务，如 MinIO）
        if (StringUtils.hasText(properties.getEndpoint())) {
            builder.endpointOverride(URI.create(properties.getEndpoint()));
        }

        // 配置路径风格访问
        if (properties.isPathStyleAccessEnabled()) {
            builder.serviceConfiguration(S3Configuration.builder()
                    .pathStyleAccessEnabled(true)
                    .build());
        }

        return builder.build();
    }

    @Bean
    @ConditionalOnMissingBean
    public DocumentStorageService documentStorageService(
            S3Client s3Client,
            S3StorageProperties properties) {
        log.info("Auto-configuring S3DocumentStorage: {}", properties.getBucketName());
        return new S3DocumentStorage(s3Client, properties);
    }
}

