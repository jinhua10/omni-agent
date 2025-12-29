package top.yumbo.ai.omni.storage.impl.s3;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * AWS S3 对象存储配置属性
 * (AWS S3 Object Storage Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.document-storage.s3")
public class S3StorageProperties {

    /**
     * AWS 区域
     * 默认: us-east-1
     */
    private String region = "us-east-1";

    /**
     * Access Key ID
     */
    private String accessKeyId;

    /**
     * Secret Access Key
     */
    private String secretAccessKey;

    /**
     * 存储桶名称
     * 默认: omni-agent-documents
     */
    private String bucketName = "omni-agent-documents";

    /**
     * 自定义 Endpoint（可选，用于兼容 S3 的服务）
     */
    private String endpoint;

    /**
     * 是否使用路径风格访问
     * 默认: false
     */
    private boolean pathStyleAccessEnabled = false;
}

