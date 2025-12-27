package top.yumbo.ai.omni.storage.minio;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * MinIO 对象存储配置属性
 * (MinIO Object Storage Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.document-storage.minio")
public class MinIOStorageProperties {

    /**
     * MinIO 服务地址
     * 默认: http://localhost:9000
     */
    private String endpoint = "http://localhost:9000";

    /**
     * Access Key (用户名)
     */
    private String accessKey = "minioadmin";

    /**
     * Secret Key (密码)
     */
    private String secretKey = "minioadmin";

    /**
     * 存储桶名称
     * 默认: omni-agent-documents
     */
    private String bucketName = "omni-agent-documents";

    /**
     * 是否使用 HTTPS
     * 默认: false
     */
    private boolean secure = false;

    /**
     * 区域（可选）
     * 默认: us-east-1
     */
    private String region = "us-east-1";
}

