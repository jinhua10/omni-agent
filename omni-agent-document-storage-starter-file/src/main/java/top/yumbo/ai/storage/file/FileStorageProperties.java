package top.yumbo.ai.storage.file;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * File 存储配置属性
 * (File Storage Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.document-storage.file")
public class FileStorageProperties {

    /**
     * 基础存储目录
     */
    private String baseDirectory = "./data/storage";
}

