package top.yumbo.ai.omni.storage.file;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

/**
 * File 文档存储自动配置
 * (File Document Storage Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(FileStorageProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.document-storage.type",
    havingValue = "file",
    matchIfMissing = true // 默认使用 File
)
public class FileDocumentStorageAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public DocumentStorageService documentStorageService(FileStorageProperties properties) {
        log.info("Auto-configuring FileDocumentStorage at: {}", properties.getBaseDirectory());
        return new FileDocumentStorage(properties.getBaseDirectory());
    }
}

