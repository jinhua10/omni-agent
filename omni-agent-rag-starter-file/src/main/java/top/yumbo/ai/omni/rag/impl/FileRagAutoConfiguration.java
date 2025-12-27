package top.yumbo.ai.omni.rag.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.rag.RagService;

/**
 * File RAG 自动配置
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(FileRagProperties.class)
@ConditionalOnProperty(prefix = "omni.rag.file", name = "enabled", havingValue = "true", matchIfMissing = true)
public class FileRagAutoConfiguration {

    @Bean
    public RagService fileRagService(FileRagProperties properties) {
        log.info("初始化 FileRagService: indexPath={}, domainId={}",
                properties.getIndexPath(), properties.getDefaultDomainId());

        FileRagService service = new FileRagService(
                properties.getDefaultDomainId(),
                properties.getIndexPath()
        );

        service.init();
        return service;
    }
}

