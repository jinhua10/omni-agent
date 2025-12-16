package top.yumbo.ai.persistence.file;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;

/**
 * File 持久化自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@EnableConfigurationProperties(FilePersistenceProperties.class)
public class FilePersistenceAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(QuestionClassifierPersistence.class)
    public FilePersistence filePersistence() {
        log.info("🚀 Auto-configuring FilePersistence (JSON-based file storage)");
        return new FilePersistence();
    }
}
