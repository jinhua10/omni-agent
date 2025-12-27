package top.yumbo.ai.omni.persistence.sqlite;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;

/**
 * SQLite 持久化自动配置
 * (SQLite Persistence Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(SQLitePersistenceProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.persistence.type",
    havingValue = "sqlite"
)
public class SQLitePersistenceAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(QuestionClassifierPersistence.class)
    public QuestionClassifierPersistence questionClassifierPersistence(SQLitePersistenceProperties properties) {
        log.info("Auto-configuring SQLitePersistence: {}", properties.getDbPath());
        return new SQLitePersistence(properties);
    }
}

