package top.yumbo.ai.omni.persistence.h2;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;

/**
 * H2 持久化自动配置
 * (H2 Persistence Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(H2PersistenceProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.persistence.type",
    havingValue = "h2"
)
public class H2PersistenceAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(QuestionClassifierPersistence.class)
    public QuestionClassifierPersistence questionClassifierPersistence(H2PersistenceProperties properties) {
        log.info("Auto-configuring H2Persistence: {}", properties.getUrl());
        return new H2Persistence(properties);
    }
}

