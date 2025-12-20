package top.yumbo.ai.persistence.memory;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;

/**
 * Memory 持久化自动配置
 * (Memory Persistence Auto Configuration)
 *
 * <p>
 * 自动配置条件 (Auto-configuration conditions):
 * 1. 当没有其他 QuestionClassifierPersistence Bean 时生效
 * 2. 或者当配置 omni-agent.persistence.type=memory 时生效
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnProperty(
    name = "omni-agent.persistence.type",
    havingValue = "memory",
    matchIfMissing = true // 默认使用 Memory
)
public class MemoryPersistenceAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(QuestionClassifierPersistence.class)
    public QuestionClassifierPersistence questionClassifierPersistence() {
        log.info("Auto-configuring MemoryPersistence");
        log.warn("Using in-memory storage - data will not persist across restarts");
        return new MemoryPersistence();
    }
}

