package top.yumbo.ai.omni.knowledge.registry.h2;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;

/**
 * H2 知识注册表自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnClass(JdbcTemplate.class)
@ConditionalOnProperty(
        prefix = "omni-agent.knowledge-registry",
        name = "type",
        havingValue = "h2"
)
@EnableConfigurationProperties(H2KnowledgeRegistryProperties.class)
public class H2KnowledgeRegistryAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(KnowledgeRegistry.class)
    public KnowledgeRegistry knowledgeRegistry(
            JdbcTemplate jdbcTemplate,
            H2KnowledgeRegistryProperties properties) {

        log.info("🚀 初始化 H2 知识注册表");
        log.info("   - 表名: {}", properties.getTableName());
        log.info("   - 角色表名: knowledge_roles");

        ObjectMapper objectMapper = new ObjectMapper();
        objectMapper.registerModule(new JavaTimeModule());
        objectMapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

        return new H2KnowledgeRegistry(
                jdbcTemplate,
                objectMapper,
                properties.getTableName(),
                "knowledge_roles"
        );
    }
}

