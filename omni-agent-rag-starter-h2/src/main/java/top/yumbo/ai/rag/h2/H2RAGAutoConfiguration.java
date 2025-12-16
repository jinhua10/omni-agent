package top.yumbo.ai.rag.h2;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.rag.api.RAGService;

/**
 * H2 RAG 自动配置类
 * (H2 RAG Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@ConditionalOnClass(org.h2.Driver.class)
@org.springframework.boot.autoconfigure.condition.ConditionalOnProperty(
    name = "omni-agent.rag.type",
    havingValue = "h2"
)
@EnableConfigurationProperties(H2RAGProperties.class)
public class H2RAGAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(RAGService.class)
    public H2RAGService h2RAGService(H2RAGProperties properties) {
        log.info("创建 H2 RAG Service");
        log.info("  数据库URL: {}", properties.getUrl());
        log.info("  连接池大小: {}-{}", properties.getMinPoolSize(), properties.getMaxPoolSize());

        return new H2RAGService(properties);
    }
}
