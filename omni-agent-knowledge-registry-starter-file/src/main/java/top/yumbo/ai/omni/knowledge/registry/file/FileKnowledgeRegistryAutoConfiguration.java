package top.yumbo.ai.omni.knowledge.registry.file;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;

/**
 * 文件知识注册表自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnProperty(
        prefix = "omni-agent.knowledge-registry",
        name = "type",
        havingValue = "file",
        matchIfMissing = true  // 默认使用 file 类型
)
@EnableConfigurationProperties(FileKnowledgeRegistryProperties.class)
public class FileKnowledgeRegistryAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(KnowledgeRegistry.class)
    public KnowledgeRegistry knowledgeRegistry(FileKnowledgeRegistryProperties properties) {
        log.info("🚀 初始化文件知识注册表");
        log.info("   - 存储路径: {}", properties.getBasePath());
        log.info("   - 格式化输出: {}", properties.isPrettyPrint());

        return new FileKnowledgeRegistry(
                properties.getBasePath(),
                properties.isPrettyPrint()
        );
    }
}

