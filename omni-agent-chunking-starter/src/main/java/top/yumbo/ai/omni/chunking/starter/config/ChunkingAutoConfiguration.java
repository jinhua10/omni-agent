package top.yumbo.ai.omni.chunking.starter.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.chunking.ChunkingService;
import top.yumbo.ai.omni.chunking.starter.DefaultChunkingService;

/**
 * 分块服务自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(ChunkingProperties.class)
@ConditionalOnProperty(prefix = "omni-agent.chunking", name = "enabled", havingValue = "true", matchIfMissing = true)
public class ChunkingAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public ChunkingService chunkingService(ChunkingProperties properties) {
        log.info("✅ 初始化分块服务，默认策略: {}", properties.getStrategy());
        return new DefaultChunkingService(properties);
    }
}



