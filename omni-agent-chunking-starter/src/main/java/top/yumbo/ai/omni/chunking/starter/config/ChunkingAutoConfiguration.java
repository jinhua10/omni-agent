package top.yumbo.ai.omni.chunking.starter.config;
}
    }
        return new DefaultChunkingService(properties);
        log.info("✅ 初始化分块服务，默认策略: {}", properties.getStrategy());
    public ChunkingService chunkingService(ChunkingProperties properties) {
    @ConditionalOnMissingBean
    @Bean

public class ChunkingAutoConfiguration {
@ConditionalOnProperty(prefix = "omni-agent.chunking", name = "enabled", havingValue = "true", matchIfMissing = true)
@EnableConfigurationProperties(ChunkingProperties.class)
@Configuration
@Slf4j
 */
 * @since 1.0.0
 * @author OmniAgent Team
 *
 * 分块服务自动配置
/**

import top.yumbo.ai.omni.chunking.starter.DefaultChunkingService;
import top.yumbo.ai.omni.chunking.ChunkingService;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Bean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import lombok.extern.slf4j.Slf4j;


