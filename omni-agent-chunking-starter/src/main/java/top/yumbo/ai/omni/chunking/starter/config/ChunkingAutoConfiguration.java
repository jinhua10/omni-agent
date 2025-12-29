package top.yumbo.ai.omni.chunking.starter.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.chunking.ChunkingService;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.ChunkingStrategyManager;
import top.yumbo.ai.omni.chunking.starter.DefaultChunkingService;
import top.yumbo.ai.omni.chunking.starter.strategy.*;

import java.util.HashMap;
import java.util.Map;

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

    /**
     * 创建分块策略管理器 Bean
     * 注意：需要在 ChunkingService 之前创建
     */
    @Bean
    @ConditionalOnMissingBean
    public ChunkingStrategyManager chunkingStrategyManager(ChunkingProperties properties) {
        log.info("🔧 初始化分块策略管理器");

        // 注册所有策略
        Map<ChunkingStrategy, ChunkingStrategyExecutor> strategies = new HashMap<>();
        strategies.put(ChunkingStrategy.FIXED_LENGTH, new FixedLengthStrategy(properties));
        strategies.put(ChunkingStrategy.PARAGRAPH, new ParagraphStrategy(properties));
        strategies.put(ChunkingStrategy.SENTENCE, new SentenceStrategy(properties));
        strategies.put(ChunkingStrategy.MARKDOWN, new MarkdownStrategy(properties));

        // PPL 和 SEMANTIC 策略可选（需要额外依赖）
        try {
            strategies.put(ChunkingStrategy.PPL, new PPLChunkingStrategy(properties));
            log.info("✅ PPL 分块策略已注册");
        } catch (NoClassDefFoundError e) {
            log.warn("⚠️ PPL 分块策略不可用（需要 omni-agent-ppl-onnx 依赖）");
        }

        try {
            strategies.put(ChunkingStrategy.SEMANTIC, new SemanticStrategy(properties));
            log.info("✅ 语义分块策略已注册");
        } catch (Exception e) {
            log.warn("⚠️ 语义分块策略不可用: {}", e.getMessage());
        }

        return new ChunkingStrategyManager(properties, strategies);
    }

    @Bean
    @ConditionalOnMissingBean
    public ChunkingService chunkingService(ChunkingProperties properties) {
        log.info("✅ 初始化分块服务，默认策略: {}", properties.getStrategy());
        return new DefaultChunkingService(properties);
    }
}



