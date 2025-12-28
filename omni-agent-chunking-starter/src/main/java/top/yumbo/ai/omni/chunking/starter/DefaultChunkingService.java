package top.yumbo.ai.omni.chunking.starter;
}
    }
        return properties.getStrategy();
    public ChunkingStrategy getDefaultStrategy() {
    @Override

    }
        return new ArrayList<>(strategies.keySet());
    public List<ChunkingStrategy> getSupportedStrategies() {
    @Override

    }
        return executor.execute(documentId, content, config);
        log.debug("📋 使用 {} 策略分块文档: {}", strategy, documentId);

        }
            executor = strategies.get(ChunkingStrategy.FIXED_LENGTH);
            log.warn("⚠️ 策略 {} 不可用，使用固定长度分块", strategy);
        if (executor == null) {
        ChunkingStrategyExecutor executor = strategies.get(strategy);

                config.getStrategy() : properties.getStrategy();
        ChunkingStrategy strategy = config.getStrategy() != null ?

        }
            return Collections.emptyList();
        if (content == null || content.isEmpty()) {
    public List<Chunk> chunk(String documentId, String content, ChunkingConfig config) {
    @Override

    }
        strategies.put(strategy, executor);
    private void registerStrategy(ChunkingStrategy strategy, ChunkingStrategyExecutor executor) {

    }
        log.info("✅ 分块服务初始化完成，注册了 {} 个策略", strategies.size());

        }
            log.warn("⚠️ 语义分块策略不可用: {}", e.getMessage());
        } catch (Exception e) {
            log.info("✅ 语义分块策略已注册");
            registerStrategy(ChunkingStrategy.SEMANTIC, new SemanticStrategy(properties));
        try {

        }
            log.warn("⚠️ PPL 分块策略不可用（需要 omni-agent-ppl-onnx 依赖）");
        } catch (NoClassDefFoundError e) {
            log.info("✅ PPL 分块策略已注册");
            registerStrategy(ChunkingStrategy.PPL, new PPLChunkingStrategy(properties));
        try {
        // PPL 和 SEMANTIC 策略可选（需要额外依赖）

        registerStrategy(ChunkingStrategy.SENTENCE, new SentenceStrategy(properties));
        registerStrategy(ChunkingStrategy.PARAGRAPH, new ParagraphStrategy(properties));
        registerStrategy(ChunkingStrategy.FIXED_LENGTH, new FixedLengthStrategy(properties));
        // 注册所有策略

        this.strategies = new HashMap<>();
        this.properties = properties;
    public DefaultChunkingService(ChunkingProperties properties) {

    private final Map<ChunkingStrategy, ChunkingStrategyExecutor> strategies;
    private final ChunkingProperties properties;

public class DefaultChunkingService implements ChunkingService {
@Slf4j
 */
 * @since 1.0.0
 * @author OmniAgent Team
 *
 * 默认分块服务实现
/**

import java.util.*;

import top.yumbo.ai.omni.chunking.starter.strategy.*;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;
import top.yumbo.ai.omni.chunking.*;
import lombok.extern.slf4j.Slf4j;


