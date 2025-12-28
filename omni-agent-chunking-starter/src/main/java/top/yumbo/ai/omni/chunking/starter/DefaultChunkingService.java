package top.yumbo.ai.omni.chunking.starter;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.chunking.*;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;
import top.yumbo.ai.omni.chunking.starter.strategy.*;

import java.util.*;

/**
 * 默认分块服务实现
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class DefaultChunkingService implements ChunkingService {

    private final ChunkingProperties properties;
    private final Map<ChunkingStrategy, ChunkingStrategyExecutor> strategies;
    /**
     * -- GETTER --
     *  获取策略管理器（用于高级用法）
     */
    @Getter
    private final ChunkingStrategyManager strategyManager;

    public DefaultChunkingService(ChunkingProperties properties) {
        this.properties = properties;
        this.strategies = new HashMap<>();

        // 注册所有策略
        registerStrategy(ChunkingStrategy.FIXED_LENGTH, new FixedLengthStrategy(properties));
        registerStrategy(ChunkingStrategy.PARAGRAPH, new ParagraphStrategy(properties));
        registerStrategy(ChunkingStrategy.SENTENCE, new SentenceStrategy(properties));

        // PPL 和 SEMANTIC 策略可选（需要额外依赖）
        try {
            registerStrategy(ChunkingStrategy.PPL, new PPLChunkingStrategy(properties));
            log.info("✅ PPL 分块策略已注册");
        } catch (NoClassDefFoundError e) {
            log.warn("⚠️ PPL 分块策略不可用（需要 omni-agent-ppl-onnx 依赖）");
        }

        try {
            registerStrategy(ChunkingStrategy.SEMANTIC, new SemanticStrategy(properties));
            log.info("✅ 语义分块策略已注册");
        } catch (Exception e) {
            log.warn("⚠️ 语义分块策略不可用: {}", e.getMessage());
        }

        // 初始化策略管理器
        this.strategyManager = new ChunkingStrategyManager(properties, strategies);

        log.info("✅ 分块服务初始化完成，注册了 {} 个策略", strategies.size());
    }

    private void registerStrategy(ChunkingStrategy strategy, ChunkingStrategyExecutor executor) {
        strategies.put(strategy, executor);
    }

    @Override
    public List<Chunk> chunk(String documentId, String content, ChunkingConfig config) {
        if (content == null || content.isEmpty()) {
            return Collections.emptyList();
        }

        ChunkingStrategy strategy = config.getStrategy() != null ?
                config.getStrategy() : properties.getStrategy();

        ChunkingStrategyExecutor executor = strategies.get(strategy);
        if (executor == null) {
            log.warn("⚠️ 策略 {} 不可用，使用固定长度分块", strategy);
            executor = strategies.get(ChunkingStrategy.FIXED_LENGTH);
        }

        log.debug("📋 使用 {} 策略分块文档: {}", strategy, documentId);
        return executor.execute(documentId, content, config);
    }

    /**
     * 自动选择策略分块
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param fileName 文件名（用于推断文档类型）
     * @return 分块列表
     */
    public List<Chunk> chunkWithAutoStrategy(String documentId, String content, String fileName) {
        return strategyManager.chunkWithAutoStrategy(documentId, content, fileName);
    }

    /**
     * 使用指定策略分块
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param strategy 分块策略
     * @param config 分块配置
     * @return 分块列表
     */
    public List<Chunk> chunkWithStrategy(String documentId, String content,
                                        ChunkingStrategy strategy, ChunkingConfig config) {
        return strategyManager.chunkWithStrategy(documentId, content, strategy, config);
    }

    @Override
    public List<ChunkingStrategy> getSupportedStrategies() {
        return new ArrayList<>(strategies.keySet());
    }

    @Override
    public ChunkingStrategy getDefaultStrategy() {
        return properties.getStrategy();
    }

}



