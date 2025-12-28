package top.yumbo.ai.omni.chunking.starter;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import top.yumbo.ai.omni.chunking.*;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;
import top.yumbo.ai.omni.chunking.starter.strategy.*;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import java.util.*;

/**
 * 默认分块服务实现（重构版）
 *
 * <p>整合了分块策略管理和存储功能，提供完整的分块服务</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 2.0.0 - 重构为完整的分块服务，集成存储功能
 */
@Slf4j
public class DefaultChunkingService implements ChunkingService {

    private final ChunkingProperties properties;
    private final Map<ChunkingStrategy, ChunkingStrategyExecutor> strategies;
    @Getter
    private final ChunkingStrategyManager strategyManager;
    @Autowired(required = false)
    private DocumentStorageService storageService;

    // 默认分块大小
    private static final int DEFAULT_CHUNK_SIZE = 500;
    private static final int DEFAULT_OVERLAP_SIZE = 50;

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

        // 添加 MARKDOWN 策略（如果可用）
        try {
            // TODO: 实现 MarkdownStrategy
            // registerStrategy(ChunkingStrategy.MARKDOWN, new MarkdownStrategy(properties));
            log.debug("⚠️ Markdown 分块策略尚未实现");
        } catch (Exception e) {
            log.debug("⚠️ Markdown 分块策略不可用: {}", e.getMessage());
        }

        // 初始化策略管理器
        this.strategyManager = new ChunkingStrategyManager(properties, strategies);

        log.info("✅ 分块服务初始化完成，注册了 {} 个策略", strategies.size());
    }

    private void registerStrategy(ChunkingStrategy strategy, ChunkingStrategyExecutor executor) {
        strategies.put(strategy, executor);
    }

    // ========== 核心分块方法 ==========

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

    @Override
    public List<String> chunkAndStore(String documentId, String content) {
        return chunkAndStore(documentId, content, createDefaultConfig());
    }

    @Override
    public List<String> chunkAndStore(String documentId, String content, ChunkingConfig config) {
        if (storageService == null) {
            log.warn("⚠️ DocumentStorageService 未配置，无法存储分块");
            return Collections.emptyList();
        }

        if (content == null || content.trim().isEmpty()) {
            log.warn("Empty content for document: {}", documentId);
            return new ArrayList<>();
        }

        try {
            // 1. 切分文档
            List<Chunk> chunks = chunk(documentId, content, config);

            // 2. 存储到存储服务
            List<String> chunkIds = storageService.saveChunks(documentId, chunks);

            log.info("✅ 切分并存储文档 {}: {} 个分块", documentId, chunkIds.size());
            return chunkIds;
        } catch (Exception e) {
            log.error("切分并存储文档失败: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    // ========== 智能分块方法 ==========

    @Override
    public List<Chunk> chunkWithAutoStrategy(String documentId, String content, String fileName) {
        return strategyManager.chunkWithAutoStrategy(documentId, content, fileName);
    }

    @Override
    public List<Chunk> chunkWithStrategy(String documentId, String content,
                                        ChunkingStrategy strategy, ChunkingConfig config) {
        return strategyManager.chunkWithStrategy(documentId, content, strategy, config);
    }

    // ========== 查询方法 ==========

    @Override
    public List<Chunk> getChunks(String documentId) {
        if (storageService == null) {
            log.warn("⚠️ DocumentStorageService 未配置，无法获取分块");
            return Collections.emptyList();
        }

        try {
            return storageService.getChunksByDocument(documentId);
        } catch (Exception e) {
            log.error("获取文档分块失败: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public Chunk getChunk(String chunkId) {
        if (storageService == null) {
            log.warn("⚠️ DocumentStorageService 未配置，无法获取分块");
            return null;
        }

        try {
            return storageService.getChunk(chunkId).orElse(null);
        } catch (Exception e) {
            log.error("获取分块失败: {}", chunkId, e);
            return null;
        }
    }

    // ========== 删除方法 ==========

    @Override
    public void deleteChunks(String documentId) {
        if (storageService == null) {
            log.warn("⚠️ DocumentStorageService 未配置，无法删除分块");
            return;
        }

        try {
            storageService.deleteChunksByDocument(documentId);
            log.info("✅ 删除文档分块: {}", documentId);
        } catch (Exception e) {
            log.error("删除文档分块失败: {}", documentId, e);
        }
    }

    @Override
    public void deleteChunk(String chunkId) {
        if (storageService == null) {
            log.warn("⚠️ DocumentStorageService 未配置，无法删除分块");
            return;
        }

        try {
            storageService.deleteChunk(chunkId);
            log.info("✅ 删除分块: {}", chunkId);
        } catch (Exception e) {
            log.error("删除分块失败: {}", chunkId, e);
        }
    }

    // ========== 更新方法 ==========

    @Override
    public List<String> rechunkAndStore(String documentId, String content) {
        return rechunkAndStore(documentId, content, createDefaultConfig());
    }

    @Override
    public List<String> rechunkAndStore(String documentId, String content, ChunkingConfig config) {
        if (storageService == null) {
            log.warn("⚠️ DocumentStorageService 未配置，无法重新切分存储");
            return Collections.emptyList();
        }

        // 先删除旧的分块
        deleteChunks(documentId);

        // 重新切分和存储
        return chunkAndStore(documentId, content, config);
    }

    // ========== 策略管理 ==========

    @Override
    public List<ChunkingStrategy> getSupportedStrategies() {
        return new ArrayList<>(strategies.keySet());
    }

    @Override
    public ChunkingStrategy getDefaultStrategy() {
        return properties.getStrategy();
    }

    // ========== 辅助方法 ==========

    /**
     * 创建默认配置
     */
    private ChunkingConfig createDefaultConfig() {
        return ChunkingConfig.builder()
                .strategy(properties.getStrategy())
                .maxChunkSize(DEFAULT_CHUNK_SIZE)
                .overlap(DEFAULT_OVERLAP_SIZE)
                .build();
    }

    /**
     * 从 documentId 提取文件名
     */
    private String extractFileName(String documentId) {
        if (documentId == null) {
            return null;
        }
        // 格式: doc_timestamp_filename
        String[] parts = documentId.split("_", 3);
        return parts.length >= 3 ? parts[2] : null;
    }
}



