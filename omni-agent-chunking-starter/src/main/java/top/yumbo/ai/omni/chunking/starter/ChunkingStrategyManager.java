package top.yumbo.ai.omni.chunking.starter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.chunking.*;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;
import top.yumbo.ai.omni.chunking.starter.strategy.ChunkingStrategyExecutor;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 分块策略管理器
 *
 * <p>根据文档类型和内容特征，自动选择最佳的分块策略</p>
 * <p>从 core/old/chunking 迁移而来</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class ChunkingStrategyManager {

    private final ChunkingProperties properties;
    private final Map<ChunkingStrategy, ChunkingStrategyExecutor> strategies;

    public ChunkingStrategyManager(ChunkingProperties properties,
                                  Map<ChunkingStrategy, ChunkingStrategyExecutor> strategies) {
        this.properties = properties;
        this.strategies = strategies;
        log.info("✅ ChunkingStrategyManager 初始化完成，注册了 {} 个策略", strategies.size());
    }

    /**
     * 根据文档类型和内容自动选择分块策略
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param fileName 文件名（用于推断类型）
     * @return 分块结果
     */
    public List<Chunk> chunkWithAutoStrategy(String documentId, String content, String fileName) {
        log.debug("📄 [Chunking] Starting auto chunking - docId: {}, fileName: {}, content length: {}",
                documentId, fileName, content.length());

        long startTime = System.currentTimeMillis();

        // 1. 推断文档类型
        DocumentType docType = inferDocumentType(fileName, content);
        log.debug("📄 [Chunking] Inferred document type: {}", docType);

        // 2. 选择最佳策略
        ChunkingStrategy strategy = selectBestStrategy(docType, content);
        log.debug("📄 [Chunking] Selected strategy: {}", strategy);

        // 3. 构建配置
        ChunkingConfig config = buildConfig(docType, strategy);
        log.debug("📄 [Chunking] Strategy config: {}", config);

        log.info("✅ Auto-selected chunking strategy: {} for document type: {}", strategy, docType);

        // 4. 执行分块
        List<Chunk> chunks = chunkWithStrategy(documentId, content, strategy, config);

        long duration = System.currentTimeMillis() - startTime;

        // Debug 日志：分块结果
        log.debug("📄 [Chunking] Completed in {}ms - Generated {} chunks", duration, chunks.size());
        for (int i = 0; i < Math.min(chunks.size(), 3); i++) {
            Chunk chunk = chunks.get(i);
            log.debug("📄 [Chunking] Chunk #{}: id={}, content length={}, preview: {}",
                    i + 1, chunk.getId(), chunk.getContent().length(),
                    chunk.getContent().substring(0, Math.min(100, chunk.getContent().length())) + "...");
        }
        if (chunks.size() > 3) {
            log.debug("📄 [Chunking] ... and {} more chunks", chunks.size() - 3);
        }

        return chunks;
    }

    /**
     * 使用指定策略分块
     */
    public List<Chunk> chunkWithStrategy(String documentId, String content,
                                        ChunkingStrategy strategy, ChunkingConfig config) {
        ChunkingStrategyExecutor executor = strategies.get(strategy);

        if (executor == null) {
            log.warn("⚠️ Strategy {} not found, using default: FIXED_LENGTH", strategy);
            executor = strategies.get(ChunkingStrategy.FIXED_LENGTH);
        }

        if (executor == null) {
            throw new IllegalStateException("No chunking strategy available");
        }

        return executor.execute(documentId, content, config);
    }

    /**
     * 推断文档类型
     */
    private DocumentType inferDocumentType(String fileName, String content) {
        if (fileName == null) {
            return DocumentType.GENERAL;
        }

        String lowerName = fileName.toLowerCase();

        // 技术文档
        if (lowerName.contains("readme") || lowerName.contains("doc") ||
                lowerName.contains("guide") || lowerName.contains("tutorial")) {
            return DocumentType.TECHNICAL;
        }

        // API文档
        if (lowerName.contains("api") || lowerName.contains("swagger") ||
                lowerName.contains("openapi")) {
            return DocumentType.API;
        }

        // FAQ
        if (lowerName.contains("faq") || lowerName.contains("q&a") ||
                lowerName.contains("问答")) {
            return DocumentType.FAQ;
        }

        // 代码文件
        if (lowerName.endsWith(".java") || lowerName.endsWith(".py") ||
                lowerName.endsWith(".js") || lowerName.endsWith(".cpp") ||
                lowerName.endsWith(".go") || lowerName.endsWith(".rs")) {
            return DocumentType.CODE;
        }

        // Markdown
        if (lowerName.endsWith(".md")) {
            if (content != null && content.contains("```")) {
                return DocumentType.TECHNICAL;  // 包含代码块
            }
            return DocumentType.MARKDOWN;
        }

        // 长文章（根据内容长度判断）
        if (content != null && content.length() > 5000) {
            int paragraphCount = content.split("\\n\\s*\\n").length;
            if (paragraphCount > 10) {
                return DocumentType.LONG_ARTICLE;
            }
        }

        return DocumentType.GENERAL;
    }

    /**
     * 根据文档类型选择最佳策略
     */
    private ChunkingStrategy selectBestStrategy(DocumentType docType, String content) {
        return switch (docType) {
            case TECHNICAL -> ChunkingStrategy.SEMANTIC;      // 技术文档用语义分块
            case API -> ChunkingStrategy.PPL;                 // API文档用PPL分块
            case CODE -> ChunkingStrategy.SEMANTIC;           // 代码用语义分块
            case FAQ -> ChunkingStrategy.SENTENCE;            // FAQ用句子边界
            case MARKDOWN -> ChunkingStrategy.PARAGRAPH;      // Markdown用段落分块
            case LONG_ARTICLE -> ChunkingStrategy.PPL;        // 长文章用PPL分块
            default -> ChunkingStrategy.FIXED_LENGTH;         // 默认固定大小
        };
    }

    /**
     * 根据文档类型构建配置
     */
    private ChunkingConfig buildConfig(DocumentType docType, ChunkingStrategy strategy) {
        ChunkingConfig.ChunkingConfigBuilder builder = ChunkingConfig.builder()
                .strategy(strategy);

        // 根据文档类型调整参数
        switch (docType) {
            case TECHNICAL, CODE -> {
                builder.maxChunkSize(600)   // 技术文档稍大
                        .minChunkSize(200)
                        .overlap(100);       // 更多重叠
            }
            case FAQ -> {
                builder.maxChunkSize(300)   // FAQ较短
                        .minChunkSize(100);
            }
            case LONG_ARTICLE -> {
                builder.maxChunkSize(800)   // 长文章更大
                        .minChunkSize(300);
            }
            default -> {
                builder.maxChunkSize(properties.getGeneral().getMaxChunkSize())
                        .minChunkSize(properties.getGeneral().getMinChunkSize())
                        .overlap(properties.getFixedLength().getOverlap());
            }
        }

        return builder.build();
    }

    /**
     * 获取所有可用策略
     */
    public List<ChunkingStrategy> getAvailableStrategies() {
        return new ArrayList<>(strategies.keySet());
    }

    /**
     * 文档类型枚举
     */
    public enum DocumentType {
        TECHNICAL,      // 技术文档
        API,            // API文档
        FAQ,            // FAQ
        CODE,           // 代码文件
        MARKDOWN,       // Markdown文档
        LONG_ARTICLE,   // 长文章
        GENERAL         // 通用文档
    }
}

