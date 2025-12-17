package top.yumbo.ai.omni.core.chunking;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.core.chunking.strategy.ChunkingStrategy;
import top.yumbo.ai.storage.api.model.Chunk;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 分块策略管理器
 *
 * 根据文档类型和内容特征，自动选择最佳的分块策略
 *
 * 策略选择规则（参考 RAG_ALGORITHM_DECISION_TREE.md）：
 * - 技术文档 → Semantic Chunking（保持代码完整性）
 * - API文档 → Metadata Filter + 结构化分块
 * - FAQ文档 → 句子边界分块
 * - 长篇文章 → 段落分块
 * - 代码库 → Semantic Chunking + 结构化
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class ChunkingStrategyManager {

    // 所有注册的分块策略
    private final Map<String, ChunkingStrategy> strategies = new ConcurrentHashMap<>();

    // 默认策略
    private static final String DEFAULT_STRATEGY = "fixed_size";

    @Autowired(required = false)
    public ChunkingStrategyManager(List<ChunkingStrategy> strategyList) {
        // 自动注册所有策略
        if (strategyList != null) {
            for (ChunkingStrategy strategy : strategyList) {
                registerStrategy(strategy);
            }
        }
        log.info("ChunkingStrategyManager initialized with {} strategies", strategies.size());
    }

    /**
     * 注册分块策略
     */
    public void registerStrategy(ChunkingStrategy strategy) {
        strategies.put(strategy.getStrategyName(), strategy);
        log.info("Registered chunking strategy: {} - {}",
                strategy.getStrategyName(), strategy.getDescription());
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
        // 1. 推断文档类型
        DocumentType docType = inferDocumentType(fileName, content);

        // 2. 选择最佳策略
        String strategyName = selectBestStrategy(docType, content);

        // 3. 获取策略参数
        Map<String, Object> params = getStrategyParams(docType, content);

        log.info("Auto-selected chunking strategy: {} for document type: {}",
                strategyName, docType);

        // 4. 执行分块
        return chunkWithStrategy(documentId, content, strategyName, params);
    }

    /**
     * 使用指定策略分块
     */
    public List<Chunk> chunkWithStrategy(String documentId, String content,
                                        String strategyName, Map<String, Object> params) {
        ChunkingStrategy strategy = strategies.get(strategyName);

        if (strategy == null) {
            log.warn("Strategy not found: {}, using default: {}", strategyName, DEFAULT_STRATEGY);
            strategy = strategies.get(DEFAULT_STRATEGY);
        }

        if (strategy == null) {
            throw new IllegalStateException("No chunking strategy available");
        }

        return strategy.chunk(documentId, content, params);
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
            // 进一步分析内容
            if (content != null && content.contains("```")) {
                return DocumentType.TECHNICAL;  // 包含代码块
            }
            return DocumentType.MARKDOWN;
        }

        // 长文章（根据内容长度判断）
        if (content != null && content.length() > 5000) {
            // 检查是否有段落结构
            int paragraphCount = content.split("\\n\\s*\\n").length;
            if (paragraphCount > 10) {
                return DocumentType.LONG_ARTICLE;
            }
        }

        return DocumentType.GENERAL;
    }

    /**
     * 根据文档类型选择最佳策略
     *
     * 参考 RAG_ALGORITHM_DECISION_TREE.md 的推荐
     */
    private String selectBestStrategy(DocumentType docType, String content) {
        return switch (docType) {
            case TECHNICAL -> "semantic";  // 技术文档用语义分块
            case API -> "structured";      // API文档用结构化分块
            case CODE -> "semantic";       // 代码用语义分块
            case FAQ -> "sentence_boundary"; // FAQ用句子边界
            case MARKDOWN -> "paragraph";  // Markdown用段落分块
            case LONG_ARTICLE -> "paragraph"; // 长文章用段落分块
            default -> DEFAULT_STRATEGY;   // 默认固定大小
        };
    }

    /**
     * 获取策略参数
     */
    private Map<String, Object> getStrategyParams(DocumentType docType, String content) {
        Map<String, Object> params = new HashMap<>();

        // 根据文档类型调整参数
        switch (docType) {
            case TECHNICAL, CODE -> {
                params.put("chunkSize", 600);  // 技术文档稍大
                params.put("overlapSize", 100); // 更多重叠
            }
            case FAQ -> {
                params.put("targetSize", 300);  // FAQ较短
            }
            case LONG_ARTICLE -> {
                params.put("maxParagraphsPerChunk", 4); // 长文章多段落
            }
            default -> {
                params.put("chunkSize", 500);
                params.put("overlapSize", 50);
            }
        }

        return params;
    }

    /**
     * 获取所有可用策略
     */
    public List<String> getAvailableStrategies() {
        return new ArrayList<>(strategies.keySet());
    }

    /**
     * 获取策略信息
     */
    public Map<String, String> getStrategyInfo(String strategyName) {
        ChunkingStrategy strategy = strategies.get(strategyName);
        if (strategy == null) {
            return Collections.emptyMap();
        }

        return Map.of(
            "name", strategy.getStrategyName(),
            "description", strategy.getDescription(),
            "defaultParams", strategy.getDefaultParams().toString()
        );
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

