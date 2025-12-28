package top.yumbo.ai.omni.chunking.starter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import java.util.ArrayList;
import java.util.List;

/**
 * 文档分块服务 - 负责文档的切分和存储
 * (Document Chunking Service - Responsible for document chunking and storage)
 *
 * <p>
 * 重构说明 (Refactoring Notes):
 * - 使用 DocumentStorageService 接口存储分块
 * - 删除硬编码的文件存储
 * - 支持多种存储后端（File/MongoDB/S3/Redis等）
 * </p>
 *
 * 特点 (Features):
 * - 智能文档切分 (Smart document chunking)
 * - 可插拔的存储后端 (Pluggable storage backend)
 * - 保留分块上下文 (Preserve chunk context)
 * - 支持多种切分策略 (Support multiple chunking strategies)
 *
 * @author OmniAgent Team
 * @since 1.0.0 (Refactored)
 * @version 3.0.0 - 重构为可插拔架构
 */
@Slf4j
@Service
public class DocumentChunkingService {

    private final DocumentStorageService storageService;
    private final ChunkingStrategyManager strategyManager;

    // 默认分块大小
    private static final int DEFAULT_CHUNK_SIZE = 500;

    // 默认重叠大小
    private static final int DEFAULT_OVERLAP_SIZE = 50;

    /**
     * 构造函数 - Spring 自动注入存储服务和策略管理器
     *
     * @param storageService 文档存储服务接口
     * @param strategyManager 分块策略管理器
     */
    @Autowired
    public DocumentChunkingService(DocumentStorageService storageService,
                                  ChunkingStrategyManager strategyManager) {
        this.storageService = storageService;
        this.strategyManager = strategyManager;
        log.info("DocumentChunkingService initialized with storage: {} and strategy manager",
                 storageService.getClass().getSimpleName());
    }

    /**
     * 切分文档并存储
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @return 分块ID列表
     */
    public List<String> chunkAndStore(String documentId, String content) {
        if (content == null || content.trim().isEmpty()) {
            log.warn("Empty content for document: {}", documentId);
            return new ArrayList<>();
        }

        try {
            // 1. 切分文档
            List<Chunk> chunks = chunkDocument(documentId, content);

            // 2. 存储到存储服务
            List<String> chunkIds = storageService.saveChunks(documentId, chunks);

            log.info("Chunked and stored document {}: {} chunks", documentId, chunkIds.size());
            return chunkIds;
        } catch (Exception e) {
            log.error("Failed to chunk and store document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    /**
     * 切分文档（不存储）- 自动选择最佳策略
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @return 分块列表
     */
    public List<Chunk> chunkDocument(String documentId, String content) {
        // 从 documentId 提取文件名（如果有）
        String fileName = extractFileName(documentId);
        return chunkDocument(documentId, content, fileName);
    }

    /**
     * 切分文档（不存储）- 自动选择最佳策略（带文件名）
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param fileName 文件名（用于推断文档类型）
     * @return 分块列表
     */
    public List<Chunk> chunkDocument(String documentId, String content, String fileName) {
        // 使用策略管理器自动选择最佳分块策略
        return strategyManager.chunkWithAutoStrategy(documentId, content, fileName);
    }

    /**
     * 切分文档（不存储）- 使用固定大小策略（向后兼容）
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param chunkSize 分块大小
     * @param overlapSize 重叠大小
     * @return 分块列表
     */
    public List<Chunk> chunkDocument(String documentId, String content,
                                    int chunkSize, int overlapSize) {
        return strategyManager.chunkWithStrategy(documentId, content, ChunkingStrategy.FIXED_LENGTH,
                ChunkingConfig.builder().maxChunkSize(chunkSize).overlap(overlapSize).build());
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

    // 注意：固定大小分块逻辑已移至 FixedSizeChunkingStrategy
    // 本服务现在使用策略模式，通过 ChunkingStrategyManager 动态选择分块算法

    /**
     * 获取文档的所有分块
     *
     * @param documentId 文档ID
     * @return 分块列表
     */
    public List<Chunk> getChunks(String documentId) {
        try {
            return storageService.getChunksByDocument(documentId);
        } catch (Exception e) {
            log.error("Failed to get chunks for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    /**
     * 删除文档的所有分块
     *
     * @param documentId 文档ID
     */
    public void deleteChunks(String documentId) {
        try {
            storageService.deleteChunksByDocument(documentId);
            log.info("Deleted chunks for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete chunks for document: {}", documentId, e);
        }
    }

    /**
     * 重新切分和存储文档
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @return 分块ID列表
     */
    public List<String> rechunkAndStore(String documentId, String content) {
        // 先删除旧的分块
        deleteChunks(documentId);

        // 重新切分和存储
        return chunkAndStore(documentId, content);
    }
}

