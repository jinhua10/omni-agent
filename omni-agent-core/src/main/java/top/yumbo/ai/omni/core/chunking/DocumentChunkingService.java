package top.yumbo.ai.omni.core.chunking;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Chunk;

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

    // 默认分块大小
    private static final int DEFAULT_CHUNK_SIZE = 500;

    // 默认重叠大小
    private static final int DEFAULT_OVERLAP_SIZE = 50;

    /**
     * 构造函数 - Spring 自动注入存储服务
     *
     * @param storageService 文档存储服务接口
     */
    @Autowired
    public DocumentChunkingService(DocumentStorageService storageService) {
        this.storageService = storageService;
        log.info("DocumentChunkingService initialized with storage: {}",
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
     * 切分文档（不存储）
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @return 分块列表
     */
    public List<Chunk> chunkDocument(String documentId, String content) {
        return chunkDocument(documentId, content, DEFAULT_CHUNK_SIZE, DEFAULT_OVERLAP_SIZE);
    }

    /**
     * 切分文档（指定大小）
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param chunkSize 分块大小
     * @param overlapSize 重叠大小
     * @return 分块列表
     */
    public List<Chunk> chunkDocument(String documentId, String content,
                                     int chunkSize, int overlapSize) {
        List<Chunk> chunks = new ArrayList<>();

        if (content == null || content.isEmpty()) {
            return chunks;
        }

        int contentLength = content.length();
        int position = 0;
        int sequence = 0;

        while (position < contentLength) {
            // 计算当前分块的结束位置
            int endPosition = Math.min(position + chunkSize, contentLength);

            // 提取分块内容
            String chunkContent = content.substring(position, endPosition);

            // 创建分块对象
            Chunk chunk = Chunk.builder()
                .documentId(documentId)
                .content(chunkContent)
                .sequence(sequence)
                .startPosition(position)
                .endPosition(endPosition)
                .createdAt(System.currentTimeMillis())
                .build();

            chunks.add(chunk);

            // 移动到下一个分块（考虑重叠）
            int nextPosition = endPosition - overlapSize;

            // 确保position不会是负数，且能够前进
            if (nextPosition <= position || nextPosition >= contentLength) {
                break;
            }

            position = nextPosition;
            sequence++;
        }

        log.debug("Chunked document {} into {} chunks", documentId, chunks.size());
        return chunks;
    }

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

