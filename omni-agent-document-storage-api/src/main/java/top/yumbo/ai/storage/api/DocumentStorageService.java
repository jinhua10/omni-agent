package top.yumbo.ai.storage.api;

import top.yumbo.ai.storage.api.model.Chunk;
import top.yumbo.ai.storage.api.model.Image;
import top.yumbo.ai.storage.api.model.PPLData;
import top.yumbo.ai.storage.api.model.OptimizationData;
import top.yumbo.ai.storage.api.model.StorageStatistics;
import top.yumbo.ai.storage.api.model.DocumentMetadata;

import java.util.List;
import java.util.Optional;

/**
 * 文档存储服务接口
 * (Document Storage Service Interface)
 *
 * <p>用于存储文档分块、图像、RAG优化数据等大文件/非结构化数据</p>
 * <p>支持多种后端: File, MongoDB, S3, MinIO, Redis, Elasticsearch</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface DocumentStorageService {

    // ========== 原始文档存储 (Raw Document Storage) ==========

    /**
     * 保存原始文档文件
     * @param documentId 文档ID
     * @param filename 文件名
     * @param fileData 文件数据
     * @return 文档存储ID
     */
    String saveDocument(String documentId, String filename, byte[] fileData);

    /**
     * 获取原始文档文件
     * @param documentId 文档ID
     * @return 文档数据
     */
    Optional<byte[]> getDocument(String documentId);

    /**
     * 删除原始文档文件
     * @param documentId 文档ID
     */
    void deleteDocument(String documentId);

    // ========== 文档分块存储 (Chunk Storage) ==========

    /**
     * 保存文档分块
     * @param documentId 文档ID
     * @param chunk 分块数据
     * @return 分块ID
     */
    String saveChunk(String documentId, Chunk chunk);

    /**
     * 批量保存分块
     * @param documentId 文档ID
     * @param chunks 分块列表
     * @return 分块ID列表
     */
    List<String> saveChunks(String documentId, List<Chunk> chunks);

    /**
     * 获取文档分块
     * @param chunkId 分块ID
     * @return 分块数据
     */
    Optional<Chunk> getChunk(String chunkId);

    /**
     * 获取文档的所有分块
     * @param documentId 文档ID
     * @return 分块列表
     */
    List<Chunk> getChunksByDocument(String documentId);

    /**
     * 删除分块
     * @param chunkId 分块ID
     */
    void deleteChunk(String chunkId);

    /**
     * 删除文档的所有分块
     * @param documentId 文档ID
     */
    void deleteChunksByDocument(String documentId);

    // ========== 图像存储 (Image Storage) ==========

    /**
     * 保存图像
     * @param documentId 文档ID
     * @param image 图像数据
     * @return 图像ID
     */
    String saveImage(String documentId, Image image);

    /**
     * 获取图像
     * @param imageId 图像ID
     * @return 图像数据
     */
    Optional<Image> getImage(String imageId);

    /**
     * 获取文档的所有图像
     * @param documentId 文档ID
     * @return 图像列表
     */
    List<Image> getImagesByDocument(String documentId);

    /**
     * 删除图像
     * @param imageId 图像ID
     */
    void deleteImage(String imageId);

    /**
     * 删除文档的所有图像
     * @param documentId 文档ID
     */
    void deleteImagesByDocument(String documentId);

    // ========== PPL 数据存储 (PPL Data Storage) ==========
    // 注意: PPL方法保留用于向后兼容，推荐使用下方的通用优化数据方法

    /**
     * 保存 PPL 分析结果
     * @param documentId 文档ID
     * @param data PPL数据
     * @return PPL数据ID
     * @deprecated 推荐使用 {@link #saveOptimizationData(String, OptimizationData)}
     */
    @Deprecated
    String savePPLData(String documentId, PPLData data);

    /**
     * 获取 PPL 数据
     * @param documentId 文档ID
     * @return PPL数据
     * @deprecated 推荐使用 {@link #getOptimizationData(String, String)}
     */
    @Deprecated
    Optional<PPLData> getPPLData(String documentId);

    /**
     * 删除 PPL 数据
     * @param documentId 文档ID
     * @deprecated 推荐使用 {@link #deleteOptimizationData(String, String)}
     */
    @Deprecated
    void deletePPLData(String documentId);

    // ========== RAG优化数据存储 (RAG Optimization Data Storage) ==========

    /**
     * 保存RAG优化分析结果（通用方法）
     * <p>支持多种优化算法：PPL, HyDE, Rerank, QueryExpansion等</p>
     *
     * @param documentId 文档ID
     * @param data 优化数据
     * @return 优化数据ID
     */
    String saveOptimizationData(String documentId, OptimizationData data);

    /**
     * 获取指定类型的优化数据
     *
     * @param documentId 文档ID
     * @param optimizationType 优化类型 (例如: "ppl", "hyde", "rerank")
     * @return 优化数据
     */
    Optional<OptimizationData> getOptimizationData(String documentId, String optimizationType);

    /**
     * 获取文档的所有优化数据
     *
     * @param documentId 文档ID
     * @return 优化数据列表
     */
    List<OptimizationData> getAllOptimizationData(String documentId);

    /**
     * 删除指定类型的优化数据
     *
     * @param documentId 文档ID
     * @param optimizationType 优化类型
     */
    void deleteOptimizationData(String documentId, String optimizationType);

    /**
     * 删除文档的所有优化数据
     *
     * @param documentId 文档ID
     */
    void deleteAllOptimizationData(String documentId);

    // ========== 文档管理 (Document Management) ==========

    /**
     * 列出所有文档
     * @return 文档信息列表
     */
    List<DocumentMetadata> listAllDocuments();

    /**
     * 列出文档（分页）
     * @param offset 偏移量
     * @param limit 限制数量
     * @return 文档信息列表
     */
    List<DocumentMetadata> listDocuments(int offset, int limit);

    /**
     * 搜索文档（按文件名）
     * @param keyword 关键词
     * @return 文档信息列表
     */
    List<DocumentMetadata> searchDocuments(String keyword);

    /**
     * 获取文档总数
     * @return 文档数量
     */
    long getDocumentCount();

    /**
     * 清理文档相关的所有数据
     * 包括分块、图像、PPL数据、优化数据
     * @param documentId 文档ID
     */
    void cleanupDocument(String documentId);

    /**
     * 检查文档是否存在
     * @param documentId 文档ID
     * @return 是否存在
     */
    boolean documentExists(String documentId);

    /**
     * 获取文档大小（字节数）
     * @param documentId 文档ID
     * @return 文档大小
     */
    long getDocumentSize(String documentId);

    // ========== 统计信息 (Statistics) ==========

    /**
     * 获取存储统计信息
     * @return 统计信息
     */
    StorageStatistics getStatistics();

    /**
     * 获取健康状态
     * @return 健康状态
     */
    boolean isHealthy();
}

