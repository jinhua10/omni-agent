package top.yumbo.ai.storage.api;

import top.yumbo.ai.storage.api.model.Chunk;
import top.yumbo.ai.storage.api.model.Image;
import top.yumbo.ai.storage.api.model.PPLData;
import top.yumbo.ai.storage.api.model.StorageStatistics;

import java.util.List;
import java.util.Optional;

/**
 * 文档存储服务接口
 * (Document Storage Service Interface)
 *
 * <p>用于存储文档分块、图像、PPL数据等大文件/非结构化数据</p>
 * <p>支持多种后端: File, MongoDB, S3, MinIO, Redis, Elasticsearch</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface DocumentStorageService {

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

    /**
     * 保存 PPL 分析结果
     * @param documentId 文档ID
     * @param data PPL数据
     * @return PPL数据ID
     */
    String savePPLData(String documentId, PPLData data);

    /**
     * 获取 PPL 数据
     * @param documentId 文档ID
     * @return PPL数据
     */
    Optional<PPLData> getPPLData(String documentId);

    /**
     * 删除 PPL 数据
     * @param documentId 文档ID
     */
    void deletePPLData(String documentId);

    // ========== 文档管理 (Document Management) ==========

    /**
     * 清理文档相关的所有数据
     * 包括分块、图像、PPL数据
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

