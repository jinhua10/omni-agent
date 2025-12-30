package top.yumbo.ai.omni.storage.api;

import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.storage.api.model.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * 文档存储服务接口
 * (Document Storage Service Interface)
 *
 * <h3>职责范围 (Responsibilities)</h3>
 * <p>本接口用于存储<strong>业务数据和内容</strong>，管理文档、图像、文本等大文件和非结构化数据</p>
 *
 * <h3>适用场景 (Use Cases)</h3>
 * <ul>
 *   <li>✅ 存储原始文档文件（PDF, PPT, Word等）</li>
 *   <li>✅ 保存提取的文本内容（可能很大）</li>
 *   <li>✅ 管理文档分块和图像</li>
 *   <li>✅ 存储RAG优化分析数据</li>
 *   <li>✅ 数据量大（MB-GB级别），简单CRUD</li>
 * </ul>
 *
 * <h3>不适用场景 (Not For)</h3>
 * <ul>
 *   <li>❌ 系统配置管理（请使用 {top.yumbo.ai.rag.hope.persistence.QuestionClassifierPersistence}）</li>
 *   <li>❌ 规则和元数据（请使用 Persistence API）</li>
 *   <li>❌ 需要复杂查询的结构化数据（请使用 Persistence API）</li>
 * </ul>
 *
 * <h3>支持的后端 (Supported Backends)</h3>
 * <p>File, MongoDB, S3, MinIO, Redis, Elasticsearch</p>
 *
 * <h3>与 Persistence 层的区别 (vs Persistence Layer)</h3>
 * <table border="1">
 *   <tr><th>特性</th><th>Storage (本接口)</th><th>Persistence</th></tr>
 *   <tr><td>数据类型</td><td>非结构化内容</td><td>结构化配置</td></tr>
 *   <tr><td>数据量</td><td>大（MB-GB）</td><td>小（KB）</td></tr>
 *   <tr><td>用途</td><td>业务数据</td><td>系统配置</td></tr>
 *   <tr><td>类比</td><td>图书馆"书架"</td><td>图书馆"目录"</td></tr>
 * </table>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * top.yumbo.ai.rag.hope.persistence.QuestionClassifierPersistence 配置和元数据持久化服务
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
     * 批量保存原始文档 ⭐ NEW
     * @param documents 文档列表（Map包含documentId, filename, fileData）
     * @return 批量操作结果
     */
    default BatchOperationResult saveDocuments(List<Map<String, Object>> documents) {
        List<String> successIds = new ArrayList<>();
        List<String> failureIds = new ArrayList<>();
        Map<String, String> errorMessages = new java.util.HashMap<>();

        for (Map<String, Object> doc : documents) {
            try {
                String documentId = (String) doc.get("documentId");
                String filename = (String) doc.get("filename");
                byte[] fileData = (byte[]) doc.get("fileData");

                String id = saveDocument(documentId, filename, fileData);
                successIds.add(id);
            } catch (Exception e) {
                String documentId = (String) doc.getOrDefault("documentId", "unknown");
                failureIds.add(documentId);
                errorMessages.put(documentId, e.getMessage());
            }
        }

        return BatchOperationResult.builder()
                .successCount(successIds.size())
                .failureCount(failureIds.size())
                .totalCount(documents.size())
                .successIds(successIds)
                .failureIds(failureIds)
                .errorMessages(errorMessages)
                .build();
    }

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

    /**
     * 批量删除原始文档 ⭐ NEW
     * @param documentIds 文档ID列表
     * @return 批量操作结果
     */
    default BatchOperationResult deleteDocuments(List<String> documentIds) {
        List<String> successIds = new ArrayList<>();
        List<String> failureIds = new ArrayList<>();
        Map<String, String> errorMessages = new java.util.HashMap<>();

        for (String documentId : documentIds) {
            try {
                deleteDocument(documentId);
                successIds.add(documentId);
            } catch (Exception e) {
                failureIds.add(documentId);
                errorMessages.put(documentId, e.getMessage());
            }
        }

        return BatchOperationResult.builder()
                .successCount(successIds.size())
                .failureCount(failureIds.size())
                .totalCount(documentIds.size())
                .successIds(successIds)
                .failureIds(failureIds)
                .errorMessages(errorMessages)
                .build();
    }

    // ========== 提取文本存储 (Extracted Text Storage) ⭐ NEW ==========

    /**
     * 保存提取的文本
     * @param documentId 文档ID
     * @param text 提取的文本内容
     * @return 存储ID
     */
    String saveExtractedText(String documentId, String text);

    /**
     * 获取提取的文本
     * @param documentId 文档ID
     * @return 提取的文本内容
     */
    Optional<String> getExtractedText(String documentId);

    /**
     * 删除提取的文本
     * @param documentId 文档ID
     */
    void deleteExtractedText(String documentId);

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

    /**
     * 检查图片是否已存在（通过哈希值去重）⭐ NEW
     * @param imageHash 图片哈希值
     * @return 如果存在返回已有的图片ID，否则返回空
     */
    default Optional<String> findImageByHash(String imageHash) {
        return Optional.empty();
    }

    /**
     * 批量保存图片 ⭐ NEW
     * @param documentId 文档ID
     * @param images 图片列表
     * @return 保存的图片ID列表
     */
    default List<String> saveImages(String documentId, List<Image> images) {
        List<String> imageIds = new ArrayList<>();
        for (Image image : images) {
            String imageId = saveImage(documentId, image);
            if (imageId != null) {
                imageIds.add(imageId);
            }
        }
        return imageIds;
    }

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

    // ========== 文档元数据管理 (Document Metadata Management) ⭐ ENHANCED ==========

    /**
     * 保存文档元数据
     * @param metadata 文档元数据
     */
    void saveMetadata(DocumentMetadata metadata);

    /**
     * 获取文档元数据
     * @param documentId 文档ID
     * @return 文档元数据
     */
    Optional<DocumentMetadata> getMetadata(String documentId);

    /**
     * 获取所有文档元数据（不推荐用于大数据量）
     * @return 文档元数据列表
     * @deprecated 推荐使用 {@link #getAllMetadata(PageRequest)} 分页查询
     */
    @Deprecated
    List<DocumentMetadata> getAllMetadata();

    /**
     * 分页查询文档元数据 ⭐ NEW
     * @param pageRequest 分页请求
     * @return 分页结果
     */
    default PageResult<DocumentMetadata> getAllMetadata(PageRequest pageRequest) {
        // 默认实现：使用旧方法并手动分页
        List<DocumentMetadata> allMetadata = getAllMetadata();
        int offset = pageRequest.getOffset();
        int limit = pageRequest.getLimit();

        List<DocumentMetadata> pagedContent = allMetadata.stream()
                .skip(offset)
                .limit(limit)
                .toList();

        return PageResult.of(pagedContent, pageRequest, allMetadata.size());
    }

    /**
     * 搜索文档元数据（支持分页）⭐ NEW
     * @param keyword 关键词
     * @param pageRequest 分页请求
     * @return 分页结果
     */
    default PageResult<DocumentMetadata> searchMetadata(String keyword, PageRequest pageRequest) {
        List<DocumentMetadata> searchResults = searchDocuments(keyword);
        int offset = pageRequest.getOffset();
        int limit = pageRequest.getLimit();

        List<DocumentMetadata> pagedContent = searchResults.stream()
                .skip(offset)
                .limit(limit)
                .toList();

        return PageResult.of(pagedContent, pageRequest, searchResults.size());
    }

    /**
     * 删除文档元数据
     * @param documentId 文档ID
     */
    void deleteMetadata(String documentId);

    /**
     * 批量删除文档元数据 ⭐ NEW
     * @param documentIds 文档ID列表
     * @return 批量操作结果
     */
    default BatchOperationResult deleteMetadataBatch(List<String> documentIds) {
        List<String> successIds = new ArrayList<>();
        List<String> failureIds = new ArrayList<>();
        Map<String, String> errorMessages = new java.util.HashMap<>();

        for (String documentId : documentIds) {
            try {
                deleteMetadata(documentId);
                successIds.add(documentId);
            } catch (Exception e) {
                failureIds.add(documentId);
                errorMessages.put(documentId, e.getMessage());
            }
        }

        return BatchOperationResult.builder()
                .successCount(successIds.size())
                .failureCount(failureIds.size())
                .totalCount(documentIds.size())
                .successIds(successIds)
                .failureIds(failureIds)
                .errorMessages(errorMessages)
                .build();
    }

    // ========== 文档管理 (Document Management) ⭐ ENHANCED ==========

    /**
     * 列出所有文档（不推荐用于大数据量）
     * @return 文档信息列表
     * @deprecated 推荐使用 {@link #listDocuments(PageRequest)} 分页查询
     */
    @Deprecated
    List<DocumentMetadata> listAllDocuments();

    /**
     * 列出文档（分页）
     * @param offset 偏移量
     * @param limit 限制数量
     * @return 文档信息列表
     */
    List<DocumentMetadata> listDocuments(int offset, int limit);

    /**
     * 列出文档（分页增强版）⭐ NEW
     * @param pageRequest 分页请求
     * @return 分页结果
     */
    default PageResult<DocumentMetadata> listDocuments(PageRequest pageRequest) {
        List<DocumentMetadata> documents = listDocuments(
                pageRequest.getOffset(),
                pageRequest.getLimit()
        );
        long total = getDocumentCount();
        return PageResult.of(documents, pageRequest, total);
    }

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
     * 批量清理文档 ⭐ NEW
     * @param documentIds 文档ID列表
     * @return 批量操作结果
     */
    default BatchOperationResult cleanupDocuments(List<String> documentIds) {
        List<String> successIds = new ArrayList<>();
        List<String> failureIds = new ArrayList<>();
        Map<String, String> errorMessages = new java.util.HashMap<>();

        for (String documentId : documentIds) {
            try {
                cleanupDocument(documentId);
                successIds.add(documentId);
            } catch (Exception e) {
                failureIds.add(documentId);
                errorMessages.put(documentId, e.getMessage());
            }
        }

        return BatchOperationResult.builder()
                .successCount(successIds.size())
                .failureCount(failureIds.size())
                .totalCount(documentIds.size())
                .successIds(successIds)
                .failureIds(failureIds)
                .errorMessages(errorMessages)
                .build();
    }

    /**
     * 检查文档是否存在
     * @param documentId 文档ID
     * @return 是否存在
     */
    boolean documentExists(String documentId);

    /**
     * 批量检查文档是否存在 ⭐ NEW
     * @param documentIds 文档ID列表
     * @return 存在的文档ID列表和不存在的文档ID列表
     */
    default Map<String, List<String>> checkDocumentsExist(List<String> documentIds) {
        List<String> existingIds = new ArrayList<>();
        List<String> missingIds = new ArrayList<>();

        for (String documentId : documentIds) {
            if (documentExists(documentId)) {
                existingIds.add(documentId);
            } else {
                missingIds.add(documentId);
            }
        }

        Map<String, List<String>> result = new java.util.HashMap<>();
        result.put("existing", existingIds);
        result.put("missing", missingIds);
        return result;
    }

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

    // ========== 文件系统浏览 (File System Browse) ==========

    /**
     * 列出指定路径下的文件和文件夹
     * @param virtualPath 虚拟路径
     * @return 文件和文件夹列表，每项包含: name, type(file/directory), path, size, modified等
     */
    List<Map<String, Object>> listFiles(String virtualPath);

    /**
     * 读取文件内容
     * @param virtualPath 虚拟路径
     * @return 文件内容
     */
    byte[] readFile(String virtualPath);

    /**
     * 删除文件或文件夹
     * @param virtualPath 虚拟路径
     * @return 是否删除成功
     */
    boolean deleteFile(String virtualPath);

    /**
     * 创建目录
     * @param virtualPath 虚拟路径
     * @return 是否创建成功
     */
    boolean createDirectory(String virtualPath);

    /**
     * 获取存储统计信息（指定路径）
     * @param virtualPath 虚拟路径
     * @return 统计信息，包含: totalFiles, totalFolders, totalSize等
     */
    Map<String, Object> getStorageStats(String virtualPath);
}

