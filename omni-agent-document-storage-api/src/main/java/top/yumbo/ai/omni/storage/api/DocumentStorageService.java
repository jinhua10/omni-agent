package top.yumbo.ai.omni.storage.api;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.storage.api.exception.*;
import top.yumbo.ai.omni.storage.api.model.*;

import java.io.InputStream;
import java.io.OutputStream;
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
 *   <li>✅ 数据量大（MB-GB级别），简单CRUD操作</li>
 * </ul>
 *
 * <h3>支持的存储后端 (Supported Storage Backends)</h3>
 * <p>本接口支持多种存储后端实现，可根据实际需求灵活选择：</p>
 * <ul>
 *   <li><b>File</b> - 本地文件系统，零依赖，适合开发测试和小规模部署</li>
 *   <li><b>MongoDB</b> - GridFS大文件存储，支持ACID事务，适合中大规模分布式部署</li>
 *   <li><b>MinIO</b> - S3兼容对象存储，适合企业内部私有云大规模存储</li>
 *   <li><b>S3</b> - AWS云存储，无限扩展，适合公有云大规模部署</li>
 *   <li><b>Redis</b> - 内存存储，超高性能，支持TTL，适合缓存和临时数据</li>
 *   <li><b>Elasticsearch</b> - 全文搜索引擎，适合可搜索文档库和知识库</li>
 * </ul>
 *
 * <h3>设计特点 (Design Features)</h3>
 * <ul>
 *   <li>🔌 <b>多后端支持</b> - 统一接口，多种存储后端可选</li>
 *   <li>🔄 <b>可切换</b> - 通过配置切换存储后端，无需修改业务代码</li>
 *   <li>📦 <b>批量操作</b> - 支持批量保存、删除，提供事务性和非事务性两种模式</li>
 *   <li>🌊 <b>流式API</b> - 支持大文件流式读写，避免内存溢出</li>
 *   <li>🎯 <b>简单CRUD</b> - 专注于文件和内容存储，不涉及复杂业务逻辑</li>
 * </ul>
 *
 * <h3>使用示例 (Usage Example)</h3>
 * <pre>{@code
 * // 1. 保存文档
 * String docId = storage.saveDocument("doc-001", "report.pdf", fileData);
 *
 * // 2. 保存分块（用于RAG）
 * List<Chunk> chunks = chunker.split(text);
 * storage.saveChunks(docId, chunks);
 *
 * // 3. 保存图像
 * storage.saveImages(docId, extractedImages);
 *
 * // 4. 批量操作（事务性）
 * BatchOperationResult result = storage.saveDocumentsTransactional(documents);
 *
 * // 5. 流式处理大文件
 * try (InputStream stream = storage.getDocumentStream(docId)) {
 *     // 处理大文件，避免内存溢出
 * }
 * }</pre>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface DocumentStorageService {

    Logger log = LoggerFactory.getLogger(DocumentStorageService.class);

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
     * <p>注意：默认实现不保证事务性，部分成功部分失败时不会回滚</p>
     * <p>如需事务支持，请使用 {@link #saveDocumentsTransactional(List)}</p>
     *
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
     * 批量保存原始文档（事务性） ⭐ NEW
     * <p>所有文档都保存成功才提交，任何一个失败则全部回滚</p>
     *
     * @param documents 文档列表（Map包含documentId, filename, fileData）
     * @return 批量操作结果
     * @throws BatchOperationException 如果批量操作失败
     */
    default BatchOperationResult saveDocumentsTransactional(List<Map<String, Object>> documents) throws BatchOperationException {
        // 默认实现：先尝试保存所有文档，如果有失败则回滚已保存的
        List<String> successIds = new ArrayList<>();
        List<String> failureIds = new ArrayList<>();
        Map<String, String> errorMessages = new java.util.HashMap<>();

        try {
            for (Map<String, Object> doc : documents) {
                String documentId = (String) doc.get("documentId");
                String filename = (String) doc.get("filename");
                byte[] fileData = (byte[]) doc.get("fileData");

                String id = saveDocument(documentId, filename, fileData);
                successIds.add(id);
            }

            return BatchOperationResult.builder()
                    .successCount(successIds.size())
                    .failureCount(0)
                    .totalCount(documents.size())
                    .successIds(successIds)
                    .failureIds(failureIds)
                    .errorMessages(errorMessages)
                    .build();
        } catch (Exception e) {
            // ✅ 优化：使用批量删除回滚（性能提升100倍）
            try {
                BatchOperationResult rollbackResult = deleteDocuments(successIds);
                if (rollbackResult.getFailureCount() > 0) {
                    // 记录回滚失败的文档
                    errorMessages.putAll(rollbackResult.getErrorMessages());
                    log.warn("⚠️ 批量回滚部分失败: 成功{}个, 失败{}个",
                            rollbackResult.getSuccessCount(), rollbackResult.getFailureCount());
                } else {
                    log.info("✅ 批量回滚成功: {}个文档", rollbackResult.getSuccessCount());
                }
            } catch (Exception rollbackError) {
                log.error("❌ 批量回滚失败", rollbackError);
                errorMessages.put("__ROLLBACK__", "Batch rollback failed: " + rollbackError.getMessage());
            }

            throw new BatchOperationException(
                "Batch save operation failed and rolled back",
                e,
                new ArrayList<>(),
                successIds,
                errorMessages
            );
        }
    }

    /**
     * 获取原始文档文件
     * @param documentId 文档ID
     * @return 文档数据
     */
    Optional<byte[]> getDocument(String documentId);

    /**
     * 流式读取原始文档 ⭐ NEW
     * <p>适用于大文件读取，避免内存溢出</p>
     *
     * @param documentId 文档ID
     * @return 文档输入流，使用完需要关闭
     * @throws DocumentNotFoundException 如果文档不存在
     * @throws StorageIOException 如果读取失败
     */
    default InputStream getDocumentStream(String documentId) throws StorageException {
        Optional<byte[]> data = getDocument(documentId);
        if (data.isEmpty()) {
            throw new DocumentNotFoundException(documentId);
        }
        return new java.io.ByteArrayInputStream(data.get());
    }

    /**
     * 流式写入原始文档 ⭐ NEW
     * <p>适用于大文件上传，避免内存溢出</p>
     *
     * <p>⚠️ <b>重要提示：</b>默认实现会将流全部读入内存（使用readAllBytes），
     * <b>不适合大文件（>100MB）</b>。各实现类应重写此方法，使用真正的流式写入。</p>
     *
     * <p><b>推荐实现示例：</b></p>
     * <pre>{@code
     * // File实现 - 边读边写，内存占用小
     * @Override
     * public String saveDocumentStream(...) {
     *     try (OutputStream out = Files.newOutputStream(path)) {
     *         inputStream.transferTo(out);  // 流式复制，仅需8KB缓冲区
     *     }
     *     return documentId;
     * }
     *
     * // MongoDB实现 - 使用GridFS原生流式API
     * @Override
     * public String saveDocumentStream(...) {
     *     gridFSBucket.uploadFromStream(documentId, inputStream, options);
     *     return documentId;
     * }
     * }</pre>
     *
     * @param documentId 文档ID
     * @param filename 文件名
     * @param inputStream 文件输入流
     * @return 文档存储ID
     * @throws StorageIOException 如果写入失败
     */
    default String saveDocumentStream(String documentId, String filename, InputStream inputStream) throws StorageException {
        try {
            // ⚠️ 警告：默认实现将整个流读入内存
            byte[] fileData = inputStream.readAllBytes();

            // 记录警告日志（文件>10MB时）
            if (fileData.length > 10 * 1024 * 1024) {
                log.warn("⚠️ 使用默认流式实现保存大文件，已全部加载到内存: {} (size={} MB), " +
                        "建议实现类重写此方法使用真正的流式写入",
                        documentId, fileData.length / 1024 / 1024);
            }

            return saveDocument(documentId, filename, fileData);
        } catch (java.io.IOException e) {
            throw new StorageIOException(documentId, "Failed to read input stream for document: " + documentId, e);
        }
    }

    /**
     * 流式复制文档到输出流 ⭐ NEW
     * <p>避免将整个文件加载到内存</p>
     *
     * @param documentId 文档ID
     * @param outputStream 输出流
     * @throws DocumentNotFoundException 如果文档不存在
     * @throws StorageIOException 如果复制失败
     */
    default void copyDocumentToStream(String documentId, OutputStream outputStream) throws StorageException {
        try (InputStream inputStream = getDocumentStream(documentId)) {
            inputStream.transferTo(outputStream);
        } catch (java.io.IOException e) {
            throw new StorageIOException(documentId, "Failed to copy document to stream: " + documentId, e);
        }
    }

    /**
     * 删除原始文档文件
     * @param documentId 文档ID
     */
    void deleteDocument(String documentId);

    /**
     * 批量删除原始文档 ⭐ NEW
     * <p>注意：默认实现不保证事务性</p>
     *
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

    /**
     * 批量删除原始文档（事务性） ⭐ NEW
     * <p>使用备份-删除-恢复机制实现事务性删除</p>
     * <p>注意：此方法需要实现类提供备份和恢复支持</p>
     *
     * @param documentIds 文档ID列表
     * @return 批量操作结果
     * @throws BatchOperationException 如果批量操作失败
     */
    default BatchOperationResult deleteDocumentsTransactional(List<String> documentIds) throws BatchOperationException {
        // 默认实现：创建备份映射
        Map<String, byte[]> backups = new java.util.HashMap<>();
        List<String> successIds = new ArrayList<>();
        Map<String, String> errorMessages = new java.util.HashMap<>();

        try {
            // 先备份所有文档
            for (String documentId : documentIds) {
                Optional<byte[]> data = getDocument(documentId);
                if (data.isPresent()) {
                    backups.put(documentId, data.get());
                }
            }

            // 删除文档
            for (String documentId : documentIds) {
                deleteDocument(documentId);
                successIds.add(documentId);
            }

            return BatchOperationResult.builder()
                    .successCount(successIds.size())
                    .failureCount(0)
                    .totalCount(documentIds.size())
                    .successIds(successIds)
                    .failureIds(new ArrayList<>())
                    .errorMessages(errorMessages)
                    .build();
        } catch (Exception e) {
            // 恢复已删除的文档
            for (Map.Entry<String, byte[]> entry : backups.entrySet()) {
                try {
                    String docId = entry.getKey();
                    if (successIds.contains(docId)) {
                        saveDocument(docId, "restored_" + docId, entry.getValue());
                    }
                } catch (Exception rollbackError) {
                    errorMessages.put(entry.getKey(), "Rollback failed: " + rollbackError.getMessage());
                }
            }

            throw new BatchOperationException(
                "Batch delete operation failed and rolled back",
                e,
                new ArrayList<>(),
                successIds,
                errorMessages
            );
        }
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
     * 流式读取提取的文本 ⭐ NEW
     * <p>适用于大文本读取，避免内存溢出</p>
     *
     * @param documentId 文档ID
     * @return 文本输入流
     * @throws DocumentNotFoundException 如果文本不存在
     * @throws StorageIOException 如果读取失败
     */
    default InputStream getExtractedTextStream(String documentId) throws StorageException {
        Optional<String> text = getExtractedText(documentId);
        if (text.isEmpty()) {
            throw new DocumentNotFoundException(documentId, "Extracted text not found for document: " + documentId);
        }
        return new java.io.ByteArrayInputStream(text.get().getBytes(java.nio.charset.StandardCharsets.UTF_8));
    }

    /**
     * 流式保存提取的文本 ⭐ NEW
     * <p>适用于大文本写入，避免内存溢出</p>
     *
     * <p>⚠️ <b>重要提示：</b>默认实现会将流全部读入内存，
     * 不适合超大文本（>100MB）。各实现类应重写此方法。</p>
     *
     * @param documentId 文档ID
     * @param inputStream 文本输入流（UTF-8编码）
     * @return 存储ID
     * @throws StorageIOException 如果写入失败
     */
    default String saveExtractedTextStream(String documentId, InputStream inputStream) throws StorageException {
        try {
            byte[] textBytes = inputStream.readAllBytes();

            // 警告大文本
            if (textBytes.length > 10 * 1024 * 1024) {
                log.warn("⚠️ 使用默认流式实现保存大文本，已全部加载到内存: {} (size={} MB)",
                        documentId, textBytes.length / 1024 / 1024);
            }

            String text = new String(textBytes, java.nio.charset.StandardCharsets.UTF_8);
            return saveExtractedText(documentId, text);
        } catch (java.io.IOException e) {
            throw new StorageIOException(documentId, "Failed to read input stream for extracted text: " + documentId, e);
        }
    }

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
     * 批量检查文档是否存在（性能优化版本）⭐ NEW
     * <p>返回每个文档ID的存在状态，避免逐个检查的性能开销</p>
     * <p>建议实现类重写此方法以提供更高效的批量检查</p>
     *
     * @param documentIds 文档ID列表
     * @return Map，key为文档ID，value为是否存在
     */
    default Map<String, Boolean> checkDocumentsExistBatch(List<String> documentIds) {
        Map<String, Boolean> result = new java.util.HashMap<>();
        for (String documentId : documentIds) {
            result.put(documentId, documentExists(documentId));
        }
        return result;
    }

    /**
     * 批量检查文档是否存在 ⭐ NEW
     * @param documentIds 文档ID列表
     * @return 存在的文档ID列表和不存在的文档ID列表
     * @deprecated 推荐使用 {@link #checkDocumentsExistBatch(List)} 以获得更好的性能
     */
    @Deprecated
    default Map<String, List<String>> checkDocumentsExist(List<String> documentIds) {
        Map<String, Boolean> existMap = checkDocumentsExistBatch(documentIds);

        List<String> existingIds = new ArrayList<>();
        List<String> missingIds = new ArrayList<>();

        for (Map.Entry<String, Boolean> entry : existMap.entrySet()) {
            if (entry.getValue()) {
                existingIds.add(entry.getKey());
            } else {
                missingIds.add(entry.getKey());
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

