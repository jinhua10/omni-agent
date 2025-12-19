package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import top.yumbo.ai.omni.web.util.DocumentParserUtil;
import top.yumbo.ai.omni.web.util.FileStorageUtil;
import top.yumbo.ai.rag.api.model.SearchResult;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.omni.core.document.DocumentProcessor;
import top.yumbo.ai.omni.core.document.DocumentProcessorManager;
import top.yumbo.ai.omni.core.chunking.ChunkingStrategyManager;
import top.yumbo.ai.omni.core.image.ImageStorageService;
import top.yumbo.ai.storage.api.model.Chunk;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 文档管理控制器（简化版）
 * (Document Management Controller - Simplified)
 *
 * <p>基于分块API的文档管理</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/documents")
@RequiredArgsConstructor
public class DocumentManagementController {

    private final DocumentStorageService storageService;
    private final RAGService ragService;
    private final DocumentProcessorManager documentProcessorManager;
    private final ChunkingStrategyManager chunkingStrategyManager;
    private final ImageStorageService imageStorageService;

    // 可选依赖：RAG 优化服务 ⭐
    @Autowired(required = false)
    private top.yumbo.ai.omni.core.optimization.RAGOptimizationService ragOptimizationService;

    /**
     * 上传文档（简化版：直接索引到RAG）
     * POST /api/documents/upload
     */
    @PostMapping("/upload")
    public UploadResponse uploadDocument(
            @RequestParam("file") MultipartFile file,
            @RequestParam(value = "autoIndex", defaultValue = "true") boolean autoIndex) {

        UploadResponse response = new UploadResponse();

        try {
            if (file.isEmpty()) {
                response.setSuccess(false);
                response.setMessage("文件不能为空");
                return response;
            }

            String filename = file.getOriginalFilename() != null ? file.getOriginalFilename() : "unknown";
            log.info("上传文档: filename={}, size={} bytes", filename, file.getSize());

            // 生成文档ID
            String documentId = "doc_" + System.currentTimeMillis() + "_" +
                filename.replaceAll("[^a-zA-Z0-9._-]", "_");

            // 保存原始文件到 DocumentStorageService (保存到 data/storage/documents/文件名.原扩展名)
            log.info("💾 保存原始文件到存储服务...");
            String savedDocId = storageService.saveDocument(filename, filename, file.getBytes());
            if (savedDocId == null) {
                throw new Exception("保存原始文件失败");
            }
            log.info("✅ 原始文件已保存: {}", filename);

            // === 新流程：使用 DocumentProcessorManager 处理文档 ===
            String content;
            String fileExtension = getFileExtension(filename);

            try {
                log.info("🔄 使用 DocumentProcessorManager 处理文档...");

                // 1. 构建处理上下文
                DocumentProcessor.ProcessingContext context = DocumentProcessor.ProcessingContext.builder()
                        .fileBytes(file.getBytes())
                        .filePath(null)  // 使用字节数组，不需要路径
                        .fileExtension(fileExtension)
                        .originalFileName(filename)
                        .fileSize(file.getSize())
                        .options(new HashMap<>())
                        .build();

                // 2. 处理文档（文本提取 / Vision LLM 识别）
                DocumentProcessor.ProcessingResult result = documentProcessorManager.processDocument(context);

                if (result.isSuccess()) {
                    content = result.getContent();
                    log.info("✅ 文档处理成功: processor={}, 内容长度={} chars, 耗时={}ms",
                            result.getProcessorName(), content.length(), result.getProcessingTimeMs());

                    // 2.1 保存提取的图片到存储 ⭐ 使用文件名,包含 metadata
                    if (result.getImages() != null && !result.getImages().isEmpty()) {
                        log.info("🖼️ 保存提取的图片: {} 张", result.getImages().size());
                        int savedImageCount = 0;
                        for (DocumentProcessor.ExtractedImage extractedImage : result.getImages()) {
                            try {
                                String imageId = imageStorageService.saveImage(
                                        filename,  // 使用文件名而不是 documentId
                                        extractedImage.getData(),
                                        extractedImage.getFormat(),
                                        extractedImage.getMetadata());  // 传递 metadata ⭐
                                if (imageId != null) {
                                    savedImageCount++;
                                }
                            } catch (Exception ex) {
                                log.warn("⚠️ 保存图片失败: {}", ex.getMessage());
                            }
                        }
                        log.info("✅ 图片已保存: {} 张", savedImageCount);
                    }
                } else {
                    throw new Exception("文档处理失败: " + result.getError());
                }

            } catch (Exception e) {
                log.warn("⚠️ DocumentProcessor 处理失败，降级使用 DocumentParserUtil: {}", e.getMessage());
                try {
                    content = DocumentParserUtil.parseDocument(file);
                } catch (Exception ex) {
                    log.warn("⚠️ DocumentParserUtil 也失败，使用原始字节内容");
                    content = new String(file.getBytes(), StandardCharsets.UTF_8);
                }
            }

            // === 新流程：使用 ChunkingStrategyManager 进行分块 ===
            if (autoIndex) {
                try {
                    log.info("📦 使用 ChunkingStrategyManager 进行分块...");

                    // 1. 使用分块策略管理器进行分块（自动选择策略）
                    List<Chunk> chunks = chunkingStrategyManager.chunkWithAutoStrategy(
                            documentId, content, filename);
                    log.info("✅ 分块完成: 共 {} 个块, 策略: {}",
                            chunks.size(),
                            chunks.isEmpty() ? "unknown" : chunks.get(0).getMetadata().get("strategy"));

                    // 2. 保存分块到 DocumentStorageService（会保存到 ./data/storage/chunks/文件名/ 目录）⭐
                    log.info("💾 保存分块到存储服务...");
                    List<String> savedChunkIds = storageService.saveChunks(filename, chunks);
                    log.info("✅ 分块已保存到存储: {} 个文件", savedChunkIds.size());

                    // 3. 为每个块创建文档并索引到 RAG
                    log.info("📇 索引分块到 RAG...");
                    int indexed = 0;
                    for (Chunk chunk : chunks) {
                        Document document = Document.builder()
                                .id(chunk.getId())
                                .title(filename + " (块 " + chunk.getSequence() + ")")
                                .content(chunk.getContent())
                                .summary("块 " + chunk.getSequence())
                                .source("upload")
                                .type("chunk")
                                .metadata(Map.of(
                                        "fileName", filename,
                                        "storagePath", filename,                    // ⭐ 存储路径
                                        "documentId", documentId,
                                        "chunkIndex", chunk.getSequence()
                                ))
                                .build();

                        ragService.indexDocument(document);
                        indexed++;
                    }

                    log.info("✅ 索引完成: 共索引 {} 个文档块", indexed);

                    // === 可选：生成 PPL 和 Optimization 数据 ⭐
                    if (ragOptimizationService != null) {
                        try {
                            log.info("📊 生成 RAG 优化数据...");
                            generateOptimizationData(filename, content, chunks);
                        } catch (Exception optEx) {
                            log.warn("⚠️ 生成优化数据失败（不影响主流程）: {}", optEx.getMessage());
                        }
                    }

                    response.setMessage("文档上传成功，已分块并索引（" + indexed + " 个块）");

                } catch (Exception e) {
                    log.warn("⚠️ 分块失败，降级使用整文档索引: {}", e.getMessage());

                    // 降级：直接索引整个文档
                    Document document = Document.builder()
                            .id(documentId)
                            .title(filename)
                            .content(content)
                            .source("upload")
                            .type("document")
                            .build();

                    ragService.indexDocument(document);
                    response.setMessage("文档上传成功（未分块）");
                }
            } else {
                response.setMessage("文档上传成功（未索引）");
            }

            response.setSuccess(true);
            response.setFileName(filename);
            response.setFileSize(file.getSize());
            response.setDocumentId(documentId);
            response.setAutoIndexed(autoIndex);

            log.info("文档上传成功: id={}", documentId);

        } catch (Exception e) {
            log.error("文档上传失败", e);
            response.setSuccess(false);
            response.setMessage("文档上传失败: " + e.getMessage());
        }

        return response;
    }

    /**
     * 批量上传文档
     * POST /api/documents/upload-batch
     */
    @PostMapping("/upload-batch")
    public BatchUploadResponse uploadBatch(
            @RequestParam("files") MultipartFile[] files,
            @RequestParam(value = "autoIndex", defaultValue = "true") boolean autoIndex) {

        BatchUploadResponse response = new BatchUploadResponse();
        List<UploadResult> results = new ArrayList<>();
        int successCount = 0;
        int failCount = 0;

        try {
            log.info("批量上传文档: count={}", files.length);

            for (MultipartFile file : files) {
                UploadResult uploadResult = new UploadResult();
                uploadResult.setFileName(file.getOriginalFilename());

                try {
                    if (file.isEmpty()) {
                        uploadResult.setSuccess(false);
                        uploadResult.setMessage("文件为空");
                        failCount++;
                        results.add(uploadResult);
                        continue;
                    }

                    String filename = file.getOriginalFilename() != null ? file.getOriginalFilename() : "unknown";

                    // 生成文档ID
                    String documentId = "doc_" + System.currentTimeMillis() + "_" +
                        filename.replaceAll("[^a-zA-Z0-9._-]", "_");

                    // 保存原始文件到 DocumentStorageService (保存到 data/storage/documents/文件名.原扩展名)
                    String savedDocId = storageService.saveDocument(filename, filename, file.getBytes());
                    if (savedDocId == null) {
                        throw new Exception("保存原始文件失败");
                    }

                    // === 使用 DocumentProcessorManager 处理文档 ===
                    String content;
                    String fileExtension = getFileExtension(filename);

                    try {
                        log.info("🔄 使用 DocumentProcessorManager 处理文档: {}", filename);

                        DocumentProcessor.ProcessingContext context = DocumentProcessor.ProcessingContext.builder()
                                .fileBytes(file.getBytes())
                                .filePath(null)
                                .fileExtension(fileExtension)
                                .originalFileName(filename)
                                .fileSize(file.getSize())
                                .options(new HashMap<>())
                                .build();

                        DocumentProcessor.ProcessingResult result = documentProcessorManager.processDocument(context);

                        if (result.isSuccess()) {
                            content = result.getContent();
                            log.info("✅ 文档处理成功: processor={}, 内容长度={} chars",
                                    result.getProcessorName(), content.length());

                            // 保存提取的图片 ⭐ 使用文件名,包含 metadata
                            if (result.getImages() != null && !result.getImages().isEmpty()) {
                                log.info("🖼️ 保存提取的图片: {} 张", result.getImages().size());
                                int savedImageCount = 0;
                                for (DocumentProcessor.ExtractedImage extractedImage : result.getImages()) {
                                    try {
                                        String imageId = imageStorageService.saveImage(
                                                filename,  // 使用文件名而不是 documentId
                                                extractedImage.getData(),
                                                extractedImage.getFormat(),
                                                extractedImage.getMetadata());  // 传递 metadata ⭐
                                        if (imageId != null) {
                                            savedImageCount++;
                                        }
                                    } catch (Exception ex) {
                                        log.warn("⚠️ 保存图片失败: {}", ex.getMessage());
                                    }
                                }
                                log.info("✅ 图片已保存: {} 张", savedImageCount);
                            }
                        } else {
                            throw new Exception("文档处理失败: " + result.getError());
                        }

                    } catch (Exception e) {
                        log.warn("⚠️ DocumentProcessor 处理失败，降级使用 DocumentParserUtil: {}", e.getMessage());
                        try {
                            content = DocumentParserUtil.parseDocument(file);
                        } catch (Exception ex) {
                            log.warn("⚠️ DocumentParserUtil 也失败，使用原始字节内容");
                            content = new String(file.getBytes(), StandardCharsets.UTF_8);
                        }
                    }

                    // === 使用 ChunkingStrategyManager 进行分块 ===
                    if (autoIndex) {
                        try {
                            log.info("📦 使用 ChunkingStrategyManager 进行分块: {}", filename);

                            List<Chunk> chunks = chunkingStrategyManager.chunkWithAutoStrategy(
                                    documentId, content, filename);
                            log.info("✅ 分块完成: 共 {} 个块", chunks.size());

                            // 保存分块到存储服务⭐
                            log.info("💾 保存分块到存储服务: {}", filename);
                            List<String> savedChunkIds = storageService.saveChunks(filename, chunks);
                            log.info("✅ 分块已保存: {} 个文件", savedChunkIds.size());

                            // 索引到 RAG
                            int indexed = 0;
                            for (Chunk chunk : chunks) {
                                Document document = Document.builder()
                                        .id(chunk.getId())
                                        .title(filename + " (块 " + chunk.getSequence() + ")")
                                        .content(chunk.getContent())
                                        .summary("块 " + chunk.getSequence())
                                        .source("upload")
                                        .type("chunk")
                                        .metadata(Map.of(
                                                "fileName", filename,
                                                "storagePath", filename,                    // ⭐ 存储路径
                                                "documentId", documentId,
                                                "chunkIndex", chunk.getSequence()
                                        ))
                                        .build();

                                ragService.indexDocument(document);
                                indexed++;
                            }

                            // === 可选：生成 PPL 和 Optimization 数据 ⭐
                            if (ragOptimizationService != null) {
                                try {
                                    log.info("📊 生成 RAG 优化数据: {}", filename);
                                    generateOptimizationData(filename, content, chunks);
                                } catch (Exception optEx) {
                                    log.warn("⚠️ 生成优化数据失败: {}", optEx.getMessage());
                                }
                            }

                            uploadResult.setMessage("上传成功，已分块并索引（" + indexed + " 个块）");

                        } catch (Exception e) {
                            log.warn("⚠️ 分块失败，降级使用整文档索引: {}", e.getMessage());

                            Document document = Document.builder()
                                    .id(documentId)
                                    .title(filename)
                                    .content(content)
                                    .source("upload")
                                    .type("document")
                                    .build();

                            ragService.indexDocument(document);
                            uploadResult.setMessage("上传成功（未分块）");
                        }
                    } else {
                        uploadResult.setMessage("上传成功（未索引）");
                    }

                    uploadResult.setSuccess(true);
                    uploadResult.setDocumentId(documentId);
                    uploadResult.setFileSize(file.getSize());
                    successCount++;

                } catch (Exception e) {
                    log.error("上传文件失败: {}", file.getOriginalFilename(), e);
                    uploadResult.setSuccess(false);
                    uploadResult.setMessage("上传失败: " + e.getMessage());
                    failCount++;
                }

                results.add(uploadResult);
            }

            response.setSuccess(true);
            response.setMessage(String.format("批量上传完成: 成功 %d, 失败 %d", successCount, failCount));
            response.setSuccessCount(successCount);
            response.setFailureCount(failCount);
            response.setResults(results);

            log.info("批量上传完成: success={}, fail={}", successCount, failCount);

        } catch (Exception e) {
            log.error("批量上传失败", e);
            response.setSuccess(false);
            response.setMessage("批量上传失败: " + e.getMessage());
            response.setSuccessCount(successCount);
            response.setFailureCount(failCount);
            response.setResults(results);
        }

        return response;
    }

    /**
     * 下载文档
     * GET /api/documents/download
     */
    @GetMapping("/download")
    public ResponseEntity<Resource> downloadDocument(@RequestParam String fileName) {
        try {
            log.info("下载文档请求: fileName={}", fileName);

            // 查找文件
            Path filePath = FileStorageUtil.findFileByName(fileName);
            if (filePath == null || !Files.exists(filePath)) {
                log.warn("文件不存在: {}", fileName);
                return ResponseEntity.notFound().build();
            }

            // 加载文件为资源
            Resource resource = new UrlResource(filePath.toUri());
            if (!resource.exists() || !resource.isReadable()) {
                log.error("文件不可读: {}", filePath);
                return ResponseEntity.notFound().build();
            }

            // 获取文件的 MIME 类型
            String contentType = Files.probeContentType(filePath);
            if (contentType == null) {
                contentType = "application/octet-stream";
            }

            // 对文件名进行 URL 编码，支持中文文件名
            String encodedFileName = URLEncoder.encode(fileName, StandardCharsets.UTF_8.toString())
                    .replaceAll("\\+", "%20");

            log.info("文件下载成功: {}, size={} bytes", fileName, Files.size(filePath));

            return ResponseEntity.ok()
                    .contentType(MediaType.parseMediaType(contentType))
                    .header(HttpHeaders.CONTENT_DISPOSITION,
                            "attachment; filename=\"" + fileName + "\"; filename*=UTF-8''" + encodedFileName)
                    .body(resource);

        } catch (Exception e) {
            log.error("文件下载失败: {}", fileName, e);
            return ResponseEntity.internalServerError().build();
        }
    }

    /**
     * 删除文档
     * DELETE /api/documents/{documentId}
     *
     * 注意：documentId可以是文档ID或文件名，会自动查找匹配的文档
     */
    @DeleteMapping("/{documentId}")
    public Map<String, Object> deleteDocument(@PathVariable String documentId) {
        Map<String, Object> result = new HashMap<>();

        try {
            log.info("🗑️ 删除文档请求: {}", documentId);
            log.debug("文档ID字节长度: {}, 实际字符数: {}", documentId.getBytes().length, documentId.length());

            // 尝试查找文档（可能传入的是文件名）
            String actualDocumentId = documentId;

            // 如果documentId不是以doc_开头，可能是文件名，需要搜索对应的文档
            if (!documentId.startsWith("doc_")) {
                log.info("检测到可能是文件名，尝试搜索对应的文档: {}", documentId);

                // 使用文件名搜索文档
                List<SearchResult> searchResults = ragService.searchByText(documentId, 10);

                // 查找title完全匹配的文档
                for (SearchResult sr : searchResults) {
                    Document doc = sr.getDocument();
                    if (doc != null && doc.getTitle() != null && doc.getTitle().equals(documentId)) {
                        actualDocumentId = doc.getId();
                        log.info("找到匹配的文档ID: {}", actualDocumentId);
                        break;
                    }
                }

                // 如果没找到完全匹配的，使用第一个搜索结果
                if (actualDocumentId.equals(documentId) && !searchResults.isEmpty() && searchResults.get(0).getDocument() != null) {
                    actualDocumentId = searchResults.get(0).getDocument().getId();
                    log.info("使用第一个搜索结果的文档ID: {}", actualDocumentId);
                }
            }

            // 1. 删除原始文档文件
            storageService.deleteDocument(actualDocumentId);
            log.info("原始文档已删除: {}", actualDocumentId);

            // 2. 删除文档的所有分块
            storageService.deleteChunksByDocument(actualDocumentId);
            // 3. 删除文档的所有图片
            storageService.deleteImagesByDocument(actualDocumentId);
            // 4. 删除RAG索引
            boolean deleted = ragService.deleteDocument(actualDocumentId);

            if (deleted) {
                result.put("status", "success");
                result.put("message", "文档删除成功（包括原始文件、分块、图片）");
                result.put("documentId", actualDocumentId);
                log.info("文档删除成功: {}", actualDocumentId);
            } else {
                result.put("status", "error");
                result.put("message", "文档删除失败：RAG删除返回false");
                result.put("documentId", actualDocumentId);
                log.warn("文档删除失败: {}", actualDocumentId);
            }

        } catch (Exception e) {
            log.error("删除文档失败: {}", documentId, e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 批量删除文档
     * POST /api/documents/delete/batch
     */
    @PostMapping("/delete/batch")
    public Map<String, Object> deleteDocuments(@RequestBody BatchDeleteRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            int successCount = 0;
            int failCount = 0;

            for (String documentId : request.getDocumentIds()) {
                try {
                    // 删除原始文档
                    storageService.deleteDocument(documentId);
                    // 删除分块
                    storageService.deleteChunksByDocument(documentId);
                    // 删除图片
                    storageService.deleteImagesByDocument(documentId);
                    // 删除RAG索引
                    ragService.deleteDocument(documentId);
                    successCount++;
                } catch (Exception e) {
                    failCount++;
                    log.warn("删除文档失败: {}", documentId, e);
                }
            }

            result.put("status", "success");
            result.put("totalCount", request.getDocumentIds().size());
            result.put("successCount", successCount);
            result.put("failCount", failCount);
            result.put("message", String.format("删除完成: 成功 %d, 失败 %d", successCount, failCount));

        } catch (Exception e) {
            log.error("批量删除失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 获取文档列表（分页）⭐ 从存储服务获取实际文档
     * GET /api/documents/list
     */
    @GetMapping("/list")
    public ListResponse listDocuments(
            @RequestParam(defaultValue = "") String keyword,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "20") int pageSize) {

        ListResponse response = new ListResponse();

        try {
            log.info("获取文档列表: keyword={}, page={}, pageSize={}", keyword, page, pageSize);

            List<top.yumbo.ai.storage.api.model.DocumentMetadata> metadataList;
            long totalCount;

            // 从 DocumentStorageService 获取文档列表 ⭐
            if (keyword != null && !keyword.trim().isEmpty()) {
                // 搜索文档
                metadataList = storageService.searchDocuments(keyword);
                totalCount = metadataList.size();
            } else {
                // 获取所有文档（分页）
                totalCount = storageService.getDocumentCount();
                int offset = (page - 1) * pageSize;
                metadataList = storageService.listDocuments(offset, pageSize);
            }

            // 转换为 DocumentInfo
            List<DocumentInfo> documents = metadataList.stream()
                    .map(metadata -> {
                        DocumentInfo info = new DocumentInfo();
                        info.setDocumentId(metadata.getDocumentId());
                        info.setFileName(metadata.getFilename());
                        info.setFileSize(metadata.getFileSize() != null ? metadata.getFileSize() : 0);
                        info.setFileType(metadata.getFileType() != null ? metadata.getFileType() : "unknown");
                        info.setUploadTime(metadata.getUploadTime() != null ? metadata.getUploadTime() : new Date());
                        info.setIndexed(metadata.getIndexed() != null ? metadata.getIndexed() : false);
                        return info;
                    })
                    .collect(Collectors.toList());

            // 分页处理（如果是搜索结果，需要在内存中分页）
            int total = (int) totalCount;
            int totalPages = (int) Math.ceil((double) total / pageSize);

            if (keyword != null && !keyword.trim().isEmpty()) {
                // 搜索结果需要在内存中分页
                int startIndex = (page - 1) * pageSize;
                int endIndex = Math.min(startIndex + pageSize, total);
                if (startIndex < total) {
                    documents = documents.subList(startIndex, endIndex);
                } else {
                    documents = new ArrayList<>();
                }
            }

            response.setSuccess(true);
            response.setDocuments(documents);
            response.setTotal(total);
            response.setPage(page);
            response.setPageSize(pageSize);
            response.setTotalPages(totalPages);

            log.info("返回文档列表: total={}, page={}, pageSize={}", total, page, pageSize);

        } catch (Exception e) {
            log.error("获取文档列表失败", e);
            response.setSuccess(false);
            response.setMessage("获取文档列表失败: " + e.getMessage());
        }

        return response;
    }


    /**
     * 获取文档统计
     * GET /api/documents/statistics
     */
    @GetMapping("/statistics")
    public Map<String, Object> getStatistics() {
        Map<String, Object> result = new HashMap<>();

        try {
            var ragStats = ragService.getStatistics();

            result.put("status", "success");
            result.put("totalDocuments", ragStats.getTotalDocuments());
            result.put("indexSize", ragStats.getIndexSize());
            result.put("indexType", ragStats.getIndexType());
            result.put("healthy", ragStats.isHealthy());

        } catch (Exception e) {
            log.error("获取统计失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 搜索文档
     * GET /api/documents/search
     */
    @GetMapping("/search")
    public Map<String, Object> searchDocuments(
            @RequestParam String keyword,
            @RequestParam(defaultValue = "10") int limit) {

        Map<String, Object> result = new HashMap<>();

        try {
            // 使用RAG搜索文档
            List<top.yumbo.ai.rag.api.model.SearchResult> searchResults =
                ragService.searchByText(keyword, limit);

            // 提取唯一的文档源
            List<String> documentIds = searchResults.stream()
                .map(sr -> sr.getDocument().getSource())
                .filter(Objects::nonNull)
                .distinct()
                .collect(Collectors.toList());

            result.put("status", "success");
            result.put("keyword", keyword);
            result.put("documents", documentIds);
            result.put("count", documentIds.size());
            result.put("results", searchResults);

        } catch (Exception e) {
            log.error("搜索文档失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 生成并保存 PPL 和 Optimization 数据
     *
     * @param documentId 文档ID（使用文件名）
     * @param content 文档内容
     * @param chunks 文档分块列表
     */
    private void generateOptimizationData(String documentId, String content, List<Chunk> chunks) {
        if (ragOptimizationService == null) {
            return;
        }

        try {
            // 1. 生成 PPL (Probable Point of Loss) 数据
            // 分析文档中的关键点，用于优化检索
            List<String> probablePoints = new ArrayList<>();
            Map<String, Float> scores = new HashMap<>();

            // 简单实现：使用分块的序号作为关键点
            for (int i = 0; i < chunks.size(); i++) {
                Chunk chunk = chunks.get(i);
                String pointId = "chunk_" + i;
                probablePoints.add(pointId);

                // 简单评分：根据内容长度和位置
                float score = 0.5f + (float) i / chunks.size() * 0.5f;
                if (chunk.getContent().length() > 500) {
                    score += 0.2f;
                }
                scores.put(pointId, Math.min(score, 1.0f));
            }

            // 保存 PPL 数据
            top.yumbo.ai.storage.api.model.PPLData pplData = top.yumbo.ai.storage.api.model.PPLData.builder()
                    .documentId(documentId)
                    .probablePoints(probablePoints)
                    .scores(scores)
                    .modelVersion("simple-v1.0")
                    .analyzedAt(System.currentTimeMillis())
                    .metadata(Map.of(
                            "chunkCount", chunks.size(),
                            "contentLength", content.length(),
                            "generatedBy", "DocumentManagementController"
                    ))
                    .build();

            String pplResult = storageService.savePPLData(documentId, pplData);
            if (pplResult != null) {
                log.info("✅ PPL 数据已保存: {}", documentId);
            }

            // 2. 生成通用 Optimization 数据
            // 保存文档的基本统计信息，用于 RAG 优化
            Map<String, Object> optimizationInfo = new HashMap<>();
            optimizationInfo.put("totalChunks", chunks.size());
            optimizationInfo.put("avgChunkSize", chunks.stream()
                    .mapToInt(c -> c.getContent().length())
                    .average()
                    .orElse(0.0));
            optimizationInfo.put("totalContentLength", content.length());
            optimizationInfo.put("probablePoints", probablePoints);

            Map<String, Object> metadata = new HashMap<>();
            metadata.put("generatedAt", System.currentTimeMillis());
            metadata.put("version", "1.0");
            metadata.put("generator", "auto");

            Map<String, Double> metrics = new HashMap<>();
            metrics.put("chunkCount", (double) chunks.size());
            metrics.put("avgChunkSize", chunks.stream()
                    .mapToInt(c -> c.getContent().length())
                    .average()
                    .orElse(0.0));

            String optResult = ragOptimizationService.saveOptimizationData(
                    documentId,
                    "DOCUMENT_STATS",  // 优化类型
                    optimizationInfo,
                    metadata,
                    metrics
            );

            if (optResult != null) {
                log.info("✅ Optimization 数据已保存: {} type=DOCUMENT_STATS", documentId);
            }

        } catch (Exception e) {
            log.error("生成优化数据失败: {}", documentId, e);
            throw e;
        }
    }

    // ========== 辅助方法 ==========

    /**
     * 获取文件扩展名
     */
    private String getFileExtension(String filename) {
        if (filename == null || filename.isEmpty()) {
            return "";
        }
        int lastDot = filename.lastIndexOf('.');
        if (lastDot > 0 && lastDot < filename.length() - 1) {
            return filename.substring(lastDot + 1).toLowerCase();
        }
        return "";
    }

    // ========== DTO 类 ==========

    @Data
    public static class UploadResponse {
        private boolean success;
        private String message;
        private String fileName;
        private long fileSize;
        private String documentId;
        private boolean autoIndexed;
    }

    @Data
    public static class BatchUploadResponse {
        private boolean success;
        private String message;
        private int successCount;
        private int failureCount;
        private List<UploadResult> results;
    }

    @Data
    public static class UploadResult {
        private boolean success;
        private String message;
        private String fileName;
        private String documentId;
        private long fileSize;
    }

    @Data
    public static class BatchDeleteRequest {
        private List<String> documentIds;
    }

    @Data
    public static class ListResponse {
        private boolean success;
        private String message;
        private List<DocumentInfo> documents;
        private int total;
        private int page;
        private int pageSize;
        private int totalPages;
    }

    @Data
    public static class DocumentInfo {
        private String documentId;
        private String fileName;
        private long fileSize;
        private String fileType;
        private Date uploadTime;
        private boolean indexed;
    }
}



