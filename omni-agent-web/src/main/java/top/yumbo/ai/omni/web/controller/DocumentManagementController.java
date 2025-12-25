package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import top.yumbo.ai.omni.web.util.FileStorageUtil;
import top.yumbo.ai.rag.api.model.SearchResult;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.omni.core.document.DocumentProcessorManager;
import top.yumbo.ai.omni.core.chunking.ChunkingStrategyManager;
import top.yumbo.ai.omni.core.image.ImageStorageService;
import top.yumbo.ai.omni.web.service.FileWatcherService;
import org.springframework.beans.factory.annotation.Value;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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
    private final FileWatcherService fileWatcherService;
    private final top.yumbo.ai.omni.web.service.DocumentProcessingService documentProcessingService;

    // ⭐ 直接从配置文件读取监听目录
    @Value("${omni-agent.file-watcher.watch-directory:./data/documents}")
    private String watchDirectory;


    /**
     * 上传文档（异步处理版本）⭐
     * POST /api/documents/upload
     * <p>
     * 新逻辑（中转站模式）：
     * 1. 先保存文件到监听目录（data/documents）作为中转站
     * 2. 触发异步RAG处理：文本提取 → 分块 → 索引
     * 3. RAG处理完成后，由DocumentProcessingService保存到存储服务（虚拟路径系统）
     * 4. 返回"索引中"状态，前端订阅WebSocket进度
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
            log.info("📤 上传文档（异步-中转站模式）: filename={}, size={} bytes", filename, file.getSize());

            // ⭐ 步骤1：先保存到监听目录作为中转站
            Path watchDir = Paths.get(watchDirectory);
            if (!Files.exists(watchDir)) {
                Files.createDirectories(watchDir);
            }

            Path targetFile = watchDir.resolve(filename);
            file.transferTo(targetFile);

            log.info("✅ 文件已保存到中转站（监听目录）: {}", targetFile);

            // ⭐ 步骤2：生成文档ID（使用原始文件名）
            String documentId = filename;

            // ⭐ 步骤3：触发异步RAG处理流程（推送WebSocket进度）
            // 处理完成后会自动保存到存储服务
            documentProcessingService.processDocument(documentId, filename, file.getBytes())
                    .exceptionally(throwable -> {
                        log.error("❌ 文档处理异常: documentId={}", documentId, throwable);
                        return null;
                    });

            response.setSuccess(true);
            response.setMessage("文件上传成功，正在处理中...");
            response.setFileName(filename);
            response.setFileSize(file.getSize());
            response.setDocumentId(documentId);  // ⭐ 返回documentId供前端订阅进度
            response.setAutoIndexed(true);
            response.setIndexing(true);  // ⭐ 索引中状态

            log.info("📤 文档上传成功（异步-中转站模式）: filename={}, documentId={}, 等待RAG处理", filename, documentId);

        } catch (Exception e) {
            log.error("文档上传失败", e);
            response.setSuccess(false);
            response.setMessage("文档上传失败: " + e.getMessage());
        }

        return response;
    }

    /**
     * 批量上传文档（异步处理版本）⭐
     * POST /api/documents/upload-batch
     * <p>
     * 新逻辑：
     * 1. 批量保存文件到监听目录
     * 2. 返回"索引中"状态
     * 3. 由 FileWatcherService 自动处理
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
            log.info("📤 批量上传文档（异步）: count={}", files.length);

            // 确保监听目录存在
            Path watchDir = Paths.get(watchDirectory);
            if (!Files.exists(watchDir)) {
                Files.createDirectories(watchDir);
            }

            for (MultipartFile file : files) {
                UploadResult uploadResult = new UploadResult();
                uploadResult.setFileName(file.getOriginalFilename());

                try {
                    if (file.isEmpty()) {
                        uploadResult.setSuccess(false);
                        uploadResult.setMessage("文件为空");
                        uploadResult.setIndexing(false);
                        failCount++;
                        results.add(uploadResult);
                        continue;
                    }

                    // ⭐ 直接保存到监听目录
                    String filename = file.getOriginalFilename();
                    Path targetFile = watchDir.resolve(filename);
                    file.transferTo(targetFile);

                    uploadResult.setSuccess(true);
                    uploadResult.setMessage("文件上传成功，正在索引中...");
                    uploadResult.setDocumentId(null);  // 索引完成后才有
                    uploadResult.setFileSize(file.getSize());
                    uploadResult.setIndexing(true);  // ⭐ 索引中状态
                    successCount++;

                    log.info("✅ 文件已保存: {}", filename);

                } catch (Exception e) {
                    log.error("上传文件失败: {}", file.getOriginalFilename(), e);
                    uploadResult.setSuccess(false);
                    uploadResult.setMessage("上传失败: " + e.getMessage());
                    uploadResult.setIndexing(false);
                    failCount++;
                }

                results.add(uploadResult);
            }

            response.setSuccess(true);
            response.setMessage(String.format("批量上传完成: 成功 %d, 失败 %d。文件正在后台索引中...", successCount, failCount));
            response.setSuccessCount(successCount);
            response.setFailureCount(failCount);
            response.setResults(results);

            log.info("📤 批量上传完成: success={}, fail={}, 文件将自动索引", successCount, failCount);

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
     * 获取文档详情
     * GET /api/documents/{documentId}
     */
    @GetMapping("/{documentId}")
    public Map<String, Object> getDocumentDetails(@PathVariable String documentId) {
        Map<String, Object> result = new HashMap<>();

        try {
            log.info("获取文档详情请求: {}", documentId);

            // 尝试从RAG查找文档
            List<SearchResult> searchResults = ragService.searchByText(documentId, 10);
            Document doc = null;

            // 如果是文件路径，尝试查找匹配的文档
            for (SearchResult sr : searchResults) {
                Document d = sr.getDocument();
                if (d != null && d.getTitle() != null && d.getTitle().equals(documentId)) {
                    doc = d;
                    break;
                }
            }

            // 如果没找到，使用第一个结果
            if (doc == null && !searchResults.isEmpty() && searchResults.get(0).getDocument() != null) {
                doc = searchResults.getFirst().getDocument();
            }

            if (doc != null) {
                result.put("success", true);
                result.put("documentId", doc.getId());
                result.put("fileName", doc.getTitle());

                // 从metadata中获取mimeType
                if (doc.getMetadata() != null && doc.getMetadata().containsKey("mimeType")) {
                    result.put("mimeType", doc.getMetadata().get("mimeType"));
                } else {
                    result.put("mimeType", doc.getType());
                }

                // 使用createdAt时间戳
                result.put("uploadTime", doc.getCreatedAt());

                // 获取文件大小
                try {
                    Path filePath = FileStorageUtil.findFileByName(doc.getTitle());
                    if (filePath != null && Files.exists(filePath)) {
                        result.put("fileSize", Files.size(filePath));
                    }
                } catch (Exception e) {
                    log.warn("无法获取文件大小: {}", doc.getTitle(), e);
                }

                // 获取提取的文本长度 (使用content字段)
                if (doc.getContent() != null) {
                    result.put("extractedLength", doc.getContent().length());
                }

                log.info("文档详情获取成功: {}", doc.getTitle());
            } else {
                result.put("success", false);
                result.put("message", "文档不存在");
                log.warn("文档不存在: {}", documentId);
            }

        } catch (Exception e) {
            log.error("获取文档详情失败: {}", documentId, e);
            result.put("success", false);
            result.put("message", "获取文档详情失败: " + e.getMessage());
        }

        return result;
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
            String encodedFileName = URLEncoder.encode(fileName, StandardCharsets.UTF_8)
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
     * 获取待处理的文件列表（pending 区域）⭐
     * GET /api/documents/pending
     * <p>
     * 返回 data/documents 目录下等待索引的文件
     */
    @GetMapping("/pending")
    public PendingFilesResponse getPendingFiles() {
        PendingFilesResponse response = new PendingFilesResponse();

        try {
            Path watchDir = Paths.get(watchDirectory);

            if (!Files.exists(watchDir)) {
                response.setSuccess(true);
                response.setFiles(Collections.emptyList());
                response.setCount(0);
                return response;
            }

            List<PendingFileInfo> pendingFiles = new ArrayList<>();

            // 扫描监听目录
            Files.walk(watchDir)
                    .filter(Files::isRegularFile)
                    .filter(path -> {
                        String name = path.getFileName().toString();
                        // 过滤临时文件和隐藏文件
                        return !name.startsWith(".") && !name.startsWith("~") && !name.endsWith(".tmp");
                    })
                    .forEach(filePath -> {
                        try {
                            Path relativePath = watchDir.relativize(filePath);
                            String relativePathStr = relativePath.toString().replace('\\', '/');
                            String fileName = filePath.getFileName().toString();

                            // 检查处理状态
                            boolean isProcessing = fileWatcherService.isFileProcessing(relativePathStr);

                            PendingFileInfo fileInfo = new PendingFileInfo();
                            fileInfo.setFileName(fileName);
                            fileInfo.setRelativePath(relativePathStr);
                            fileInfo.setFileSize(Files.size(filePath));
                            fileInfo.setUploadTime(Files.getLastModifiedTime(filePath).toMillis());
                            fileInfo.setProcessing(isProcessing);
                            fileInfo.setCancelable(!isProcessing);  // 未开始处理的可以取消

                            pendingFiles.add(fileInfo);

                        } catch (Exception e) {
                            log.warn("读取文件信息失败: {}", filePath, e);
                        }
                    });

            response.setSuccess(true);
            response.setFiles(pendingFiles);
            response.setCount(pendingFiles.size());

        } catch (Exception e) {
            log.error("获取待处理文件失败", e);
            response.setSuccess(false);
            response.setMessage("获取待处理文件失败: " + e.getMessage());
        }

        return response;
    }

    /**
     * 取消文件索引（从待处理列表删除）⭐
     * DELETE /api/documents/pending/{fileName}
     * <p>
     * 只有未开始处理的文件才能取消
     */
    @DeleteMapping("/pending/{fileName:.+}")
    public Map<String, Object> cancelPendingFile(@PathVariable String fileName) {
        Map<String, Object> result = new HashMap<>();

        try {
            log.info("🗑️ 取消索引请求: {}", fileName);

            Path watchDir = Paths.get(watchDirectory);
            Path filePath = watchDir.resolve(fileName);

            if (!Files.exists(filePath)) {
                result.put("success", false);
                result.put("message", "文件不存在");
                return result;
            }

            // 检查文件是否正在处理
            boolean isProcessing = fileWatcherService.isFileProcessing(fileName);

            if (isProcessing) {
                result.put("success", false);
                result.put("message", "文件正在处理中，无法取消");
                return result;
            }

            // 删除文件
            Files.delete(filePath);
            log.info("✅ 已取消索引并删除文件: {}", fileName);

            result.put("success", true);
            result.put("message", "文件已删除");

        } catch (Exception e) {
            log.error("取消索引失败: {}", fileName, e);
            result.put("success", false);
            result.put("message", "取消索引失败: " + e.getMessage());
        }

        return result;
    }

    /**
     * 将文档从存储目录复制到待处理目录（加入流程视图）⭐
     * POST /api/documents/copy-to-pending
     * <p>
     * 用于将已完成的文档重新加入流程视图进行处理
     * 使用虚拟路径系统：从 storage 读取，写入到 data/documents 临时目录
     */
    @PostMapping("/copy-to-pending")
    public Map<String, Object> copyToPending(@RequestBody Map<String, String> request) {
        Map<String, Object> result = new HashMap<>();

        try {
            String virtualPath = request.get("path");
            String fileName = request.get("fileName");

            if (virtualPath == null || fileName == null) {
                result.put("success", false);
                result.put("message", "缺少必要参数: path 和 fileName");
                return result;
            }

            log.info("📋 将文档加入待处理队列: virtualPath={}, fileName={}", virtualPath, fileName);

            // 1. 从存储服务读取文档（虚拟路径系统）⭐
            log.debug("从存储服务读取文档: {}", virtualPath);
            byte[] fileData = storageService.readFile(virtualPath);

            if (fileData == null || fileData.length == 0) {
                result.put("success", false);
                result.put("message", "无法读取文档数据（文件可能不存在或为空）");
                log.warn("⚠️ 读取文档数据失败: virtualPath={}", virtualPath);
                return result;
            }

            log.info("✅ 成功从存储服务读取文档: {} bytes", fileData.length);

            // 2. 确保监听目录存在
            Path watchDir = Paths.get(watchDirectory);
            if (!Files.exists(watchDir)) {
                Files.createDirectories(watchDir);
                log.info("📁 创建监听目录: {}", watchDir);
            }

            // 3. 写入到监听目录（临时目录）
            Path targetFile = watchDir.resolve(fileName);

            // 如果文件已存在，生成新文件名避免覆盖
            if (Files.exists(targetFile)) {
                String baseName = fileName.contains(".")
                        ? fileName.substring(0, fileName.lastIndexOf('.'))
                        : fileName;
                String extension = fileName.contains(".")
                        ? fileName.substring(fileName.lastIndexOf('.'))
                        : "";

                int counter = 1;
                do {
                    fileName = baseName + "_" + counter + extension;
                    targetFile = watchDir.resolve(fileName);
                    counter++;
                } while (Files.exists(targetFile) && counter < 100);

                log.info("📝 文件名已存在，生成新名称: {}", fileName);
            }

            Files.write(targetFile, fileData);
            log.info("✅ 文档已写入待处理目录: {}", targetFile);

            // 4. 生成文档ID（使用文件名）
            String documentId = fileName;

            result.put("success", true);
            result.put("message", "文档已加入待处理列表，等待处理");
            result.put("documentId", documentId);
            result.put("fileName", fileName);

            log.info("🎉 文档已加入流程视图: documentId={}, path={}", documentId, targetFile);

        } catch (Exception e) {
            log.error("❌ 将文档加入待处理队列失败", e);
            result.put("success", false);
            result.put("message", "操作失败: " + e.getMessage());
        }

        return result;
    }

    /**
     * 删除文档
     * DELETE /api/documents/{documentId}
     * <p>
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
                    actualDocumentId = searchResults.getFirst().getDocument().getId();
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


    // ========== DTO 类 ==========


    @Data
    public static class UploadResponse {
        private boolean success;
        private String message;
        private String fileName;
        private long fileSize;
        private String documentId;
        private boolean autoIndexed;
        private boolean indexing;  // ⭐ 是否正在索引中
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
        private boolean indexing;  // ⭐ 是否正在索引中
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

    // ========== Pending 文件相关 DTO ⭐ ==========

    @Data
    public static class PendingFilesResponse {
        private boolean success;
        private String message;
        private List<PendingFileInfo> files;
        private int count;
    }

    @Data
    public static class PendingFileInfo {
        private String fileName;
        private String relativePath;
        private long fileSize;
        private long uploadTime;
        private boolean processing;     // 是否正在处理
        private boolean cancelable;     // 是否可以取消
    }
}



