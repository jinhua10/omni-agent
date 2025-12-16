package top.yumbo.ai.omni.example.basic.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import top.yumbo.ai.rag.api.model.SearchResult;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;

import java.nio.charset.StandardCharsets;
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

            // 读取文档内容
            String content = new String(file.getBytes(), StandardCharsets.UTF_8);

            // 直接索引到RAG
            if (autoIndex) {
                Document document = Document.builder()
                    .id(documentId)
                    .title(filename)
                    .content(content)
                    .source("upload")
                    .type("document")
                    .build();

                ragService.indexDocument(document);
            }

            response.setSuccess(true);
            response.setMessage("文档上传成功");
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

                    // 读取文档内容
                    String content = new String(file.getBytes(), StandardCharsets.UTF_8);

                    // 直接索引到RAG
                    if (autoIndex) {
                        Document document = Document.builder()
                            .id(documentId)
                            .title(filename)
                            .content(content)
                            .source("upload")
                            .type("document")
                            .build();

                        ragService.indexDocument(document);
                    }

                    uploadResult.setSuccess(true);
                    uploadResult.setMessage("上传成功");
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
     * 删除文档
     * DELETE /api/documents/{documentId}
     */
    @DeleteMapping("/{documentId}")
    public Map<String, Object> deleteDocument(@PathVariable String documentId) {
        Map<String, Object> result = new HashMap<>();

        try {
            log.info("删除文档: {}", documentId);

            // 删除文档的所有分块
            storageService.deleteChunksByDocument(documentId);
            // 删除文档的所有图片
            storageService.deleteImagesByDocument(documentId);
            // 删除RAG索引
            ragService.deleteDocument(documentId);

            result.put("status", "success");
            result.put("message", "文档删除成功");
            result.put("documentId", documentId);

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
                    storageService.deleteChunksByDocument(documentId);
                    storageService.deleteImagesByDocument(documentId);
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
     * 获取文档列表（分页）
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

            // 获取文档总数
            long totalCount = ragService.getDocumentCount();
            List<DocumentInfo> allDocuments = new ArrayList<>();

            // 如果有关键词，则搜索；否则获取所有文档
            if (keyword != null && !keyword.trim().isEmpty()) {
                // 关键词搜索
                log.info("执行关键词搜索: keyword={}", keyword);
                List<SearchResult> searchResults =
                    ragService.searchByText(keyword, 100); // 搜索更多结果用于分页

                // 提取唯一的文档
                Map<String, DocumentInfo> documentMap = new HashMap<>();
                for (var result : searchResults) {
                    var doc = result.getDocument();
                    if (doc != null && doc.getId() != null) {
                        if (!documentMap.containsKey(doc.getId())) {
                            DocumentInfo docInfo = new DocumentInfo();
                            docInfo.setDocumentId(doc.getId());
                            docInfo.setFileName(doc.getTitle() != null ? doc.getTitle() : doc.getId());
                            docInfo.setFileSize(doc.getContent() != null ? doc.getContent().length() : 0);
                            docInfo.setFileType("text");
                            docInfo.setUploadTime(new Date());
                            docInfo.setIndexed(true);
                            documentMap.put(doc.getId(), docInfo);
                        }
                    }
                }
                allDocuments.addAll(documentMap.values());
            } else {
                // 没有关键词时，获取所有文档（分页）
                log.info("获取所有文档: totalCount={}", totalCount);
                int offset = (page - 1) * pageSize;
                List<Document> documents = ragService.getAllDocuments(offset, pageSize);

                for (Document doc : documents) {
                    DocumentInfo docInfo = new DocumentInfo();
                    docInfo.setDocumentId(doc.getId());
                    docInfo.setFileName(doc.getTitle() != null ? doc.getTitle() : doc.getId());
                    docInfo.setFileSize(doc.getContent() != null ? doc.getContent().length() : 0);
                    docInfo.setFileType(doc.getType() != null ? doc.getType() : "text");
                    docInfo.setUploadTime(doc.getCreatedAt() != null ? new Date(doc.getCreatedAt()) : new Date());
                    docInfo.setIndexed(true);
                    allDocuments.add(docInfo);
                }
            }

            // 分页处理
            List<DocumentInfo> pagedDocuments;
            int total;

            if (keyword != null && !keyword.trim().isEmpty()) {
                // 搜索结果需要在内存中分页
                total = allDocuments.size();
                int startIndex = (page - 1) * pageSize;
                int endIndex = Math.min(startIndex + pageSize, total);

                if (startIndex >= total) {
                    pagedDocuments = new ArrayList<>();
                } else {
                    pagedDocuments = allDocuments.subList(startIndex, endIndex);
                }
            } else {
                // getAllDocuments 已经分页，直接使用
                pagedDocuments = allDocuments;
                total = (int) totalCount;
            }

            int totalPages = (int) Math.ceil((double) total / pageSize);

            response.setSuccess(true);
            response.setDocuments(pagedDocuments);
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

