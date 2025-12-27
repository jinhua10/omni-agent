package top.yumbo.ai.omni.web.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.web.dto.ApiDtos.BatchIndexRequest;
import top.yumbo.ai.omni.web.dto.DocumentRequest;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.SearchResult;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * RAG 管理控制器
 *
 * <p>提供 RAG 索引和检索管理接口：</p>
 * <ul>
 *   <li>索引单个文档</li>
 *   <li>批量索引文档</li>
 *   <li>重建索引</li>
 *   <li>文本搜索</li>
 *   <li>统计信息</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/rag")
@RequiredArgsConstructor
public class RAGManagementController {

    private final RAGService ragService;
    private final DocumentStorageService storageService;

    /**
     * 索引单个文档
     *
     * @param request 文档请求
     * @return 索引结果
     */
    @PostMapping("/index")
    public Map<String, Object> indexDocument(@RequestBody DocumentRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            Document document = Document.builder()
                    .id(request.getId())
                    .title(request.getTitle())
                    .content(request.getContent())
                    .summary(request.getSummary())
                    .type("example")
                    .source("api")
                    .build();

            String docId = ragService.indexDocument(document);
            result.put("status", "success");
            result.put("documentId", docId);
            result.put("message", "Document indexed successfully");
            log.info("✅ 文档索引成功: id={}", docId);
        } catch (Exception e) {
            log.error("❌ 文档索引失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 批量索引文档
     *
     * @param request 批量索引请求
     * @return 索引结果
     */
    @PostMapping("/index/batch")
    public Map<String, Object> indexDocuments(@RequestBody BatchIndexRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            List<String> docIds = ragService.indexDocuments(request.getDocuments());

            result.put("status", "success");
            result.put("indexedCount", docIds.size());
            result.put("documentIds", docIds);
            result.put("message", "Documents indexed successfully");
            log.info("✅ 批量索引完成: count={}", docIds.size());
        } catch (Exception e) {
            log.error("❌ 批量索引失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 重建索引
     *
     * @return 重建结果
     */
    @PostMapping("/rebuild")
    public Map<String, Object> rebuildIndex() {
        Map<String, Object> result = new HashMap<>();

        try {
            ragService.rebuildIndex();

            result.put("status", "success");
            result.put("message", "Index rebuild completed");
            result.put("statistics", ragService.getStatistics());
            log.info("✅ 索引重建完成");
        } catch (Exception e) {
            log.error("❌ 索引重建失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * RAG 文本搜索
     *
     * @param query 查询文本
     * @param topK  返回结果数量
     * @return 搜索结果
     */
    @GetMapping("/search")
    public Map<String, Object> searchByText(
            @RequestParam String query,
            @RequestParam(defaultValue = "10") int topK) {
        Map<String, Object> result = new HashMap<>();

        try {
            List<SearchResult> searchResults = ragService.searchByText(query, topK);
            result.put("status", "success");
            result.put("query", query);
            result.put("resultCount", searchResults.size());
            result.put("results", searchResults);
            log.info("✅ RAG 搜索完成: query={}, count={}", query, searchResults.size());
        } catch (Exception e) {
            log.error("❌ RAG 搜索失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 获取 RAG 统计信息
     *
     * @return 统计信息
     */
    @GetMapping("/statistics")
    public Map<String, Object> getRAGStatistics() {
        Map<String, Object> result = new HashMap<>();

        try {
            result.put("status", "success");
            result.put("statistics", ragService.getStatistics());
            result.put("healthy", ragService.isHealthy());
        } catch (Exception e) {
            log.error("❌ 获取 RAG 统计信息失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 获取存储统计信息
     *
     * @return 统计信息
     */
    @GetMapping("/storage/statistics")
    public Map<String, Object> getStorageStatistics() {
        Map<String, Object> result = new HashMap<>();

        try {
            result.put("status", "success");
            result.put("statistics", storageService.getStatistics());
            result.put("healthy", storageService.isHealthy());
        } catch (Exception e) {
            log.error("❌ 获取存储统计信息失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }
}

