package top.yumbo.ai.omni.example.basic.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.SearchResult;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 基础示例控制器
 * 
 * <p>演示如何使用四维可插拔服务：</p>
 * <ul>
 *   <li>QuestionClassifierPersistence - 持久化服务</li>
 *   <li>DocumentStorageService - 文档存储服务</li>
 *   <li>RAGService - RAG检索服务</li>
 *   <li>AIService - AI推理服务（待实现）</li>
 * </ul>
 * 
 * @author Jinhua Yu
 * @since 1.0.0
 */
@RestController
@RequestMapping("/api/demo")
@RequiredArgsConstructor
public class DemoController {

    private final QuestionClassifierPersistence persistence;
    private final DocumentStorageService storageService;
    private final RAGService ragService;
    // private final AIService aiService;  // TODO: 等待 AI Starter 实现

    /**
     * 健康检查
     */
    @GetMapping("/health")
    public Map<String, Object> health() {
        Map<String, Object> result = new HashMap<>();
        result.put("status", "UP");
        result.put("persistence", "Memory");
        result.put("documentStorage", "File");
        result.put("rag", "File/Lucene");
        result.put("ai", "Ollama (Pending)");
        result.put("message", "OmniAgent is running with pluggable architecture!");
        return result;
    }

    /**
     * RAG 索引文档示例
     */
    @PostMapping("/rag/index")
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
        } catch (Exception e) {
            result.put("status", "error");
            result.put("error", e.getMessage());
        }
        
        return result;
    }

    /**
     * RAG 文本搜索示例
     */
    @GetMapping("/rag/search")
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
        } catch (Exception e) {
            result.put("status", "error");
            result.put("error", e.getMessage());
        }
        
        return result;
    }

    /**
     * 获取 RAG 统计信息
     */
    @GetMapping("/rag/statistics")
    public Map<String, Object> getRAGStatistics() {
        Map<String, Object> result = new HashMap<>();
        
        try {
            result.put("status", "success");
            result.put("statistics", ragService.getStatistics());
            result.put("healthy", ragService.isHealthy());
        } catch (Exception e) {
            result.put("status", "error");
            result.put("error", e.getMessage());
        }
        
        return result;
    }

    /**
     * 获取存储统计信息
     */
    @GetMapping("/storage/statistics")
    public Map<String, Object> getStorageStatistics() {
        Map<String, Object> result = new HashMap<>();
        
        try {
            result.put("status", "success");
            result.put("statistics", storageService.getStatistics());
            result.put("healthy", storageService.isHealthy());
        } catch (Exception e) {
            result.put("status", "error");
            result.put("error", e.getMessage());
        }
        
        return result;
    }

    /**
     * 文档请求对象
     */
    @lombok.Data
    public static class DocumentRequest {
        private String id;
        private String title;
        private String content;
        private String summary;
    }
}
