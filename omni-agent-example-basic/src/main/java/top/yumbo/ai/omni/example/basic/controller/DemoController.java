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
 * <p>演示如何使用OmniAgent的可插拔服务：</p>
 * <ul>
 *   <li>QuestionClassifierPersistence - 持久化服务</li>
 *   <li>DocumentStorageService - 文档存储服务</li>
 *   <li>RAGService - RAG检索服务</li>
 *   <li>AIService - AI推理服务 ⭐ 已实现</li>
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
    private final top.yumbo.ai.ai.api.AIService aiService;

    /**
     * 健康检查
     */
    @GetMapping("/health")
    public Map<String, Object> health() {
        Map<String, Object> result = new HashMap<>();
        result.put("status", "UP");
        result.put("persistence", persistence.getClass().getSimpleName());
        result.put("documentStorage", storageService.getClass().getSimpleName());
        result.put("rag", ragService.getClass().getSimpleName());
        result.put("ai", aiService.getClass().getSimpleName());
        result.put("aiModel", aiService.getCurrentModel());
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

    // ========== AI 服务 API ==========

    /**
     * AI 简单对话
     */
    @PostMapping("/ai/chat")
    public Map<String, Object> chat(@RequestBody ChatRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            String answer = aiService.chat(request.getMessage());
            result.put("status", "success");
            result.put("question", request.getMessage());
            result.put("answer", answer);
            result.put("model", aiService.getCurrentModel());
        } catch (Exception e) {
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * AI 流式对话 (Server-Sent Events) ⭐ NEW
     * 实时返回AI生成的每个token
     */
    @GetMapping(value = "/ai/chat/stream", produces = "text/event-stream")
    public reactor.core.publisher.Flux<String> chatStream(@RequestParam String message) {
        try {
            // 构建简单的消息列表
            List<top.yumbo.ai.ai.api.model.ChatMessage> messages = List.of(
                top.yumbo.ai.ai.api.model.ChatMessage.builder()
                    .role("user")
                    .content(message)
                    .build()
            );

            // 返回流式响应，每个token作为SSE事件发送
            return aiService.chatFlux(messages)
                .map(token -> "data: " + token + "\n\n")
                .onErrorResume(e -> reactor.core.publisher.Flux.just(
                    "data: [ERROR] " + e.getMessage() + "\n\n"
                ));
        } catch (Exception e) {
            return reactor.core.publisher.Flux.just(
                "data: [ERROR] " + e.getMessage() + "\n\n"
            );
        }
    }

    /**
     * AI 流式对话 (POST方式，支持更复杂的参数) ⭐ NEW
     */
    @PostMapping(value = "/ai/chat/stream", produces = "text/event-stream")
    public reactor.core.publisher.Flux<String> chatStreamPost(@RequestBody StreamChatRequest request) {
        try {
            // 构建消息列表
            List<top.yumbo.ai.ai.api.model.ChatMessage> messages = new java.util.ArrayList<>();

            // 添加系统提示（如果有）
            if (request.getSystemPrompt() != null && !request.getSystemPrompt().isEmpty()) {
                messages.add(top.yumbo.ai.ai.api.model.ChatMessage.builder()
                    .role("system")
                    .content(request.getSystemPrompt())
                    .build());
            }

            // 添加历史消息（如果有）
            if (request.getHistory() != null) {
                messages.addAll(request.getHistory());
            }

            // 添加当前用户消息
            messages.add(top.yumbo.ai.ai.api.model.ChatMessage.builder()
                .role("user")
                .content(request.getMessage())
                .build());

            // 返回流式响应
            return aiService.chatFlux(messages)
                .map(token -> "data: " + escapeJson(token) + "\n\n")
                .onErrorResume(e -> reactor.core.publisher.Flux.just(
                    "data: {\"error\": \"" + escapeJson(e.getMessage()) + "\"}\n\n"
                ));
        } catch (Exception e) {
            return reactor.core.publisher.Flux.just(
                "data: {\"error\": \"" + escapeJson(e.getMessage()) + "\"}\n\n"
            );
        }
    }

    /**
     * AI 文本生成
     */
    @PostMapping("/ai/generate")
    public Map<String, Object> generate(@RequestBody GenerateRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            String generated = aiService.generate(request.getPrompt());
            result.put("status", "success");
            result.put("prompt", request.getPrompt());
            result.put("generated", generated);
            result.put("model", aiService.getCurrentModel());
        } catch (Exception e) {
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * AI 流式文本生成 (Server-Sent Events) ⭐ NEW
     */
    @PostMapping(value = "/ai/generate/stream", produces = "text/event-stream")
    public reactor.core.publisher.Flux<String> generateStream(@RequestBody GenerateRequest request) {
        try {
            top.yumbo.ai.ai.api.model.AIRequest aiRequest = top.yumbo.ai.ai.api.model.AIRequest.builder()
                .prompt(request.getPrompt())
                .temperature(request.getTemperature() != null ? request.getTemperature() : 0.7f)
                .maxTokens(request.getMaxTokens() != null ? request.getMaxTokens() : 2048)
                .build();

            return aiService.generateFlux(aiRequest)
                .map(token -> "data: " + escapeJson(token) + "\n\n")
                .onErrorResume(e -> reactor.core.publisher.Flux.just(
                    "data: {\"error\": \"" + escapeJson(e.getMessage()) + "\"}\n\n"
                ));
        } catch (Exception e) {
            return reactor.core.publisher.Flux.just(
                "data: {\"error\": \"" + escapeJson(e.getMessage()) + "\"}\n\n"
            );
        }
    }

    /**
     * AI 高级对话（支持多轮）
     */
    @PostMapping("/ai/chat/advanced")
    public Map<String, Object> chatAdvanced(@RequestBody AdvancedChatRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            top.yumbo.ai.ai.api.model.AIResponse response = aiService.chat(
                request.getSystemPrompt(),
                request.getMessages()
            );

            result.put("status", "success");
            result.put("response", response);
            result.put("model", aiService.getCurrentModel());
        } catch (Exception e) {
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 获取可用的AI模型列表
     */
    @GetMapping("/ai/models")
    public Map<String, Object> listModels() {
        Map<String, Object> result = new HashMap<>();

        try {
            result.put("status", "success");
            result.put("currentModel", aiService.getCurrentModel());
            result.put("models", aiService.listModels());
        } catch (Exception e) {
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * RAG + AI 组合查询示例
     * 先用RAG检索相关文档，再用AI生成答案
     */
    @PostMapping("/ai/rag-chat")
    public Map<String, Object> ragChat(@RequestBody RagChatRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            // 1. 使用RAG检索相关文档
            List<SearchResult> searchResults = ragService.searchByText(
                request.getQuestion(),
                request.getTopK() != null ? request.getTopK() : 5
            );

            // 2. 构建上下文
            StringBuilder context = new StringBuilder();
            context.append("基于以下参考信息回答问题：\n\n");
            for (int i = 0; i < searchResults.size(); i++) {
                SearchResult sr = searchResults.get(i);
                if (sr.getDocument() != null) {
                    context.append(String.format("[参考%d] %s\n", i + 1, sr.getDocument().getContent()));
                }
            }
            context.append("\n问题：").append(request.getQuestion());

            // 3. 使用AI生成答案
            String answer = aiService.chat(context.toString());

            result.put("status", "success");
            result.put("question", request.getQuestion());
            result.put("answer", answer);
            result.put("sources", searchResults);
            result.put("model", aiService.getCurrentModel());
        } catch (Exception e) {
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    // ========== 请求对象 ==========

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

    /**
     * 简单对话请求
     */
    @lombok.Data
    public static class ChatRequest {
        private String message;
    }

    /**
     * 文本生成请求
     */
    @lombok.Data
    public static class GenerateRequest {
        private String prompt;
        private Float temperature;
        private Integer maxTokens;
    }

    /**
     * 流式对话请求
     */
    @lombok.Data
    public static class StreamChatRequest {
        private String message;
        private String systemPrompt;
        private List<top.yumbo.ai.ai.api.model.ChatMessage> history;
    }

    /**
     * 高级对话请求
     */
    @lombok.Data
    public static class AdvancedChatRequest {
        private String systemPrompt;
        private List<top.yumbo.ai.ai.api.model.ChatMessage> messages;
    }

    /**
     * RAG+AI组合查询请求
     */
    @lombok.Data
    public static class RagChatRequest {
        private String question;
        private Integer topK;
    }

    // ========== 工具方法 ==========

    /**
     * JSON字符串转义（用于SSE事件）
     */
    private String escapeJson(String text) {
        if (text == null) {
            return "";
        }
        return text
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\t", "\\t");
    }
}
