package top.yumbo.ai.omni.example.basic.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.codec.ServerSentEvent;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Flux;
import top.yumbo.ai.ai.api.model.ChatMessage;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.SearchResult;
import top.yumbo.ai.omni.core.hope.HOPEKnowledgeManager;
import top.yumbo.ai.omni.core.role.Role;
import top.yumbo.ai.omni.core.role.RoleService;
import top.yumbo.ai.omni.core.query.QueryService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 基础示例控制器 - 增强版
 *
 * <p>演示如何使用OmniAgent的可插拔服务：</p>
 * <ul>
 *   <li>QuestionClassifierPersistence - 持久化服务</li>
 *   <li>DocumentStorageService - 文档存储服务</li>
 *   <li>RAGService - RAG检索服务</li>
 *   <li>AIService - AI推理服务</li>
 *   <li>HOPEKnowledgeManager - HOPE三层知识架构</li>
 *   <li>RoleService - 角色知识库</li>
 *   <li>QueryService - 智能问答</li>
 * </ul>
 *
 * @author Jinhua Yu
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api")
@CrossOrigin(origins = "*")  // 添加 CORS 支持
@RequiredArgsConstructor
public class DemoController {

    private final QuestionClassifierPersistence persistence;
    private final DocumentStorageService storageService;
    private final RAGService ragService;
    private final top.yumbo.ai.ai.api.AIService aiService;
    private final HOPEKnowledgeManager hopeManager;
    private final RoleService roleService;
    private final QueryService queryService;

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

    // ========== 知识库问答 API ==========

    /**
     * 智能问答 (统一入口)
     * 支持三种模式：
     * 1. knowledgeMode="none" - 直接LLM回答（不使用知识库）
     * 2. knowledgeMode="rag" - 传统RAG检索回答
     * 3. knowledgeMode="role" - 角色知识库回答
     */
    @PostMapping("/qa/ask")
    public Map<String, Object> ask(@RequestBody QuestionRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            String question = request.getQuestion();
            String knowledgeMode = request.getKnowledgeMode() != null ? request.getKnowledgeMode() : "rag";
            String roleName = request.getRoleName();
            String hopeSessionId = request.getHopeSessionId();

            log.info("收到问答请求: question={}, mode={}, role={}, session={}",
                    question, knowledgeMode, roleName, hopeSessionId);

            String answer;
            List<SearchResult> references = null;

            switch (knowledgeMode.toLowerCase()) {
                case "none":
                    // 直接LLM模式
                    answer = aiService.chat(question);
                    break;

                case "role":
                    // 角色知识库模式
                    if (roleName == null || roleName.isEmpty()) {
                        result.put("status", "error");
                        result.put("error", "roleName is required for role mode");
                        return result;
                    }

                    // 获取角色信息（getRole 返回 Role，不是 Optional）
                    Role roleEntity = roleService.getRole(roleName);

                    // 使用RAG检索
                    references = ragService.searchByText(question, 5);

                    // 构建包含角色信息和上下文的提示词
                    String roleContext = buildRoleContext(references);
                    String rolePrompt = String.format(
                            "你是%s，%s\n\n基于以下知识回答问题：\n\n%s\n\n问题：%s",
                            roleEntity.getName(), roleEntity.getDescription(), roleContext, question
                    );
                    answer = aiService.chat(rolePrompt);
                    break;

                case "rag":
                default:
                    // 传统RAG模式
                    references = ragService.searchByText(question, 5);

                    // 构建RAG提示词
                    String context = buildContext(references);
                    String prompt = String.format(
                            "基于以下知识回答问题：\n\n%s\n\n问题：%s",
                            context, question
                    );
                    answer = aiService.chat(prompt);
                    break;
            }

            result.put("status", "success");
            result.put("question", question);
            result.put("answer", answer);
            result.put("knowledgeMode", knowledgeMode);
            result.put("model", aiService.getCurrentModel());

            if (references != null && !references.isEmpty()) {
                result.put("referenceCount", references.size());
                result.put("references", references);
            }

            // 如果有HOPE会话ID，记录到会话历史
            if (hopeSessionId != null && !hopeSessionId.isEmpty()) {
                result.put("hopeSessionId", hopeSessionId);
                // TODO: 保存到HOPE会话历史
            }

        } catch (Exception e) {
            log.error("问答失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 流式问答
     */
    @GetMapping(value = "/qa/ask/stream", produces = "text/event-stream")
    public reactor.core.publisher.Flux<String> askStream(
            @RequestParam String question,
            @RequestParam(defaultValue = "rag") String knowledgeMode,
            @RequestParam(required = false) String roleName) {

        try {
            List<SearchResult> references;
            String prompt;

            if ("none".equals(knowledgeMode)) {
                // 直接LLM
                prompt = question;
            } else if ("role".equals(knowledgeMode) && roleName != null) {
                // 角色知识库（getRole 返回 Role，不是 Optional）
                top.yumbo.ai.omni.core.role.Role role = roleService.getRole(roleName);
                references = ragService.searchByText(question, 5);
                String context = buildRoleContext(references);
                prompt = String.format(
                        "你是%s，%s\n\n基于以下知识回答问题：\n\n%s\n\n问题：%s",
                        role.getName(), role.getDescription(), context, question
                );
            } else {
                // 传统RAG
                references = ragService.searchByText(question, 5);
                String context = buildContext(references);
                prompt = String.format("基于以下知识回答问题：\n\n%s\n\n问题：%s", context, question);
            }

            // 构建消息
            List<top.yumbo.ai.ai.api.model.ChatMessage> messages = List.of(
                    top.yumbo.ai.ai.api.model.ChatMessage.builder()
                            .role("user")
                            .content(prompt)
                            .build()
            );

            return aiService.chatFlux(messages)
                    .map(token -> "data: " + token + "\n\n")
                    .onErrorResume(e -> reactor.core.publisher.Flux.just(
                            "data: [ERROR] " + e.getMessage() + "\n\n"
                    ));

        } catch (Exception e) {
            log.error("流式问答失败", e);
            return reactor.core.publisher.Flux.just(
                    "data: [ERROR] " + e.getMessage() + "\n\n"
            );
        }
    }

    /**
     * HOPE会话查询
     * 使用HOPE三层知识架构进行智能问答
     */
    @PostMapping("/qa/hope")
    public Map<String, Object> hopeQuery(@RequestBody HOPEQueryRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            String question = request.getQuestion();
            String sessionId = request.getSessionId();

            log.info("HOPE查询: question={}, session={}", question, sessionId);

            // 使用HOPE管理器查询
            // HOPE会自动根据问题类型路由到合适的知识层
            // - 高频层：快速响应
            // - 普通层：常规知识
            // - 永久层：核心知识

            // TODO: 实现HOPE查询逻辑
            // String hopeAnswer = hopeManager.query(question, sessionId);

            // 临时实现：使用RAG
            List<SearchResult> references = ragService.searchByText(question, 5);
            String context = buildContext(references);
            String prompt = String.format(
                    "【HOPE智能问答】基于以下知识回答问题：\n\n%s\n\n问题：%s",
                    context, question
            );
            String answer = aiService.chat(prompt);

            result.put("status", "success");
            result.put("question", question);
            result.put("answer", answer);
            result.put("sessionId", sessionId);
            result.put("hopeEnabled", true);
            result.put("references", references);

        } catch (Exception e) {
            log.error("HOPE查询失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 批量索引文档
     */
    @PostMapping("/rag/index/batch")
    public Map<String, Object> indexDocuments(@RequestBody BatchIndexRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            List<String> docIds = ragService.indexDocuments(request.getDocuments());

            result.put("status", "success");
            result.put("indexedCount", docIds.size());
            result.put("documentIds", docIds);
            result.put("message", "Documents indexed successfully");
        } catch (Exception e) {
            log.error("批量索引失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 重建索引
     */
    @PostMapping("/rag/rebuild")
    public Map<String, Object> rebuildIndex() {
        Map<String, Object> result = new HashMap<>();

        try {
            ragService.rebuildIndex();

            result.put("status", "success");
            result.put("message", "Index rebuild completed");
            result.put("statistics", ragService.getStatistics());
        } catch (Exception e) {
            log.error("重建索引失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 双轨流式问答 ⭐ NEW
     * 同时返回AI答案流和参考文档
     * GET /api/qa/stream/dual-track
     */
    @GetMapping(value = "/qa/stream/dual-track", produces = "text/event-stream")
    public Flux<ServerSentEvent<String>> dualTrackStream(
            @RequestParam String question,
            @RequestParam(defaultValue = "none") String knowledgeMode,
            @RequestParam(required = false) String roleName) {

        try {
            log.info("双轨流式问答: question={}, mode={}, role={}", question, knowledgeMode, roleName);

            // 1. 先发送参考文档（如果需要）
            Flux<ServerSentEvent<String>> referencesFlux = Flux.empty();

            if (!"none".equals(knowledgeMode)) {
                List<SearchResult> references = ragService.searchByText(question, 5);
                log.info("📚 检索到 {} 个参考文档", references.size());

                // 发送参考文档事件
                referencesFlux = Flux.fromIterable(references)
                        .doOnNext(ref -> log.info("📄 发送参考文档: {}",
                                ref.getDocument().getTitle() != null ? ref.getDocument().getTitle() : "无标题"))
                        .map(ref -> {
                            try {
                                String refJson = String.format(
                                        "{\"type\":\"reference\",\"title\":\"%s\",\"content\":\"%s\",\"score\":%.2f}",
                                        escapeJson(ref.getDocument().getTitle() != null ? ref.getDocument().getTitle() : ""),
                                        escapeJson(ref.getDocument().getContent()),
                                        ref.getScore()
                                );
                                return ServerSentEvent.<String>builder()
                                        .data(refJson)
                                        .build();
                            } catch (Exception e) {
                                log.error("❌ 发送参考文档失败: {}", e.getMessage());
                                return ServerSentEvent.<String>builder()
                                        .data("{\"type\":\"error\",\"message\":\"" + escapeJson(e.getMessage()) + "\"}")
                                        .build();
                            }
                        });
            } else {
                log.info("🚫 不使用知识库模式，跳过参考文档检索");
            }

            // 2. 构建AI提示词
            String prompt;
            if ("none".equals(knowledgeMode)) {
                prompt = question;
            } else if ("role".equals(knowledgeMode) && roleName != null) {
                Role role = roleService.getRole(roleName);
                List<SearchResult> references = ragService.searchByText(question, 5);
                String context = buildRoleContext(references);
                prompt = String.format(
                        "你是%s，%s\n\n基于以下知识回答问题：\n\n%s\n\n问题：%s",
                        role.getName(), role.getDescription(), context, question
                );
            } else {
                List<SearchResult> references = ragService.searchByText(question, 5);
                String context = buildContext(references);
                prompt = String.format("基于以下知识回答问题：\n\n%s\n\n问题：%s", context, question);
            }

            // 3. AI答案流
            List<ChatMessage> messages = List.of(
                    ChatMessage.builder()
                            .role("user")
                            .content(prompt)
                            .build()
            );

            // 统一使用 answer 类型发送 token（前端只处理 answer 类型）
            Flux<ServerSentEvent<String>> answerFlux =
                    aiService.chatFlux(messages)
                            .doOnSubscribe(sub -> log.info("🚀 开始流式生成答案..."))
                            .doOnNext(token -> {
                                // 实时输出每个 token
                                log.info("📤 发送 token: [{}]", token);
                            })
                            .doOnComplete(() -> log.info("✅ 答案流生成完成"))
                            .doOnError(e -> log.error("❌ 答案流生成失败: {}", e.getMessage()))
                            .map(token -> {
                                String jsonData = "{\"type\":\"answer\",\"token\":\"" + escapeJson(token) + "\"}";
                                log.debug("📦 SSE 数据包: {}", jsonData);
                                return ServerSentEvent.<String>builder()
                                        .data(jsonData)
                                        .build();
                            });

            // 添加完成事件
            Flux<ServerSentEvent<String>> completeFlux = Flux.just(
                    ServerSentEvent.<String>builder()
                            .data("{\"type\":\"complete\"}")
                            .build()
            ).doOnNext(event -> log.info("🏁 发送完成标记"));

            // 4. 合并三个流：参考文档 -> 答案流 -> 完成标记
            return Flux.concat(referencesFlux, answerFlux, completeFlux)
                    .doOnSubscribe(sub -> log.info("🌊 开始双轨流式传输..."))
                    .doOnComplete(() -> log.info("✅ 双轨流式传输完成"))
                    .onErrorResume(e -> {
                        log.error("❌ 双轨流式问答失败", e);
                        return Flux.just(
                                ServerSentEvent.<String>builder()
                                        .data("{\"type\":\"error\",\"message\":\"" + escapeJson(e.getMessage()) + "\"}")
                                        .build()
                        );
                    });

        } catch (Exception e) {
            log.error("双轨流式问答初始化失败", e);
            return Flux.just(
                    ServerSentEvent.<String>builder()
                            .data("{\"type\":\"error\",\"message\":\"" + escapeJson(e.getMessage()) + "\"}")
                            .build()
            );
        }
    }

    /**
     * 获取相似问题 ⭐ NEW
     * 基于RAG检索返回相似的问题
     * GET /api/qa/similar
     */
    @GetMapping("/qa/similar")
    public Map<String, Object> getSimilarQuestions(@RequestParam String question) {
        Map<String, Object> result = new HashMap<>();

        try {
            log.info("获取相似问题: {}", question);

            // 使用RAG搜索相似问题
            List<SearchResult> searchResults = ragService.searchByText(question, 5);

            // 提取问题（假设文档标题是问题）
            List<Map<String, Object>> similarQuestions = new java.util.ArrayList<>();
            for (SearchResult sr : searchResults) {
                Map<String, Object> item = new HashMap<>();

                // 如果有标题，使用标题作为问题
                String questionText = sr.getDocument().getTitle() != null && !sr.getDocument().getTitle().isEmpty()
                        ? sr.getDocument().getTitle()
                        : sr.getDocument().getContent().substring(0, Math.min(50, sr.getDocument().getContent().length())) + "...";

                item.put("question", questionText);
                item.put("score", sr.getScore());
                item.put("documentId", sr.getDocument().getId());

                similarQuestions.add(item);
            }

            result.put("status", "success");
            result.put("query", question);
            result.put("count", similarQuestions.size());
            result.put("questions", similarQuestions);

        } catch (Exception e) {
            log.error("获取相似问题失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
            result.put("questions", new java.util.ArrayList<>()); // 返回空列表
        }

        return result;
    }

    // ========== 辅助方法 ==========

    private String buildContext(List<SearchResult> references) {
        if (references == null || references.isEmpty()) {
            return "暂无相关知识";
        }

        StringBuilder context = new StringBuilder();
        for (int i = 0; i < references.size(); i++) {
            SearchResult ref = references.get(i);
            context.append(String.format("[文档%d] ", i + 1));
            if (ref.getDocument().getTitle() != null) {
                context.append(ref.getDocument().getTitle()).append("\n");
            }
            context.append(ref.getDocument().getContent()).append("\n\n");
        }
        return context.toString();
    }

    private String buildRoleContext(List<SearchResult> references) {
        if (references == null || references.isEmpty()) {
            return "暂无相关角色知识";
        }

        StringBuilder context = new StringBuilder();
        for (int i = 0; i < references.size(); i++) {
            SearchResult ref = references.get(i);
            context.append(String.format("[角色知识%d] ", i + 1));
            context.append(ref.getDocument().getContent()).append("\n\n");
        }
        return context.toString();
    }

    // ========== DTO 类 ==========

    @Data
    public static class QuestionRequest {
        private String question;
        private String knowledgeMode;  // "none", "rag", "role"
        private String roleName;
        private String hopeSessionId;
    }

    @Data
    public static class HOPEQueryRequest {
        private String question;
        private String sessionId;
    }

    @Data
    public static class BatchIndexRequest {
        private List<Document> documents;
    }
}
