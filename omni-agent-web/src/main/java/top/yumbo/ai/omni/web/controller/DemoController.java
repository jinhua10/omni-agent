package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
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
@RequiredArgsConstructor
public class DemoController {

    private final QuestionClassifierPersistence persistence;
    private final DocumentStorageService storageService;
    private final RAGService ragService;
    private final top.yumbo.ai.ai.api.AIService aiService;
    private final HOPEKnowledgeManager hopeManager;
    private final RoleService roleService;
    private final QueryService queryService;
    private final top.yumbo.ai.omni.core.query.EnhancedQueryService enhancedQueryService;

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
    public SseEmitter chatStream(@RequestParam String message) {
        log.info("AI流式对话: message={}", message);

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
            try {
                // 构建简单的消息列表
                List<top.yumbo.ai.ai.api.model.ChatMessage> messages = List.of(
                        top.yumbo.ai.ai.api.model.ChatMessage.builder()
                                .role("user")
                                .content(message)
                                .build()
                );

                // 流式发送 AI 响应
                aiService.chatFlux(messages)
                        .doOnNext(token -> {
                            try {
                                emitter.send(SseEmitter.event().data(token));
                                log.debug("📤 发送 token: [{}]", token);
                            } catch (Exception e) {
                                log.error("❌ 发送 token 失败: {}", e.getMessage());
                                emitter.completeWithError(e);
                            }
                        })
                        .doOnComplete(() -> {
                            log.info("✅ AI 流式对话完成");
                            emitter.complete();
                        })
                        .doOnError(e -> {
                            log.error("❌ AI 流式对话失败: {}", e.getMessage());
                            try {
                                emitter.send(SseEmitter.event().data("[ERROR] " + e.getMessage()));
                            } catch (Exception ex) {
                                log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                            }
                            emitter.completeWithError(e);
                        })
                        .subscribe();
            } catch (Exception e) {
                log.error("❌ AI 流式对话初始化失败", e);
                try {
                    emitter.send(SseEmitter.event().data("[ERROR] " + e.getMessage()));
                    emitter.completeWithError(e);
                } catch (Exception ex) {
                    log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                }
            }
        }).start();

        emitter.onTimeout(() -> {
            log.warn("⏰ SSE 连接超时");
            emitter.complete();
        });

        emitter.onError(e -> log.error("❌ SSE 连接错误: {}", e.getMessage()));
        emitter.onCompletion(() -> log.info("✅ SSE 连接关闭"));

        return emitter;
    }

    /**
     * AI 流式对话 (POST方式，支持更复杂的参数) ⭐ NEW
     */
    @PostMapping(value = "/ai/chat/stream", produces = "text/event-stream")
    public SseEmitter chatStreamPost(@RequestBody StreamChatRequest request) {
        log.info("AI流式对话(POST): message={}", request.getMessage());

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
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

                // 流式发送 AI 响应
                aiService.chatFlux(messages)
                        .doOnNext(token -> {
                            try {
                                emitter.send(SseEmitter.event().data(escapeJson(token)));
                                log.debug("📤 发送 token: [{}]", token);
                            } catch (Exception e) {
                                log.error("❌ 发送 token 失败: {}", e.getMessage());
                                emitter.completeWithError(e);
                            }
                        })
                        .doOnComplete(() -> {
                            log.info("✅ AI 流式对话完成");
                            emitter.complete();
                        })
                        .doOnError(e -> {
                            log.error("❌ AI 流式对话失败: {}", e.getMessage());
                            try {
                                emitter.send(SseEmitter.event()
                                        .data("{\"error\": \"" + escapeJson(e.getMessage()) + "\"}"));
                            } catch (Exception ex) {
                                log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                            }
                            emitter.completeWithError(e);
                        })
                        .subscribe();
            } catch (Exception e) {
                log.error("❌ AI 流式对话初始化失败", e);
                try {
                    emitter.send(SseEmitter.event()
                            .data("{\"error\": \"" + escapeJson(e.getMessage()) + "\"}"));
                    emitter.completeWithError(e);
                } catch (Exception ex) {
                    log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                }
            }
        }).start();

        emitter.onTimeout(() -> {
            log.warn("⏰ SSE 连接超时");
            emitter.complete();
        });

        emitter.onError(e -> log.error("❌ SSE 连接错误: {}", e.getMessage()));
        emitter.onCompletion(() -> log.info("✅ SSE 连接关闭"));

        return emitter;
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
    public SseEmitter generateStream(@RequestBody GenerateRequest request) {
        log.info("AI流式生成: prompt={}", request.getPrompt());

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
            try {
                top.yumbo.ai.ai.api.model.AIRequest aiRequest = top.yumbo.ai.ai.api.model.AIRequest.builder()
                        .prompt(request.getPrompt())
                        .temperature(request.getTemperature() != null ? request.getTemperature() : 0.7f)
                        .maxTokens(request.getMaxTokens() != null ? request.getMaxTokens() : 2048)
                        .build();

                // 流式发送 AI 响应
                aiService.generateFlux(aiRequest)
                        .doOnNext(token -> {
                            try {
                                emitter.send(SseEmitter.event().data(escapeJson(token)));
                                log.debug("📤 发送 token: [{}]", token);
                            } catch (Exception e) {
                                log.error("❌ 发送 token 失败: {}", e.getMessage());
                                emitter.completeWithError(e);
                            }
                        })
                        .doOnComplete(() -> {
                            log.info("✅ AI 流式生成完成");
                            emitter.complete();
                        })
                        .doOnError(e -> {
                            log.error("❌ AI 流式生成失败: {}", e.getMessage());
                            try {
                                emitter.send(SseEmitter.event()
                                        .data("{\"error\": \"" + escapeJson(e.getMessage()) + "\"}"));
                            } catch (Exception ex) {
                                log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                            }
                            emitter.completeWithError(e);
                        })
                        .subscribe();
            } catch (Exception e) {
                log.error("❌ AI 流式生成初始化失败", e);
                try {
                    emitter.send(SseEmitter.event()
                            .data("{\"error\": \"" + escapeJson(e.getMessage()) + "\"}"));
                    emitter.completeWithError(e);
                } catch (Exception ex) {
                    log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                }
            }
        }).start();

        emitter.onTimeout(() -> {
            log.warn("⏰ SSE 连接超时");
            emitter.complete();
        });

        emitter.onError(e -> log.error("❌ SSE 连接错误: {}", e.getMessage()));
        emitter.onCompletion(() -> log.info("✅ SSE 连接关闭"));

        return emitter;
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
    public SseEmitter askStream(
            @RequestParam String question,
            @RequestParam(defaultValue = "rag") String knowledgeMode,
            @RequestParam(required = false) String roleName) {

        log.info("流式问答: question={}, mode={}, role={}", question, knowledgeMode, roleName);

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
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

                // 流式发送 AI 响应
                aiService.chatFlux(messages)
                        .doOnNext(token -> {
                            try {
                                emitter.send(SseEmitter.event().data(token));
                                log.debug("📤 发送 token: [{}]", token);
                            } catch (Exception e) {
                                log.error("❌ 发送 token 失败: {}", e.getMessage());
                                emitter.completeWithError(e);
                            }
                        })
                        .doOnComplete(() -> {
                            log.info("✅ 流式问答完成");
                            emitter.complete();
                        })
                        .doOnError(e -> {
                            log.error("❌ 流式问答失败: {}", e.getMessage());
                            try {
                                emitter.send(SseEmitter.event().data("[ERROR] " + e.getMessage()));
                            } catch (Exception ex) {
                                log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                            }
                            emitter.completeWithError(e);
                        })
                        .subscribe();
            } catch (Exception e) {
                log.error("❌ 流式问答初始化失败", e);
                try {
                    emitter.send(SseEmitter.event().data("[ERROR] " + e.getMessage()));
                    emitter.completeWithError(e);
                } catch (Exception ex) {
                    log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                }
            }
        }).start();

        emitter.onTimeout(() -> {
            log.warn("⏰ SSE 连接超时");
            emitter.complete();
        });

        emitter.onError(e -> log.error("❌ SSE 连接错误: {}", e.getMessage()));
        emitter.onCompletion(() -> log.info("✅ SSE 连接关闭"));

        return emitter;
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
     * 双轨流式问答 ⭐ 重构版
     *
     * <p>双轨输出架构：</p>
     * <ul>
     *   <li><b>左轨（left）</b>：传统 RAG + LLM 回答
     *       <br>- 检索相关文档
     *       <br>- 构建上下文
     *       <br>- LLM生成答案
     *   </li>
     *   <li><b>右轨（right）</b>：HOPE智能系统 / 角色知识库
     *       <br>- HOPE三层知识架构（自我学习）
     *       <br>- 算法市场优化
     *       <br>- 知识最小概念综合
     *       <br>- 角色专业回答（如果选择角色）
     *   </li>
     * </ul>
     *
     * <p>知识模式说明：</p>
     * <ul>
     *   <li>none: 单轨模式，仅LLM</li>
     *   <li>rag: 双轨模式，左轨RAG+LLM，右轨HOPE智能系统</li>
     *   <li>role: 双轨模式，左轨RAG+LLM，右轨角色知识库</li>
     * </ul>
     *
     * @param question 用户问题
     * @param knowledgeMode 知识库模式: none | rag | role
     * @param roleName 角色名称（role模式必需）
     * @return SSE流
     */
    @GetMapping(value = "/qa/stream/dual-track", produces = "text/event-stream")
    public SseEmitter dualTrackStream(
            @RequestParam String question,
            @RequestParam(defaultValue = "none") String knowledgeMode,
            @RequestParam(required = false) String roleName) {

        log.info("🚂 双轨流式问答: question={}, mode={}, role={}", question, knowledgeMode, roleName);

        // 创建 SseEmitter，超时时间 5 分钟
        SseEmitter emitter = new SseEmitter(300000L);

        // 异步处理
        new Thread(() -> {
            try {
                // 判断是否为双轨模式
                final boolean isDualTrack = !"none".equals(knowledgeMode);

                if (!isDualTrack) {
                    // === 单轨模式（仅LLM） ===
                    handleSingleTrack(emitter, question);
                } else {
                    // === 双轨模式 ===
                    // 1. 检索参考文档
                    List<SearchResult> references = ragService.searchByText(question, 5);
                    log.info("📚 检索到 {} 个参考文档", references.size());

                    // 发送参考文档或友好提示
                    sendReferences(emitter, references);

                    // 2. 并行生成双轨回答
                    if ("role".equals(knowledgeMode)) {
                        // 角色模式：左轨RAG+LLM，右轨角色专业回答
                        handleRoleMode(emitter, question, roleName, references);
                    } else {
                        // RAG模式：左轨RAG+LLM，右轨HOPE智能系统
                        handleRagMode(emitter, question, references);
                    }
                }

            } catch (Exception e) {
                log.error("❌ 双轨流式问答失败", e);
                sendError(emitter, e.getMessage());
            }
        }).start();

        // 设置超时和错误处理
        setupEmitterCallbacks(emitter);

        return emitter;
    }

    /**
     * 处理单轨模式（仅LLM）
     */
    private void handleSingleTrack(SseEmitter emitter, String question) {
        log.info("🚂 单轨模式：纯LLM");

        List<ChatMessage> messages = List.of(
                ChatMessage.builder()
                        .role("user")
                        .content(question)
                        .build()
        );

        aiService.chatFlux(messages)
                .doOnNext(token -> {
                    try {
                        sendToken(emitter, "llm", token);
                    } catch (Exception e) {
                        log.error("❌ 发送LLM token失败: {}", e.getMessage());
                    }
                })
                .doOnComplete(() -> sendComplete(emitter))
                .doOnError(e -> sendError(emitter, e.getMessage()))
                .subscribe();
    }

    /**
     * 处理RAG模式：左轨RAG+LLM，右轨HOPE智能系统
     */
    private void handleRagMode(SseEmitter emitter, String question, List<SearchResult> references) {
        log.info("🚂 双轨模式：RAG + HOPE智能系统");

        // CountDownLatch用于协调两个轨道
        java.util.concurrent.CountDownLatch leftTrackLatch = new java.util.concurrent.CountDownLatch(1);

        // 左轨：传统RAG + LLM（使用普通检索）
        String leftContext = buildContext(references);
        String leftPrompt = leftContext.isEmpty()
                ? String.format("问题：%s\n\n注意：未检索到相关文档，请基于你的通用知识回答。", question)
                : String.format("基于以下知识回答问题：\n\n%s\n\n问题：%s", leftContext, question);

        List<ChatMessage> leftMessages = List.of(
                ChatMessage.builder()
                        .role("user")
                        .content(leftPrompt)
                        .build()
        );

        log.info("⬅️ 启动左轨：传统RAG+LLM");

        aiService.chatFlux(leftMessages)
                .doOnNext(token -> {
                    try {
                        sendToken(emitter, "left", token);
                    } catch (Exception e) {
                        log.error("❌ 发送左轨token失败: {}", e.getMessage());
                    }
                })
                .doOnComplete(() -> {
                    log.info("✅ 左轨完成");
                    leftTrackLatch.countDown();
                })
                .doOnError(e -> {
                    log.error("❌ 左轨失败: {}", e.getMessage());
                    sendWarning(emitter, "left", "左轨（RAG+LLM）生成失败");
                    leftTrackLatch.countDown();
                })
                .subscribe();

        // 等待左轨完成
        try {
            leftTrackLatch.await(120, java.util.concurrent.TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            log.error("❌ 左轨超时", e);
        }

        // 右轨：HOPE智能系统（自我学习 + 算法市场优化）
        log.info("➡️ 启动右轨：HOPE智能系统 + 算法市场优化");

        // 使用HOPE进行智能查询
        HOPEKnowledgeManager.QueryResult hopeResult = hopeManager.smartQuery(question, null);

        // 使用增强查询服务进行优化检索（查询扩展 + 重排序）
        List<SearchResult> enhancedReferences;
        try {
            log.info("🔍 使用算法市场增强检索（查询扩展 + 重排序）");
            enhancedReferences = enhancedQueryService.fullyEnhancedSearch(question, 5);
            log.info("📈 增强检索完成：获得 {} 个优化结果", enhancedReferences.size());
        } catch (Exception e) {
            log.warn("⚠️ 增强检索失败，使用原始检索结果: {}", e.getMessage());
            enhancedReferences = references;
        }

        // 构建HOPE增强提示词（使用优化后的检索结果）
        String rightPrompt = buildHOPEPrompt(question, hopeResult, enhancedReferences);

        List<ChatMessage> rightMessages = List.of(
                ChatMessage.builder()
                        .role("user")
                        .content(rightPrompt)
                        .build()
        );

        aiService.chatFlux(rightMessages)
                .doOnNext(token -> {
                    try {
                        sendToken(emitter, "right", token);
                    } catch (Exception e) {
                        log.error("❌ 发送右轨token失败: {}", e.getMessage());
                    }
                })
                .doOnComplete(() -> {
                    log.info("✅ 右轨完成");
                    sendComplete(emitter);
                })
                .doOnError(e -> {
                    log.error("❌ 右轨失败: {}", e.getMessage());
                    sendWarning(emitter, "right", "右轨（HOPE智能系统）生成失败：" + e.getMessage());
                    sendError(emitter, e.getMessage());
                })
                .subscribe();
    }

    /**
     * 处理角色模式：左轨RAG+LLM，右轨角色专业回答
     */
    private void handleRoleMode(SseEmitter emitter, String question, String roleName, List<SearchResult> references) {
        log.info("🚂 双轨模式：RAG + 角色知识库 (role={})", roleName);

        // 获取角色信息
        Role role = roleService.getRole(roleName != null ? roleName : "default");
        log.info("🎭 使用角色: {} - {}", role.getName(), role.getDescription());

        // CountDownLatch用于协调两个轨道
        java.util.concurrent.CountDownLatch leftTrackLatch = new java.util.concurrent.CountDownLatch(1);

        // 左轨：传统RAG + LLM
        String leftContext = buildContext(references);
        String leftPrompt = leftContext.isEmpty()
                ? String.format("问题：%s\n\n注意：未检索到相关文档，请基于你的通用知识回答。", question)
                : String.format("基于以下知识回答问题：\n\n%s\n\n问题：%s", leftContext, question);

        List<ChatMessage> leftMessages = List.of(
                ChatMessage.builder()
                        .role("user")
                        .content(leftPrompt)
                        .build()
        );

        log.info("⬅️ 启动左轨：传统RAG+LLM");

        aiService.chatFlux(leftMessages)
                .doOnNext(token -> {
                    try {
                        sendToken(emitter, "left", token);
                    } catch (Exception e) {
                        log.error("❌ 发送左轨token失败: {}", e.getMessage());
                    }
                })
                .doOnComplete(() -> {
                    log.info("✅ 左轨完成");
                    leftTrackLatch.countDown();
                })
                .doOnError(e -> {
                    log.error("❌ 左轨失败: {}", e.getMessage());
                    sendWarning(emitter, "left", "左轨（RAG+LLM）生成失败");
                    leftTrackLatch.countDown();
                })
                .subscribe();

        // 等待左轨完成
        try {
            leftTrackLatch.await(120, java.util.concurrent.TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            log.error("❌ 左轨超时", e);
        }

        // 右轨：角色专业回答
        log.info("➡️ 启动右轨：角色 [{}] 专业回答", role.getName());

        // 构建角色提示词
        String roleContext = buildRoleContext(references);
        String rightPrompt = String.format(
                "你是%s，%s\n\n" +
                "作为专业角色，请基于以下知识给出你的专业见解：\n\n%s\n\n" +
                "问题：%s\n\n" +
                "请以你的角色身份，结合专业知识回答。",
                role.getName(),
                role.getDescription(),
                roleContext.isEmpty() ? "暂无特定知识，请基于角色专业性回答" : roleContext,
                question
        );

        List<ChatMessage> rightMessages = List.of(
                ChatMessage.builder()
                        .role("user")
                        .content(rightPrompt)
                        .build()
        );

        aiService.chatFlux(rightMessages)
                .doOnNext(token -> {
                    try {
                        sendToken(emitter, "right", token);
                    } catch (Exception e) {
                        log.error("❌ 发送右轨token失败: {}", e.getMessage());
                    }
                })
                .doOnComplete(() -> {
                    log.info("✅ 右轨完成");
                    sendComplete(emitter);
                })
                .doOnError(e -> {
                    log.error("❌ 右轨失败: {}", e.getMessage());
                    sendWarning(emitter, "right", "右轨（角色专业回答）生成失败：" + e.getMessage());
                    sendError(emitter, e.getMessage());
                })
                .subscribe();
    }

    /**
     * 构建HOPE增强提示词
     */
    private String buildHOPEPrompt(String question, HOPEKnowledgeManager.QueryResult hopeResult,
                                    List<SearchResult> references) {
        StringBuilder prompt = new StringBuilder();

        prompt.append("【HOPE智能系统 - 自我学习回答】\n\n");
        prompt.append(String.format("问题类型：%s\n", hopeResult.getQuestionType()));
        prompt.append(String.format("建议知识层：%s\n", hopeResult.getSuggestedLayer()));
        prompt.append(String.format("置信度：%.2f\n\n", hopeResult.getConfidence()));

        // 如果HOPE已有答案，使用它
        if (hopeResult.getAnswer() != null && !hopeResult.getAnswer().isEmpty()) {
            prompt.append("系统学习到的答案：\n");
            prompt.append(hopeResult.getAnswer()).append("\n\n");
        }

        // 添加检索到的上下文
        String context = buildContext(references);
        if (!context.isEmpty()) {
            prompt.append("补充知识：\n");
            prompt.append(context).append("\n\n");
        }

        prompt.append("问题：").append(question).append("\n\n");
        prompt.append("请综合系统学习的知识和补充知识，给出专业且经过自我学习优化的回答。");

        return prompt.toString();
    }

    /**
     * 发送参考文档或友好提示
     */
    private void sendReferences(SseEmitter emitter, List<SearchResult> references) {
        try {
            if (references.isEmpty()) {
                String noResultJson = "{\"type\":\"info\",\"message\":\"未检索到相关文档，将基于通用知识和系统学习回答\"}";
                emitter.send(SseEmitter.event().data(noResultJson));
                log.info("💡 发送无检索结果提示");
            } else {
                for (SearchResult ref : references) {
                    String refJson = String.format(
                            "{\"type\":\"reference\",\"title\":\"%s\",\"content\":\"%s\",\"score\":%.2f}",
                            escapeJson(ref.getDocument().getTitle() != null ? ref.getDocument().getTitle() : ""),
                            escapeJson(ref.getDocument().getContent()),
                            ref.getScore()
                    );
                    emitter.send(SseEmitter.event().data(refJson));
                    log.debug("📄 发送参考文档");
                }
            }
        } catch (Exception e) {
            log.error("❌ 发送参考文档失败: {}", e.getMessage());
        }
    }

    /**
     * 发送token
     */
    private void sendToken(SseEmitter emitter, String track, String token) throws Exception {
        String jsonData = String.format(
                "{\"content\":\"%s\",\"chunkIndex\":%d}",
                escapeJson(token),
                0
        );
        emitter.send(SseEmitter.event()
                .name(track)
                .data(jsonData));
        log.debug("📤 [{}] token: [{}]", track.toUpperCase(), token);
    }

    /**
     * 发送完成标记
     */
    private void sendComplete(SseEmitter emitter) {
        try {
            emitter.send(SseEmitter.event()
                    .name("complete")
                    .data("{\"type\":\"complete\"}"));
            log.info("✅ 双轨流式问答完成");
            emitter.complete();
        } catch (Exception e) {
            log.error("❌ 发送完成标记失败: {}", e.getMessage());
            emitter.completeWithError(e);
        }
    }

    /**
     * 发送警告
     */
    private void sendWarning(SseEmitter emitter, String track, String message) {
        try {
            String warningJson = String.format(
                    "{\"type\":\"warning\",\"track\":\"%s\",\"message\":\"%s\"}",
                    track, escapeJson(message)
            );
            emitter.send(SseEmitter.event().data(warningJson));
        } catch (Exception e) {
            log.error("❌ 发送警告失败: {}", e.getMessage());
        }
    }

    /**
     * 发送错误
     */
    private void sendError(SseEmitter emitter, String message) {
        try {
            String errorJson = String.format(
                    "{\"type\":\"error\",\"message\":\"%s\"}",
                    escapeJson(message)
            );
            emitter.send(SseEmitter.event()
                    .name("error")
                    .data(errorJson));
            emitter.completeWithError(new RuntimeException(message));
        } catch (Exception e) {
            log.error("❌ 发送错误消息失败: {}", e.getMessage());
        }
    }

    /**
     * 设置Emitter回调
     */
    private void setupEmitterCallbacks(SseEmitter emitter) {
        emitter.onTimeout(() -> {
            log.warn("⏰ SSE连接超时");
            emitter.complete();
        });

        emitter.onError(e -> log.error("❌ SSE连接错误: {}", e.getMessage()));

        emitter.onCompletion(() -> log.info("✅ SSE连接关闭"));
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
