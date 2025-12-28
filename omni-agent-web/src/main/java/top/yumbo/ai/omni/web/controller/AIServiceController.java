package top.yumbo.ai.omni.web.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.ai.api.model.AIRequest;
import top.yumbo.ai.omni.ai.api.model.AIResponse;
import top.yumbo.ai.omni.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.knowledge.registry.qa.util.ContextBuilder;
import top.yumbo.ai.omni.web.dto.ApiDtos.*;
import top.yumbo.ai.omni.web.util.JsonUtil;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.SearchResult;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * AI 服务控制器
 *
 * <p>提供 AI 基础服务接口：</p>
 * <ul>
 *   <li>简单对话</li>
 *   <li>流式对话（GET/POST）</li>
 *   <li>高级对话（多轮）</li>
 *   <li>文本生成</li>
 *   <li>流式文本生成</li>
 *   <li>模型列表</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/ai")
@RequiredArgsConstructor
public class AIServiceController {

    private final AIService aiService;
    private final RagService ragService;

    /**
     * AI 简单对话
     *
     * @param request 对话请求
     * @return 对话结果
     */
    @PostMapping("/chat")
    public Map<String, Object> chat(@RequestBody ChatRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            String answer = aiService.chat(request.getMessage());
            result.put("status", "success");
            result.put("question", request.getMessage());
            result.put("answer", answer);
            result.put("model", aiService.getCurrentModel());
        } catch (Exception e) {
            log.error("AI 对话失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * AI 流式对话 (GET 方式)
     * 实时返回 AI 生成的每个 token
     *
     * @param message 用户消息
     * @return SSE 流
     */
    @GetMapping(value = "/chat/stream", produces = "text/event-stream")
    public SseEmitter chatStreamGet(@RequestParam String message) {
        log.info("AI 流式对话(GET): message={}", message);

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
            try {
                List<ChatMessage> messages = List.of(
                        ChatMessage.builder()
                                .role("user")
                                .content(message)
                                .build()
                );

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
                handleError(emitter, e);
            }
        }).start();

        setupEmitterCallbacks(emitter);
        return emitter;
    }

    /**
     * AI 流式对话 (POST 方式，支持更复杂的参数)
     *
     * @param request 流式对话请求
     * @return SSE 流
     */
    @PostMapping(value = "/chat/stream", produces = "text/event-stream")
    public SseEmitter chatStreamPost(@RequestBody StreamChatRequest request) {
        log.info("AI 流式对话(POST): message={}", request.getMessage());

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
            try {
                List<ChatMessage> messages = buildMessages(request);

                aiService.chatFlux(messages)
                        .doOnNext(token -> {
                            try {
                                emitter.send(SseEmitter.event().data(JsonUtil.escapeJson(token)));
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
                                        .data("{\"error\": \"" + JsonUtil.escapeJson(e.getMessage()) + "\"}"));
                            } catch (Exception ex) {
                                log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                            }
                            emitter.completeWithError(e);
                        })
                        .subscribe();
            } catch (Exception e) {
                log.error("❌ AI 流式对话初始化失败", e);
                handleError(emitter, e);
            }
        }).start();

        setupEmitterCallbacks(emitter);
        return emitter;
    }

    /**
     * AI 高级对话（支持多轮）
     *
     * @param request 高级对话请求
     * @return 对话结果
     */
    @PostMapping("/chat/advanced")
    public Map<String, Object> chatAdvanced(@RequestBody AdvancedChatRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            AIResponse response = aiService.chat(
                    request.getSystemPrompt(),
                    request.getMessages()
            );

            result.put("status", "success");
            result.put("response", response);
            result.put("model", aiService.getCurrentModel());
        } catch (Exception e) {
            log.error("AI 高级对话失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * AI 文本生成
     *
     * @param request 生成请求
     * @return 生成结果
     */
    @PostMapping("/generate")
    public Map<String, Object> generate(@RequestBody GenerateRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            String generated = aiService.generate(request.getPrompt());
            result.put("status", "success");
            result.put("prompt", request.getPrompt());
            result.put("generated", generated);
            result.put("model", aiService.getCurrentModel());
        } catch (Exception e) {
            log.error("AI 文本生成失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * AI 流式文本生成
     *
     * @param request 生成请求
     * @return SSE 流
     */
    @PostMapping(value = "/generate/stream", produces = "text/event-stream")
    public SseEmitter generateStream(@RequestBody GenerateRequest request) {
        log.info("AI 流式生成: prompt={}", request.getPrompt());

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
            try {
                AIRequest aiRequest = AIRequest.builder()
                        .prompt(request.getPrompt())
                        .temperature(request.getTemperature() != null ? request.getTemperature() : 0.7f)
                        .maxTokens(request.getMaxTokens() != null ? request.getMaxTokens() : 2048)
                        .build();

                aiService.generateFlux(aiRequest)
                        .doOnNext(token -> {
                            try {
                                emitter.send(SseEmitter.event().data(JsonUtil.escapeJson(token)));
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
                                        .data("{\"error\": \"" + JsonUtil.escapeJson(e.getMessage()) + "\"}"));
                            } catch (Exception ex) {
                                log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                            }
                            emitter.completeWithError(e);
                        })
                        .subscribe();
            } catch (Exception e) {
                log.error("❌ AI 流式生成初始化失败", e);
                handleError(emitter, e);
            }
        }).start();

        setupEmitterCallbacks(emitter);
        return emitter;
    }

    /**
     * 获取可用的 AI 模型列表
     *
     * @return 模型列表
     */
    @GetMapping("/models")
    public Map<String, Object> listModels() {
        Map<String, Object> result = new HashMap<>();

        try {
            result.put("status", "success");
            result.put("currentModel", aiService.getCurrentModel());
            result.put("models", aiService.listModels());
        } catch (Exception e) {
            log.error("获取模型列表失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * RAG + AI 组合查询
     * 先用 RAG 检索相关文档，再用 AI 生成答案
     *
     * @param request RAG 对话请求
     * @return 查询结果
     */
    @PostMapping("/rag-chat")
    public Map<String, Object> ragChat(@RequestBody RagChatRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            // 1. 使用 RAG 检索相关文档
            var documents = ragService.semanticSearch(
                    request.getQuestion(),
                    request.getTopK() != null ? request.getTopK() : 5
            );

            // 转换为 SearchResult
            List<SearchResult> searchResults = documents.stream()
                    .map(SearchResult::fromDocument)
                    .toList();

            // 2. 构建上下文
            String context = ContextBuilder.buildContext(searchResults);
            String prompt = String.format(
                    "基于以下参考信息回答问题：\n\n%s\n\n问题：%s",
                    context, request.getQuestion()
            );

            // 3. 使用 AI 生成答案
            String answer = aiService.chat(prompt);

            result.put("status", "success");
            result.put("question", request.getQuestion());
            result.put("answer", answer);
            result.put("sources", searchResults);
            result.put("model", aiService.getCurrentModel());
            log.info("✅ RAG+AI 组合查询完成: question={}", request.getQuestion());
        } catch (Exception e) {
            log.error("❌ RAG+AI 组合查询失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    // ========== 私有辅助方法 ==========

    /**
     * 构建消息列表
     */
    private List<ChatMessage> buildMessages(StreamChatRequest request) {
        List<ChatMessage> messages = new ArrayList<>();

        // 添加系统提示（如果有）
        if (request.getSystemPrompt() != null && !request.getSystemPrompt().isEmpty()) {
            messages.add(ChatMessage.builder()
                    .role("system")
                    .content(request.getSystemPrompt())
                    .build());
        }

        // 添加历史消息（如果有）
        if (request.getHistory() != null) {
            messages.addAll(request.getHistory());
        }

        // 添加当前用户消息
        messages.add(ChatMessage.builder()
                .role("user")
                .content(request.getMessage())
                .build());

        return messages;
    }

    /**
     * 设置 SSE Emitter 回调
     */
    private void setupEmitterCallbacks(SseEmitter emitter) {
        emitter.onTimeout(() -> {
            log.warn("⏰ SSE 连接超时");
            emitter.complete();
        });

        emitter.onError(e -> log.error("❌ SSE 连接错误: {}", e.getMessage()));
        emitter.onCompletion(() -> log.info("✅ SSE 连接关闭"));
    }

    /**
     * 处理错误
     */
    private void handleError(SseEmitter emitter, Exception e) {
        try {
            emitter.send(SseEmitter.event().data("[ERROR] " + e.getMessage()));
            emitter.completeWithError(e);
        } catch (Exception ex) {
            log.error("❌ 发送错误消息失败: {}", ex.getMessage());
        }
    }
}






