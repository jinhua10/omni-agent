package top.yumbo.ai.omni.web.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.core.role.Role;
import top.yumbo.ai.omni.core.role.RoleService;
import top.yumbo.ai.omni.web.dto.ApiDtos.*;
import top.yumbo.ai.omni.web.util.ContextBuilder;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.SearchResult;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 智能问答控制器
 *
 * <p>提供统一的智能问答接口，支持多种知识模式：</p>
 * <ul>
 *   <li>none - 直接 LLM 回答（不使用知识库）</li>
 *   <li>rag - 传统 RAG 检索回答</li>
 *   <li>role - 角色知识库回答</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/qa")
@RequiredArgsConstructor
public class QAController {

    private final AIService aiService;
    private final RagService ragService;
    private final RoleService roleService;

    /**
     * 智能问答（统一入口）
     *
     * @param request 问答请求
     * @return 问答结果
     */
    @PostMapping("/ask")
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
                    // 直接 LLM 模式
                    answer = aiService.chat(question);
                    break;

                case "role":
                    // 角色知识库模式
                    if (roleName == null || roleName.isEmpty()) {
                        result.put("status", "error");
                        result.put("error", "roleName is required for role mode");
                        return result;
                    }

                    Role roleEntity = roleService.getRole(roleName);
                    var roleDocuments = ragService.semanticSearch(question, 5);
                    references = roleDocuments.stream().map(SearchResult::fromDocument).toList();

                    String roleContext = ContextBuilder.buildRoleContext(references);
                    String rolePrompt = String.format(
                            "你是%s，%s\n\n基于以下知识回答问题：\n\n%s\n\n问题：%s",
                            roleEntity.getName(), roleEntity.getDescription(), roleContext, question
                    );
                    answer = aiService.chat(rolePrompt);
                    break;

                case "rag":
                default:
                    // 传统 RAG 模式
                    var ragDocuments = ragService.semanticSearch(question, 5);
                    references = ragDocuments.stream().map(SearchResult::fromDocument).toList();
                    String context = ContextBuilder.buildContext(references);
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

            if (hopeSessionId != null && !hopeSessionId.isEmpty()) {
                result.put("hopeSessionId", hopeSessionId);
                // TODO: 保存到 HOPE 会话历史
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
     *
     * @param question      问题
     * @param knowledgeMode 知识模式
     * @param roleName      角色名称（role 模式时需要）
     * @return SSE 流
     */
    @GetMapping(value = "/ask/stream", produces = "text/event-stream")
    public SseEmitter askStream(
            @RequestParam String question,
            @RequestParam(defaultValue = "rag") String knowledgeMode,
            @RequestParam(required = false) String roleName) {

        log.info("流式问答: question={}, mode={}, role={}", question, knowledgeMode, roleName);

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
            try {
                String prompt = buildPrompt(question, knowledgeMode, roleName);

                List<ChatMessage> messages = List.of(
                        ChatMessage.builder()
                                .role("user")
                                .content(prompt)
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
                            log.info("✅ 流式问答完成");
                            emitter.complete();
                        })
                        .doOnError(e -> {
                            log.error("❌ 流式问答失败: {}", e.getMessage());
                            try {
                                emitter.send(SseEmitter.event().data("[ERROR] " + e.getMessage()));
                                emitter.completeWithError(e);
                            } catch (Exception ex) {
                                log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                            }
                        })
                        .subscribe();
            } catch (Exception e) {
                log.error("❌ 流式问答初始化失败", e);
                handleError(emitter, e);
            }
        }).start();

        setupEmitterCallbacks(emitter);
        return emitter;
    }

    /**
     * HOPE 会话查询
     * 使用 HOPE 三层知识架构进行智能问答
     *
     * @param request HOPE 查询请求
     * @return 查询结果
     */
    @PostMapping("/hope")
    public Map<String, Object> hopeQuery(@RequestBody HOPEQueryRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            String question = request.getQuestion();
            String sessionId = request.getSessionId();

            log.info("HOPE 查询: question={}, session={}", question, sessionId);

            // TODO: 实现 HOPE 查询逻辑
            // String hopeAnswer = hopeManager.query(question, sessionId);

            // 临时实现：使用 RAG
            var documents_temp = ragService.semanticSearch(question, 5);
            List<SearchResult> references = documents_temp.stream().map(SearchResult::fromDocument).toList();
            String context = ContextBuilder.buildContext(references);
            String prompt = String.format(
                    "【HOPE 智能问答】基于以下知识回答问题：\n\n%s\n\n问题：%s",
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
            log.error("HOPE 查询失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 获取相似问题
     *
     * @param question 问题
     * @param topK     返回数量
     * @return 相似问题列表
     */
    @GetMapping("/similar")
    public Map<String, Object> getSimilarQuestions(
            @RequestParam String question,
            @RequestParam(defaultValue = "5") int topK) {

        Map<String, Object> result = new HashMap<>();

        try {
            var documents_temp = ragService.semanticSearch(question, topK);
            List<SearchResult> searchResults = documents_temp.stream().map(SearchResult::fromDocument).toList();

            result.put("status", "success");
            result.put("question", question);
            result.put("similarCount", searchResults.size());
            result.put("similar", searchResults);
            log.info("✅ 获取相似问题完成: question={}, count={}", question, searchResults.size());
        } catch (Exception e) {
            log.error("❌ 获取相似问题失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    // ========== 私有辅助方法 ==========

    /**
     * 构建提示词
     */
    private String buildPrompt(String question, String knowledgeMode, String roleName) {
        if ("none".equals(knowledgeMode)) {
            return question;
        } else if ("role".equals(knowledgeMode) && roleName != null) {
            Role role = roleService.getRole(roleName);
            var documents_temp = ragService.semanticSearch(question, 5);
            List<SearchResult> references = documents_temp.stream().map(SearchResult::fromDocument).toList();
            String context = ContextBuilder.buildRoleContext(references);
            return String.format(
                    "你是%s，%s\n\n基于以下知识回答问题：\n\n%s\n\n问题：%s",
                    role.getName(), role.getDescription(), context, question
            );
        } else {
            var documents_temp = ragService.semanticSearch(question, 5);
            List<SearchResult> references = documents_temp.stream().map(SearchResult::fromDocument).toList();
            String context = ContextBuilder.buildContext(references);
            return String.format("基于以下知识回答问题：\n\n%s\n\n问题：%s", context, question);
        }
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






