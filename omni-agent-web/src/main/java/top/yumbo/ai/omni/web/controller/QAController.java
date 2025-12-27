package top.yumbo.ai.omni.web.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.core.qa.model.IntelligentQARequest;
import top.yumbo.ai.omni.core.qa.model.IntelligentQAResponse;
import top.yumbo.ai.omni.core.qa.service.IntelligentQAService;
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
 *   <li>intelligent/none - 智能问答模式（Phase 3）
 *       <ul>
 *         <li>自动意图分析</li>
 *         <li>智能知识检索</li>
 *         <li>知识缺口检测</li>
 *         <li>交互式学习</li>
 *         <li>多轮对话支持</li>
 *       </ul>
 *   </li>
 *   <li>rag - 传统 RAG 检索回答</li>
 *   <li>role - 角色知识库回答</li>
 * </ul>
 *
 * <h3>使用示例</h3>
 * <pre>
 * // 智能问答模式（推荐）
 * POST /api/qa/ask
 * {
 *   "question": "如何实现用户认证？",
 *   "knowledgeMode": "intelligent",
 *   "userId": "user123",
 *   "hopeSessionId": "session-uuid"  // 用于多轮对话
 * }
 *
 * // 流式智能问答
 * GET /api/qa/ask/stream?question=如何实现用户认证&knowledgeMode=intelligent&conversationId=xxx
 * </pre>
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

    @Autowired(required = false)
    private IntelligentQAService intelligentQAService;

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
            Map<String, Object> intentAnalysis = null;

            switch (knowledgeMode.toLowerCase()) {
                case "intelligent":
                case "none":
                    // 智能问答模式（Phase 3）- 替代原 none 模式
                    if (intelligentQAService != null) {
                        try {
                            IntelligentQARequest qaRequest = IntelligentQARequest.builder()
                                    .question(question)
                                    .conversationId(hopeSessionId) // 使用 hopeSessionId 作为对话ID
                                    .userId(request.getUserId() != null ? request.getUserId() : "anonymous")
                                    .build();

                            IntelligentQAResponse qaResponse = intelligentQAService.ask(qaRequest);

                            answer = qaResponse.getAnswer();
                            if (qaResponse.getReferences() != null && !qaResponse.getReferences().isEmpty()) {
                                references = qaResponse.getReferences().stream()
                                        .map(SearchResult::fromDocument)
                                        .toList();
                            }

                            // 添加智能问答特有的信息
                            result.put("conversationId", qaResponse.getConversationId());
                            result.put("hasKnowledge", qaResponse.getHasKnowledge());
                            result.put("knowledgeSufficient", qaResponse.getKnowledgeSufficient());
                            result.put("needsMoreInfo", qaResponse.getNeedsMoreInfo());

                            // 意图分析信息
                            if (qaResponse.getIntent() != null) {
                                intentAnalysis = new HashMap<>();
                                intentAnalysis.put("intent", qaResponse.getIntent().getIntent());
                                intentAnalysis.put("entities", qaResponse.getIntent().getEntities());
                                intentAnalysis.put("techStack", qaResponse.getIntent().getTechStack());
                                intentAnalysis.put("missingInfo", qaResponse.getIntent().getMissingInfo());
                                intentAnalysis.put("confidence", qaResponse.getIntent().getConfidence());
                            }

                            log.info("✅ 使用智能问答模式");
                            break;
                        } catch (Exception e) {
                            log.warn("智能问答失败，降级到直接 AI 模式: {}", e.getMessage());
                            // 降级到直接 AI
                            answer = aiService.chat(question);
                            break;
                        }
                    } else {
                        // 智能问答服务不可用，使用直接 AI
                        log.info("智能问答服务未启用，使用直接 AI 模式");
                        answer = aiService.chat(question);
                    }
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

            if (intentAnalysis != null) {
                result.put("intentAnalysis", intentAnalysis);
            }

            if (hopeSessionId != null && !hopeSessionId.isEmpty()) {
                result.put("hopeSessionId", hopeSessionId);
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
     * @param conversationId 对话ID（intelligent 模式时使用）
     * @param userId        用户ID（intelligent 模式时使用）
     * @return SSE 流
     */
    @GetMapping(value = "/ask/stream", produces = "text/event-stream")
    public SseEmitter askStream(
            @RequestParam String question,
            @RequestParam(defaultValue = "rag") String knowledgeMode,
            @RequestParam(required = false) String roleName,
            @RequestParam(required = false) String conversationId,
            @RequestParam(required = false) String userId) {

        log.info("流式问答: question={}, mode={}, role={}, conversationId={}",
                question, knowledgeMode, roleName, conversationId);

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
            try {
                String prompt;

                // 如果是智能模式，先进行意图分析和知识检索
                if (("intelligent".equals(knowledgeMode) || "none".equals(knowledgeMode))
                        && intelligentQAService != null) {
                    try {
                        // 使用智能问答服务构建更好的提示词
                        IntelligentQARequest qaRequest = IntelligentQARequest.builder()
                                .question(question)
                                .conversationId(conversationId)
                                .userId(userId != null ? userId : "anonymous")
                                .build();

                        // 调用智能问答获取增强的提示词（非流式部分）
                        IntelligentQAResponse qaResponse = intelligentQAService.ask(qaRequest);

                        // 发送元数据事件（意图分析结果）
                        Map<String, Object> metadata = new HashMap<>();
                        metadata.put("type", "metadata");
                        metadata.put("conversationId", qaResponse.getConversationId());
                        metadata.put("needsMoreInfo", qaResponse.getNeedsMoreInfo());
                        if (qaResponse.getIntent() != null) {
                            metadata.put("intent", qaResponse.getIntent().getIntent());
                            metadata.put("confidence", qaResponse.getIntent().getConfidence());
                        }
                        emitter.send(SseEmitter.event()
                                .name("metadata")
                                .data(metadata));

                        // 如果需要更多信息，使用 AI 流式发送问题
                        if (qaResponse.getNeedsMoreInfo()) {
                            // 构建请求更多信息的提示词，让 AI 流式输出
                            String requestPrompt = String.format(
                                    "用户提问：%s\n\n" +
                                    "需要更多信息才能回答。请礼貌地向用户说明需要以下信息，并逐条询问：\n%s",
                                    question,
                                    qaResponse.getAnswer()
                            );

                            List<ChatMessage> requestMessages = List.of(
                                    ChatMessage.builder()
                                            .role("user")
                                            .content(requestPrompt)
                                            .build()
                            );

                            // 使用 AI 流式输出请求
                            aiService.chatFlux(requestMessages)
                                    .doOnNext(token -> {
                                        try {
                                            emitter.send(SseEmitter.event().data(token));
                                        } catch (Exception e) {
                                            log.error("❌ 发送请求信息失败: {}", e.getMessage());
                                            emitter.completeWithError(e);
                                        }
                                    })
                                    .doOnComplete(() -> {
                                        log.info("✅ 请求更多信息发送完成");
                                        emitter.complete();
                                    })
                                    .doOnError(e -> {
                                        log.error("❌ 发送请求失败: {}", e.getMessage());
                                        emitter.completeWithError(e);
                                    })
                                    .subscribe();
                            return;
                        }

                        // 使用智能问答构建的增强提示词进行流式生成
                        // 这里重新构建一个完整的提示词，包含知识检索结果
                        StringBuilder enhancedPrompt = new StringBuilder();
                        enhancedPrompt.append("用户问题：").append(question).append("\n\n");

                        if (qaResponse.getIntent() != null && qaResponse.getIntent().getIntent() != null) {
                            enhancedPrompt.append("意图分析：").append(qaResponse.getIntent().getIntent()).append("\n\n");
                        }

                        if (qaResponse.getReferences() != null && !qaResponse.getReferences().isEmpty()) {
                            enhancedPrompt.append("知识库相关内容：\n");
                            int index = 1;
                            for (var doc : qaResponse.getReferences()) {
                                enhancedPrompt.append("\n【知识").append(index++).append("】\n");
                                enhancedPrompt.append(doc.getContent()).append("\n");
                            }
                            enhancedPrompt.append("\n基于以上知识，请详细回答用户的问题。");
                        } else {
                            enhancedPrompt.append("请基于你的知识回答用户的问题。");
                        }

                        prompt = enhancedPrompt.toString();

                        log.info("✅ 使用智能问答模式（流式）");
                    } catch (Exception e) {
                        log.warn("智能问答失败，降级到普通模式: {}", e.getMessage());
                        prompt = buildPrompt(question, "rag", roleName);
                    }
                } else {
                    // 其他模式使用原有逻辑
                    prompt = buildPrompt(question, knowledgeMode, roleName);
                }

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






