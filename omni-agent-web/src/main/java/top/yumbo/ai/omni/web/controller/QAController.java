package top.yumbo.ai.omni.web.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.knowledge.registry.qa.service.QAOrchestrationService;
import top.yumbo.ai.omni.knowledge.registry.qa.service.QAOrchestrationService.QARequest;
import top.yumbo.ai.omni.knowledge.registry.qa.service.QAOrchestrationService.QAResult;
import top.yumbo.ai.omni.web.service.AsyncStreamQAService;
import top.yumbo.ai.omni.knowledge.registry.qa.service.IntelligentQAService;
import top.yumbo.ai.omni.rag.model.ContextBuilder;
import top.yumbo.ai.omni.web.dto.ApiDtos.*;
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

    @Autowired(required = false)
    private IntelligentQAService intelligentQAService;

    @Autowired
    private QAOrchestrationService orchestrationService;

    @Autowired
    private AsyncStreamQAService asyncStreamQAService;

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

            log.info("收到问答请求: question={}, mode={}, session={}",
                    question, knowledgeMode, request.getHopeSessionId());

            // 使用编排服务执行问答
            QARequest qaRequest = QARequest.of(
                    question,
                    knowledgeMode,
                    request.getRoleName(),
                    request.getHopeSessionId(),
                    request.getUserId()
            );

            QAResult qaResult = orchestrationService.executeQA(qaRequest);

            // 构建响应
            if (!qaResult.isSuccess()) {
                result.put("status", "error");
                result.put("error", qaResult.getError());
                return result;
            }

            result.put("status", "success");
            result.put("question", question);
            result.put("answer", qaResult.getAnswer());
            result.put("knowledgeMode", knowledgeMode);
            result.put("model", aiService.getCurrentModel());

            // 添加可选信息
            if (qaResult.getReferences() != null && !qaResult.getReferences().isEmpty()) {
                List<SearchResult> references = qaResult.getReferences().stream()
                        .map(SearchResult::fromDocument)
                        .toList();
                result.put("referenceCount", references.size());
                result.put("references", references);
            }

            if (qaResult.getIntentAnalysis() != null) {
                result.put("intentAnalysis", qaResult.getIntentAnalysis());
            }

            if (qaResult.getConversationId() != null) {
                result.put("conversationId", qaResult.getConversationId());
            }

            if (qaResult.getHasKnowledge() != null) {
                result.put("hasKnowledge", qaResult.getHasKnowledge());
                result.put("knowledgeSufficient", qaResult.getKnowledgeSufficient());
                result.put("needsMoreInfo", qaResult.getNeedsMoreInfo());
            }

            if (request.getHopeSessionId() != null) {
                result.put("hopeSessionId", request.getHopeSessionId());
            }

        } catch (Exception e) {
            log.error("问答失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 流式问答（优化版：使用异步线程池）
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

        log.info("流式问答: question={}, mode={}, conversationId={}",
                question, knowledgeMode, conversationId);

        SseEmitter emitter = new SseEmitter(300000L);
        setupEmitterCallbacks(emitter);

        // 使用异步服务处理流式响应
        if ("intelligent".equals(knowledgeMode) || "none".equals(knowledgeMode)) {
            asyncStreamQAService.processIntelligentStream(question, conversationId, userId, emitter);
        } else {
            String prompt = orchestrationService.buildPrompt(question, knowledgeMode, roleName);
            asyncStreamQAService.processSimpleStream(prompt, emitter);
        }

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
}






