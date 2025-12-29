package top.yumbo.ai.omni.web.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.knowledge.registry.qa.model.IntelligentQARequest;
import top.yumbo.ai.omni.knowledge.registry.qa.model.IntelligentQAResponse;
import top.yumbo.ai.omni.knowledge.registry.qa.service.IntelligentQAService;
import top.yumbo.ai.omni.knowledge.registry.qa.service.QAOrchestrationService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 异步流式问答服务
 *
 * <p>使用 Spring 异步支持和响应式编程，提升性能</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class AsyncStreamQAService {

    @Autowired
    private AIService aiService;

    @Autowired(required = false)
    private IntelligentQAService intelligentQAService;

    @Autowired
    private QAOrchestrationService orchestrationService;

    /**
     * 异步处理智能问答流式响应
     */
    @Async("qaTaskExecutor")
    public void processIntelligentStream(
            String question,
            String conversationId,
            String userId,
            SseEmitter emitter) {

        try {
            if (intelligentQAService == null) {
                processSimpleStream(question, emitter);
                return;
            }

            // 1. 构建请求
            IntelligentQARequest qaRequest = IntelligentQARequest.builder()
                    .question(question)
                    .conversationId(conversationId)
                    .userId(userId != null ? userId : "anonymous")
                    .build();

            // 2. 执行智能问答（非流式部分）
            IntelligentQAResponse qaResponse = intelligentQAService.ask(qaRequest);

            // 3. 发送元数据
            sendMetadata(qaResponse, emitter);

            // 4. 处理需要更多信息的情况
            if (qaResponse.getNeedsMoreInfo()) {
                streamRequestForInfo(question, qaResponse.getAnswer(), emitter);
                return;
            }

            // 5. 流式生成完整答案
            String enhancedPrompt = orchestrationService.buildEnhancedPrompt(question, qaResponse);
            streamAnswer(enhancedPrompt, emitter);

        } catch (Exception e) {
            log.error("智能问答流式处理失败", e);
            handleStreamError(emitter, e);
        }
    }

    /**
     * 异步处理普通流式响应
     */
    @Async("qaTaskExecutor")
    public void processSimpleStream(String prompt, SseEmitter emitter) {
        try {
            streamAnswer(prompt, emitter);
        } catch (Exception e) {
            log.error("流式处理失败", e);
            handleStreamError(emitter, e);
        }
    }

    /**
     * 发送元数据
     */
    private void sendMetadata(IntelligentQAResponse qaResponse, SseEmitter emitter) {
        try {
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("type", "metadata");
            metadata.put("conversationId", qaResponse.getConversationId());
            metadata.put("needsMoreInfo", qaResponse.getNeedsMoreInfo());

            if (qaResponse.getIntent() != null) {
                metadata.put("intent", qaResponse.getIntent().getIntent());
                metadata.put("confidence", qaResponse.getIntent().getConfidence());
            }

            emitter.send(SseEmitter.event().name("metadata").data(metadata));
        } catch (Exception e) {
            log.error("发送元数据失败", e);
        }
    }

    /**
     * 流式发送请求更多信息
     */
    private void streamRequestForInfo(String question, String answer, SseEmitter emitter) {
        String requestPrompt = String.format(
                "用户提问：%s\n\n需要更多信息才能回答。请礼貌地向用户说明需要以下信息，并逐条询问：\n%s",
                question, answer
        );

        streamAnswer(requestPrompt, emitter);
    }

    /**
     * 流式发送答案（核心流式逻辑）
     */
    private void streamAnswer(String prompt, SseEmitter emitter) {
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
                    handleStreamError(emitter, e);
                })
                .subscribe();
    }

    /**
     * 处理流式错误
     */
    private void handleStreamError(SseEmitter emitter, Throwable e) {
        try {
            emitter.send(SseEmitter.event().data("[ERROR] " + e.getMessage()));
            emitter.completeWithError(e);
        } catch (Exception ex) {
            log.error("❌ 发送错误消息失败: {}", ex.getMessage());
        }
    }
}


