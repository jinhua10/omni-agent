package top.yumbo.ai.omni.web.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import top.yumbo.ai.ai.api.AIService;
import top.yumbo.ai.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.core.hope.HOPEKnowledgeManager;
import top.yumbo.ai.omni.core.role.Role;
import top.yumbo.ai.omni.core.role.RoleService;
import top.yumbo.ai.omni.marketplace.EnhancedQueryService;
import top.yumbo.ai.omni.web.controller.SystemController;
import top.yumbo.ai.omni.web.util.ContextBuilder;
import top.yumbo.ai.omni.web.util.JsonUtil;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.SearchResult;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * 高级问答控制器
 *
 * <p>提供高级问答功能：</p>
 * <ul>
 *   <li>双轨流式问答 - 并行展示传统RAG和HOPE智能系统的回答</li>
 *   <li>角色双轨问答 - 对比传统RAG和角色专业回答</li>
 *   <li>单轨LLM问答 - 纯LLM回答</li>
 * </ul>
 *
 * <p><b>双轨架构说明：</b></p>
 * <ul>
 *   <li><b>左轨</b>：传统 RAG + LLM 回答</li>
 *   <li><b>右轨</b>：HOPE智能系统 / 角色专业回答</li>
 *   <li><b>并行执行</b>：两个轨道同时生成，实时流式输出</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/qa/advanced")
@RequiredArgsConstructor
public class AdvancedQAController {

    private final AIService aiService;
    private final RAGService ragService;
    private final RoleService roleService;
    private final HOPEKnowledgeManager hopeManager;
    private final EnhancedQueryService enhancedQueryService;
    private final SystemController systemController;

    /**
     * 线程池（用于双轨并行处理）
     */
    private final ExecutorService executorService = Executors.newFixedThreadPool(10);

    /**
     * 双轨流式问答
     *
     * <p>支持三种模式：</p>
     * <ul>
     *   <li>none - 单轨LLM模式</li>
     *   <li>rag - 双轨RAG模式（左轨：RAG+LLM，右轨：HOPE智能系统）</li>
     *   <li>role - 双轨角色模式（左轨：RAG+LLM，右轨：角色专业回答）</li>
     * </ul>
     *
     * @param question      问题
     * @param userId        用户ID
     * @param knowledgeMode 知识模式
     * @param roleName      角色名称（role模式时需要）
     * @return SSE流
     */
    @GetMapping(value = "/dual-track/stream", produces = "text/event-stream")
    public SseEmitter dualTrackStream(
            @RequestParam String question,
            @RequestParam String userId,
            @RequestParam(defaultValue = "none") String knowledgeMode,
            @RequestParam(required = false) String roleName) {

        log.info("🚂 双轨流式问答: question={}, userId={}, mode={}, role={}",
                question, userId, knowledgeMode, roleName);

        SseEmitter emitter = new SseEmitter(300000L);
        StringBuilder fullAnswerBuilder = new StringBuilder();

        executorService.submit(() -> {
            try {
                boolean isDualTrack = !"none".equals(knowledgeMode);

                if (!isDualTrack) {
                    // 单轨模式
                    handleSingleTrack(emitter, question, fullAnswerBuilder);
                } else {
                    // 双轨模式
                    List<SearchResult> references = ragService.searchByText(question, 5);
                    log.info("📚 检索到 {} 个参考文档", references.size());

                    sendReferences(emitter, references);

                    if ("role".equals(knowledgeMode)) {
                        handleRoleMode(emitter, question, roleName, references, fullAnswerBuilder);
                    } else {
                        handleRagMode(emitter, question, references, fullAnswerBuilder);
                    }
                }

                // 保存对话历史
                systemController.saveConversationHistory(userId, question, fullAnswerBuilder.toString());

            } catch (Exception e) {
                log.error("❌ 双轨流式问答失败", e);
                sendError(emitter, e.getMessage());
            }
        });

        setupEmitterCallbacks(emitter);
        return emitter;
    }

    // ========== 私有方法 ==========

    /**
     * 处理单轨模式（仅LLM）
     */
    private void handleSingleTrack(SseEmitter emitter, String question, StringBuilder fullAnswerBuilder) {
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
                        fullAnswerBuilder.append(token);
                    } catch (Exception e) {
                        log.error("❌ 发送LLM token失败: {}", e.getMessage());
                    }
                })
                .doOnComplete(() -> sendComplete(emitter))
                .doOnError(e -> sendError(emitter, e.getMessage()))
                .subscribe();
    }

    /**
     * 处理RAG模式：左轨RAG+LLM，右轨HOPE智能系统（并行执行）
     */
    private void handleRagMode(SseEmitter emitter, String question, List<SearchResult> references,
                                StringBuilder fullAnswerBuilder) {
        log.info("🚂 双轨模式：RAG + HOPE智能系统（并行执行）");

        CountDownLatch bothTracksLatch = new CountDownLatch(2);
        AtomicBoolean hasError = new AtomicBoolean(false);

        // 左轨：传统RAG + LLM
        executorService.submit(() -> {
            try {
                String leftContext = ContextBuilder.buildContext(references);
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

                CountDownLatch leftLatch = new CountDownLatch(1);

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
                            leftLatch.countDown();
                        })
                        .doOnError(e -> {
                            log.error("❌ 左轨失败: {}", e.getMessage());
                            sendWarning(emitter, "left", "左轨（RAG+LLM）生成失败");
                            hasError.set(true);
                            leftLatch.countDown();
                        })
                        .subscribe();

                leftLatch.await(120, TimeUnit.SECONDS);

            } catch (Exception e) {
                log.error("❌ 左轨执行异常", e);
                hasError.set(true);
            } finally {
                bothTracksLatch.countDown();
            }
        });

        // 右轨：HOPE智能系统 + 算法市场优化
        executorService.submit(() -> {
            try {
                log.info("➡️ 启动右轨：HOPE智能系统 + 算法市场优化");

                HOPEKnowledgeManager.QueryResult hopeResult = hopeManager.smartQuery(question, null);

                List<SearchResult> enhancedReferences;
                try {
                    log.info("🔍 使用算法市场增强检索（查询扩展 + 重排序）");
                    enhancedReferences = enhancedQueryService.fullyEnhancedSearch(question, 5);
                    log.info("📈 增强检索完成：获得 {} 个优化结果", enhancedReferences.size());
                } catch (Exception e) {
                    log.warn("⚠️ 增强检索失败，使用原始检索结果: {}", e.getMessage());
                    enhancedReferences = references;
                }

                String rightPrompt = buildHOPEPrompt(question, hopeResult, enhancedReferences);

                List<ChatMessage> rightMessages = List.of(
                        ChatMessage.builder()
                                .role("user")
                                .content(rightPrompt)
                                .build()
                );

                CountDownLatch rightLatch = new CountDownLatch(1);

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
                            rightLatch.countDown();
                        })
                        .doOnError(e -> {
                            log.error("❌ 右轨失败: {}", e.getMessage());
                            sendWarning(emitter, "right", "右轨（HOPE智能系统）生成失败：" + e.getMessage());
                            hasError.set(true);
                            rightLatch.countDown();
                        })
                        .subscribe();

                rightLatch.await(120, TimeUnit.SECONDS);

            } catch (Exception e) {
                log.error("❌ 右轨执行异常", e);
                hasError.set(true);
            } finally {
                bothTracksLatch.countDown();
            }
        });

        // 等待两个轨道都完成
        try {
            bothTracksLatch.await(240, TimeUnit.SECONDS);
            log.info("✅ 双轨并行执行完成");

            if (!hasError.get()) {
                sendComplete(emitter);
            } else {
                sendError(emitter, "部分轨道执行失败");
            }
        } catch (InterruptedException e) {
            log.error("❌ 等待双轨完成超时", e);
            sendError(emitter, "双轨执行超时");
        }
    }

    /**
     * 处理角色模式：左轨RAG+LLM，右轨角色专业回答（并行执行）
     */
    private void handleRoleMode(SseEmitter emitter, String question, String roleName,
                                 List<SearchResult> references, StringBuilder fullAnswerBuilder) {
        log.info("🚂 双轨模式：RAG + 角色知识库 (role={})（并行执行）", roleName);

        Role role = roleService.getRole(roleName != null ? roleName : "default");
        log.info("🎭 使用角色: {} - {}", role.getName(), role.getDescription());

        CountDownLatch bothTracksLatch = new CountDownLatch(2);
        AtomicBoolean hasError = new AtomicBoolean(false);

        // 左轨：传统RAG + LLM
        executorService.submit(() -> {
            try {
                String leftContext = ContextBuilder.buildContext(references);
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

                CountDownLatch leftLatch = new CountDownLatch(1);

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
                            leftLatch.countDown();
                        })
                        .doOnError(e -> {
                            log.error("❌ 左轨失败: {}", e.getMessage());
                            sendWarning(emitter, "left", "左轨（RAG+LLM）生成失败");
                            hasError.set(true);
                            leftLatch.countDown();
                        })
                        .subscribe();

                leftLatch.await(120, TimeUnit.SECONDS);

            } catch (Exception e) {
                log.error("❌ 左轨执行异常", e);
                hasError.set(true);
            } finally {
                bothTracksLatch.countDown();
            }
        });

        // 右轨：角色专业回答
        executorService.submit(() -> {
            try {
                log.info("➡️ 启动右轨：角色 [{}] 专业回答", role.getName());

                String roleContext = ContextBuilder.buildRoleContext(references);
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

                CountDownLatch rightLatch = new CountDownLatch(1);

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
                            rightLatch.countDown();
                        })
                        .doOnError(e -> {
                            log.error("❌ 右轨失败: {}", e.getMessage());
                            sendWarning(emitter, "right", "右轨（角色专业回答）生成失败：" + e.getMessage());
                            hasError.set(true);
                            rightLatch.countDown();
                        })
                        .subscribe();

                rightLatch.await(120, TimeUnit.SECONDS);

            } catch (Exception e) {
                log.error("❌ 右轨执行异常", e);
                hasError.set(true);
            } finally {
                bothTracksLatch.countDown();
            }
        });

        // 等待两个轨道都完成
        try {
            bothTracksLatch.await(240, TimeUnit.SECONDS);
            log.info("✅ 双轨并行执行完成");

            if (!hasError.get()) {
                sendComplete(emitter);
            } else {
                sendError(emitter, "部分轨道执行失败");
            }
        } catch (InterruptedException e) {
            log.error("❌ 等待双轨完成超时", e);
            sendError(emitter, "双轨执行超时");
        }
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

        if (hopeResult.getAnswer() != null && !hopeResult.getAnswer().isEmpty()) {
            prompt.append("系统学习到的答案：\n");
            prompt.append(hopeResult.getAnswer()).append("\n\n");
        }

        String context = ContextBuilder.buildContext(references);
        if (!context.isEmpty()) {
            prompt.append("补充知识：\n");
            prompt.append(context).append("\n\n");
        }

        prompt.append("问题：").append(question).append("\n\n");
        prompt.append("请综合系统学习的知识和补充知识，给出专业且经过自我学习优化的回答。");

        return prompt.toString();
    }

    /**
     * 发送参考文档
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
                            JsonUtil.escapeJson(ref.getDocument().getTitle() != null ? ref.getDocument().getTitle() : ""),
                            JsonUtil.escapeJson(ref.getDocument().getContent()),
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
                JsonUtil.escapeJson(token),
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
                    track, JsonUtil.escapeJson(message)
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
                    JsonUtil.escapeJson(message)
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
}

