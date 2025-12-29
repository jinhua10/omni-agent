package top.yumbo.ai.omni.ai.archive;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.ai.api.model.AIResponse;
import top.yumbo.ai.omni.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.ai.archive.model.AICallArchive;

import java.util.List;

/**
 * AI调用归档切面
 *
 * <p>自动拦截AIService的所有调用，异步归档调用记录</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Aspect
@Component
@RequiredArgsConstructor
@ConditionalOnProperty(name = "omni-agent.ai.archive.enabled", havingValue = "true", matchIfMissing = true)
public class AICallArchiveAspect {

    private final AICallArchiveService archiveService;

    /**
     * 拦截AIService的所有方法
     */
    @Pointcut("execution(* top.yumbo.ai.ai.api.AIService.*(..))")
    public void aiServiceMethods() {}

    /**
     * 拦截具体实现类的方法
     */
    @Pointcut("execution(* top.yumbo.ai..*.*.chat*(..)) || " +
              "execution(* top.yumbo.ai..*.*.generate*(..)) || " +
              "execution(* top.yumbo.ai..*.*.chatWithVision*(..))")
    public void aiImplementationMethods() {}

    @Around("aiServiceMethods() || aiImplementationMethods()")
    public Object aroundAICall(ProceedingJoinPoint joinPoint) throws Throwable {
        // 获取方法信息
        String methodName = joinPoint.getSignature().getName();
        Object target = joinPoint.getTarget();
        Object[] args = joinPoint.getArgs();

        // 跳过某些方法
        if (shouldSkip(methodName)) {
            return joinPoint.proceed();
        }

        long startTime = System.currentTimeMillis();

        try {
            // 判断是否是流式方法
            boolean isStream = isStreamMethod(methodName);

            // 执行原方法
            Object result = joinPoint.proceed();

            // 处理归档
            if (isStream) {
                return handleStreamResult(result, target, methodName, args, startTime);
            } else {
                handleNonStreamResult(result, target, methodName, args, startTime);
                return result;
            }

        } catch (Throwable throwable) {
            // 记录失败的调用
            handleFailedCall(throwable, target, methodName, args, startTime);
            throw throwable;
        }
    }

    /**
     * 处理流式调用结果
     */
    private Object handleStreamResult(Object result, Object target, String methodName, Object[] args, long startTime) {
        if (result instanceof Flux) {
            Flux<?> flux = (Flux<?>) result;

            // 创建基础归档数据
            ArchiveContext context = new ArchiveContext(target, methodName, args, startTime, true);

            // 判断Flux类型
            if (methodName.contains("Response")) {
                // Flux<AIResponse>
                return handleAIResponseFlux(flux, context);
            } else {
                // Flux<String>
                return handleStringFlux(flux, context);
            }
        }

        return result;
    }

    /**
     * 处理 Flux&lt;String&gt; 流
     */
    private Flux<String> handleStringFlux(Flux<?> flux, ArchiveContext context) {
        @SuppressWarnings("unchecked")
        Flux<String> stringFlux = (Flux<String>) flux;

        StringBuilder fullResponse = new StringBuilder();

        return stringFlux
                .doOnNext(chunk -> fullResponse.append(chunk))
                .doOnComplete(() -> {
                    // 流完成后异步归档
                    AICallArchive archive = context.buildArchive(
                            fullResponse.toString(),
                            true,
                            null,
                            null,
                            null,
                            System.currentTimeMillis() - context.startTime
                    );

                    archiveService.archiveAsync(archive)
                            .subscribe(
                                    id -> log.debug("✅ 流式调用已归档: {}", id),
                                    error -> log.error("❌ 流式调用归档失败", error)
                            );
                })
                .doOnError(error -> {
                    // 流失败时也归档
                    AICallArchive archive = context.buildArchive(
                            fullResponse.toString(),
                            false,
                            error.getMessage(),
                            null,
                            null,
                            System.currentTimeMillis() - context.startTime
                    );

                    archiveService.archiveAsync(archive).subscribe();
                });
    }

    /**
     * 处理 Flux&lt;AIResponse&gt; 流
     */
    private Flux<?> handleAIResponseFlux(Flux<?> flux, ArchiveContext context) {
        @SuppressWarnings("unchecked")
        Flux<AIResponse> responseFlux = (Flux<AIResponse>) flux;

        final AIResponse[] lastResponse = new AIResponse[1];

        return responseFlux
                .doOnNext(response -> lastResponse[0] = response)
                .doOnComplete(() -> {
                    if (lastResponse[0] != null) {
                        AIResponse resp = lastResponse[0];
                        AICallArchive archive = context.buildArchive(
                                resp.getText(),
                                resp.isSuccess(),
                                resp.getError(),
                                resp.getModel(),
                                convertTokenUsage(resp),
                                System.currentTimeMillis() - context.startTime
                        );

                        archiveService.archiveAsync(archive).subscribe();
                    }
                })
                .doOnError(error -> {
                    AICallArchive archive = context.buildArchive(
                            lastResponse[0] != null ? lastResponse[0].getText() : "",
                            false,
                            error.getMessage(),
                            null,
                            null,
                            System.currentTimeMillis() - context.startTime
                    );

                    archiveService.archiveAsync(archive).subscribe();
                });
    }

    /**
     * 处理非流式调用结果
     */
    private void handleNonStreamResult(Object result, Object target, String methodName, Object[] args, long startTime) {
        long durationMs = System.currentTimeMillis() - startTime;

        ArchiveContext context = new ArchiveContext(target, methodName, args, startTime, false);

        // 解析响应
        String responseText = null;
        Boolean success = true;
        String error = null;
        String model = null;
        AICallArchive.TokenUsage tokenUsage = null;

        if (result instanceof AIResponse) {
            AIResponse response = (AIResponse) result;
            responseText = response.getText();
            success = response.isSuccess();
            error = response.getError();
            model = response.getModel();
            tokenUsage = convertTokenUsage(response);
        } else if (result instanceof String) {
            responseText = (String) result;
        }

        // 异步归档
        AICallArchive archive = context.buildArchive(responseText, success, error, model, tokenUsage, durationMs);
        archiveService.archiveAsync(archive)
                .subscribe(
                        id -> log.debug("✅ AI调用已归档: {}", id),
                        error2 -> log.error("❌ AI调用归档失败", error2)
                );
    }

    /**
     * 处理失败的调用
     */
    private void handleFailedCall(Throwable throwable, Object target, String methodName, Object[] args, long startTime) {
        long durationMs = System.currentTimeMillis() - startTime;

        ArchiveContext context = new ArchiveContext(target, methodName, args, startTime, isStreamMethod(methodName));
        AICallArchive archive = context.buildArchive(null, false, throwable.getMessage(), null, null, durationMs);

        archiveService.archiveAsync(archive).subscribe();
    }

    /**
     * 归档上下文（内部辅助类，避免在方法签名中暴露 Builder）
     */
    private class ArchiveContext {
        final Object target;
        final String methodName;
        final Object[] args;
        final long startTime;
        final boolean isStream;
        final String serviceType;
        final AICallArchive.CallType callType;
        final String model;
        final String systemPrompt;
        final String userInput;
        final List<ChatMessage> messages;

        ArchiveContext(Object target, String methodName, Object[] args, long startTime, boolean isStream) {
            this.target = target;
            this.methodName = methodName;
            this.args = args;
            this.startTime = startTime;
            this.isStream = isStream;
            this.serviceType = getServiceType(target);
            this.callType = getCallType(methodName);

            // 解析参数
            ParsedArguments parsed = parseMethodArguments(args, target);
            this.model = parsed.model;
            this.systemPrompt = parsed.systemPrompt;
            this.userInput = parsed.userInput;
            this.messages = parsed.messages;
        }

        AICallArchive buildArchive(String responseText, Boolean success, String error,
                                   String modelOverride, AICallArchive.TokenUsage tokenUsage, long durationMs) {
            return AICallArchive.builder()
                    .archiveId(AICallArchive.generateArchiveId())
                    .timestamp(startTime)
                    .callTime(AICallArchive.getCurrentTimeISO())
                    .serviceType(serviceType)
                    .callType(callType)
                    .source("aop")
                    .isStream(isStream)
                    .model(modelOverride != null ? modelOverride : model)
                    .systemPrompt(systemPrompt)
                    .userInput(userInput)
                    .messages(messages)
                    .responseText(responseText)
                    .success(success)
                    .error(error)
                    .tokenUsage(tokenUsage)
                    .durationMs(durationMs)
                    .build();
        }
    }

    /**
     * 解析后的参数（内部辅助类）
     */
    private static class ParsedArguments {
        String model;
        String systemPrompt;
        String userInput;
        List<ChatMessage> messages;
    }

    /**
     * 解析方法参数
     */
    @SuppressWarnings("unchecked")
    private ParsedArguments parseMethodArguments(Object[] args, Object target) {
        ParsedArguments result = new ParsedArguments();

        if (args == null || args.length == 0) {
            return result;
        }

        // 获取当前模型
        if (target instanceof AIService) {
            try {
                result.model = ((AIService) target).getCurrentModel();
            } catch (Exception e) {
                // ignore
            }
        }

        // 解析参数
        for (Object arg : args) {
            if (arg instanceof String) {
                // 系统提示或用户输入
                if (result.systemPrompt == null) {
                    result.systemPrompt = (String) arg;
                } else if (result.userInput == null) {
                    result.userInput = (String) arg;
                }
            } else if (arg instanceof List) {
                List<?> list = (List<?>) arg;
                if (!list.isEmpty() && list.get(0) instanceof ChatMessage) {
                    result.messages = (List<ChatMessage>) list;
                }
            }
        }

        return result;
    }

    /**
     * 获取服务类型
     */
    private String getServiceType(Object target) {
        String className = target.getClass().getSimpleName();
        if (className.contains("Ollama")) return "ollama";
        if (className.contains("Online")) return "online-api";
        if (className.contains("Onnx") || className.contains("ONNX")) return "onnx";
        return "unknown";
    }

    /**
     * 获取调用类型
     */
    private AICallArchive.CallType getCallType(String methodName) {
        if (methodName.contains("Vision") || methodName.contains("vision")) {
            return AICallArchive.CallType.VISION;
        }
        if (methodName.contains("chat") || methodName.contains("Chat")) {
            return AICallArchive.CallType.CHAT;
        }
        if (methodName.contains("generate") || methodName.contains("Generate")) {
            return AICallArchive.CallType.GENERATE;
        }
        if (methodName.contains("embed") || methodName.contains("Embed")) {
            return AICallArchive.CallType.EMBEDDING;
        }
        return AICallArchive.CallType.OTHER;
    }

    /**
     * 判断是否是流式方法
     */
    private boolean isStreamMethod(String methodName) {
        return methodName.contains("Stream") ||
               methodName.contains("stream") ||
               methodName.contains("Flux") ||
               methodName.contains("flux");
    }

    /**
     * 判断是否应该跳过
     */
    private boolean shouldSkip(String methodName) {
        // 跳过某些工具方法
        return methodName.equals("getCurrentModel") ||
               methodName.equals("setCurrentModel") ||
               methodName.equals("isHealthy") ||
               methodName.equals("getAvailableModels");
    }

    /**
     * 转换Token使用统计
     */
    private AICallArchive.TokenUsage convertTokenUsage(AIResponse response) {
        if (response.getPromptTokens() == null && response.getCompletionTokens() == null) {
            return null;
        }

        return AICallArchive.TokenUsage.builder()
                .promptTokens(response.getPromptTokens())
                .completionTokens(response.getCompletionTokens())
                .totalTokens(response.getTotalTokens())
                .build();
    }
}

