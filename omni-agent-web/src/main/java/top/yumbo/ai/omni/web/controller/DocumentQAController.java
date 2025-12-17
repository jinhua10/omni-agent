package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import top.yumbo.ai.omni.web.service.DocumentQAService;

/**
 * 完整文档AI问答控制器
 * (Document Q&A Controller)
 *
 * <p>对完整文档进行AI问答</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/document-qa")
@RequiredArgsConstructor
public class DocumentQAController {

    private final DocumentQAService documentQAService;

    /**
     * 对文档进行AI问答
     * POST /api/document-qa/query
     */
    @PostMapping("/query")
    public ResponseEntity<DocumentQAService.DocumentQAReport> queryDocument(
            @RequestBody DocumentQARequest request) {

        try {
            log.info("收到文档问答请求: documentId={}, question={}",
                request.getDocumentId(), request.getQuestion());

            DocumentQAService.DocumentQAReport report = documentQAService.queryDocument(
                request.getDocumentId(),
                request.getQuestion()
            );

            if (report.isSuccess()) {
                return ResponseEntity.ok(report);
            } else {
                return ResponseEntity.internalServerError().body(report);
            }

        } catch (Exception e) {
            log.error("文档问答失败", e);

            DocumentQAService.DocumentQAReport errorReport = new DocumentQAService.DocumentQAReport();
            errorReport.setSuccess(false);
            errorReport.setErrorMessage(e.getMessage());

            return ResponseEntity.internalServerError().body(errorReport);
        }
    }

    /**
     * 流式文档问答
     * GET /api/document-qa/query/stream
     */
    @GetMapping(value = "/query/stream", produces = "text/event-stream")
    public SseEmitter queryDocumentStream(
            @RequestParam String documentId,
            @RequestParam String question) {

        log.info("收到流式文档问答请求: documentId={}, question={}", documentId, question);

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
            try {
                documentQAService.queryDocumentStream(documentId, question)
                        .doOnNext(token -> {
                            try {
                                emitter.send(SseEmitter.event()
                                        .data(token));
                                log.debug("📤 发送 token: [{}]", token);
                            } catch (Exception e) {
                                log.error("❌ 发送 token 失败: {}", e.getMessage());
                                emitter.completeWithError(e);
                            }
                        })
                        .doOnComplete(() -> {
                            log.info("✅ 流式文档问答完成");
                            emitter.complete();
                        })
                        .doOnError(e -> {
                            log.error("❌ 流式文档问答失败: {}", e.getMessage());
                            try {
                                emitter.send(SseEmitter.event()
                                        .data("[ERROR] " + e.getMessage()));
                            } catch (Exception ex) {
                                log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                            }
                            emitter.completeWithError(e);
                        })
                        .subscribe();
            } catch (Exception e) {
                log.error("❌ 流式文档问答初始化失败", e);
                try {
                    emitter.send(SseEmitter.event()
                            .data("[ERROR] " + e.getMessage()));
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
     * 流式文档问答（POST方式）
     * POST /api/document-qa/query/stream
     */
    @PostMapping(value = "/query/stream", produces = "text/event-stream")
    public SseEmitter queryDocumentStreamPost(
            @RequestBody DocumentQARequest request) {

        log.info("收到流式文档问答请求(POST): documentId={}, question={}",
                request.getDocumentId(), request.getQuestion());

        SseEmitter emitter = new SseEmitter(300000L);

        new Thread(() -> {
            try {
                documentQAService.queryDocumentStream(
                        request.getDocumentId(),
                        request.getQuestion()
                )
                        .doOnNext(token -> {
                            try {
                                emitter.send(SseEmitter.event()
                                        .data(token));
                                log.debug("📤 发送 token: [{}]", token);
                            } catch (Exception e) {
                                log.error("❌ 发送 token 失败: {}", e.getMessage());
                                emitter.completeWithError(e);
                            }
                        })
                        .doOnComplete(() -> {
                            log.info("✅ 流式文档问答完成");
                            emitter.complete();
                        })
                        .doOnError(e -> {
                            log.error("❌ 流式文档问答失败: {}", e.getMessage());
                            try {
                                emitter.send(SseEmitter.event()
                                        .data("[ERROR] " + e.getMessage()));
                            } catch (Exception ex) {
                                log.error("❌ 发送错误消息失败: {}", ex.getMessage());
                            }
                            emitter.completeWithError(e);
                        })
                        .subscribe();
            } catch (Exception e) {
                log.error("❌ 流式文档问答初始化失败", e);
                try {
                    emitter.send(SseEmitter.event()
                            .data("[ERROR] " + e.getMessage()));
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

    // ========== DTO 类 ==========

    @Data
    public static class DocumentQARequest {
        private String documentId;
        private String question;
    }
}

