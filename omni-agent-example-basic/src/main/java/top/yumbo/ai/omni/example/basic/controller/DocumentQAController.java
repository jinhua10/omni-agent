package top.yumbo.ai.omni.example.basic.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.example.basic.service.DocumentQAService;

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
@CrossOrigin(origins = "*")
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
    public reactor.core.publisher.Flux<String> queryDocumentStream(
            @RequestParam String documentId,
            @RequestParam String question) {

        try {
            log.info("收到流式文档问答请求: documentId={}, question={}", documentId, question);

            return documentQAService.queryDocumentStream(documentId, question);

        } catch (Exception e) {
            log.error("流式文档问答失败", e);
            return reactor.core.publisher.Flux.just(
                "data: [ERROR] " + e.getMessage() + "\n\n"
            );
        }
    }

    /**
     * 流式文档问答（POST方式）
     * POST /api/document-qa/query/stream
     */
    @PostMapping(value = "/query/stream", produces = "text/event-stream")
    public reactor.core.publisher.Flux<String> queryDocumentStreamPost(
            @RequestBody DocumentQARequest request) {

        try {
            log.info("收到流式文档问答请求(POST): documentId={}, question={}",
                request.getDocumentId(), request.getQuestion());

            return documentQAService.queryDocumentStream(
                request.getDocumentId(),
                request.getQuestion()
            );

        } catch (Exception e) {
            log.error("流式文档问答失败", e);
            return reactor.core.publisher.Flux.just(
                "data: [ERROR] " + e.getMessage() + "\n\n"
            );
        }
    }

    // ========== DTO 类 ==========

    @Data
    public static class DocumentQARequest {
        private String documentId;
        private String question;
    }
}

