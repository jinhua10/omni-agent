package top.yumbo.ai.omni.web.controller.rag;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.common.i18n.I18N;
import top.yumbo.ai.omni.web.model.rag.ProcessingProgress;
import top.yumbo.ai.omni.web.service.rag.ProcessingProgressService;

import java.util.Map;

/**
 * 文档处理进度控制器
 * (Document Processing Progress Controller)
 *
 * <p>
 * 提供文档处理进度的 REST API
 * (Provides REST API for document processing progress)
 * </p>
 *
 * @author AI Reviewer Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@RestController
@RequestMapping("/api/rag/progress")
public class ProcessingProgressController {

    @Autowired
    private ProcessingProgressService progressService;

    /**
     * 获取文档处理进度
     * (Get document processing progress)
     *
     * @param documentId 文档ID (Document ID)
     * @param lang       语言 (Language: zh/en)
     * @return 处理进度 (Processing progress)
     */
    @GetMapping("/{documentId}")
    public ResponseEntity<Map<String, Object>> getProgress(
            @PathVariable String documentId,
            @RequestHeader(value = "Accept-Language", defaultValue = "zh") String lang) {

        log.debug(I18N.get("rag.progress.query.start", documentId));

        ProcessingProgress progress = progressService.getProgress(documentId);

        if (progress == null) {
            log.warn(I18N.get("rag.progress.notfound", documentId));
            return ResponseEntity.status(404).body(Map.of(
                    "success", false,
                    "error", I18N.getLang("rag.progress.notfound", lang, documentId)
            ));
        }

        return ResponseEntity.ok(Map.of(
                "success", true,
                "message", I18N.getLang("rag.progress.query.success", lang),
                "data", progress
        ));
    }

    /**
     * 获取文档处理详情
     * (Get document processing details)
     *
     * @param documentId 文档ID (Document ID)
     * @param lang       语言 (Language: zh/en)
     * @return 处理详情 (Processing details)
     */
    @GetMapping("/{documentId}/details")
    public ResponseEntity<Map<String, Object>> getDetails(
            @PathVariable String documentId,
            @RequestHeader(value = "Accept-Language", defaultValue = "zh") String lang) {

        ProcessingProgress progress = progressService.getProgress(documentId);

        if (progress == null) {
            return ResponseEntity.status(404).body(Map.of(
                    "success", false,
                    "error", I18N.getLang("rag.progress.notfound", lang, documentId)
            ));
        }

        // 构建详细信息 (Build detailed information)
        Map<String, Object> details = new java.util.HashMap<>();
        details.put("documentId", progress.getDocumentId());
        details.put("documentName", progress.getDocumentName());
        details.put("stage", progress.getStage().getName(lang));
        details.put("stageCode", progress.getStage().getCode());
        details.put("progress", progress.getProgress());
        details.put("status", progress.getStatus());
        details.put("details", progress.getDetails() != null ? progress.getDetails() : Map.of());
        details.put("preview", progress.getPreview() != null ? progress.getPreview() : "");
        details.put("errorMessage", progress.getErrorMessage() != null ? progress.getErrorMessage() : "");
        details.put("startTime", progress.getStartTime());
        details.put("updateTime", progress.getUpdateTime());

        return ResponseEntity.ok(Map.of(
                "success", true,
                "data", details
        ));
    }

    /**
     * 删除进度记录
     * (Delete progress record)
     *
     * @param documentId 文档ID (Document ID)
     * @param lang       语言 (Language: zh/en)
     * @return 删除结果 (Delete result)
     */
    @DeleteMapping("/{documentId}")
    public ResponseEntity<Map<String, Object>> deleteProgress(
            @PathVariable String documentId,
            @RequestHeader(value = "Accept-Language", defaultValue = "zh") String lang) {

        log.info(I18N.get("rag.progress.delete.start", documentId));

        progressService.removeProgress(documentId);

        return ResponseEntity.ok(Map.of(
                "success", true,
                "message", I18N.getLang("rag.progress.delete.success", lang, documentId)
        ));
    }
}






