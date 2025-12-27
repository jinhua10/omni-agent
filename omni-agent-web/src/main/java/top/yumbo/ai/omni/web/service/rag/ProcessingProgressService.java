package top.yumbo.ai.omni.web.service.rag;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.web.model.rag.ProcessingProgress;
import top.yumbo.ai.omni.web.model.rag.ProcessingStage;
import top.yumbo.ai.omni.web.websocket.DocumentProcessingWebSocketHandler;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 文档处理进度服务
 * (Document Processing Progress Service)
 *
 * <p>
 * 管理文档处理的实时进度追踪，并通过 WebSocket 推送更新
 * (Manages real-time tracking of document processing progress and pushes updates via WebSocket)
 * </p>
 *
 * @author AI Reviewer Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@Service
public class ProcessingProgressService {

    @Autowired
    @Lazy
    private DocumentProcessingWebSocketHandler webSocketHandler;

    /**
     * 存储所有文档的处理进度
     * (Store processing progress for all documents)
     */
    private final Map<String, ProcessingProgress> progressMap = new ConcurrentHashMap<>();

    /**
     * 开始处理文档
     * (Start processing document)
     *
     * @param documentId   文档ID (Document ID)
     * @param documentName 文档名称 (Document name)
     */
    public void startProcessing(String documentId, String documentName) {
        ProcessingProgress progress = new ProcessingProgress();
        progress.setDocumentId(documentId);
        progress.setDocumentName(documentName);
        progress.setStage(ProcessingStage.UPLOAD);
        progress.setProgress(0);
        progress.setStatus(ProcessingProgress.ProcessingStatus.RUNNING);
        progress.setStartTime(LocalDateTime.now());
        progress.setUpdateTime(LocalDateTime.now());

        progressMap.put(documentId, progress);
        log.info("📄 开始处理文档: documentId={}, name={}", documentId, documentName);
    }

    /**
     * 更新处理进度
     * (Update processing progress)
     *
     * @param documentId 文档ID (Document ID)
     * @param stage      处理阶段 (Processing stage)
     * @param progress   进度百分比 (Progress percentage)
     */
    public void updateProgress(String documentId, ProcessingStage stage, int progress) {
        ProcessingProgress p = progressMap.get(documentId);
        if (p == null) {
            log.warn("⚠️ 文档进度不存在: documentId={}", documentId);
            return;
        }

        p.setStage(stage);
        p.setProgress(progress);
        p.setUpdateTime(LocalDateTime.now());

        log.debug("🔄 更新文档处理进度: documentId={}, stage={}, progress={}%",
                documentId, stage.getCode(), progress);

        // 广播进度更新 (Broadcast progress update)
        broadcastProgress(documentId, p);
    }

    /**
     * 更新阶段详情
     * (Update stage details)
     *
     * @param documentId 文档ID (Document ID)
     * @param details    阶段详情 (Stage details)
     */
    public void updateDetails(String documentId, ProcessingProgress.StageDetails details) {
        ProcessingProgress p = progressMap.get(documentId);
        if (p != null) {
            p.setDetails(details);
            p.setUpdateTime(LocalDateTime.now());
        }
    }

    /**
     * 设置预览内容
     * (Set preview content)
     *
     * @param documentId 文档ID (Document ID)
     * @param preview    预览内容 (Preview content)
     */
    public void setPreview(String documentId, String preview) {
        ProcessingProgress p = progressMap.get(documentId);
        if (p != null) {
            p.setPreview(preview);
            p.setUpdateTime(LocalDateTime.now());
        }
    }

    /**
     * 标记处理完成
     * (Mark processing as completed)
     *
     * @param documentId 文档ID (Document ID)
     */
    public void markCompleted(String documentId) {
        ProcessingProgress p = progressMap.get(documentId);
        if (p == null) {
            log.warn("⚠️ 文档进度不存在: documentId={}", documentId);
            return;
        }

        p.setStage(ProcessingStage.COMPLETED);
        p.setProgress(100);
        p.setStatus(ProcessingProgress.ProcessingStatus.COMPLETED);
        p.setUpdateTime(LocalDateTime.now());

        log.info("✅ 文档处理完成: documentId={}, name={}", documentId, p.getDocumentName());

        // 广播完成状态 (Broadcast completion status)
        broadcastProgress(documentId, p);
    }

    /**
     * 标记处理失败
     * (Mark processing as failed)
     *
     * @param documentId   文档ID (Document ID)
     * @param stage        失败阶段 (Failed stage)
     * @param errorMessage 错误信息 (Error message)
     */
    public void markFailed(String documentId, ProcessingStage stage, String errorMessage) {
        ProcessingProgress p = progressMap.get(documentId);
        if (p == null) {
            log.warn("⚠️ 文档进度不存在: documentId={}", documentId);
            return;
        }

        p.setStage(stage);
        p.setStatus(ProcessingProgress.ProcessingStatus.FAILED);
        p.setErrorMessage(errorMessage);
        p.setUpdateTime(LocalDateTime.now());

        log.error("❌ 文档处理失败: documentId={}, stage={}, error={}",
                documentId, stage.getCode(), errorMessage);

        // 广播失败状态 (Broadcast failure status)
        broadcastProgress(documentId, p);
    }

    /**
     * 广播进度更新
     * (Broadcast progress update)
     */
    private void broadcastProgress(String documentId, ProcessingProgress prog) {
        if (webSocketHandler != null) {
            try {
                // ⭐ 将 ProcessingProgress 转换为 Map
                Map<String, Object> progressMap = new HashMap<>();
                progressMap.put("documentId", prog.getDocumentId());
                progressMap.put("documentName", prog.getDocumentName());
                progressMap.put("stage", prog.getStage() != null ? prog.getStage().name() : null);
                progressMap.put("status", prog.getStatus() != null ? prog.getStatus().name() : "PROCESSING");
                progressMap.put("percentage", prog.getProgress()); // progress 字段对应前端的 percentage
                progressMap.put("message", prog.getErrorMessage() != null ? prog.getErrorMessage() : "处理中...");
                progressMap.put("startTime", prog.getStartTime());

                // 从 details 的 metadata 获取更多信息
                if (prog.getDetails() != null && prog.getDetails().getMetadata() != null) {
                    Object chunks = prog.getDetails().getMetadata().get("chunks");
                    Object vectors = prog.getDetails().getMetadata().get("vectors");
                    if (chunks != null) progressMap.put("chunks", chunks);
                    if (vectors != null) progressMap.put("vectors", vectors);
                }

                webSocketHandler.broadcastProgress(documentId, progressMap);
                log.debug("📢 已推送进度更新: documentId={}, percentage={}%", documentId, prog.getProgress());
            } catch (Exception e) {
                log.error("❌ 广播进度更新失败: documentId={}", documentId, e);
            }
        }
    }

    /**
     * 获取文档处理进度
     * (Get document processing progress)
     *
     * @param documentId 文档ID (Document ID)
     * @return 处理进度 (Processing progress)
     */
    public ProcessingProgress getProgress(String documentId) {
        return progressMap.get(documentId);
    }

    /**
     * 移除文档进度记录
     * (Remove document progress record)
     *
     * @param documentId 文档ID (Document ID)
     */
    public void removeProgress(String documentId) {
        progressMap.remove(documentId);
        log.debug("🗑️ 移除文档进度记录: documentId={}", documentId);
    }

    /**
     * 清除所有进度记录
     * (Clear all progress records)
     */
    public void clearAll() {
        progressMap.clear();
        log.info("🧹 清除所有文档进度记录");
    }
}



