package top.yumbo.ai.omni.web.model.rag;

import lombok.Data;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

/**
 * 文档处理进度
 * (Document Processing Progress)
 *
 * <p>
 * 用于追踪文档处理的实时进度，支持 WebSocket 推送
 * (Used to track real-time document processing progress, supports WebSocket push)
 * </p>
 *
 * @author AI Reviewer Team
 * @since 2.0.0 (Phase 4)
 */
@Data
public class ProcessingProgress {

    /**
     * 文档ID (Document ID)
     */
    private String documentId;

    /**
     * 文档名称 (Document name)
     */
    private String documentName;

    /**
     * 当前处理阶段 (Current processing stage)
     */
    private ProcessingStage stage;

    /**
     * 进度百分比 (Progress percentage: 0-100)
     */
    private int progress;

    /**
     * 处理状态 (Processing status)
     */
    private ProcessingStatus status;

    /**
     * 阶段详情 (Stage details)
     */
    private StageDetails details;

    /**
     * 预览内容（可选）
     * (Preview content - optional)
     */
    private String preview;

    /**
     * 错误信息（如果失败）
     * (Error message if failed)
     */
    private String errorMessage;

    /**
     * 开始时间 (Start time)
     */
    private LocalDateTime startTime;

    /**
     * 更新时间 (Update time)
     */
    private LocalDateTime updateTime;

    /**
     * 处理状态枚举
     * (Processing Status Enum)
     */
    public enum ProcessingStatus {
        /**
         * 运行中 (Running)
         */
        RUNNING,

        /**
         * 已完成 (Completed)
         */
        COMPLETED,

        /**
         * 失败 (Failed)
         */
        FAILED
    }

    /**
     * 阶段详情
     * (Stage Details)
     */
    @Data
    public static class StageDetails {
        /**
         * 当前步骤 (Current step)
         */
        private String currentStep;

        /**
         * 总步骤数 (Total steps)
         */
        private int totalSteps;

        /**
         * 当前步骤索引 (Current step index)
         */
        private int currentStepIndex;

        /**
         * 已耗时（毫秒）(Elapsed time in milliseconds)
         */
        private long elapsedTimeMs;

        /**
         * 预计剩余时间（毫秒）(Estimated remaining time in milliseconds)
         */
        private long estimatedRemainingMs;

        /**
         * 额外信息 (Additional information)
         */
        private Map<String, Object> metadata = new HashMap<>();

        /**
         * 添加元数据
         * (Add metadata)
         */
        public void addMetadata(String key, Object value) {
            this.metadata.put(key, value);
        }
    }

    /**
     * 创建运行中的进度
     * (Create running progress)
     */
    public static ProcessingProgress running(String documentId, String documentName,
                                            ProcessingStage stage, int progress) {
        ProcessingProgress p = new ProcessingProgress();
        p.setDocumentId(documentId);
        p.setDocumentName(documentName);
        p.setStage(stage);
        p.setProgress(progress);
        p.setStatus(ProcessingStatus.RUNNING);
        p.setUpdateTime(LocalDateTime.now());
        return p;
    }

    /**
     * 创建完成的进度
     * (Create completed progress)
     */
    public static ProcessingProgress completed(String documentId, String documentName) {
        ProcessingProgress p = new ProcessingProgress();
        p.setDocumentId(documentId);
        p.setDocumentName(documentName);
        p.setStage(ProcessingStage.COMPLETED);
        p.setProgress(100);
        p.setStatus(ProcessingStatus.COMPLETED);
        p.setUpdateTime(LocalDateTime.now());
        return p;
    }

    /**
     * 创建失败的进度
     * (Create failed progress)
     */
    public static ProcessingProgress failed(String documentId, String documentName,
                                           ProcessingStage stage, String errorMessage) {
        ProcessingProgress p = new ProcessingProgress();
        p.setDocumentId(documentId);
        p.setDocumentName(documentName);
        p.setStage(stage);
        p.setStatus(ProcessingStatus.FAILED);
        p.setErrorMessage(errorMessage);
        p.setUpdateTime(LocalDateTime.now());
        return p;
    }
}






