package top.yumbo.ai.omni.web.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Map;

/**
 * 文档处理步骤
 * (Document Processing Step)
 *
 * 用于管理文档处理的各个步骤状态和流程导航
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class DocumentProcessStep {

    /**
     * 步骤类型
     */
    public enum StepType {
        TEXT_EXTRACTION("文本提取", 1),
        CHUNKING("智能分块", 2),
        INDEXING("向量索引", 3),
        COMPLETED("已完成", 4);

        private final String displayName;
        private final int order;

        StepType(String displayName, int order) {
            this.displayName = displayName;
            this.order = order;
        }

        public String getDisplayName() {
            return displayName;
        }

        public int getOrder() {
            return order;
        }

        /**
         * 获取下一步
         */
        public StepType getNext() {
            switch (this) {
                case TEXT_EXTRACTION:
                    return CHUNKING;
                case CHUNKING:
                    return INDEXING;
                case INDEXING:
                    return COMPLETED;
                default:
                    return COMPLETED;
            }
        }

        /**
         * 获取上一步
         */
        public StepType getPrevious() {
            switch (this) {
                case CHUNKING:
                    return TEXT_EXTRACTION;
                case INDEXING:
                    return CHUNKING;
                case COMPLETED:
                    return INDEXING;
                default:
                    return TEXT_EXTRACTION;
            }
        }
    }

    /**
     * 步骤状态
     */
    public enum StepStatus {
        PENDING("待执行"),
        PROCESSING("执行中"),
        COMPLETED("已完成"),
        FAILED("失败"),
        SKIPPED("已跳过");

        private final String displayName;

        StepStatus(String displayName) {
            this.displayName = displayName;
        }

        public String getDisplayName() {
            return displayName;
        }
    }

    /**
     * 文档ID
     */
    private String documentId;

    /**
     * 当前步骤
     */
    private StepType currentStep;

    /**
     * 步骤状态
     */
    private StepStatus status;

    /**
     * 进度（0-100）
     */
    private Integer progress;

    /**
     * 步骤结果数据
     */
    private Map<String, Object> result;

    /**
     * 错误信息
     */
    private String errorMessage;

    /**
     * 步骤开始时间
     */
    private Long startTime;

    /**
     * 步骤完成时间
     */
    private Long endTime;

    /**
     * 是否可以执行下一步
     */
    public boolean canGoNext() {
        return status == StepStatus.COMPLETED && currentStep != StepType.COMPLETED;
    }

    /**
     * 是否可以返回上一步
     */
    public boolean canGoPrevious() {
        return currentStep != StepType.TEXT_EXTRACTION;
    }

    /**
     * 是否可以重新执行当前步骤
     */
    public boolean canReExecute() {
        return status == StepStatus.COMPLETED || status == StepStatus.FAILED;
    }
}






