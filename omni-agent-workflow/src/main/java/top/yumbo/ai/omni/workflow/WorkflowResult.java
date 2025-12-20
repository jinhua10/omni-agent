package top.yumbo.ai.omni.workflow;

import lombok.Data;
import java.util.Map;

/**
 * 工作流执行结果
 * (Workflow Execution Result)
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Data
@lombok.Builder
@lombok.NoArgsConstructor
@lombok.AllArgsConstructor
public class WorkflowResult {

    /**
     * 执行ID
     */
    private String executionId;

    /**
     * 工作流名称
     */
    private String workflowName;

    /**
     * 工作流版本
     */
    private String workflowVersion;

    /**
     * 执行状态
     */
    private ExecutionStatus status;

    /**
     * 开始时间
     */
    private Long startTime;

    /**
     * 结束时间
     */
    private Long endTime;

    /**
     * 执行时长（毫秒）
     */
    private Long duration;

    /**
     * 最终结果
     */
    private Object finalResult;

    /**
     * 所有步骤的结果
     */
    private Map<String, Object> stepResults;

    /**
     * 错误信息（如果失败）
     */
    private String error;

    /**
     * 错误堆栈
     */
    private String errorStack;

    /**
     * 执行元数据
     */
    private Map<String, Object> metadata;

    /**
     * 执行状态枚举
     */
    public enum ExecutionStatus {
        PENDING,    // 等待执行
        RUNNING,    // 执行中
        SUCCESS,    // 成功
        FAILED,     // 失败
        CANCELLED   // 已取消
    }

    /**
     * 获取执行时长
     */
    public Long getDuration() {
        if (duration != null) {
            return duration;
        }
        if (startTime != null && endTime != null) {
            return endTime - startTime;
        }
        return null;
    }

    /**
     * 判断是否成功
     */
    public boolean isSuccess() {
        return ExecutionStatus.SUCCESS.equals(status);
    }

    /**
     * 判断是否失败
     */
    public boolean isFailed() {
        return ExecutionStatus.FAILED.equals(status);
    }

    /**
     * 判断是否正在运行
     */
    public boolean isRunning() {
        return ExecutionStatus.RUNNING.equals(status);
    }
}

