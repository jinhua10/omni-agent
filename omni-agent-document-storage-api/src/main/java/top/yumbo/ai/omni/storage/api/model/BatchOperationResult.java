package top.yumbo.ai.omni.storage.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

/**
 * 批量操作结果
 * (Batch Operation Result)
 *
 * @author OmniAgent Team
 * @since 1.0.1
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BatchOperationResult implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 成功数量
     */
    private int successCount;

    /**
     * 失败数量
     */
    private int failureCount;

    /**
     * 总数量
     */
    private int totalCount;

    /**
     * 成功的ID列表
     */
    private List<String> successIds;

    /**
     * 失败的ID列表
     */
    private List<String> failureIds;

    /**
     * 错误信息（失败ID -> 错误消息）
     */
    private java.util.Map<String, String> errorMessages;

    /**
     * 是否全部成功
     */
    public boolean isAllSuccess() {
        return failureCount == 0 && successCount == totalCount;
    }

    /**
     * 是否全部失败
     */
    public boolean isAllFailure() {
        return successCount == 0 && failureCount == totalCount;
    }

    /**
     * 是否部分成功
     */
    public boolean isPartialSuccess() {
        return successCount > 0 && failureCount > 0;
    }

    /**
     * 成功率
     */
    public double getSuccessRate() {
        if (totalCount == 0) {
            return 0.0;
        }
        return (double) successCount / totalCount * 100;
    }
}

