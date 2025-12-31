package top.yumbo.ai.omni.storage.api.exception;

/**
 * 批量操作异常
 * (Batch Operation Exception)
 *
 * <p>当批量操作失败时抛出此异常，包含失败详情</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class BatchOperationException extends StorageException {

    /**
     * 成功的ID列表
     */
    private final java.util.List<String> successIds;

    /**
     * 失败的ID列表
     */
    private final java.util.List<String> failureIds;

    /**
     * 错误消息映射
     */
    private final java.util.Map<String, String> errorMessages;

    public BatchOperationException(String message,
                                   java.util.List<String> successIds,
                                   java.util.List<String> failureIds,
                                   java.util.Map<String, String> errorMessages) {
        super("BATCH_OPERATION_ERROR", message);
        this.successIds = successIds;
        this.failureIds = failureIds;
        this.errorMessages = errorMessages;
    }

    public BatchOperationException(String message,
                                   Throwable cause,
                                   java.util.List<String> successIds,
                                   java.util.List<String> failureIds,
                                   java.util.Map<String, String> errorMessages) {
        super("BATCH_OPERATION_ERROR", message, cause);
        this.successIds = successIds;
        this.failureIds = failureIds;
        this.errorMessages = errorMessages;
    }

    public java.util.List<String> getSuccessIds() {
        return successIds;
    }

    public java.util.List<String> getFailureIds() {
        return failureIds;
    }

    public java.util.Map<String, String> getErrorMessages() {
        return errorMessages;
    }

    @Override
    public String toString() {
        return super.toString() +
               " [successCount=" + (successIds != null ? successIds.size() : 0) +
               ", failureCount=" + (failureIds != null ? failureIds.size() : 0) + "]";
    }
}

