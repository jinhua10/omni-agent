package top.yumbo.ai.omni.marketplace.strategy;

import lombok.Getter;

/**
 * 策略执行异常
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Getter
public class StrategyExecutionException extends Exception {

    private final String strategyId;
    private final ExecutionErrorCode errorCode;

    public StrategyExecutionException(String strategyId, ExecutionErrorCode errorCode, String message) {
        super(message);
        this.strategyId = strategyId;
        this.errorCode = errorCode;
    }

    public StrategyExecutionException(String strategyId, ExecutionErrorCode errorCode, String message, Throwable cause) {
        super(message, cause);
        this.strategyId = strategyId;
        this.errorCode = errorCode;
    }

    /**
     * 执行错误代码
     */
    public enum ExecutionErrorCode {
        /** 超时 */
        TIMEOUT,

        /** 内存不足 */
        OUT_OF_MEMORY,

        /** 参数无效 */
        INVALID_PARAMETERS,

        /** 输入数据无效 */
        INVALID_INPUT,

        /** 权限不足 */
        PERMISSION_DENIED,

        /** 依赖缺失 */
        MISSING_DEPENDENCY,

        /** 内部错误 */
        INTERNAL_ERROR,

        /** 不支持的操作 */
        UNSUPPORTED_OPERATION,

        /** 资源限制 */
        RESOURCE_LIMIT_EXCEEDED
    }
}

