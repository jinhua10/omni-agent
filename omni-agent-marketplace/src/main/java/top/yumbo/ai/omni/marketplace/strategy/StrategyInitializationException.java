package top.yumbo.ai.omni.marketplace.strategy;

import lombok.Getter;

/**
 * 策略初始化异常
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Getter
public class StrategyInitializationException extends Exception {

    private final String strategyId;
    private final InitializationErrorCode errorCode;

    public StrategyInitializationException(String strategyId, InitializationErrorCode errorCode, String message) {
        super(message);
        this.strategyId = strategyId;
        this.errorCode = errorCode;
    }

    public StrategyInitializationException(String strategyId, InitializationErrorCode errorCode, String message, Throwable cause) {
        super(message, cause);
        this.strategyId = strategyId;
        this.errorCode = errorCode;
    }

    /**
     * 初始化错误代码
     */
    public enum InitializationErrorCode {
        /** 配置无效 */
        INVALID_CONFIGURATION,

        /** 依赖缺失 */
        MISSING_DEPENDENCY,

        /** 资源加载失败 */
        RESOURCE_LOAD_FAILED,

        /** 兼容性问题 */
        INCOMPATIBLE_VERSION,

        /** 权限不足 */
        INSUFFICIENT_PERMISSIONS,

        /** 初始化超时 */
        INITIALIZATION_TIMEOUT,

        /** 内部错误 */
        INTERNAL_ERROR
    }
}

