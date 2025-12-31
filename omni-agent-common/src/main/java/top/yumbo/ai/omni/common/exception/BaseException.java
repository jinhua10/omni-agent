package top.yumbo.ai.omni.common.exception;

import lombok.Getter;

/**
 * 基础异常类
 * <p>
 * 所有自定义异常的基类
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Getter
public class BaseException extends RuntimeException {

    private String code;

    public BaseException(String message) {
        super(message);
        this.code = "UNKNOWN_ERROR";
    }

    public BaseException(String message, Throwable cause) {
        super(message, cause);
        this.code = "UNKNOWN_ERROR";
    }

    public BaseException(String code, String message) {
        super(message);
        this.code = code;
    }

    public BaseException(String code, String message, Throwable cause) {
        super(message, cause);
        this.code = code;
    }

}

