package top.yumbo.ai.omni.common.exception;

/**
 * 基础异常类
 * <p>
 * 所有自定义异常的基类
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public class BaseException extends RuntimeException {

    private String code;

    public BaseException(String message) {
        super(message);
    }

    public BaseException(String message, Throwable cause) {
        super(message, cause);
    }

    public BaseException(String code, String message) {
        super(message);
        this.code = code;
    }

    public BaseException(String code, String message, Throwable cause) {
        super(message, cause);
        this.code = code;
    }

    public String getCode() {
        return code;
    }
}

