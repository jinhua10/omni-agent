package top.yumbo.ai.omni.common.exception;

/**
 * 验证异常
 * <p>
 * 用于参数验证、URL验证等场景
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public class ValidationException extends BaseException {

    private final String fieldName;
    private final Object fieldValue;

    public ValidationException(String message) {
        super("VALIDATION_ERROR", message);
        this.fieldName = null;
        this.fieldValue = null;
    }

    public ValidationException(String message, Throwable cause) {
        super("VALIDATION_ERROR", message, cause);
        this.fieldName = null;
        this.fieldValue = null;
    }

    public ValidationException(String fieldName, Object fieldValue, String message) {
        super("VALIDATION_ERROR", String.format("字段 '%s' 验证失败: %s (值: %s)",
            fieldName, message, fieldValue));
        this.fieldName = fieldName;
        this.fieldValue = fieldValue;
    }

    public String getFieldName() {
        return fieldName;
    }

    public Object getFieldValue() {
        return fieldValue;
    }
}

