package top.yumbo.ai.omni.storage.api.exception;

/**
 * 存储服务异常基类
 * (Storage Service Base Exception)
 *
 * <p>用于封装存储层的所有异常，提供统一的异常处理机制</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class StorageException extends RuntimeException {

    /**
     * 错误代码
     */
    private String errorCode;

    /**
     * 文档ID（如果适用）
     */
    private String documentId;

    /**
     * 构造函数
     *
     * @param message 错误消息
     */
    public StorageException(String message) {
        super(message);
    }

    /**
     * 构造函数
     *
     * @param message 错误消息
     * @param cause   原因
     */
    public StorageException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * 构造函数
     *
     * @param errorCode  错误代码
     * @param message    错误消息
     */
    public StorageException(String errorCode, String message) {
        super(message);
        this.errorCode = errorCode;
    }

    /**
     * 构造函数
     *
     * @param errorCode  错误代码
     * @param message    错误消息
     * @param cause      原因
     */
    public StorageException(String errorCode, String message, Throwable cause) {
        super(message, cause);
        this.errorCode = errorCode;
    }

    /**
     * 构造函数
     *
     * @param errorCode  错误代码
     * @param documentId 文档ID
     * @param message    错误消息
     */
    public StorageException(String errorCode, String documentId, String message) {
        super(message);
        this.errorCode = errorCode;
        this.documentId = documentId;
    }

    /**
     * 构造函数
     *
     * @param errorCode  错误代码
     * @param documentId 文档ID
     * @param message    错误消息
     * @param cause      原因
     */
    public StorageException(String errorCode, String documentId, String message, Throwable cause) {
        super(message, cause);
        this.errorCode = errorCode;
        this.documentId = documentId;
    }

    /**
     * 获取错误代码
     *
     * @return 错误代码
     */
    public String getErrorCode() {
        return errorCode;
    }

    /**
     * 获取文档ID
     *
     * @return 文档ID
     */
    public String getDocumentId() {
        return documentId;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(super.toString());
        if (errorCode != null) {
            sb.append(" [errorCode=").append(errorCode).append("]");
        }
        if (documentId != null) {
            sb.append(" [documentId=").append(documentId).append("]");
        }
        return sb.toString();
    }
}

