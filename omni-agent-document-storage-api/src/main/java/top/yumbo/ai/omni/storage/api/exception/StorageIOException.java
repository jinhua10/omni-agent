package top.yumbo.ai.omni.storage.api.exception;

/**
 * 存储IO异常
 * (Storage IO Exception)
 *
 * <p>当存储操作发生IO错误时抛出此异常</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class StorageIOException extends StorageException {

    public StorageIOException(String message) {
        super("STORAGE_IO_ERROR", message);
    }

    public StorageIOException(String message, Throwable cause) {
        super("STORAGE_IO_ERROR", message, cause);
    }

    public StorageIOException(String documentId, String message, Throwable cause) {
        super("STORAGE_IO_ERROR", documentId, message, cause);
    }
}

