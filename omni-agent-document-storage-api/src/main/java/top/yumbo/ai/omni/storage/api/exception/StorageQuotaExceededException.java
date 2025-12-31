package top.yumbo.ai.omni.storage.api.exception;

/**
 * 存储空间不足异常
 * (Storage Quota Exceeded Exception)
 *
 * <p>当存储空间超出限制时抛出此异常</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class StorageQuotaExceededException extends StorageException {

    private final long requestedSize;
    private final long availableSize;

    public StorageQuotaExceededException(long requestedSize, long availableSize) {
        super("STORAGE_QUOTA_EXCEEDED",
              String.format("Storage quota exceeded: requested=%d bytes, available=%d bytes",
                          requestedSize, availableSize));
        this.requestedSize = requestedSize;
        this.availableSize = availableSize;
    }

    public StorageQuotaExceededException(String documentId, long requestedSize, long availableSize) {
        super("STORAGE_QUOTA_EXCEEDED", documentId,
              String.format("Storage quota exceeded for document %s: requested=%d bytes, available=%d bytes",
                          documentId, requestedSize, availableSize));
        this.requestedSize = requestedSize;
        this.availableSize = availableSize;
    }

    public long getRequestedSize() {
        return requestedSize;
    }

    public long getAvailableSize() {
        return availableSize;
    }
}

