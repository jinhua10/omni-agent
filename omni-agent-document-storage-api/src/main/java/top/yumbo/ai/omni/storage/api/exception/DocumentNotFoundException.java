package top.yumbo.ai.omni.storage.api.exception;

/**
 * 文档未找到异常
 * (Document Not Found Exception)
 *
 * <p>当请求的文档不存在时抛出此异常</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class DocumentNotFoundException extends StorageException {

    public DocumentNotFoundException(String documentId) {
        super("DOCUMENT_NOT_FOUND", documentId, "Document not found: " + documentId);
    }

    public DocumentNotFoundException(String documentId, String message) {
        super("DOCUMENT_NOT_FOUND", documentId, message);
    }

    public DocumentNotFoundException(String documentId, String message, Throwable cause) {
        super("DOCUMENT_NOT_FOUND", documentId, message, cause);
    }
}

