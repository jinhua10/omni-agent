package top.yumbo.ai.knowledge.registry.exception;

import java.io.Serial;

/**
 * 知识注册表异常
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class KnowledgeRegistryException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = 1L;

    public KnowledgeRegistryException(String message) {
        super(message);
    }

    public KnowledgeRegistryException(String message, Throwable cause) {
        super(message, cause);
    }

    public KnowledgeRegistryException(Throwable cause) {
        super(cause);
    }
}

