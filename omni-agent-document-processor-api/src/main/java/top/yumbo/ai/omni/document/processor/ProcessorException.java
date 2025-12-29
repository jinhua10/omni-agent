package top.yumbo.ai.omni.document.processor;

import java.io.Serial;

/**
 * 文档处理器异常
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class ProcessorException extends Exception {

    @Serial
    private static final long serialVersionUID = 1L;

    public ProcessorException(String message) {
        super(message);
    }

    public ProcessorException(String message, Throwable cause) {
        super(message, cause);
    }

    public ProcessorException(Throwable cause) {
        super(cause);
    }
}

