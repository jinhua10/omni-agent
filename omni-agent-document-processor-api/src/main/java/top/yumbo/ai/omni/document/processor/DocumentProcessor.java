package top.yumbo.ai.omni.document.processor;

import java.io.InputStream;
import java.util.List;

/**
 * 文档处理器接口
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface DocumentProcessor {

    /**
     * 处理文档，提取文本
     *
     * @param documentId 文档ID
     * @param input 文档输入流
     * @return 处理结果
     * @throws ProcessorException 处理异常
     */
    ProcessedDocument process(String documentId, InputStream input) throws ProcessorException;

    /**
     * 支持的文档类型
     *
     * @return 支持的类型列表
     */
    List<DocumentType> getSupportedTypes();

    /**
     * 是否支持该类型
     *
     * @param type 文档类型
     * @return 是否支持
     */
    boolean supports(DocumentType type);

    /**
     * 是否支持该文件扩展名
     *
     * @param extension 文件扩展名（如 .pdf, .docx）
     * @return 是否支持
     */
    boolean supportsExtension(String extension);
}


