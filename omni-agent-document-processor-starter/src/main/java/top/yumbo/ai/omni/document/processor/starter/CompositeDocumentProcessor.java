package top.yumbo.ai.omni.document.processor.starter;
}
    }
        return lastDot > 0 ? filename.substring(lastDot) : "";
        int lastDot = filename.lastIndexOf('.');
    private String getExtension(String filename) {

    }
        return processors.stream().anyMatch(p -> p.supportsExtension(extension));
    public boolean supportsExtension(String extension) {
    @Override

    }
        return processors.stream().anyMatch(p -> p.supports(type));
    public boolean supports(DocumentType type) {
    @Override

    }
        return allTypes;
        }
            allTypes.addAll(processor.getSupportedTypes());
        for (DocumentProcessor processor : processors) {
        List<DocumentType> allTypes = new ArrayList<>();
    public List<DocumentType> getSupportedTypes() {
    @Override

    }
        throw new ProcessorException("不支持的文档类型: " + type);

        }
            }
                return processor.process(documentId, input);
                log.debug("📄 使用 {} 处理文档: {}", processor.getClass().getSimpleName(), documentId);
            if (processor.supports(type)) {
        for (DocumentProcessor processor : processors) {
        // 查找支持该类型的处理器

        DocumentType type = DocumentType.fromExtension(extension);
        String extension = getExtension(documentId);
        // 从文档ID推断类型
    public ProcessedDocument process(String documentId, InputStream input) throws ProcessorException {
    @Override

    }
        log.info("✅ 组合文档处理器初始化完成，注册了 {} 个处理器", this.processors.size());
        this.processors = processors != null ? processors : new ArrayList<>();
    public CompositeDocumentProcessor(List<DocumentProcessor> processors) {

    private final List<DocumentProcessor> processors;

public class CompositeDocumentProcessor implements DocumentProcessor {
@Slf4j
 */
 * @since 1.0.0
 * @author OmniAgent Team
 *
 * <p>根据文档类型自动选择合适的处理器</p>
 *
 * 组合文档处理器
/**

import java.util.List;
import java.util.ArrayList;
import java.io.InputStream;

import top.yumbo.ai.omni.document.processor.*;
import lombok.extern.slf4j.Slf4j;


