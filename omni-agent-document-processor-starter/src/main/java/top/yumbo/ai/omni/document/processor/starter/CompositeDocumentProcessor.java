package top.yumbo.ai.omni.document.processor.starter;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.document.processor.*;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * 组合文档处理器
 *
 * <p>根据文档类型自动选择合适的处理器</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class CompositeDocumentProcessor implements DocumentProcessor {

    private final List<DocumentProcessor> processors;

    public CompositeDocumentProcessor(List<DocumentProcessor> processors) {
        this.processors = processors != null ? processors : new ArrayList<>();
        log.info("✅ 组合文档处理器初始化完成，注册了 {} 个处理器", this.processors.size());
    }

    @Override
    public ProcessedDocument process(String documentId, InputStream input) throws ProcessorException {
        // 从文档ID推断类型
        String extension = getExtension(documentId);
        DocumentType type = DocumentType.fromExtension(extension);

        // 查找支持该类型的处理器
        for (DocumentProcessor processor : processors) {
            if (processor.supports(type)) {
                log.debug("📄 使用 {} 处理文档: {}", processor.getClass().getSimpleName(), documentId);
                return processor.process(documentId, input);
            }
        }

        throw new ProcessorException("不支持的文档类型: " + type);
    }

    @Override
    public List<DocumentType> getSupportedTypes() {
        List<DocumentType> allTypes = new ArrayList<>();
        for (DocumentProcessor processor : processors) {
            allTypes.addAll(processor.getSupportedTypes());
        }
        return allTypes;
    }

    @Override
    public boolean supports(DocumentType type) {
        return processors.stream().anyMatch(p -> p.supports(type));
    }

    @Override
    public boolean supportsExtension(String extension) {
        return processors.stream().anyMatch(p -> p.supportsExtension(extension));
    }

    private String getExtension(String filename) {
        int lastDot = filename.lastIndexOf('.');
        return lastDot > 0 ? filename.substring(lastDot) : "";
    }
}

