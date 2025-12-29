package top.yumbo.ai.omni.document.processor.starter;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.document.processor.DocumentProcessor;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 组合文档处理器
 * (Composite Document Processor)
 *
 * <p>
 * 根据文件扩展名选择合适的处理器进行处理。
 * 如果多个处理器支持同一扩展名，选择优先级最高的处理器。
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public class CompositeDocumentProcessor implements DocumentProcessor {

    private final List<DocumentProcessor> processors;

    public CompositeDocumentProcessor(List<DocumentProcessor> processors) {
        // 按优先级排序（数字越小优先级越高）
        this.processors = processors.stream()
                .sorted(Comparator.comparingInt(DocumentProcessor::getPriority))
                .collect(Collectors.toList());

        log.info("🔧 [Composite] 初始化组合文档处理器，共 {} 个处理器", processors.size());
        for (DocumentProcessor processor : this.processors) {
            log.info("  📌 [Composite] {} - 优先级: {}", processor.getName(), processor.getPriority());
        }
    }

    @Override
    public boolean supports(String fileExtension) {
        return processors.stream()
                .anyMatch(p -> p.supports(fileExtension));
    }

    @Override
    public String getName() {
        return "CompositeDocumentProcessor";
    }

    @Override
    public int getPriority() {
        return Integer.MAX_VALUE; // 最低优先级
    }

    @Override
    public ProcessingResult process(ProcessingContext context) throws DocumentProcessingException {
        String extension = context.getFileExtension();

        // 查找支持该扩展名的处理器
        DocumentProcessor selectedProcessor = processors.stream()
                .filter(p -> p.supports(extension))
                .findFirst()
                .orElse(null);

        if (selectedProcessor == null) {
            throw new DocumentProcessingException(
                    "没有找到支持 '" + extension + "' 格式的文档处理器"
            );
        }

        log.info("🎯 [Composite] 文件扩展名: {}, 选择处理器: {}",
                extension, selectedProcessor.getName());

        return selectedProcessor.process(context);
    }
}

