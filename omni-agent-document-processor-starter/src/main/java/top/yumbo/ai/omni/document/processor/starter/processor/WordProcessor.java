package top.yumbo.ai.omni.document.processor.starter.processor;

import lombok.extern.slf4j.Slf4j;
import org.apache.poi.hwpf.HWPFDocument;
import org.apache.poi.hwpf.extractor.WordExtractor;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.apache.poi.xwpf.extractor.XWPFWordExtractor;
import top.yumbo.ai.omni.document.processor.*;
import top.yumbo.ai.omni.document.processor.starter.config.DocumentProcessorProperties;

import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Word 文档处理器
 *
 * <p>支持 .doc 和 .docx 格式</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class WordProcessor implements DocumentProcessor {

    private final DocumentProcessorProperties properties;

    public WordProcessor(DocumentProcessorProperties properties) {
        this.properties = properties;
    }

    @Override
    public ProcessedDocument process(String documentId, InputStream input) throws ProcessorException {
        try {
            log.debug("📄 开始处理 Word 文档: {}", documentId);

            String extension = getExtension(documentId);
            String text;
            Map<String, Object> metadata = new HashMap<>();

            if (".docx".equalsIgnoreCase(extension)) {
                // 处理 .docx (Word 2007+)
                try (XWPFDocument document = new XWPFDocument(input);
                     XWPFWordExtractor extractor = new XWPFWordExtractor(document)) {
                    text = extractor.getText();
                    metadata.put("format", "docx");
                    metadata.put("paragraphCount", document.getParagraphs().size());
                }
            } else {
                // 处理 .doc (Word 97-2003)
                try (HWPFDocument document = new HWPFDocument(input);
                     WordExtractor extractor = new WordExtractor(document)) {
                    text = extractor.getText();
                    metadata.put("format", "doc");
                }
            }

            log.info("✅ Word 处理完成: {} ({} 字符)", documentId, text.length());

            return ProcessedDocument.builder()
                    .documentId(documentId)
                    .documentType(DocumentType.WORD)
                    .text(text)
                    .characterCount(text.length())
                    .metadata(metadata)
                    .success(true)
                    .build();

        } catch (Exception e) {
            log.error("❌ Word 处理失败: {}", documentId, e);
            throw new ProcessorException("Word 处理失败: " + e.getMessage(), e);
        }
    }

    @Override
    public List<DocumentType> getSupportedTypes() {
        return Arrays.asList(DocumentType.WORD);
    }

    @Override
    public boolean supports(DocumentType type) {
        return type == DocumentType.WORD;
    }

    @Override
    public boolean supportsExtension(String extension) {
        return ".doc".equalsIgnoreCase(extension) ||
               ".docx".equalsIgnoreCase(extension);
    }

    private String getExtension(String filename) {
        int lastDot = filename.lastIndexOf('.');
        return lastDot > 0 ? filename.substring(lastDot) : "";
    }
}



