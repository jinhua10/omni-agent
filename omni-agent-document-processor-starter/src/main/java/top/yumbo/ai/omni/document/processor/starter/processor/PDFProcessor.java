package top.yumbo.ai.omni.document.processor.starter.processor;

import lombok.extern.slf4j.Slf4j;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import top.yumbo.ai.omni.document.processor.*;
import top.yumbo.ai.omni.document.processor.starter.config.DocumentProcessorProperties;

import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * PDF 文档处理器
 *
 * <p>从 core/old/document 迁移而来</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class PDFProcessor implements DocumentProcessor {

    private final DocumentProcessorProperties properties;

    public PDFProcessor(DocumentProcessorProperties properties) {
        this.properties = properties;
    }

    @Override
    public ProcessedDocument process(String documentId, InputStream input) throws ProcessorException {
        try {
            log.debug("📄 开始处理 PDF 文档: {}", documentId);

            PDDocument document = PDDocument.load(input);

            try {
                // 提取文本
                PDFTextStripper stripper = new PDFTextStripper();
                String text = stripper.getText(document);

                // 获取元数据
                Map<String, Object> metadata = new HashMap<>();
                metadata.put("pageCount", document.getNumberOfPages());
                metadata.put("pdfVersion", document.getVersion());

                // 尝试获取文档信息
                if (document.getDocumentInformation() != null) {
                    if (document.getDocumentInformation().getTitle() != null) {
                        metadata.put("title", document.getDocumentInformation().getTitle());
                    }
                    if (document.getDocumentInformation().getAuthor() != null) {
                        metadata.put("author", document.getDocumentInformation().getAuthor());
                    }
                }

                log.info("✅ PDF 处理完成: {} ({} 页, {} 字符)",
                    documentId, document.getNumberOfPages(), text.length());

                return ProcessedDocument.builder()
                        .documentId(documentId)
                        .documentType(DocumentType.PDF)
                        .text(text)
                        .pageCount(document.getNumberOfPages())
                        .characterCount(text.length())
                        .metadata(metadata)
                        .success(true)
                        .build();

            } finally {
                document.close();
            }

        } catch (Exception e) {
            log.error("❌ PDF 处理失败: {}", documentId, e);
            throw new ProcessorException("PDF 处理失败: " + e.getMessage(), e);
        }
    }

    @Override
    public List<DocumentType> getSupportedTypes() {
        return Arrays.asList(DocumentType.PDF);
    }

    @Override
    public boolean supports(DocumentType type) {
        return type == DocumentType.PDF;
    }

    @Override
    public boolean supportsExtension(String extension) {
        return ".pdf".equalsIgnoreCase(extension);
    }
}



