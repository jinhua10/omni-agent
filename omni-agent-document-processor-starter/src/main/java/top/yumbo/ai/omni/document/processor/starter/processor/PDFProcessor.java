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
 * PDF 文档处理器（增强版）
 *
 * <p>从 core/old/document 迁移而来</p>
 * <p>功能：逐页处理、页码标记、结构化输出</p>
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
                StringBuilder text = new StringBuilder();
                int pageCount = document.getNumberOfPages();

                // 逐页处理
                for (int pageIndex = 0; pageIndex < pageCount; pageIndex++) {
                    int pageNumber = pageIndex + 1;

                    // 添加页码分隔
                    if (pageIndex > 0) {
                        text.append("\n---\n\n");
                    }
                    text.append("## 第 ").append(pageNumber).append(" 页\n\n");

                    // 提取该页文本
                    String pageText = extractPageText(document, pageNumber);
                    if (pageText != null && !pageText.trim().isEmpty()) {
                        text.append(pageText.trim()).append("\n\n");
                    }
                }

                // 获取元数据
                Map<String, Object> metadata = new HashMap<>();
                metadata.put("totalPages", pageCount);
                metadata.put("pdfVersion", document.getVersion());
                metadata.put("format", "pdf");

                // 尝试获取文档信息
                if (document.getDocumentInformation() != null) {
                    if (document.getDocumentInformation().getTitle() != null) {
                        metadata.put("title", document.getDocumentInformation().getTitle());
                    }
                    if (document.getDocumentInformation().getAuthor() != null) {
                        metadata.put("author", document.getDocumentInformation().getAuthor());
                    }
                    if (document.getDocumentInformation().getSubject() != null) {
                        metadata.put("subject", document.getDocumentInformation().getSubject());
                    }
                }

                String content = text.toString();
                log.info("✅ PDF 处理完成: {} ({} 页, {} 字符)",
                    documentId, pageCount, content.length());

                return ProcessedDocument.builder()
                        .documentId(documentId)
                        .documentType(DocumentType.PDF)
                        .text(content)
                        .pageCount(pageCount)
                        .characterCount(content.length())
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

    /**
     * 提取指定页的文本
     */
    private String extractPageText(PDDocument document, int pageNumber) {
        try {
            PDFTextStripper stripper = new PDFTextStripper();
            stripper.setStartPage(pageNumber);
            stripper.setEndPage(pageNumber);

            String text = stripper.getText(document);
            return text != null ? text.trim() : "";
        } catch (Exception e) {
            log.warn("提取 PDF 第 {} 页文本失败", pageNumber, e);
            return "";
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

