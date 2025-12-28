package top.yumbo.ai.omni.document.processor.starter.processor;

import lombok.extern.slf4j.Slf4j;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;
import org.apache.pdfbox.text.PDFTextStripper;
import top.yumbo.ai.omni.document.processor.*;
import top.yumbo.ai.omni.document.processor.starter.config.DocumentProcessorProperties;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.*;

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

    /**
     * 提取页面中的图片
     */
    private List<ExtractedImage> extractImagesFromPage(PDPage page, int pageNumber) {
        List<ExtractedImage> images = new ArrayList<>();
        int imageIndex = 0;

        try {
            if (page.getResources() != null && page.getResources().getXObjectNames() != null) {
                for (org.apache.pdfbox.cos.COSName name : page.getResources().getXObjectNames()) {
                    try {
                        org.apache.pdfbox.pdmodel.graphics.PDXObject xObject =
                                page.getResources().getXObject(name);

                        if (xObject instanceof PDImageXObject) {
                            PDImageXObject imageObject = (PDImageXObject) xObject;
                            ExtractedImage image = extractPDFImage(imageObject, pageNumber, imageIndex++);
                            if (image != null) {
                                images.add(image);
                            }
                        }
                    } catch (Exception e) {
                        log.warn("提取 PDF 图片失败: page={}, name={}", pageNumber, name, e);
                    }
                }
            }
        } catch (Exception e) {
            log.warn("提取 PDF 第 {} 页图片失败", pageNumber, e);
        }

        return images;
    }

    /**
     * 提取 PDF 图片对象
     */
    private ExtractedImage extractPDFImage(PDImageXObject imageObject, int pageNumber, int imageIndex) {
        try {
            BufferedImage bufferedImage = imageObject.getImage();
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            String format = detectImageFormat(imageObject);
            ImageIO.write(bufferedImage, format, baos);
            byte[] imageData = baos.toByteArray();

            Map<String, Object> metadata = new HashMap<>();
            metadata.put("pageNumber", pageNumber);
            metadata.put("imageIndex", imageIndex);
            metadata.put("documentType", "PDF");
            metadata.put("colorSpace", imageObject.getColorSpace().getName());

            return ExtractedImage.builder()
                    .imageId(UUID.randomUUID().toString())
                    .data(imageData)
                    .format(format)
                    .pageNumber(pageNumber)
                    .width(bufferedImage.getWidth())
                    .height(bufferedImage.getHeight())
                    .metadata(metadata)
                    .createdAt(System.currentTimeMillis())
                    .build();
        } catch (Exception e) {
            log.warn("提取 PDF 图片失败", e);
            return null;
        }
    }

    /**
     * 检测图片格式
     */
    private String detectImageFormat(PDImageXObject imageObject) {
        try {
            String suffix = imageObject.getSuffix();
            if (suffix != null && !suffix.isEmpty()) {
                return suffix.toLowerCase();
            }
        } catch (Exception e) {
            // Ignore
        }
        return "png";
    }
}

