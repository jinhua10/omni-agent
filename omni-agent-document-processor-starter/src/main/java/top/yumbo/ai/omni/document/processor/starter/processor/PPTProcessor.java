package top.yumbo.ai.omni.document.processor.starter.processor;

import org.apache.poi.xslf.usermodel.*;
import org.apache.poi.hslf.usermodel.*;
import top.yumbo.ai.omni.document.processor.*;
import top.yumbo.ai.omni.document.processor.starter.config.DocumentProcessorProperties;

import java.io.InputStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * PowerPoint 文档处理器
 *
 * <p>支持 .ppt 和 .pptx 格式</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class PPTProcessor implements DocumentProcessor {

    private final DocumentProcessorProperties properties;

    public PPTProcessor(DocumentProcessorProperties properties) {
        this.properties = properties;
    }

    @Override
    public ProcessedDocument process(String documentId, InputStream input) throws ProcessorException {
        try {
            log.debug("📄 开始处理 PPT 文档: {}", documentId);

            String extension = getExtension(documentId);
            String text;
            int slideCount;

            if (".pptx".equalsIgnoreCase(extension)) {
                // 处理 .pptx (PowerPoint 2007+)
                XMLSlideShow ppt = new XMLSlideShow(input);
                StringBuilder sb = new StringBuilder();
                slideCount = ppt.getSlides().size();

                for (int i = 0; i < slideCount; i++) {
                    XSLFSlide slide = ppt.getSlides().get(i);
                    sb.append("## 幻灯片 ").append(i + 1).append("\n\n");

                    for (XSLFShape shape : slide.getShapes()) {
                        if (shape instanceof XSLFTextShape) {
                            XSLFTextShape textShape = (XSLFTextShape) shape;
                            String shapeText = textShape.getText();
                            if (shapeText != null && !shapeText.trim().isEmpty()) {
                                sb.append(shapeText).append("\n");
                            }
                        }
                    }
                    sb.append("\n");
                }

                text = sb.toString();
                ppt.close();

            } else {
                // 处理 .ppt (PowerPoint 97-2003)
                HSLFSlideShow ppt = new HSLFSlideShow(input);
                StringBuilder sb = new StringBuilder();
                slideCount = ppt.getSlides().size();

                for (int i = 0; i < slideCount; i++) {
                    HSLFSlide slide = ppt.getSlides().get(i);
                    sb.append("## 幻灯片 ").append(i + 1).append("\n\n");

                    for (HSLFShape shape : slide.getShapes()) {
                        if (shape instanceof HSLFTextShape) {
                            HSLFTextShape textShape = (HSLFTextShape) shape;
                            String shapeText = textShape.getText();
                            if (shapeText != null && !shapeText.trim().isEmpty()) {
                                sb.append(shapeText).append("\n");
                            }
                        }
                    }
                    sb.append("\n");
                }

                text = sb.toString();
                ppt.close();
            }

            Map<String, Object> metadata = new HashMap<>();
            metadata.put("slideCount", slideCount);

            log.info("✅ PPT 处理完成: {} ({} 张幻灯片)", documentId, slideCount);

            return ProcessedDocument.builder()
                    .documentId(documentId)
                    .documentType(DocumentType.PPT)
                    .text(text)
                    .pageCount(slideCount)
                    .characterCount(text.length())
                    .metadata(metadata)
                    .success(true)
                    .build();

        } catch (Exception e) {
            log.error("❌ PPT 处理失败: {}", documentId, e);
            throw new ProcessorException("PPT 处理失败: " + e.getMessage(), e);
        }
    }

    @Override
    public List<DocumentType> getSupportedTypes() {
        return Arrays.asList(DocumentType.PPT);
    }

    @Override
    public boolean supports(DocumentType type) {
        return type == DocumentType.PPT;
    }

    @Override
    public boolean supportsExtension(String extension) {
        return ".ppt".equalsIgnoreCase(extension) ||
               ".pptx".equalsIgnoreCase(extension);
    }

    private String getExtension(String filename) {
        int lastDot = filename.lastIndexOf('.');
        return lastDot > 0 ? filename.substring(lastDot) : "";
    }
}

