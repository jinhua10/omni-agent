package top.yumbo.ai.omni.core.document.processor;

import lombok.extern.slf4j.Slf4j;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.graphics.image.PDImageXObject;
import org.apache.pdfbox.rendering.PDFRenderer;
import org.apache.pdfbox.text.PDFTextStripper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;

/**
 * PDF 文档处理器
 * (PDF Document Processor)
 *
 * <p>处理策略：</p>
 * <ul>
 *   <li>提取文本内容（按页码）</li>
 *   <li>提取内嵌图片的位置信息</li>
 *   <li>使用 Vision LLM 分析图片内容</li>
 *   <li>将图片描述嵌入到文本对应位置</li>
 * </ul>
 *
 * <p>配置说明：</p>
 * <ul>
 *   <li>默认启用（matchIfMissing = true），无需配置</li>
 *   <li>可通过 omni-agent.pdf.enabled=false 禁用</li>
 *   <li>未来可能添加更多配置项（如 max-pages、dpi 等）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
@ConditionalOnProperty(
    prefix = "omni-agent.pdf",
    name = "enabled",
    havingValue = "true",
    matchIfMissing = true  // 默认启用，无需配置
)
public class PDFDocumentProcessor extends AbstractDocumentProcessor {

    private static final Set<String> SUPPORTED_EXTENSIONS = Set.of("pdf");

    // ⭐ OCR 服务（可选，如果引入了 OCR starter 才会注入）
    @Autowired(required = false)
    private Object ocrService;  // 使用 Object 避免强依赖

    @Value("${omni-agent.pdf.enable-ocr:false}")
    private boolean enableOCR;  // 是否启用 OCR

    @Override
    public boolean supports(String fileExtension) {
        return SUPPORTED_EXTENSIONS.contains(fileExtension.toLowerCase());
    }

    @Override
    public String getName() {
        return "PDFProcessor";
    }

    @Override
    public int getPriority() {
        return 30; // 高优先级（与 Excel、Word 相同）
    }

    @Override
    protected ExtractedContent extractContent(ProcessingContext context) throws Exception {
        InputStream inputStream;
        if (context.getFileBytes() != null) {
            inputStream = new ByteArrayInputStream(context.getFileBytes());
        } else {
            inputStream = new FileInputStream(context.getFilePath());
        }

        try (PDDocument document = PDDocument.load(inputStream)) {
            ExtractedContent content = new ExtractedContent();
            content.getMetadata().put("format", "pdf");
            content.getMetadata().put("totalPages", document.getNumberOfPages());

            int position = 0;
            int totalImageCount = 0;

            // 逐页处理
            for (int pageIndex = 0; pageIndex < document.getNumberOfPages(); pageIndex++) {
                int pageNumber = pageIndex + 1;

                // 添加页码分隔
                if (pageIndex > 0) {
                    content.addTextBlock("\n---\n\n", position++);
                }
                content.addTextBlock("## 第 " + pageNumber + " 页\n\n", position++);

                // 提取该页文本
                String pageText = extractPageText(document, pageNumber);

                // ⭐ 如果文本为空且启用了 OCR，尝试使用 OCR 提取
                if ((pageText == null || pageText.trim().isEmpty()) && enableOCR && ocrService != null) {
                    pageText = extractPageTextByOCR(document, pageIndex, pageNumber);
                }

                if (pageText != null && !pageText.trim().isEmpty()) {
                    content.addTextBlock(pageText + "\n\n", position++);
                }

                // 提取该页图片
                PDPage page = document.getPage(pageIndex);
                List<ExtractedImage> pageImages = extractImagesFromPage(page, pageNumber);

                if (!pageImages.isEmpty()) {
                    content.addImageBlock(pageImages, position++);
                    totalImageCount += pageImages.size();
                }
            }

            content.getMetadata().put("totalImages", totalImageCount);
            log.info("📄 [PDF] 提取完成: {} 页, {} 张图片", document.getNumberOfPages(), totalImageCount);

            return content;
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

    /**
     * 提取页面中的图片
     */
    private List<ExtractedImage> extractImagesFromPage(PDPage page, int pageNumber) {
        List<ExtractedImage> images = new ArrayList<>();
        int imageIndexOnPage = 0;

        try {
            // 遍历页面资源中的所有图片
            if (page.getResources() != null && page.getResources().getXObjectNames() != null) {
                for (org.apache.pdfbox.cos.COSName name : page.getResources().getXObjectNames()) {
                    try {
                        org.apache.pdfbox.pdmodel.graphics.PDXObject xObject =
                                page.getResources().getXObject(name);

                        if (xObject instanceof PDImageXObject) {
                            PDImageXObject imageObject = (PDImageXObject) xObject;

                            // 提取图片
                            ExtractedImage image = extractPDFImage(imageObject, pageNumber, imageIndexOnPage++);
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
            // 获取图片数据
            BufferedImage bufferedImage = imageObject.getImage();

            // 转换为字节数组
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            String format = detectImageFormat(imageObject);
            ImageIO.write(bufferedImage, format, baos);
            byte[] imageData = baos.toByteArray();

            // 构建元数据
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("pageNumber", pageNumber);
            metadata.put("imageIndex", imageIndex);
            metadata.put("documentType", "PDF");
            metadata.put("width", bufferedImage.getWidth());
            metadata.put("height", bufferedImage.getHeight());
            metadata.put("colorSpace", imageObject.getColorSpace().getName());

            return ExtractedImage.builder()
                    .data(imageData)
                    .format(format)
                    .pageNumber(pageNumber)
                    .metadata(metadata)
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

        // 默认使用 PNG（无损格式）
        return "png";
    }

    /**
     * 使用 OCR 提取页面文本
     * ⭐ 仅在普通文本提取失败时使用（如扫描件、图片PDF）
     */
    private String extractPageTextByOCR(PDDocument document, int pageIndex, int pageNumber) {
        try {
            log.debug("📷 [PDF-OCR] 第 {} 页使用 OCR 提取文本", pageNumber);

            // 渲染页面为图片
            PDFRenderer renderer = new PDFRenderer(document);
            BufferedImage image = renderer.renderImageWithDPI(pageIndex, 300);

            // 使用反射调用 OCR 服务（避免硬依赖）
            java.lang.reflect.Method recognizeMethod =
                    ocrService.getClass().getMethod("recognizeText", BufferedImage.class);
            String text = (String) recognizeMethod.invoke(ocrService, image);

            if (text != null && !text.trim().isEmpty()) {
                log.info("✅ [PDF-OCR] 第 {} 页 OCR 识别成功: {} 字符", pageNumber, text.length());
                return text;
            } else {
                log.debug("⚠️ [PDF-OCR] 第 {} 页未识别到文字", pageNumber);
                return "";
            }

        } catch (Exception e) {
            log.warn("❌ [PDF-OCR] 第 {} 页 OCR 识别失败: {}", pageNumber, e.getMessage());
            return "";
        }
    }

    @Override
    public ValidationResult validate(ProcessingContext context) {
        if (context.getFileSize() > 100 * 1024 * 1024) {
            return ValidationResult.builder()
                    .valid(false)
                    .message("PDF 文件过大（超过100MB）")
                    .build();
        }

        return ValidationResult.builder()
                .valid(true)
                .message("验证通过")
                .build();
    }
}

