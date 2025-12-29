package top.yumbo.ai.omni.document.processor.starter.processor;

import lombok.extern.slf4j.Slf4j;
import org.apache.poi.hslf.usermodel.HSLFSlide;
import org.apache.poi.hslf.usermodel.HSLFSlideShow;
import org.apache.poi.hslf.usermodel.HSLFTextShape;
import org.apache.poi.xslf.usermodel.XMLSlideShow;
import org.apache.poi.xslf.usermodel.XSLFSlide;
import org.apache.poi.xslf.usermodel.XSLFTextShape;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.document.processor.AbstractDocumentProcessor;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;
import java.util.List;

/**
 * PowerPoint 文档处理器
 * (PowerPoint Document Processor)
 *
 * <p>处理策略：</p>
 * <ul>
 *   <li>将每张幻灯片渲染为高分辨率图片</li>
 *   <li>提取幻灯片中的文本内容作为上下文</li>
 *   <li>使用 Vision LLM 分析幻灯片图片</li>
 *   <li>结合文本和 Vision LLM 分析结果生成最终内容</li>
 * </ul>
 *
 * <p>配置说明：</p>
 * <ul>
 *   <li>默认启用（matchIfMissing = true），无需配置</li>
 *   <li>可通过 omni-agent.ppt.enabled=false 禁用</li>
 *   <li>可通过 omni-agent.ppt.render-scale 配置渲染缩放倍数（默认 2.0）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
@ConditionalOnProperty(
    prefix = "omni-agent.ppt",
    name = "enabled",
    havingValue = "true",
    matchIfMissing = true  // 默认启用，无需配置
)
public class PPTProcessor extends AbstractDocumentProcessor {

    private static final Set<String> SUPPORTED_EXTENSIONS = Set.of("ppt", "pptx");

    // 渲染分辨率缩放倍数（默认 2.0，提高清晰度）
    private static final double RENDER_SCALE = 2.0;

    @Override
    public boolean supports(String fileExtension) {
        return SUPPORTED_EXTENSIONS.contains(fileExtension.toLowerCase());
    }

    @Override
    public String getName() {
        return "PPTProcessor";
    }

    @Override
    public int getPriority() {
        return 30; // 高优先级（与 Excel、Word、PDF 相同）
    }

    @Override
    protected ExtractedContent extractContent(ProcessingContext context) throws Exception {
        String ext = context.getFileExtension().toLowerCase();

        if ("pptx".equals(ext)) {
            return extractPptxContent(context);
        } else {
            return extractPptContent(context);
        }
    }

    /**
     * 提取 .pptx 文档内容（PowerPoint 2007+）
     */
    private ExtractedContent extractPptxContent(ProcessingContext context) throws Exception {
        InputStream inputStream;
        if (context.getFileBytes() != null) {
            inputStream = new ByteArrayInputStream(context.getFileBytes());
        } else {
            inputStream = new FileInputStream(context.getFilePath());
        }

        try (XMLSlideShow ppt = new XMLSlideShow(inputStream)) {
            ExtractedContent content = new ExtractedContent();
            content.getMetadata().put("format", "pptx");

            List<XSLFSlide> slides = ppt.getSlides();
            content.getMetadata().put("totalSlides", slides.size());

            log.info("🔍 [PPT] PowerPoint 包含 {} 张幻灯片", slides.size());

            // ⭐ 先提取所有幻灯片的文字，用于构建上下文
            List<String> slideTexts = new ArrayList<>();
            for (XSLFSlide slide : slides) {
                StringBuilder slideText = new StringBuilder();
                slide.getShapes().forEach(shape -> {
                    if (shape instanceof XSLFTextShape) {
                        String text = ((XSLFTextShape) shape).getText();
                        if (text != null && !text.trim().isEmpty()) {
                            slideText.append(text).append(" ");
                        }
                    }
                });
                slideTexts.add(slideText.toString().trim());
            }

            // 获取幻灯片尺寸
            Dimension pageSize = ppt.getPageSize();
            int width = (int) (pageSize.getWidth() * RENDER_SCALE);
            int height = (int) (pageSize.getHeight() * RENDER_SCALE);

            log.debug("📐 幻灯片尺寸: 原始={}x{}, 渲染={}x{} (缩放{}x)",
                    (int)pageSize.getWidth(), (int)pageSize.getHeight(),
                    width, height, RENDER_SCALE);

            int position = 0;

            // 转换每张幻灯片为图片
            for (int i = 0; i < slides.size(); i++) {
                XSLFSlide slide = slides.get(i);
                int slideNumber = i + 1;

                // 添加幻灯片分隔
                if (i > 0) {
                    content.addTextBlock("\n---\n\n", position++);
                }
                content.addTextBlock("## 幻灯片 " + slideNumber + "\n\n", position++);

                // 如果幻灯片有文本，先添加文本
                String slideText = slideTexts.get(i);
                if (!slideText.isEmpty()) {
                    content.addTextBlock("**文本内容：**\n" + slideText + "\n\n", position++);
                }

                // 渲染幻灯片为图片
                BufferedImage img = renderSlide(slide, width, height);
                byte[] imageData = imageToBytes(img);

                // ⭐ 创建 metadata，包含文字内容和文档信息
                Map<String, Object> imageMetadata = new HashMap<>();
                imageMetadata.put("slideText", slideText);
                imageMetadata.put("fileName", context.getOriginalFileName());
                imageMetadata.put("totalSlides", slides.size());
                imageMetadata.put("slideNumber", slideNumber);
                imageMetadata.put("imageIndex", 0);

                // ⭐ 添加前几张幻灯片的文字作为上下文（帮助 Vision LLM 理解主题）
                if (i < 3) {
                    List<String> contextTexts = new ArrayList<>();
                    for (int j = 0; j < Math.min(3, slideTexts.size()); j++) {
                        if (!slideTexts.get(j).isEmpty()) {
                            contextTexts.add(slideTexts.get(j));
                        }
                    }
                    imageMetadata.put("documentContext", String.join(" | ", contextTexts));
                }

                // 创建 ExtractedImage
                ExtractedImage image = ExtractedImage.builder()
                        .data(imageData)
                        .format("png")
                        .pageNumber(slideNumber)
                        .position(new VisionLLMDocumentProcessor.ImagePosition(0, 0, width, height))
                        .metadata(imageMetadata)
                        .build();

                // 添加图片块
                content.addImageBlock(image, position++);

                log.debug("✅ [PPT] 成功渲染幻灯片 {} / {}", slideNumber, slides.size());
            }

            return content;
        }
    }

    /**
     * 提取 .ppt 文档内容（PowerPoint 97-2003）
     */
    private ExtractedContent extractPptContent(ProcessingContext context) throws Exception {
        InputStream inputStream;
        if (context.getFileBytes() != null) {
            inputStream = new ByteArrayInputStream(context.getFileBytes());
        } else {
            inputStream = new FileInputStream(context.getFilePath());
        }

        try (HSLFSlideShow ppt = new HSLFSlideShow(inputStream)) {
            ExtractedContent content = new ExtractedContent();
            content.getMetadata().put("format", "ppt");

            List<HSLFSlide> slides = ppt.getSlides();
            content.getMetadata().put("totalSlides", slides.size());

            log.info("🔍 [PPT] 旧版 PowerPoint 包含 {} 张幻灯片", slides.size());

            // ⭐ 先提取所有幻灯片的文字，用于构建上下文
            List<String> slideTexts = new ArrayList<>();
            for (HSLFSlide slide : slides) {
                StringBuilder slideText = new StringBuilder();
                slide.getShapes().forEach(shape -> {
                    if (shape instanceof HSLFTextShape) {
                        String text = ((HSLFTextShape) shape).getText();
                        if (text != null && !text.trim().isEmpty()) {
                            slideText.append(text).append(" ");
                        }
                    }
                });
                slideTexts.add(slideText.toString().trim());
            }

            // 获取幻灯片尺寸
            Dimension pageSize = ppt.getPageSize();
            int width = (int) (pageSize.getWidth() * RENDER_SCALE);
            int height = (int) (pageSize.getHeight() * RENDER_SCALE);

            log.debug("📐 旧版幻灯片尺寸: 原始={}x{}, 渲染={}x{} (缩放{}x)",
                    (int)pageSize.getWidth(), (int)pageSize.getHeight(),
                    width, height, RENDER_SCALE);

            int position = 0;

            // 转换每张幻灯片为图片
            for (int i = 0; i < slides.size(); i++) {
                HSLFSlide slide = slides.get(i);
                int slideNumber = i + 1;

                // 添加幻灯片分隔
                if (i > 0) {
                    content.addTextBlock("\n---\n\n", position++);
                }
                content.addTextBlock("## 幻灯片 " + slideNumber + "\n\n", position++);

                // 如果幻灯片有文本，先添加文本
                String slideText = slideTexts.get(i);
                if (!slideText.isEmpty()) {
                    content.addTextBlock("**文本内容：**\n" + slideText + "\n\n", position++);
                }

                // 渲染幻灯片为图片
                BufferedImage img = renderSlide(slide, width, height);
                byte[] imageData = imageToBytes(img);

                // ⭐ 创建 metadata，包含文字内容和文档信息
                Map<String, Object> imageMetadata = new HashMap<>();
                imageMetadata.put("slideText", slideText);
                imageMetadata.put("fileName", context.getOriginalFileName());
                imageMetadata.put("totalSlides", slides.size());
                imageMetadata.put("slideNumber", slideNumber);
                imageMetadata.put("imageIndex", 0);

                // ⭐ 添加前几张幻灯片的文字作为上下文
                if (i < 3) {
                    List<String> contextTexts = new ArrayList<>();
                    for (int j = 0; j < Math.min(3, slideTexts.size()); j++) {
                        if (!slideTexts.get(j).isEmpty()) {
                            contextTexts.add(slideTexts.get(j));
                        }
                    }
                    imageMetadata.put("documentContext", String.join(" | ", contextTexts));
                }

                // 创建 ExtractedImage
                ExtractedImage image = ExtractedImage.builder()
                        .data(imageData)
                        .format("png")
                        .pageNumber(slideNumber)
                        .position(new VisionLLMDocumentProcessor.ImagePosition(0, 0, width, height))
                        .metadata(imageMetadata)
                        .build();

                // 添加图片块
                content.addImageBlock(image, position++);

                log.debug("✅ [PPT] 成功渲染旧版幻灯片 {} / {}", slideNumber, slides.size());
            }

            return content;
        }
    }

    /**
     * 渲染 XSLF 幻灯片为高质量图片
     */
    private BufferedImage renderSlide(XSLFSlide slide, int width, int height) {
        BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = img.createGraphics();

        // ⭐ 设置高质量渲染参数
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        graphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);

        // 设置白色背景
        graphics.setPaint(Color.WHITE);
        graphics.fillRect(0, 0, width, height);

        // ⭐ 应用缩放变换
        graphics.scale(RENDER_SCALE, RENDER_SCALE);

        // 渲染幻灯片
        slide.draw(graphics);
        graphics.dispose();

        return img;
    }

    /**
     * 渲染 HSLF 幻灯片为高质量图片
     */
    private BufferedImage renderSlide(HSLFSlide slide, int width, int height) {
        BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = img.createGraphics();

        // ⭐ 设置高质量渲染参数
        graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        graphics.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        graphics.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        graphics.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);

        // 设置白色背景
        graphics.setPaint(Color.WHITE);
        graphics.fillRect(0, 0, width, height);

        // ⭐ 应用缩放变换
        graphics.scale(RENDER_SCALE, RENDER_SCALE);

        // 渲染幻灯片
        slide.draw(graphics);
        graphics.dispose();

        return img;
    }

    /**
     * 将 BufferedImage 转换为 PNG 字节数组
     */
    private byte[] imageToBytes(BufferedImage img) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ImageIO.write(img, "png", baos);
        return baos.toByteArray();
    }

    /**
     * 构建 PPT 专用的图片分析提示词
     * 覆盖父类方法，提供更适合 PPT 的提示词
     */
    @Override
    protected String buildImageAnalysisPrompt(ExtractedImage image) {
        StringBuilder prompt = new StringBuilder();

        Map<String, Object> metadata = image.getMetadata();
        if (metadata != null) {
            // 添加文档上下文
            if (metadata.containsKey("documentContext")) {
                String context = (String) metadata.get("documentContext");
                prompt.append("【文档主题】\n").append(context).append("\n\n");
            }

            // 添加当前幻灯片文本
            if (metadata.containsKey("slideText")) {
                String slideText = (String) metadata.get("slideText");
                if (!slideText.isEmpty()) {
                    prompt.append("【幻灯片文本】\n").append(slideText).append("\n\n");
                }
            }

            // 添加幻灯片信息
            if (metadata.containsKey("slideNumber") && metadata.containsKey("totalSlides")) {
                prompt.append("【位置】第 ").append(metadata.get("slideNumber"))
                      .append(" 张，共 ").append(metadata.get("totalSlides")).append(" 张\n\n");
            }
        }

        prompt.append("请分析这张幻灯片图片，提取以下信息：\n");
        prompt.append("1. 图表和图形内容（流程图、架构图、数据图表等）\n");
        prompt.append("2. 图片中的关键视觉元素\n");
        prompt.append("3. 布局和设计特点\n");
        prompt.append("4. 与文本内容的关联和补充信息\n");
        prompt.append("\n请用简洁清晰的语言描述，重点关注视觉信息。");

        return prompt.toString();
    }
}

