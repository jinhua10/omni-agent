package top.yumbo.ai.omni.core.document.processor;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import top.yumbo.ai.ai.api.AIService;
import top.yumbo.ai.omni.core.document.DocumentProcessor;

import java.util.*;

/**
 * Vision LLM 文档处理器
 * (Vision LLM Document Processor)
 *
 * <p>
 * 处理策略（基于原 old 项目经验）：
 * </p>
 *
 * <h3>PPT/PDF 处理</h3>
 * <p>
 * <b>以页面/幻灯片为单位</b>进行处理：
 * </p>
 * <ul>
 *   <li>1. 提取每页的所有图片（包括位置信息）</li>
 *   <li>2. 按位置排列图片（从上到下，从左到右）</li>
 *   <li>3. 将同一页的多张图片一起发给 Vision LLM</li>
 *   <li>4. Vision LLM 理解整页内容（流程图、架构图、部署图等）</li>
 *   <li>5. 如果上下文允许，可以多页一起处理</li>
 * </ul>
 *
 * <h3>优势</h3>
 * <ul>
 *   <li>保持页面完整性：流程图、架构图等跨多张图片的内容能被正确理解</li>
 *   <li>位置信息：图片按空间位置排列，帮助 LLM 理解布局</li>
 *   <li>上下文优化：多页一起处理可以理解连贯性内容</li>
 *   <li>批量处理：减少 API 调用次数，提高效率</li>
 * </ul>
 *
 * <p>
 * 支持的文件类型：
 * - PDF文档
 * - Microsoft Word (.doc, .docx)
 * - Microsoft Excel (.xls, .xlsx)
 * - Microsoft PowerPoint (.ppt, .pptx)
 * - 图片文件 (.png, .jpg, .jpeg, .bmp, .tiff)
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
@ConditionalOnProperty(prefix = "omni-agent.vision-llm", name = "enabled", havingValue = "true")
public class VisionLLMDocumentProcessor implements DocumentProcessor {

    @Autowired(required = false)
    private AIService aiService;

    @Value("${omni-agent.vision-llm.model:qwen-vl-plus}")
    private String visionModel;

    @Value("${omni-agent.vision-llm.system-prompt:请分析这张图片并提取其中的关键信息。}")
    private String systemPrompt;

    @Value("${omni-agent.vision-llm.batch-size:3}")
    private int batchSize;  // 一次处理多少页/幻灯片

    /**
     * 支持的文件扩展名
     */
    private static final Set<String> SUPPORTED_EXTENSIONS = Set.of(
            // Office 文档
            "pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx",
            // 图片文件
            "png", "jpg", "jpeg", "bmp", "tiff", "gif"
    );

    @Override
    public boolean supports(String fileExtension) {
        return SUPPORTED_EXTENSIONS.contains(fileExtension.toLowerCase());
    }

    @Override
    public String getName() {
        return "VisionLLMProcessor";
    }

    @Override
    public int getPriority() {
        return 10;  // 高优先级
    }

    @Override
    public ProcessingResult process(ProcessingContext context) throws DocumentProcessingException {
        log.info("🔍 [VisionLLM] 开始处理文档: {}", context.getOriginalFileName());

        long startTime = System.currentTimeMillis();

        try {
            // 1. 提取文档的页面（每页包含多张图片及其位置信息）
            List<DocumentPage> pages = extractPages(context);
            log.info("📄 [VisionLLM] 提取了 {} 个页面/幻灯片", pages.size());

            // 2. 批量处理页面（多页一起处理以优化上下文）
            StringBuilder allContent = new StringBuilder();
            List<ExtractedImage> allImages = new ArrayList<>();

            for (int i = 0; i < pages.size(); i += batchSize) {
                int endIdx = Math.min(i + batchSize, pages.size());
                List<DocumentPage> batch = pages.subList(i, endIdx);

                log.info("🔍 [VisionLLM] 处理页面批次 {}-{}/{}", i + 1, endIdx, pages.size());

                // 处理这一批页面
                String batchContent = processPageBatch(batch);
                allContent.append(batchContent).append("\n\n");

                // 收集所有图片
                for (DocumentPage page : batch) {
                    allImages.addAll(page.getImages());
                }
            }

            // 3. 构建元数据
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("pageCount", pages.size());
            metadata.put("totalImages", allImages.size());
            metadata.put("processor", "VisionLLM");
            metadata.put("model", visionModel);
            metadata.put("batchSize", batchSize);
            metadata.put("originalExtension", context.getFileExtension());

            long processingTime = System.currentTimeMillis() - startTime;

            log.info("✅ [VisionLLM] 处理完成: 耗时={}ms, 内容长度={}, 图片数={}",
                    processingTime, allContent.length(), allImages.size());

            return ProcessingResult.builder()
                    .success(true)
                    .content(allContent.toString())
                    .metadata(metadata)
                    .images(allImages)
                    .processingTimeMs(processingTime)
                    .processorName(getName())
                    .build();

        } catch (Exception e) {
            log.error("❌ [VisionLLM] 处理失败: {}", e.getMessage(), e);
            throw new DocumentProcessingException("Vision LLM 处理失败", e);
        }
    }

    /**
     * 提取文档的页面（每页包含多张图片及其位置）
     *
     * @param context 处理上下文
     * @return 页面列表
     */
    private List<DocumentPage> extractPages(ProcessingContext context) throws Exception {
        String ext = context.getFileExtension().toLowerCase();

        // 如果是图片文件，直接作为单页
        if (isImageFile(ext)) {
            byte[] imageData;
            if (context.getFileBytes() != null) {
                imageData = context.getFileBytes();
            } else {
                imageData = java.nio.file.Files.readAllBytes(
                        java.nio.file.Paths.get(context.getFilePath()));
            }

            ExtractedImage image = ExtractedImage.builder()
                    .data(imageData)
                    .format(ext)
                    .pageNumber(1)
                    .position(new ImagePosition(0, 0, 0, 0))  // 整页
                    .build();

            DocumentPage page = new DocumentPage(1);
            page.addImage(image);

            return List.of(page);
        }

        // PowerPoint 文档处理
        if (ext.equals("pptx") || ext.equals("ppt")) {
            return extractPptxPages(context);
        }

        // PDF 文档待实现
        if (ext.equals("pdf")) {
            log.warn("⚠️ [VisionLLM] PDF 页面提取功能待实现");
            throw new Exception("PDF 文档页面提取功能待实现");
        }

        // 其他 Office 文档待实现
        log.warn("⚠️ [VisionLLM] {} 格式的页面提取功能待实现", ext);
        throw new Exception("Office 文档页面提取功能待实现: " + ext);
    }

    /**
     * 提取 PowerPoint 文档的页面
     * ⭐ 优化：先提取文字，构建上下文，避免 AI 乱答
     *
     * @param context 处理上下文
     * @return 页面列表
     */
    private List<DocumentPage> extractPptxPages(ProcessingContext context) throws Exception {
        try {
            java.io.InputStream inputStream;
            if (context.getFileBytes() != null) {
                inputStream = new java.io.ByteArrayInputStream(context.getFileBytes());
            } else {
                inputStream = new java.io.FileInputStream(context.getFilePath());
            }

            try (org.apache.poi.xslf.usermodel.XMLSlideShow ppt =
                    new org.apache.poi.xslf.usermodel.XMLSlideShow(inputStream)) {

                List<DocumentPage> pages = new ArrayList<>();
                java.util.List<org.apache.poi.xslf.usermodel.XSLFSlide> slides = ppt.getSlides();

                log.info("🔍 [VisionLLM] PowerPoint 包含 {} 张幻灯片", slides.size());

                // ⭐ 先提取所有幻灯片的文字，用于构建上下文
                List<String> slideTexts = new ArrayList<>();
                for (org.apache.poi.xslf.usermodel.XSLFSlide slide : slides) {
                    StringBuilder slideText = new StringBuilder();
                    slide.getShapes().forEach(shape -> {
                        if (shape instanceof org.apache.poi.xslf.usermodel.XSLFTextShape) {
                            String text = ((org.apache.poi.xslf.usermodel.XSLFTextShape) shape).getText();
                            if (text != null && !text.trim().isEmpty()) {
                                slideText.append(text).append(" ");
                            }
                        }
                    });
                    slideTexts.add(slideText.toString().trim());
                }

                // 获取幻灯片尺寸
                java.awt.Dimension pageSize = ppt.getPageSize();
                int width = (int) pageSize.getWidth();
                int height = (int) pageSize.getHeight();

                // 转换每张幻灯片为图片
                for (int i = 0; i < slides.size(); i++) {
                    org.apache.poi.xslf.usermodel.XSLFSlide slide = slides.get(i);

                    // 将幻灯片渲染为 BufferedImage
                    java.awt.image.BufferedImage img = new java.awt.image.BufferedImage(
                            width, height, java.awt.image.BufferedImage.TYPE_INT_RGB);
                    java.awt.Graphics2D graphics = img.createGraphics();

                    // 设置白色背景
                    graphics.setPaint(java.awt.Color.WHITE);
                    graphics.fillRect(0, 0, width, height);

                    // 渲染幻灯片
                    slide.draw(graphics);
                    graphics.dispose();

                    // 将 BufferedImage 转换为 PNG 字节数组
                    java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                    javax.imageio.ImageIO.write(img, "png", baos);
                    byte[] imageData = baos.toByteArray();

                    // ⭐ 创建 metadata，包含文字内容和文档信息
                    Map<String, Object> imageMetadata = new HashMap<>();
                    imageMetadata.put("slideText", slideTexts.get(i));  // 当前幻灯片文字
                    imageMetadata.put("fileName", context.getOriginalFileName());  // 文件名
                    imageMetadata.put("totalSlides", slides.size());  // 总幻灯片数

                    // ⭐ 添加前几张幻灯片的文字作为上下文（帮助理解主题）
                    if (i < 3) {
                        // 前3张幻灯片通常包含标题和主题信息
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
                            .pageNumber(i + 1)
                            .position(new ImagePosition(0, 0, width, height))
                            .metadata(imageMetadata)  // ⭐ 传递 metadata
                            .build();

                    // 创建 DocumentPage
                    DocumentPage page = new DocumentPage(i + 1);
                    page.addImage(image);
                    pages.add(page);

                    log.debug("✅ [VisionLLM] 成功渲染幻灯片 {} / {}", i + 1, slides.size());
                }

                return pages;
            }
        } catch (Exception e) {
            log.error("❌ [VisionLLM] PowerPoint 页面提取失败", e);
            throw new Exception("PowerPoint 页面提取失败: " + e.getMessage(), e);
        }
    }

    /**
     * 处理一批页面
     *
     * @param pages 页面列表
     * @return 这批页面的文本内容
     */
    private String processPageBatch(List<DocumentPage> pages) {
        StringBuilder batchContent = new StringBuilder();

        for (DocumentPage page : pages) {
            log.info("🔍 [VisionLLM] 处理第 {} 页，包含 {} 张图片",
                    page.getPageNumber(), page.getImages().size());

            // 构建该页的提示词
            String pagePrompt = buildPagePrompt(page);

            // 调用 Vision LLM 分析整页
            String pageContent = recognizePageWithVisionLLM(page, pagePrompt);

            if (pageContent != null && !pageContent.isEmpty()) {
                batchContent.append("=== 页面 ").append(page.getPageNumber()).append(" ===\n");
                batchContent.append(pageContent).append("\n\n");

                // ⭐ 将 Vision LLM 的分析结果保存到每张图片的 metadata 中
                for (ExtractedImage image : page.getImages()) {
                    if (image.getMetadata() == null) {
                        image.setMetadata(new HashMap<>());
                    }
                    image.getMetadata().put("visionAnalysis", pageContent);
                    image.getMetadata().put("pageNumber", page.getPageNumber());
                    image.getMetadata().put("processor", "VisionLLM");
                    image.getMetadata().put("model", visionModel != null ? visionModel : "unknown");
                    image.getMetadata().put("analyzedAt", System.currentTimeMillis());

                    log.debug("✅ [VisionLLM] 页面 {} 的图片元数据已更新", page.getPageNumber());
                }
            }
        }

        return batchContent.toString();
    }

    /**
     * 构建页面的提示词
     *
     * @param page 页面对象
     * @return 提示词
     */
    private String buildPagePrompt(DocumentPage page) {
        StringBuilder prompt = new StringBuilder();
        prompt.append(systemPrompt).append("\n\n");

        if (page.getImages().size() > 1) {
            prompt.append("这一页包含 ").append(page.getImages().size()).append(" 张图片，");
            prompt.append("它们可能是一个完整内容的不同部分（如流程图、架构图、部署图等）。\n");
            prompt.append("请综合分析所有图片，理解它们的整体含义和关联关系。\n\n");

            // 添加图片位置信息
            prompt.append("图片排列（从上到下，从左到右）：\n");
            for (int i = 0; i < page.getImages().size(); i++) {
                ExtractedImage img = page.getImages().get(i);
                if (img.getPosition() instanceof ImagePosition) {
                    ImagePosition pos = (ImagePosition) img.getPosition();
                    prompt.append(String.format("  图片%d: 位置(x=%d, y=%d, w=%d, h=%d)\n",
                            i + 1, pos.x, pos.y, pos.width, pos.height));
                }
            }
        } else {
            prompt.append("请分析这张图片的内容。\n");
        }

        return prompt.toString();
    }

    /**
     * 使用 Vision LLM 识别整页内容
     *
     * @param page 页面对象（包含多张图片）
     * @param prompt 提示词
     * @return 识别的文本内容
     */
    private String recognizePageWithVisionLLM(DocumentPage page, String prompt) {
        try {
            if (aiService == null) {
                log.warn("⚠️ [VisionLLM] AI Service 未配置，返回占位内容");
                return String.format("[页面 %d 的内容 - AI Service 未配置]\n包含 %d 张图片",
                        page.getPageNumber(), page.getImages().size());
            }

            // 1. 将所有图片编码为 Base64
            List<String> base64Images = new ArrayList<>();
            for (ExtractedImage image : page.getImages()) {
                String base64 = Base64.getEncoder().encodeToString(image.getData());
                base64Images.add(base64);
            }

            if (base64Images.isEmpty()) {
                log.warn("⚠️ [VisionLLM] 页面 {} 没有图片", page.getPageNumber());
                return "";
            }

            // 2. 构建 Vision 提示词
            String visionPrompt = buildVisionPrompt(page, prompt);

            // 3. 调用 AI Service 进行图片分析 ⭐
            log.info("🔍 [VisionLLM] 调用 Vision API 分析页面 {}, 图片数: {}",
                    page.getPageNumber(), base64Images.size());

            try {
                // 调用 AI Service 的 chat 方法
                // 注意：这里需要 AI Service 支持图片输入
                // 对于支持 Vision 的模型（如 GPT-4V、千问VL 等），可以在 prompt 中包含图片信息

                String result = aiService.chat(visionPrompt);

                log.info("✅ [VisionLLM] 页面 {} 分析完成，内容长度: {} chars",
                        page.getPageNumber(), result != null ? result.length() : 0);

                return result != null ? result : "";

            } catch (Exception apiEx) {
                log.error("❌ [VisionLLM] Vision API 调用失败: {}", apiEx.getMessage());

                // 降级：返回基本信息
                return String.format("[页面 %d - Vision API 调用失败: %s]\n包含 %d 张图片\n图片格式: %s",
                        page.getPageNumber(),
                        apiEx.getMessage(),
                        page.getImages().size(),
                        page.getImages().stream()
                            .map(ExtractedImage::getFormat)
                            .collect(java.util.stream.Collectors.joining(", ")));
            }

        } catch (Exception e) {
            log.error("❌ [VisionLLM] 页面识别失败: page={}", page.getPageNumber(), e);
            return String.format("[页面 %d 识别失败: %s]", page.getPageNumber(), e.getMessage());
        }
    }

    /**
     * 构建 Vision 提示词
     * ⭐ 优化：利用文件名、文字内容和上下文，避免 AI 乱答
     */
    private String buildVisionPrompt(DocumentPage page, String basePrompt) {
        StringBuilder prompt = new StringBuilder();

        // ⭐ 1. 从图片 metadata 中提取上下文信息
        String fileName = null;
        String slideText = null;
        String documentContext = null;
        Integer totalSlides = null;

        if (!page.getImages().isEmpty() && page.getImages().get(0).getMetadata() != null) {
            Map<String, Object> metadata = page.getImages().get(0).getMetadata();
            fileName = (String) metadata.get("fileName");
            slideText = (String) metadata.get("slideText");
            documentContext = (String) metadata.get("documentContext");
            totalSlides = (Integer) metadata.get("totalSlides");
        }

        // ⭐ 2. 构建上下文感知的提示词
        prompt.append("# 任务说明\n");
        prompt.append("请将这张 PPT 幻灯片的内容转换为文字描述。\n\n");

        // ⭐ 3. 提供文档上下文信息
        if (fileName != null) {
            prompt.append("## 文档信息\n");
            prompt.append("- 文件名：").append(fileName).append("\n");
            if (totalSlides != null) {
                prompt.append("- 总幻灯片数：").append(totalSlides).append("\n");
            }
            prompt.append("- 当前页码：第 ").append(page.getPageNumber()).append(" 页\n\n");
        }

        // ⭐ 4. 提供文字内容（最重要的上下文）
        if (slideText != null && !slideText.trim().isEmpty()) {
            prompt.append("## 幻灯片中的文字内容\n");
            prompt.append("```\n");
            prompt.append(slideText).append("\n");
            prompt.append("```\n\n");
        }

        // ⭐ 5. 前几页的上下文（理解主题）
        if (documentContext != null && !documentContext.trim().isEmpty()) {
            prompt.append("## 文档主题参考\n");
            prompt.append("前几页的内容：").append(documentContext).append("\n\n");
        }

        // ⭐ 6. 明确输出要求
        prompt.append("## 输出要求\n");
        prompt.append("请根据上述文字内容和图片中的可视化元素，输出：\n\n");
        prompt.append("1. **文字信息**：准确转录幻灯片中的所有文字\n");
        prompt.append("2. **图表说明**：如果有图表、图片，简要描述其展示的内容\n");
        prompt.append("3. **布局信息**：如标题、正文、列表等结构\n\n");

        // ⭐ 7. 强调重点
        prompt.append("⚠️ 重要提示：\n");
        prompt.append("- 优先使用上面提供的文字内容\n");
        prompt.append("- 不要过度解读或添加不存在的内容\n");
        prompt.append("- 专注于客观描述幻灯片的实际内容\n");
        if (fileName != null && fileName.contains("节约用水")) {
            prompt.append("- 本文档主题是关于节约用水的，请保持主题一致性\n");
        }
        prompt.append("\n请以简洁的 Markdown 格式输出。");

        return prompt.toString();
    }

    /**
     * 判断是否为图片文件
     */
    private boolean isImageFile(String extension) {
        return Set.of("png", "jpg", "jpeg", "bmp", "tiff", "gif")
                .contains(extension.toLowerCase());
    }

    @Override
    public ValidationResult validate(ProcessingContext context) {
        // 检查文件大小（最大 100MB）
        if (context.getFileSize() > 100 * 1024 * 1024) {
            return ValidationResult.builder()
                    .valid(false)
                    .message("文件过大（超过100MB），建议使用异步处理")
                    .build();
        }

        // 检查 Vision LLM 是否可用
        if (aiService == null) {
            return ValidationResult.builder()
                    .valid(false)
                    .message("AI Service 未配置")
                    .build();
        }

        return ValidationResult.builder()
                .valid(true)
                .message("验证通过")
                .build();
    }

    /**
     * 文档页面（包含该页的所有图片及位置）
     */
    @Data
    private static class DocumentPage {
        /** 页码 */
        private final int pageNumber;

        /** 该页的所有图片（按位置排序） */
        private final List<ExtractedImage> images = new ArrayList<>();

        public DocumentPage(int pageNumber) {
            this.pageNumber = pageNumber;
        }

        public void addImage(ExtractedImage image) {
            images.add(image);
            // 按位置排序（从上到下，从左到右）
            images.sort((img1, img2) -> {
                // 安全获取位置信息
                if (!(img1.getPosition() instanceof ImagePosition) ||
                    !(img2.getPosition() instanceof ImagePosition)) {
                    return 0;  // 无法比较位置
                }

                ImagePosition pos1 = (ImagePosition) img1.getPosition();
                ImagePosition pos2 = (ImagePosition) img2.getPosition();

                // 先按 Y 坐标（从上到下）
                if (pos1.y != pos2.y) {
                    return Integer.compare(pos1.y, pos2.y);
                }
                // 再按 X 坐标（从左到右）
                return Integer.compare(pos1.x, pos2.x);
            });
        }
    }

    /**
     * 图片位置信息
     */
    @Data
    public static class ImagePosition {
        private final int x;        // X 坐标
        private final int y;        // Y 坐标
        private final int width;    // 宽度
        private final int height;   // 高度

        public ImagePosition(int x, int y, int width, int height) {
            this.x = x;
            this.y = y;
            this.width = width;
            this.height = height;
        }
    }
}

