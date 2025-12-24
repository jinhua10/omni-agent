package top.yumbo.ai.omni.core.document.processor;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import top.yumbo.ai.ai.api.AIService;
import top.yumbo.ai.ai.api.config.VisionLLMBatchProcessingProperties;
import top.yumbo.ai.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.core.document.DocumentProcessor;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

/**
 * Vision LLM 文档处理器
 * (Vision LLM Document Processor)
 *
 * <p>
 * 处理策略（优化版）：
 * </p>
 *
 * <h3>PPT/PDF 处理</h3>
 * <p>
 * <b>智能批处理 + 并行处理</b>：
 * </p>
 * <ul>
 *   <li>1. 提取每页的所有图片（包括位置信息）</li>
 *   <li>2. 根据上下文大小预判断，智能分批（尽可能多页一起处理）</li>
 *   <li>3. 多个批次并行处理，提高速度</li>
 *   <li>4. Vision LLM 理解整页内容（流程图、架构图、部署图等）</li>
 * </ul>
 *
 * <h3>优势</h3>
 * <ul>
 *   <li>智能批处理：根据上下文大小动态决定批次大小</li>
 *   <li>并行处理：多个批次并行，大幅提升处理速度</li>
 *   <li>保持页面完整性：流程图、架构图等跨多张图片的内容能被正确理解</li>
 *   <li>位置信息：图片按空间位置排列，帮助 LLM 理解布局</li>
 *   <li>减少API调用：智能合并请求，降低成本</li>
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

    /**
     * 用于在线程内透传 ProcessingContext（支持并行 batch 时也能获取 options）
     */
    private final ThreadLocal<ProcessingContext> processingContextThreadLocal = new ThreadLocal<>();

    // ⭐ 使用专门的 Vision AI Service
    @Autowired(required = false)
    @Qualifier("visionAIService")
    private AIService visionAIService;

    // 备用：如果没有 visionAIService，使用通用 aiService
    @Autowired(required = false)
    private AIService aiService;

    @Value("${omni-agent.vision-llm.model:qwen-vl-plus}")
    private String visionModel;

    @Value("${omni-agent.vision-llm.system-prompt:请分析这张图片并提取其中的关键信息。}")
    private String systemPrompt;

    // ⭐ 批处理配置
    @Autowired(required = false)
    private VisionLLMBatchProcessingProperties batchProcessingConfig;

    // ⭐ Vision LLM 线程池
    @Autowired(required = false)
    @Qualifier("visionLlmExecutor")
    private Executor visionLlmExecutor;

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
        processingContextThreadLocal.set(context);
        try {
            log.info("🔍 [VisionLLM] 开始处理文档: {}", context.getOriginalFileName());

            long startTime = System.currentTimeMillis();

            try {
                // 1. 提取文档的页面（每页包含多张图片及其位置信息）
                List<DocumentPage> pages = extractPages(context);
                log.info("📄 [VisionLLM] 提取了 {} 个页面/幻灯片", pages.size());

                // ⭐ 2. 智能分批：根据上下文大小预判断
                List<List<DocumentPage>> batches = smartBatching(pages);
                log.info("📦 [VisionLLM] 智能分批完成: {} 个批次", batches.size());
                for (int i = 0; i < batches.size(); i++) {
                    log.debug("📦 [VisionLLM] 批次 #{}: {} 个页面", i + 1, batches.get(i).size());
                }

                // ⭐ 2.1 检查是否为流式模式，并发送批次信息
                boolean isStreamingMode = context != null
                    && context.getOptions() != null
                    && Boolean.TRUE.equals(context.getOptions().get("streaming"));

                if (isStreamingMode && context.getOptions().get("streamCallback") instanceof java.util.function.Consumer) {
                    @SuppressWarnings("unchecked")
                    java.util.function.Consumer<String> callback =
                        (java.util.function.Consumer<String>) context.getOptions().get("streamCallback");

                    // ⭐ 发送批次信息（特殊标记 + JSON）
                    String batchInfo = String.format(
                        "BATCH_INFO:{\"totalBatches\":%d,\"totalPages\":%d}\n",
                        batches.size(), pages.size()
                    );
                    callback.accept(batchInfo);
                    log.info("📤 [VisionLLM] 已发送批次信息: {} 批次, {} 页面", batches.size(), pages.size());
                }

                // ⭐ 3. 处理所有批次
                List<BatchProcessingResult> batchResults;


                // ⭐ 流式模式必须串行处理，确保输出顺序正确，避免内容混乱
                if (isStreamingMode) {
                    log.info("🔄 [VisionLLM] 流式模式：使用串行处理确保输出顺序");
                    batchResults = processPageBatchesSequentially(batches, context);
                } else if (visionLlmExecutor != null && batches.size() > 1) {
                    // 非流式模式才使用并行处理
                    log.info("🚀 [VisionLLM] 非流式模式：使用并行处理提升速度");
                    batchResults = processPageBatchesInParallel(batches, context);
                } else {
                    // 串行处理（无线程池或只有一个批次）
                    batchResults = processPageBatchesSequentially(batches, context);
                }

                // 4. 合并结果
                StringBuilder allContent = new StringBuilder();
                List<ExtractedImage> allImages = new ArrayList<>();

                // 按批次顺序合并（保持页面顺序）
                for (BatchProcessingResult batchResult : batchResults) {
                    allContent.append(batchResult.getContent()).append("\n\n");
                    allImages.addAll(batchResult.getImages());
                }

                // 5. 构建元数据
                Map<String, Object> metadata = new HashMap<>();
                metadata.put("pageCount", pages.size());
                metadata.put("totalImages", allImages.size());
                metadata.put("processor", "VisionLLM");
                metadata.put("model", visionModel);
                metadata.put("batchCount", batches.size());
                metadata.put("parallelProcessing", visionLlmExecutor != null && batches.size() > 1);
                metadata.put("originalExtension", context.getFileExtension());

                long processingTime = System.currentTimeMillis() - startTime;

                log.info("✅ [VisionLLM] 处理完成: 耗时={}ms, 批次数={}, 内容长度={}, 图片数={}",
                        processingTime, batches.size(), allContent.length(), allImages.size());

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
        } finally {
            processingContextThreadLocal.remove();
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
        if (ext.equals("pptx")) {
            return extractPptxPages(context);  // 新格式，基于 XML
        } else if (ext.equals("ppt")) {
            return extractPptPages(context);   // 旧格式，二进制格式
        }

        // Word 文档处理（提取图片）
        if (ext.equals("docx")) {
            return extractDocxPages(context);  // 新格式
        } else if (ext.equals("doc")) {
            return extractDocPages(context);   // 旧格式
        }

        // Excel 文档处理（提取图片）⭐
        if (ext.equals("xlsx")) {
            return extractXlsxPages(context);  // 新格式
        } else if (ext.equals("xls")) {
            return extractXlsPages(context);   // 旧格式
        }

        // PDF 文档处理 ⭐
        if (ext.equals("pdf")) {
            return extractPdfPages(context);
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
     * 提取旧版 PowerPoint 文档的页面 (.ppt 格式)
     * ⭐ 使用 HSLFSlideShow 处理二进制格式的 PPT
     *
     * @param context 处理上下文
     * @return 页面列表
     */
    private List<DocumentPage> extractPptPages(ProcessingContext context) throws Exception {
        try {
            java.io.InputStream inputStream;
            if (context.getFileBytes() != null) {
                inputStream = new java.io.ByteArrayInputStream(context.getFileBytes());
            } else {
                inputStream = new java.io.FileInputStream(context.getFilePath());
            }

            try (org.apache.poi.hslf.usermodel.HSLFSlideShow ppt =
                    new org.apache.poi.hslf.usermodel.HSLFSlideShow(inputStream)) {

                List<DocumentPage> pages = new ArrayList<>();
                java.util.List<org.apache.poi.hslf.usermodel.HSLFSlide> slides = ppt.getSlides();

                log.info("🔍 [VisionLLM] 旧版 PowerPoint 包含 {} 张幻灯片", slides.size());

                // ⭐ 先提取所有幻灯片的文字，用于构建上下文
                List<String> slideTexts = new ArrayList<>();
                for (org.apache.poi.hslf.usermodel.HSLFSlide slide : slides) {
                    StringBuilder slideText = new StringBuilder();
                    slide.getShapes().forEach(shape -> {
                        if (shape instanceof org.apache.poi.hslf.usermodel.HSLFTextShape) {
                            String text = ((org.apache.poi.hslf.usermodel.HSLFTextShape) shape).getText();
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
                    org.apache.poi.hslf.usermodel.HSLFSlide slide = slides.get(i);

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
                    imageMetadata.put("slideText", slideTexts.get(i));
                    imageMetadata.put("fileName", context.getOriginalFileName());
                    imageMetadata.put("totalSlides", slides.size());

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
                            .pageNumber(i + 1)
                            .position(new ImagePosition(0, 0, width, height))
                            .metadata(imageMetadata)
                            .build();

                    DocumentPage page = new DocumentPage(i + 1);
                    page.addImage(image);
                    pages.add(page);

                    log.debug("✅ [VisionLLM] 成功渲染旧版幻灯片 {} / {}", i + 1, slides.size());
                }

                return pages;
            }
        } catch (Exception e) {
            log.error("❌ [VisionLLM] 旧版 PowerPoint 页面提取失败", e);
            throw new Exception("旧版 PowerPoint 页面提取失败: " + e.getMessage(), e);
        }
    }

    /**
     * 提取新版 Word 文档的图片 (.docx 格式)
     * ⭐ 将每张图片作为一页处理，支持批处理和并行
     *
     * @param context 处理上下文
     * @return 页面列表
     */
    private List<DocumentPage> extractDocxPages(ProcessingContext context) throws Exception {
        try {
            java.io.InputStream inputStream;
            if (context.getFileBytes() != null) {
                inputStream = new java.io.ByteArrayInputStream(context.getFileBytes());
            } else {
                inputStream = new java.io.FileInputStream(context.getFilePath());
            }

            try (org.apache.poi.xwpf.usermodel.XWPFDocument document =
                    new org.apache.poi.xwpf.usermodel.XWPFDocument(inputStream)) {

                List<org.apache.poi.xwpf.usermodel.XWPFPictureData> pictures = document.getAllPictures();
                log.info("🔍 [VisionLLM] Word 文档包含 {} 张图片", pictures.size());

                if (pictures.isEmpty()) {
                    log.warn("⚠️ [VisionLLM] Word 文档没有图片");
                    throw new Exception("Word 文档没有图片");
                }

                // 提取文本内容
                StringBuilder textContent = new StringBuilder();
                for (org.apache.poi.xwpf.usermodel.XWPFParagraph paragraph : document.getParagraphs()) {
                    String text = paragraph.getText();
                    if (text != null && !text.trim().isEmpty()) {
                        textContent.append(text).append(" ");
                    }
                }

                // ⭐ 每张图片作为一页，支持批处理和并行
                List<DocumentPage> pages = new ArrayList<>();
                for (int i = 0; i < pictures.size(); i++) {
                    org.apache.poi.xwpf.usermodel.XWPFPictureData picture = pictures.get(i);
                    byte[] imageData = picture.getData();

                    // 创建 metadata
                    Map<String, Object> imageMetadata = new HashMap<>();
                    imageMetadata.put("documentText", textContent.toString().trim());
                    imageMetadata.put("fileName", context.getOriginalFileName());
                    imageMetadata.put("totalImages", pictures.size());
                    imageMetadata.put("imageIndex", i);
                    imageMetadata.put("documentType", "Word");

                    // 创建 ExtractedImage
                    ExtractedImage image = ExtractedImage.builder()
                            .data(imageData)
                            .format(picture.suggestFileExtension())
                            .pageNumber(i + 1)
                            .position(new ImagePosition(0, 0, 0, 0))
                            .metadata(imageMetadata)
                            .build();

                    // ⭐ 每张图片一页
                    DocumentPage page = new DocumentPage(i + 1);
                    page.addImage(image);
                    pages.add(page);
                }

                log.info("✅ [VisionLLM] Word 文档图片提取完成: {} 页（每页1张图片）", pages.size());
                return pages;
            }
        } catch (Exception e) {
            log.error("❌ [VisionLLM] Word 文档页面提取失败", e);
            throw new Exception("Word 文档页面提取失败: " + e.getMessage(), e);
        }
    }

    /**
     * 提取旧版 Word 文档的图片 (.doc 格式)
     * ⭐ 将每张图片作为一页处理，支持批处理和并行
     *
     * @param context 处理上下文
     * @return 页面列表
     */
    private List<DocumentPage> extractDocPages(ProcessingContext context) throws Exception {
        try {
            java.io.InputStream inputStream;
            if (context.getFileBytes() != null) {
                inputStream = new java.io.ByteArrayInputStream(context.getFileBytes());
            } else {
                inputStream = new java.io.FileInputStream(context.getFilePath());
            }

            try (org.apache.poi.hwpf.HWPFDocument document =
                    new org.apache.poi.hwpf.HWPFDocument(inputStream)) {

                List<org.apache.poi.hwpf.usermodel.Picture> pictures =
                    document.getPicturesTable().getAllPictures();
                log.info("🔍 [VisionLLM] 旧版 Word 文档包含 {} 张图片", pictures.size());

                if (pictures.isEmpty()) {
                    log.warn("⚠️ [VisionLLM] 旧版 Word 文档没有图片");
                    throw new Exception("旧版 Word 文档没有图片");
                }

                // 提取文本内容
                org.apache.poi.hwpf.extractor.WordExtractor extractor =
                    new org.apache.poi.hwpf.extractor.WordExtractor(document);
                String textContent = extractor.getText();

                // ⭐ 每张图片作为一页，支持批处理和并行
                List<DocumentPage> pages = new ArrayList<>();
                for (int i = 0; i < pictures.size(); i++) {
                    org.apache.poi.hwpf.usermodel.Picture picture = pictures.get(i);
                    byte[] imageData = picture.getContent();

                    // 创建 metadata
                    Map<String, Object> imageMetadata = new HashMap<>();
                    imageMetadata.put("documentText", textContent.trim());
                    imageMetadata.put("fileName", context.getOriginalFileName());
                    imageMetadata.put("totalImages", pictures.size());
                    imageMetadata.put("imageIndex", i);
                    imageMetadata.put("documentType", "Word");

                    // 创建 ExtractedImage
                    ExtractedImage image = ExtractedImage.builder()
                            .data(imageData)
                            .format(picture.suggestFileExtension())
                            .pageNumber(i + 1)
                            .position(new ImagePosition(0, 0, 0, 0))
                            .metadata(imageMetadata)
                            .build();

                    // ⭐ 每张图片一页
                    DocumentPage page = new DocumentPage(i + 1);
                    page.addImage(image);
                    pages.add(page);
                }

                log.info("✅ [VisionLLM] 旧版 Word 文档图片提取完成: {} 页（每页1张图片）", pages.size());
                return pages;
            }
        } catch (Exception e) {
            log.error("❌ [VisionLLM] 旧版 Word 文档页面提取失败", e);
            throw new Exception("旧版 Word 文档页面提取失败: " + e.getMessage(), e);
        }
    }

    /**
     * 提取新版 Excel 文档的图片 (.xlsx 格式)
     * ⭐ 每个工作表的每张图片作为一页，支持批处理和并行
     *
     * @param context 处理上下文
     * @return 页面列表
     */
    private List<DocumentPage> extractXlsxPages(ProcessingContext context) throws Exception {
        try {
            java.io.InputStream inputStream;
            if (context.getFileBytes() != null) {
                inputStream = new java.io.ByteArrayInputStream(context.getFileBytes());
            } else {
                inputStream = new java.io.FileInputStream(context.getFilePath());
            }

            try (org.apache.poi.xssf.usermodel.XSSFWorkbook workbook =
                    new org.apache.poi.xssf.usermodel.XSSFWorkbook(inputStream)) {

                List<DocumentPage> pages = new ArrayList<>();
                int pageNumber = 1;

                for (int sheetIdx = 0; sheetIdx < workbook.getNumberOfSheets(); sheetIdx++) {
                    org.apache.poi.xssf.usermodel.XSSFSheet sheet = workbook.getSheetAt(sheetIdx);
                    org.apache.poi.xssf.usermodel.XSSFDrawing drawing = sheet.getDrawingPatriarch();

                    if (drawing != null) {
                        for (org.apache.poi.xssf.usermodel.XSSFShape shape : drawing.getShapes()) {
                            if (shape instanceof org.apache.poi.xssf.usermodel.XSSFPicture picture) {

                                try {
                                    org.apache.poi.xssf.usermodel.XSSFPictureData pictureData = picture.getPictureData();
                                    byte[] imageData = pictureData.getData();

                                    // 获取图片位置
                                    org.apache.poi.xssf.usermodel.XSSFClientAnchor anchor = picture.getClientAnchor();
                                    String location = String.format("工作表[%s] 第%d行, 第%d列",
                                            sheet.getSheetName(), anchor.getRow1() + 1, anchor.getCol1() + 1);

                                    // 创建 metadata
                                    Map<String, Object> imageMetadata = new HashMap<>();
                                    imageMetadata.put("fileName", context.getOriginalFileName());
                                    imageMetadata.put("sheetName", sheet.getSheetName());
                                    imageMetadata.put("sheetIndex", sheetIdx);
                                    imageMetadata.put("location", location);
                                    imageMetadata.put("documentType", "Excel");

                                    // 创建 ExtractedImage
                                    ExtractedImage image = ExtractedImage.builder()
                                            .data(imageData)
                                            .format(pictureData.suggestFileExtension())
                                            .pageNumber(pageNumber)
                                            .position(new ImagePosition(anchor.getCol1(), anchor.getRow1(), 0, 0))
                                            .metadata(imageMetadata)
                                            .build();

                                    // ⭐ 每张图片一页
                                    DocumentPage page = new DocumentPage(pageNumber);
                                    page.addImage(image);
                                    pages.add(page);
                                    pageNumber++;

                                } catch (Exception e) {
                                    log.warn("提取 Excel 工作表 {} 中的图片失败", sheet.getSheetName(), e);
                                }
                            }
                        }
                    }
                }

                log.info("✅ [VisionLLM] Excel 文档图片提取完成: {} 页（每页1张图片）", pages.size());
                return pages.isEmpty() ? List.of() : pages;
            }
        } catch (Exception e) {
            log.error("❌ [VisionLLM] Excel 文档页面提取失败", e);
            throw new Exception("Excel 文档页面提取失败: " + e.getMessage(), e);
        }
    }

    /**
     * 提取旧版 Excel 文档的图片 (.xls 格式)
     * ⭐ 每个工作表的每张图片作为一页，支持批处理和并行
     *
     * @param context 处理上下文
     * @return 页面列表
     */
    private List<DocumentPage> extractXlsPages(ProcessingContext context) throws Exception {
        try {
            java.io.InputStream inputStream;
            if (context.getFileBytes() != null) {
                inputStream = new java.io.ByteArrayInputStream(context.getFileBytes());
            } else {
                inputStream = new java.io.FileInputStream(context.getFilePath());
            }

            try (org.apache.poi.hssf.usermodel.HSSFWorkbook workbook =
                    new org.apache.poi.hssf.usermodel.HSSFWorkbook(inputStream)) {

                List<DocumentPage> pages = new ArrayList<>();
                int pageNumber = 1;

                for (int sheetIdx = 0; sheetIdx < workbook.getNumberOfSheets(); sheetIdx++) {
                    org.apache.poi.hssf.usermodel.HSSFSheet sheet = workbook.getSheetAt(sheetIdx);
                    org.apache.poi.hssf.usermodel.HSSFPatriarch patriarch = sheet.getDrawingPatriarch();

                    if (patriarch != null) {
                        for (org.apache.poi.hssf.usermodel.HSSFShape shape : patriarch.getChildren()) {
                            if (shape instanceof org.apache.poi.hssf.usermodel.HSSFPicture) {
                                org.apache.poi.hssf.usermodel.HSSFPicture picture =
                                        (org.apache.poi.hssf.usermodel.HSSFPicture) shape;

                                try {
                                    org.apache.poi.hssf.usermodel.HSSFPictureData pictureData = picture.getPictureData();
                                    byte[] imageData = pictureData.getData();

                                    // 获取图片位置
                                    org.apache.poi.hssf.usermodel.HSSFClientAnchor anchor = picture.getClientAnchor();
                                    String location = String.format("工作表[%s] 第%d行, 第%d列",
                                            sheet.getSheetName(), anchor.getRow1() + 1, anchor.getCol1() + 1);

                                    // 创建 metadata
                                    Map<String, Object> imageMetadata = new HashMap<>();
                                    imageMetadata.put("fileName", context.getOriginalFileName());
                                    imageMetadata.put("sheetName", sheet.getSheetName());
                                    imageMetadata.put("sheetIndex", sheetIdx);
                                    imageMetadata.put("location", location);
                                    imageMetadata.put("documentType", "Excel");

                                    // 创建 ExtractedImage
                                    ExtractedImage image = ExtractedImage.builder()
                                            .data(imageData)
                                            .format(pictureData.suggestFileExtension())
                                            .pageNumber(pageNumber)
                                            .position(new ImagePosition(anchor.getCol1(), anchor.getRow1(), 0, 0))
                                            .metadata(imageMetadata)
                                            .build();

                                    // ⭐ 每张图片一页
                                    DocumentPage page = new DocumentPage(pageNumber);
                                    page.addImage(image);
                                    pages.add(page);
                                    pageNumber++;

                                } catch (Exception e) {
                                    log.warn("提取旧版 Excel 工作表 {} 中的图片失败", sheet.getSheetName(), e);
                                }
                            }
                        }
                    }
                }

                log.info("✅ [VisionLLM] 旧版 Excel 文档图片提取完成: {} 页（每页1张图片）", pages.size());
                return pages.isEmpty() ? List.of() : pages;
            }
        } catch (Exception e) {
            log.error("❌ [VisionLLM] 旧版 Excel 文档页面提取失败", e);
            throw new Exception("旧版 Excel 文档页面提取失败: " + e.getMessage(), e);
        }
    }

    /**
     * 提取 PDF 文档的页面
     * ⭐ 每页作为一个图片，支持批处理和并行
     *
     * @param context 处理上下文
     * @return 页面列表
     */
    private List<DocumentPage> extractPdfPages(ProcessingContext context) throws Exception {
        try {
            java.io.InputStream inputStream;
            if (context.getFileBytes() != null) {
                inputStream = new java.io.ByteArrayInputStream(context.getFileBytes());
            } else {
                inputStream = new java.io.FileInputStream(context.getFilePath());
            }

            try (org.apache.pdfbox.pdmodel.PDDocument document =
                    org.apache.pdfbox.pdmodel.PDDocument.load(inputStream)) {

                int pageCount = document.getNumberOfPages();
                log.info("🔍 [VisionLLM] PDF 文档包含 {} 页", pageCount);

                List<DocumentPage> pages = new ArrayList<>();
                org.apache.pdfbox.rendering.PDFRenderer pdfRenderer =
                    new org.apache.pdfbox.rendering.PDFRenderer(document);

                for (int i = 0; i < pageCount; i++) {
                    try {
                        // 1. 提取页面文本
                        org.apache.pdfbox.text.PDFTextStripper textStripper =
                            new org.apache.pdfbox.text.PDFTextStripper();
                        textStripper.setStartPage(i + 1);
                        textStripper.setEndPage(i + 1);
                        String pageText = textStripper.getText(document);

                        // 2. 将页面渲染为图片（300 DPI，高质量）
                        java.awt.image.BufferedImage bufferedImage =
                            pdfRenderer.renderImageWithDPI(i, 300,
                                org.apache.pdfbox.rendering.ImageType.RGB);

                        // 3. 将 BufferedImage 转换为 PNG 字节数组
                        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                        javax.imageio.ImageIO.write(bufferedImage, "png", baos);
                        byte[] imageData = baos.toByteArray();

                        // 4. 创建 metadata
                        Map<String, Object> imageMetadata = new HashMap<>();
                        imageMetadata.put("fileName", context.getOriginalFileName());
                        imageMetadata.put("pageText", pageText.trim());
                        imageMetadata.put("totalPages", pageCount);
                        imageMetadata.put("pageIndex", i);
                        imageMetadata.put("documentType", "PDF");

                        // ⭐ 添加前几页的文字作为上下文（帮助理解主题）
                        if (i < 3) {
                            List<String> contextTexts = new ArrayList<>();
                            for (int j = 0; j < Math.min(3, pageCount); j++) {
                                org.apache.pdfbox.text.PDFTextStripper contextStripper =
                                    new org.apache.pdfbox.text.PDFTextStripper();
                                contextStripper.setStartPage(j + 1);
                                contextStripper.setEndPage(j + 1);
                                String contextText = contextStripper.getText(document);
                                if (!contextText.trim().isEmpty()) {
                                    contextTexts.add(contextText.trim());
                                }
                            }
                            imageMetadata.put("documentContext", String.join(" | ", contextTexts));
                        }

                        // 5. 创建 ExtractedImage
                        ExtractedImage image = ExtractedImage.builder()
                                .data(imageData)
                                .format("png")
                                .pageNumber(i + 1)
                                .position(new ImagePosition(0, 0,
                                    bufferedImage.getWidth(), bufferedImage.getHeight()))
                                .metadata(imageMetadata)
                                .build();

                        // 6. 创建 DocumentPage
                        DocumentPage page = new DocumentPage(i + 1);
                        page.addImage(image);
                        pages.add(page);

                        log.debug("✅ [VisionLLM] 成功渲染 PDF 页面 {} / {}", i + 1, pageCount);

                    } catch (Exception e) {
                        log.warn("⚠️ [VisionLLM] PDF 页面 {} 处理失败", i + 1, e);
                        // 继续处理下一页
                    }
                }

                log.info("✅ [VisionLLM] PDF 文档页面提取完成: {} 页", pages.size());
                return pages;

            }
        } catch (Exception e) {
            log.error("❌ [VisionLLM] PDF 页面提取失败", e);
            throw new Exception("PDF 页面提取失败: " + e.getMessage(), e);
        }
    }

    /**
     * 处理一批页面
     *
     * @param pages 页面列表
     * @param context 处理上下文（用于获取回调）
     * @return 这批页面的文本内容
     */
    private String processPageBatch(List<DocumentPage> pages, ProcessingContext context) {
        StringBuilder batchContent = new StringBuilder();

        for (DocumentPage page : pages) {
            log.info("🔍 [VisionLLM] 处理第 {} 页，包含 {} 张图片",
                    page.getPageNumber(), page.getImages().size());

            // 构建该页的提示词
            String pagePrompt = buildPagePrompt(page);

            // 调用 Vision LLM 分析整页（传递 context 而不是从 ThreadLocal 获取）
            String pageContent = recognizePageWithVisionLLM(page, pagePrompt, context);

            if (pageContent != null && !pageContent.isEmpty()) {
                // ⭐ 非流式模式下，每页处理完也立即通过回调发送（分批显示）
                if (context != null && context.getOptions() != null) {
                    Object streamingObj = context.getOptions().get("streaming");
                    boolean isStreaming = streamingObj instanceof Boolean && (Boolean) streamingObj;

                    // 只有非流式模式才在这里发送（流式已在 recognizePageWithVisionLLM 内发送）
                    if (!isStreaming) {
                        Object cb = context.getOptions().get("streamCallback");
                        if (cb instanceof java.util.function.Consumer) {
                            @SuppressWarnings("unchecked")
                            java.util.function.Consumer<String> callback = (java.util.function.Consumer<String>) cb;
                            // ⭐ 使用 Markdown 格式的页面标记
                            String pageHeader = String.format("\n\n---\n\n## 📄 页面 %d\n\n", page.getPageNumber());
                            callback.accept(pageHeader);
                            callback.accept(pageContent);
                            callback.accept("\n\n");
                        }
                    }
                }

                // ⭐ 累积内容时也使用 Markdown 格式
                batchContent.append("\n\n---\n\n## 📄 页面 ").append(page.getPageNumber()).append("\n\n");
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
    private String recognizePageWithVisionLLM(DocumentPage page, String prompt, ProcessingContext context) {
        try {
            AIService serviceToUse = visionAIService != null ? visionAIService : aiService;

            if (serviceToUse == null) {
                log.warn("⚠️ [VisionLLM] AI Service 未配置，返回占位内容");
                return String.format("[页面 %d 的内容 - AI Service 未配置]\n包含 %d 张图片",
                        page.getPageNumber(), page.getImages().size());
            }

            // 1. 提取所有图片数据
            List<byte[]> imagesData = new ArrayList<>();
            for (ExtractedImage image : page.getImages()) {
                imagesData.add(image.getData());
            }

            if (imagesData.isEmpty()) {
                log.warn("⚠️ [VisionLLM] 页面 {} 没有图片", page.getPageNumber());
                return "";
            }

            // 2. 构建 Vision 提示词
            String visionPrompt = buildVisionPrompt(page, prompt);

            // 2.1 检查是否需要流式输出
            java.util.function.Consumer<String> streamCallback = null;
            boolean streamingEnabled = false;
            if (context != null && context.getOptions() != null) {
                Object cb = context.getOptions().get("streamCallback");
                if (cb instanceof java.util.function.Consumer) {
                    //noinspection unchecked
                    streamCallback = (java.util.function.Consumer<String>) cb;
                    log.info("✅ [VisionLLM] 检测到流式回调");
                }
                Object streaming = context.getOptions().get("streaming");
                if (streaming instanceof Boolean) {
                    streamingEnabled = (Boolean) streaming;
                    log.info("✅ [VisionLLM] 流式模式: {}", streamingEnabled);
                }
            } else {
                log.warn("⚠️ [VisionLLM] context 或 options 为空");
            }

            // 3. 调用 AIService 进行图片分析
            log.info("🔍 [VisionLLM] 调用 Vision API 分析页面 {}, 图片数: {}, 流式模式: {}, 回调存在: {}, 使用服务: {}",
                    page.getPageNumber(), imagesData.size(), streamingEnabled, streamCallback != null,
                    visionAIService != null ? "visionAIService" : "aiService");

            // ⭐ 真正流式：优先使用 chatWithVisionFlux
            final java.util.function.Consumer<String> finalStreamCallback = streamCallback;
            final boolean finalStreamingEnabled = streamingEnabled;

            if (finalStreamingEnabled && finalStreamCallback != null) {
                log.info("🚀 [VisionLLM] 启动流式处理，页面 {}", page.getPageNumber());

                List<top.yumbo.ai.ai.api.model.ChatMessage> visionMessages = new ArrayList<>();
                visionMessages.add(ChatMessage.userWithImages(visionPrompt, imagesData));

                StringBuilder acc = new StringBuilder();

                // ⭐ 发送页面开始标记（Markdown 格式）
                String pageHeader = String.format("\n\n---\n\n## 📄 页面 %d\n\n", page.getPageNumber());
                log.info("📤 [VisionLLM] 发送页面标记: 页面 {}", page.getPageNumber());
                finalStreamCallback.accept(pageHeader);

                log.info("🔄 [VisionLLM] 开始调用 chatWithVisionFlux");
                serviceToUse.chatWithVisionFlux(visionMessages)
                        .doOnNext(token -> {
                            log.info("📥 [VisionLLM] 收到 token: {} 字符", token.length());
                            acc.append(token);
                            // ⭐ 直接发送 token，不添加额外标记（保持 Markdown 语法完整）
                            finalStreamCallback.accept(token);
                        })
                        .doOnError(err -> {
                            log.error("❌ [VisionLLM] Vision 分析失败: {}", err.getMessage(), err);
                            String errorMsg = String.format("\n\n> ⚠️ **页面 %d 分析失败**: %s\n\n",
                                page.getPageNumber(), err.getMessage());
                            finalStreamCallback.accept(errorMsg);
                        })
                        .doOnComplete(() -> log.info("✅ [VisionLLM] Flux 完成"))
                        .blockLast();

                String result = acc.toString();
                log.info("✅ [VisionLLM] 页面 {} (stream) 分析完成，内容长度: {} chars",
                        page.getPageNumber(), result.length());
                return result;
            }

            // 非流式：保持原逻辑
            try {
                String result = serviceToUse.analyzeImages(imagesData, visionPrompt);

                log.info("✅ [VisionLLM] 页面 {} 分析完成，内容长度: {} chars",
                        page.getPageNumber(), result != null ? result.length() : 0);

                return result != null ? result : "";

            } catch (UnsupportedOperationException e) {
                log.error("❌ [VisionLLM] 当前AI服务不支持Vision功能: {}", e.getMessage());

                return String.format("[页面 %d - 当前AI服务不支持Vision功能]\n" +
                                "请配置支持Vision的模型（如：qwen-vl-plus, gpt-4o等）\n" +
                                "包含 %d 张图片",
                        page.getPageNumber(), page.getImages().size());

            } catch (Exception apiEx) {
                log.error("❌ [VisionLLM] Vision API 调用失败: {}", apiEx.getMessage());

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

        // ⭐ Debug 日志：元数据信息
        log.debug("🎨 [Vision Prompt] Building prompt for page {}", page.getPageNumber());
        log.debug("🎨 [Vision Prompt] Metadata - fileName: {}, slideText length: {}, hasContext: {}",
            fileName, slideText != null ? slideText.length() : 0, documentContext != null);

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

        String finalPrompt = prompt.toString();

        // ⭐ Debug 日志：完整提示词
        log.debug("🎨 [Vision Prompt] Final prompt ({} chars):\n{}", finalPrompt.length(), finalPrompt);

        return finalPrompt;
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

    /**
     * 批处理结果
     */
    @Data
    private static class BatchProcessingResult {
        private final int batchIndex;
        private final String content;
        private final List<ExtractedImage> images;
    }

    /**
     * 智能分批：根据上下文大小预判断，尽可能多页一起处理
     * ⭐ 核心优化：减少 API 调用次数
     *
     * @param pages 所有页面
     * @return 分批后的页面列表
     */
    private List<List<DocumentPage>> smartBatching(List<DocumentPage> pages) {
        // 如果配置不存在或未启用智能批处理，使用默认批次大小
        if (batchProcessingConfig == null || !batchProcessingConfig.isEnabled()) {
            // 使用默认批次大小
            int batchSize = (batchProcessingConfig != null) ? batchProcessingConfig.getMaxBatchSize() : 5;
            List<List<DocumentPage>> batches = new ArrayList<>();
            for (int i = 0; i < pages.size(); i += batchSize) {
                int endIdx = Math.min(i + batchSize, pages.size());
                batches.add(new ArrayList<>(pages.subList(i, endIdx)));
            }
            log.debug("📦 [Smart Batching] 使用固定批次大小: {}, 批次数: {}", batchSize, batches.size());
            return batches;
        }

        // 智能分批
        List<List<DocumentPage>> batches = new ArrayList<>();
        List<DocumentPage> currentBatch = new ArrayList<>();

        for (DocumentPage page : pages) {
            // 检查是否可以添加到当前批次
            if (batchProcessingConfig.canAddMoreSlides(currentBatch.size())) {
                currentBatch.add(page);
            } else {
                // 当前批次已满，开始新批次
                if (!currentBatch.isEmpty()) {
                    batches.add(new ArrayList<>(currentBatch));
                    currentBatch.clear();
                }
                currentBatch.add(page);
            }
        }

        // 添加最后一个批次
        if (!currentBatch.isEmpty()) {
            batches.add(currentBatch);
        }

        log.debug("📦 [Smart Batching] 智能分批完成 - 总页面: {}, 批次数: {}, 平均每批: {} 页",
                pages.size(), batches.size(), (double) pages.size() / batches.size());

        return batches;
    }

    /**
     * 并行处理多个批次
     * ⭐ 核心优化：并行处理，大幅提升速度
     *
     * @param batches 所有批次
     * @param context 处理上下文（用于获取回调）
     * @return 批处理结果列表
     */
    private List<BatchProcessingResult> processPageBatchesInParallel(List<List<DocumentPage>> batches, ProcessingContext context) {
        log.info("🚀 [Parallel Processing] 开始并行处理 {} 个批次", batches.size());
        long startTime = System.currentTimeMillis();

        List<CompletableFuture<BatchProcessingResult>> futures = new ArrayList<>();

        for (int i = 0; i < batches.size(); i++) {
            final int batchIndex = i;
            final List<DocumentPage> batch = batches.get(i);

            CompletableFuture<BatchProcessingResult> future = CompletableFuture.supplyAsync(() -> {
                try {
                    log.debug("⚙️ [Thread: {}] 开始处理批次 #{}",
                        Thread.currentThread().getName(), batchIndex + 1);

                    // ⭐ 直接传递 context，不依赖 ThreadLocal
                    String content = processPageBatch(batch, context);
                    List<ExtractedImage> images = batch.stream()
                            .flatMap(page -> page.getImages().stream())
                            .collect(Collectors.toList());

                    log.debug("✅ [Thread: {}] 批次 #{} 处理完成",
                        Thread.currentThread().getName(), batchIndex + 1);

                    return new BatchProcessingResult(batchIndex, content, images);
                } catch (Exception e) {
                    log.error("❌ [Thread: {}] 批次 #{} 处理失败: {}",
                        Thread.currentThread().getName(), batchIndex + 1, e.getMessage());
                    return new BatchProcessingResult(batchIndex, "", Collections.emptyList());
                }
            }, visionLlmExecutor);

            futures.add(future);
        }


        // 等待所有批次完成
        try {
            CompletableFuture<Void> allOf = CompletableFuture.allOf(
                    futures.toArray(new CompletableFuture[0]));
            allOf.get(5, TimeUnit.MINUTES);  // 5分钟超时

            // 收集结果（按批次索引排序，保持顺序）
            List<BatchProcessingResult> results = futures.stream()
                    .map(CompletableFuture::join)
                    .sorted(Comparator.comparingInt(BatchProcessingResult::getBatchIndex))
                    .collect(Collectors.toList());

            long duration = System.currentTimeMillis() - startTime;
            log.info("✅ [Parallel Processing] 并行处理完成 - 耗时: {}ms, 平均每批: {}ms",
                    duration, duration / batches.size());

            return results;
        } catch (TimeoutException e) {
            log.error("❌ [Parallel Processing] 处理超时");
            throw new RuntimeException("Vision LLM 处理超时", e);
        } catch (Exception e) {
            log.error("❌ [Parallel Processing] 处理失败: {}", e.getMessage());
            throw new RuntimeException("Vision LLM 并行处理失败", e);
        }
    }

    /**
     * 串行处理多个批次
     *
     * @param batches 所有批次
     * @param context 处理上下文（用于获取回调）
     * @return 批处理结果列表
     */
    private List<BatchProcessingResult> processPageBatchesSequentially(List<List<DocumentPage>> batches, ProcessingContext context) {
        log.info("🔄 [Sequential Processing] 开始串行处理 {} 个批次", batches.size());
        long startTime = System.currentTimeMillis();

        List<BatchProcessingResult> results = new ArrayList<>();

        for (int i = 0; i < batches.size(); i++) {
            List<DocumentPage> batch = batches.get(i);
            log.debug("⚙️ 处理批次 {}/{}", i + 1, batches.size());

            // ⭐ 发送批次开始标记
            if (context != null && context.getOptions() != null) {
                Object cb = context.getOptions().get("streamCallback");
                if (cb instanceof java.util.function.Consumer) {
                    @SuppressWarnings("unchecked")
                    java.util.function.Consumer<String> callback = (java.util.function.Consumer<String>) cb;
                    String batchMarker = String.format("BATCH_START:{\"batchIndex\":%d,\"batchNumber\":%d,\"totalBatches\":%d}\n",
                        i, i + 1, batches.size());
                    callback.accept(batchMarker);
                    log.info("📤 [Sequential] 发送批次 {} 开始标记", i + 1);
                }
            }

            try {
                // ⭐ 传递 context
                String content = processPageBatch(batch, context);
                List<ExtractedImage> images = batch.stream()
                        .flatMap(page -> page.getImages().stream())
                        .collect(Collectors.toList());

                results.add(new BatchProcessingResult(i, content, images));

                // ⭐ 发送批次完成标记
                if (context != null && context.getOptions() != null) {
                    Object cb = context.getOptions().get("streamCallback");
                    if (cb instanceof java.util.function.Consumer) {
                        @SuppressWarnings("unchecked")
                        java.util.function.Consumer<String> callback = (java.util.function.Consumer<String>) cb;
                        String batchEndMarker = String.format("BATCH_END:{\"batchIndex\":%d,\"batchNumber\":%d}\n",
                            i, i + 1);
                        callback.accept(batchEndMarker);
                        log.info("✅ [Sequential] 批次 {} 完成", i + 1);
                    }
                }
            } catch (Exception e) {
                log.error("❌ 批次 {} 处理失败: {}", i + 1, e.getMessage());
                results.add(new BatchProcessingResult(i, "", Collections.emptyList()));
            }
        }

        long duration = System.currentTimeMillis() - startTime;
        log.info("✅ [Sequential Processing] 串行处理完成 - 耗时: {}ms", duration);

        return results;
    }
}
