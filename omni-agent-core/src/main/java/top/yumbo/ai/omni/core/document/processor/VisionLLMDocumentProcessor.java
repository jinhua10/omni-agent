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

        // TODO: 根据文件类型使用不同的提取策略
        // - PDF: 使用 Apache PDFBox
        // - PPT: 使用 Apache POI
        // - Word/Excel: 使用 Apache POI
        // - 图片: 直接作为单页

        log.warn("⚠️ [VisionLLM] 页面提取功能待实现，当前返回模拟数据");

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

        // Office 文档待实现
        throw new Exception("Office 文档页面提取功能待实现: " + ext);
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
            // TODO: 调用 Vision LLM API
            // 1. 将所有图片编码为 Base64
            List<String> base64Images = new ArrayList<>();
            for (ExtractedImage image : page.getImages()) {
                String base64 = Base64.getEncoder().encodeToString(image.getData());
                base64Images.add(base64);
            }

            // 2. 构建多模态请求
            // 根据不同的 API 提供商格式不同：
            // - 千问 VL: 支持多张图片
            // - GPT-4V: 支持多张图片
            // - Claude: 支持多张图片

            // 3. 调用 AI Service（需要支持 Vision）
            // String content = aiService.analyzeImages(base64Images, prompt);

            log.warn("⚠️ [VisionLLM] Vision LLM API 调用待实现");

            // 模拟返回
            return String.format("[页面 %d 的内容 - 待实现 Vision LLM 调用]\n包含 %d 张图片",
                    page.getPageNumber(), page.getImages().size());

        } catch (Exception e) {
            log.error("❌ [VisionLLM] 页面识别失败: page={}", page.getPageNumber(), e);
            return "";
        }
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

