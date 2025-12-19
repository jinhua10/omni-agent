package top.yumbo.ai.omni.core.document.processor;

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
 * 处理策略：
 * 1. 将文档转换为图片（PDF/PPT/Word/Excel → PNG）
 * 2. 使用 Vision LLM 识别图片内容
 * 3. 提取文本和结构化信息
 * </p>
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
            // 1. 将文档转换为图片
            List<byte[]> images = convertToImages(context);
            log.info("📸 [VisionLLM] 转换为 {} 张图片", images.size());

            // 2. 使用 Vision LLM 识别每张图片
            StringBuilder allContent = new StringBuilder();
            List<ExtractedImage> extractedImages = new ArrayList<>();

            for (int i = 0; i < images.size(); i++) {
                log.info("🔍 [VisionLLM] 处理第 {}/{} 张图片", i + 1, images.size());

                // TODO: 调用 Vision LLM API
                String imageContent = recognizeImageWithVisionLLM(images.get(i), i + 1);

                if (imageContent != null && !imageContent.isEmpty()) {
                    allContent.append("=== 页面 ").append(i + 1).append(" ===\n");
                    allContent.append(imageContent).append("\n\n");
                }

                // 保存图片信息
                extractedImages.add(ExtractedImage.builder()
                        .data(images.get(i))
                        .format("png")
                        .pageNumber(i + 1)
                        .description(imageContent)
                        .build());
            }

            // 3. 构建元数据
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("pageCount", images.size());
            metadata.put("processor", "VisionLLM");
            metadata.put("model", visionModel);
            metadata.put("originalExtension", context.getFileExtension());

            long processingTime = System.currentTimeMillis() - startTime;

            log.info("✅ [VisionLLM] 处理完成: 耗时={}ms, 内容长度={}",
                    processingTime, allContent.length());

            return ProcessingResult.builder()
                    .success(true)
                    .content(allContent.toString())
                    .metadata(metadata)
                    .images(extractedImages)
                    .processingTimeMs(processingTime)
                    .processorName(getName())
                    .build();

        } catch (Exception e) {
            log.error("❌ [VisionLLM] 处理失败: {}", e.getMessage(), e);
            throw new DocumentProcessingException("Vision LLM 处理失败", e);
        }
    }

    /**
     * 将文档转换为图片
     *
     * @param context 处理上下文
     * @return 图片列表（PNG格式）
     */
    private List<byte[]> convertToImages(ProcessingContext context) throws Exception {
        String ext = context.getFileExtension().toLowerCase();

        // TODO: 根据文件类型使用不同的转换策略
        // - PDF: 使用 Apache PDFBox 或 pdf2image
        // - Word/Excel/PPT: 使用 Apache POI + Java2D 或 LibreOffice
        // - 图片: 直接读取

        // 暂时返回模拟数据
        log.warn("⚠️ [VisionLLM] 文档转图片功能待实现，当前返回占位数据");

        // 如果是图片文件，直接返回
        if (isImageFile(ext)) {
            if (context.getFileBytes() != null) {
                return List.of(context.getFileBytes());
            }
            // 从文件路径读取
            return List.of(java.nio.file.Files.readAllBytes(
                    java.nio.file.Paths.get(context.getFilePath())));
        }

        // Office 文档转图片（待实现）
        throw new Exception("文档转图片功能待实现: " + ext);
    }

    /**
     * 使用 Vision LLM 识别图片内容
     *
     * @param imageBytes 图片字节数组
     * @param pageNumber 页码
     * @return 识别的文本内容
     */
    private String recognizeImageWithVisionLLM(byte[] imageBytes, int pageNumber) {
        try {
            // TODO: 调用 Vision LLM API
            // 1. 将图片编码为 Base64
            String base64Image = Base64.getEncoder().encodeToString(imageBytes);

            // 2. 构建 Vision LLM 请求
            // 根据不同的 API 提供商格式不同：
            // - 千问 VL: 支持 image_url
            // - GPT-4V: 支持 image_url
            // - Claude: 支持 base64

            // 3. 调用 AI Service（需要支持 Vision）
            // String content = aiService.analyzeImage(base64Image, systemPrompt);

            log.warn("⚠️ [VisionLLM] Vision LLM API 调用待实现");
            return String.format("[页面 %d 的内容 - 待实现 Vision LLM 调用]", pageNumber);

        } catch (Exception e) {
            log.error("❌ [VisionLLM] 图片识别失败: page={}", pageNumber, e);
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
}

