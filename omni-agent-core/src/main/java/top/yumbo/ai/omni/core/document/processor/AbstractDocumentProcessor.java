package top.yumbo.ai.omni.core.document.processor;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import top.yumbo.ai.ai.api.AIService;
import top.yumbo.ai.omni.core.document.DocumentProcessor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 文档处理器抽象基类
 * (Abstract Document Processor Base Class)
 *
 * <p>提供统一的文档处理流程：</p>
 * <ol>
 *   <li>提取文本内容</li>
 *   <li>提取图片</li>
 *   <li>使用 Vision LLM 将图片转换为文本描述</li>
 *   <li>将图片描述嵌入到原文本的对应位置</li>
 * </ol>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public abstract class AbstractDocumentProcessor implements DocumentProcessor {

    @Autowired(required = false)
    protected AIService visionAIService;

    /**
     * 文本块（包含文本和图片引用）
     */
    @lombok.Getter
    protected static class ContentBlock {
        private final String text;
        private final List<ExtractedImage> images;
        private final int position;

        public ContentBlock(String text, int position) {
            this.text = text;
            this.images = new ArrayList<>();
            this.position = position;
        }

        public ContentBlock(List<ExtractedImage> images, int position) {
            this.text = "";
            this.images = images;
            this.position = position;
        }


        public boolean isText() {
            return !text.isEmpty();
        }

        public boolean isImage() {
            return !images.isEmpty();
        }
    }

    /**
     * 提取的文档内容（文本 + 图片）
     */
    @lombok.Getter
    protected static class ExtractedContent {
        private final List<ContentBlock> blocks;
        private final Map<String, Object> metadata;

        public ExtractedContent() {
            this.blocks = new ArrayList<>();
            this.metadata = new HashMap<>();
        }

        public void addTextBlock(String text, int position) {
            blocks.add(new ContentBlock(text, position));
        }

        public void addImageBlock(List<ExtractedImage> images, int position) {
            blocks.add(new ContentBlock(images, position));
        }

        public void addImageBlock(ExtractedImage image, int position) {
            ContentBlock block = new ContentBlock(new ArrayList<>(), position);
            block.getImages().add(image);
            blocks.add(block);
        }
    }

    @Override
    public ProcessingResult process(ProcessingContext context) throws DocumentProcessingException {
        log.info("📄 [{}] 开始处理文档: {}", getName(), context.getOriginalFileName());

        long startTime = System.currentTimeMillis();

        try {
            // 1. 提取文档内容（文本 + 图片位置）
            ExtractedContent content = extractContent(context);

            // 2. 处理图片（调用 Vision LLM）
            processImages(content, context);

            // 3. 合并文本和图片描述
            String finalText = mergeContent(content);

            // 4. 收集所有图片
            List<ExtractedImage> allImages = collectImages(content);

            long processingTime = System.currentTimeMillis() - startTime;
            content.getMetadata().put("processingTime", processingTime);
            content.getMetadata().put("processor", getName());

            log.info("✅ [{}] 处理完成: 耗时={}ms, 文本长度={}, 图片数={}",
                    getName(), processingTime, finalText.length(), allImages.size());

            return ProcessingResult.builder()
                    .success(true)
                    .content(finalText)
                    .metadata(content.getMetadata())
                    .images(allImages)
                    .processingTimeMs(processingTime)
                    .processorName(getName())
                    .build();

        } catch (Exception e) {
            log.error("❌ [{}] 处理失败: {}", getName(), e.getMessage(), e);
            throw new DocumentProcessingException(getName() + " 处理失败", e);
        }
    }

    /**
     * 提取文档内容（子类实现）
     *
     * @param context 处理上下文
     * @return 提取的内容（文本块 + 图片块）
     */
    protected abstract ExtractedContent extractContent(ProcessingContext context) throws Exception;

    /**
     * 处理图片（调用 Vision LLM 转换为文本描述）
     */
    protected void processImages(ExtractedContent content, ProcessingContext context) {
        if (visionAIService == null) {
            log.warn("⚠️ Vision AI Service 未配置，图片将不被处理");
            return;
        }

        for (ContentBlock block : content.getBlocks()) {
            if (block.isImage()) {
                for (ExtractedImage image : block.getImages()) {
                    try {
                        // 调用 Vision LLM 分析图片
                        String imageDescription = analyzeImage(image);

                        // 将描述保存到图片元数据
                        if (image.getMetadata() == null) {
                            image.setMetadata(new HashMap<>());
                        }
                        image.getMetadata().put("visionDescription", imageDescription);

                        log.debug("🖼️ 图片分析完成: {} 字符", imageDescription.length());

                    } catch (Exception e) {
                        log.error("❌ 图片分析失败: {}", e.getMessage());
                        if (image.getMetadata() == null) {
                            image.setMetadata(new HashMap<>());
                        }
                        image.getMetadata().put("visionDescription", "[图片分析失败]");
                    }
                }
            }
        }
    }

    /**
     * 使用 Vision LLM 分析图片
     */
    protected String analyzeImage(ExtractedImage image) {
        if (visionAIService == null) {
            return "[Vision服务未配置]";
        }

        try {
            List<byte[]> imageDataList = List.of(image.getData());
            String prompt = buildImageAnalysisPrompt(image);

            String result = visionAIService.analyzeImages(imageDataList, prompt);
            return result != null ? result : "[图片分析无结果]";

        } catch (Exception e) {
            log.error("Vision LLM 调用失败", e);
            return "[图片分析失败: " + e.getMessage() + "]";
        }
    }

    /**
     * 构建图片分析提示词
     */
    protected String buildImageAnalysisPrompt(ExtractedImage image) {
        return "请描述这张图片的内容，包括：\n" +
               "1. 主要内容和对象\n" +
               "2. 图表数据（如果有）\n" +
               "3. 文字信息（如果有）\n" +
               "4. 整体含义和作用\n" +
               "\n请用简洁的语言描述，便于理解。";
    }

    /**
     * 合并文本和图片描述（按位置顺序）
     */
    protected String mergeContent(ExtractedContent content) {
        StringBuilder merged = new StringBuilder();

        // 按位置排序
        content.getBlocks().sort(java.util.Comparator.comparingInt(ContentBlock::getPosition));

        for (ContentBlock block : content.getBlocks()) {
            if (block.isText()) {
                merged.append(block.getText());
            } else if (block.isImage()) {
                // 嵌入图片描述
                for (ExtractedImage image : block.getImages()) {
                    String description = "[图片]";
                    if (image.getMetadata() != null &&
                        image.getMetadata().containsKey("visionDescription")) {
                        description = (String) image.getMetadata().get("visionDescription");
                    }

                    merged.append("\n\n📷 **[图片");
                    if (image.getPageNumber() > 0) {
                        merged.append(" - 页码 ").append(image.getPageNumber());
                    }
                    merged.append("]**\n\n");
                    merged.append(description);
                    merged.append("\n\n");
                }
            }
        }

        return merged.toString();
    }

    /**
     * 收集所有图片
     */
    protected List<ExtractedImage> collectImages(ExtractedContent content) {
        List<ExtractedImage> allImages = new ArrayList<>();

        for (ContentBlock block : content.getBlocks()) {
            if (block.isImage()) {
                allImages.addAll(block.getImages());
            }
        }

        return allImages;
    }
}


