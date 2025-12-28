package top.yumbo.ai.omni.document.processor;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.document.processor.extension.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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

    // ⭐ 扩展接口自动注入（类似于 Spring 的扩展机制）
    @Autowired(required = false)
    protected List<PreProcessor> preProcessors = new ArrayList<>();

    @Autowired(required = false)
    protected List<PostProcessor> postProcessors = new ArrayList<>();

    @Autowired(required = false)
    protected List<ContentEnhancer> contentEnhancers = new ArrayList<>();

    @Autowired(required = false)
    protected List<ImageHandler> imageHandlers = new ArrayList<>();

    @Autowired(required = false)
    protected List<MetadataExtractor> metadataExtractors = new ArrayList<>();

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
            // ⭐ 0. 前置处理（PreProcessor）
            context = applyPreProcessors(context);

            // 1. 提取文档内容（文本 + 图片位置）
            ExtractedContent content = extractContent(context);

            // ⭐ 1.5. 提取元数据（MetadataExtractor）
            applyMetadataExtractors(content, context);

            // 2. 处理图片（调用 Vision LLM + ImageHandler）
            processImages(content, context);

            // 3. 合并文本和图片描述
            String finalText = mergeContent(content);

            // ⭐ 3.5. 内容增强（ContentEnhancer）
            finalText = applyContentEnhancers(finalText, content, context);

            // 4. 收集所有图片
            List<ExtractedImage> allImages = collectImages(content);

            long processingTime = System.currentTimeMillis() - startTime;
            content.getMetadata().put("processingTime", processingTime);
            content.getMetadata().put("processor", getName());

            ProcessingResult result = ProcessingResult.builder()
                    .success(true)
                    .content(finalText)
                    .metadata(content.getMetadata())
                    .images(allImages)
                    .processingTimeMs(processingTime)
                    .processorName(getName())
                    .build();

            // ⭐ 5. 后置处理（PostProcessor）
            result = applyPostProcessors(context, result);

            log.info("✅ [{}] 处理完成: 耗时={}ms, 文本长度={}, 图片数={}",
                    getName(), processingTime, finalText.length(), allImages.size());

            return result;

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
     * 处理图片（调用 Vision LLM + ImageHandler）
     */
    protected void processImages(ExtractedContent content, ProcessingContext context) {
        if (visionAIService == null) {
            log.warn("⚠️ Vision AI Service 未配置，图片将不被处理");
        }

        for (ContentBlock block : content.getBlocks()) {
            if (block.isImage()) {
                for (ExtractedImage image : block.getImages()) {
                    try {
                        // ⭐ 先应用 ImageHandler
                        image = applyImageHandlers(image, context);

                        // 调用 Vision LLM 分析图片（如果配置了）
                        if (visionAIService != null) {
                            String imageDescription = analyzeImage(image);

                            // 将描述保存到图片元数据
                            if (image.getMetadata() == null) {
                                image.setMetadata(new HashMap<>());
                            }
                            image.getMetadata().put("visionDescription", imageDescription);

                            log.debug("🖼️ 图片分析完成: {} 字符", imageDescription.length());
                        }

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

    // ====================== 扩展点应用方法 ======================

    /**
     * 应用前置处理器
     */
    protected ProcessingContext applyPreProcessors(ProcessingContext context) throws Exception {
        if (preProcessors == null || preProcessors.isEmpty()) {
            return context;
        }

        // 过滤支持当前处理器的前置处理器，并按顺序排序
        List<PreProcessor> applicableProcessors = preProcessors.stream()
                .filter(p -> p.isEnabled() && p.supports(getName()))
                .sorted(java.util.Comparator.comparingInt(PreProcessor::getOrder))
                .collect(Collectors.toList());

        log.debug("📋 应用 {} 个前置处理器", applicableProcessors.size());

        ProcessingContext currentContext = context;
        for (PreProcessor processor : applicableProcessors) {
            try {
                log.debug("  ▶ 执行前置处理器: {}", processor.getName());
                currentContext = processor.preProcess(currentContext);
            } catch (Exception e) {
                log.error("❌ 前置处理器执行失败: {}", processor.getName(), e);
                throw e;
            }
        }

        return currentContext;
    }

    /**
     * 应用后置处理器
     */
    protected ProcessingResult applyPostProcessors(ProcessingContext context, ProcessingResult result) throws Exception {
        if (postProcessors == null || postProcessors.isEmpty()) {
            return result;
        }

        // 过滤支持当前处理器的后置处理器，并按顺序排序
        List<PostProcessor> applicableProcessors = postProcessors.stream()
                .filter(p -> p.isEnabled() && p.supports(getName()))
                .sorted(java.util.Comparator.comparingInt(PostProcessor::getOrder))
                .collect(Collectors.toList());

        log.debug("📋 应用 {} 个后置处理器", applicableProcessors.size());

        ProcessingResult currentResult = result;
        for (PostProcessor processor : applicableProcessors) {
            try {
                log.debug("  ▶ 执行后置处理器: {}", processor.getName());
                currentResult = processor.postProcess(context, currentResult);
            } catch (Exception e) {
                log.error("❌ 后置处理器执行失败: {}", processor.getName(), e);
                throw e;
            }
        }

        return currentResult;
    }

    /**
     * 应用内容增强器
     */
    protected String applyContentEnhancers(String content, ExtractedContent extractedContent,
                                          ProcessingContext context) throws Exception {
        if (contentEnhancers == null || contentEnhancers.isEmpty()) {
            return content;
        }

        // 过滤支持当前处理器的内容增强器，并按顺序排序
        List<ContentEnhancer> applicableEnhancers = contentEnhancers.stream()
                .filter(e -> e.isEnabled() && e.supports(getName()))
                .sorted(java.util.Comparator.comparingInt(ContentEnhancer::getOrder))
                .collect(Collectors.toList());

        log.debug("📋 应用 {} 个内容增强器", applicableEnhancers.size());

        String currentContent = content;
        for (ContentEnhancer enhancer : applicableEnhancers) {
            try {
                log.debug("  ▶ 执行内容增强器: {}", enhancer.getName());
                ContentEnhancer.EnhancedContent enhanced = enhancer.enhance(context, currentContent);

                // 更新内容
                if (enhanced != null && enhanced.getContent() != null) {
                    currentContent = enhanced.getContent();

                    // 将增强信息添加到元数据
                    if (enhanced.getSummary() != null) {
                        extractedContent.getMetadata().put("summary", enhanced.getSummary());
                    }
                    if (enhanced.getKeywords() != null) {
                        extractedContent.getMetadata().put("keywords", enhanced.getKeywords());
                    }
                    if (enhanced.getCategory() != null) {
                        extractedContent.getMetadata().put("category", enhanced.getCategory());
                    }
                    if (enhanced.getEntities() != null) {
                        extractedContent.getMetadata().put("entities", enhanced.getEntities());
                    }
                    if (enhanced.getMetadata() != null) {
                        extractedContent.getMetadata().putAll(enhanced.getMetadata());
                    }
                }
            } catch (Exception e) {
                log.error("❌ 内容增强器执行失败: {}", enhancer.getName(), e);
                // 不抛出异常，继续处理
            }
        }

        return currentContent;
    }

    /**
     * 应用图片处理器
     */
    protected ExtractedImage applyImageHandlers(ExtractedImage image, ProcessingContext context) throws Exception {
        if (imageHandlers == null || imageHandlers.isEmpty()) {
            return image;
        }

        // 过滤支持当前处理器的图片处理器，并按顺序排序
        List<ImageHandler> applicableHandlers = imageHandlers.stream()
                .filter(h -> h.isEnabled() && h.supports(getName()))
                .sorted(java.util.Comparator.comparingInt(ImageHandler::getOrder))
                .collect(Collectors.toList());

        if (applicableHandlers.isEmpty()) {
            return image;
        }

        log.debug("📋 应用 {} 个图片处理器", applicableHandlers.size());

        for (ImageHandler handler : applicableHandlers) {
            try {
                log.debug("  ▶ 执行图片处理器: {}", handler.getName());
                ImageHandler.ProcessedImage processed = handler.handle(context, image);

                // 更新图片数据和元数据
                if (processed != null) {
                    if (processed.getData() != null) {
                        image.setData(processed.getData());
                    }
                    if (processed.getFormat() != null) {
                        image.setFormat(processed.getFormat());
                    }

                    // 将处理信息添加到图片元数据
                    if (image.getMetadata() == null) {
                        image.setMetadata(new HashMap<>());
                    }
                    if (processed.getOcrText() != null) {
                        image.getMetadata().put("ocrText", processed.getOcrText());
                    }
                    if (processed.getDescription() != null) {
                        image.getMetadata().put("customDescription", processed.getDescription());
                    }
                    if (processed.getCategory() != null) {
                        image.getMetadata().put("category", processed.getCategory());
                    }
                    if (processed.getDetectedObjects() != null) {
                        image.getMetadata().put("detectedObjects", processed.getDetectedObjects());
                    }
                    if (processed.getMetadata() != null) {
                        image.getMetadata().putAll(processed.getMetadata());
                    }
                }
            } catch (Exception e) {
                log.error("❌ 图片处理器执行失败: {}", handler.getName(), e);
                // 不抛出异常，继续处理
            }
        }

        return image;
    }

    /**
     * 应用元数据提取器
     */
    protected void applyMetadataExtractors(ExtractedContent content, ProcessingContext context) {
        if (metadataExtractors == null || metadataExtractors.isEmpty()) {
            return;
        }

        // 过滤支持当前处理器的元数据提取器，并按顺序排序
        List<MetadataExtractor> applicableExtractors = metadataExtractors.stream()
                .filter(e -> e.isEnabled() && e.supports(getName()))
                .sorted(java.util.Comparator.comparingInt(MetadataExtractor::getOrder))
                .collect(Collectors.toList());

        log.debug("📋 应用 {} 个元数据提取器", applicableExtractors.size());

        for (MetadataExtractor extractor : applicableExtractors) {
            try {
                log.debug("  ▶ 执行元数据提取器: {}", extractor.getName());
                MetadataExtractor.ExtractedMetadata metadata = extractor.extract(context);

                // 将提取的元数据添加到内容元数据
                if (metadata != null) {
                    if (metadata.getAuthor() != null) {
                        content.getMetadata().put("author", metadata.getAuthor());
                    }
                    if (metadata.getTitle() != null) {
                        content.getMetadata().put("title", metadata.getTitle());
                    }
                    if (metadata.getSubject() != null) {
                        content.getMetadata().put("subject", metadata.getSubject());
                    }
                    if (metadata.getKeywords() != null) {
                        content.getMetadata().put("keywords", metadata.getKeywords());
                    }
                    if (metadata.getCreatedDate() != null) {
                        content.getMetadata().put("createdDate", metadata.getCreatedDate());
                    }
                    if (metadata.getModifiedDate() != null) {
                        content.getMetadata().put("modifiedDate", metadata.getModifiedDate());
                    }
                    if (metadata.getVersion() != null) {
                        content.getMetadata().put("version", metadata.getVersion());
                    }
                    if (metadata.getLanguage() != null) {
                        content.getMetadata().put("language", metadata.getLanguage());
                    }
                    if (metadata.getSecurityLevel() != null) {
                        content.getMetadata().put("securityLevel", metadata.getSecurityLevel());
                    }
                    if (metadata.getCustomProperties() != null) {
                        content.getMetadata().putAll(metadata.getCustomProperties());
                    }
                }
            } catch (Exception e) {
                log.error("❌ 元数据提取器执行失败: {}", extractor.getName(), e);
                // 不抛出异常，继续处理
            }
        }
    }
}


