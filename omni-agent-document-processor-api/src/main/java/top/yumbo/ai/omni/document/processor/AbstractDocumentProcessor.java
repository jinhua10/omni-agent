package top.yumbo.ai.omni.document.processor;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.ai.api.config.VisionLLMBatchProcessingProperties;
import top.yumbo.ai.omni.document.processor.extension.*;
import top.yumbo.ai.omni.document.processor.model.DocumentExtractionResult;
import top.yumbo.ai.omni.document.processor.service.DocumentExtractionResultService;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
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

    // ⭐ 文档提取结果服务（可选，用于持久化提取结果）
    @Autowired(required = false)
    protected DocumentExtractionResultService extractionResultService;  // 使用 Object 避免循环依赖

    // ⭐ 批处理配置
    @Autowired(required = false)
    protected VisionLLMBatchProcessingProperties batchProcessingConfig;

    // ⭐ Vision LLM 线程池（用于并行处理）
    @Autowired(required = false)
    @Qualifier("visionLlmExecutor")
    protected Executor visionLlmExecutor;

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

            // ⭐ 6. 保存提取结果到存储层（如果配置了服务）
            saveExtractionResult(context, result, startTime);

            log.info("✅ [{}] 处理完成: 耗时={}ms, 文本长度={}, 图片数={}",
                    getName(), processingTime, finalText.length(), allImages.size());

            return result;

        } catch (Exception e) {
            log.error("❌ [{}] 处理失败: {}", getName(), e.getMessage(), e);

            // ⭐ 保存失败记录
            saveFailedExtractionResult(context, e, startTime);

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
     * ⭐ 优化：支持智能批处理和并行处理
     */
    protected void processImages(ExtractedContent content, ProcessingContext context) {
        if (visionAIService == null) {
            log.warn("⚠️ Vision AI Service 未配置，图片将不被处理");
            return;
        }

        // 收集所有图片块
        List<ContentBlock> imageBlocks = content.getBlocks().stream()
                .filter(ContentBlock::isImage)
                .collect(Collectors.toList());

        if (imageBlocks.isEmpty()) {
            log.debug("📋 没有图片需要处理");
            return;
        }

        // 统计总图片数
        int totalImages = imageBlocks.stream()
                .mapToInt(block -> block.getImages().size())
                .sum();

        log.info("🖼️ 准备处理 {} 个图片块，共 {} 张图片", imageBlocks.size(), totalImages);

        // ⭐ 智能分批：将图片块分组
        List<List<ContentBlock>> batches = smartBatchingForImages(imageBlocks);
        log.info("📦 智能分批完成: {} 个批次", batches.size());

        // ⭐ 发送批次信息（流式模式）
        sendBatchInfo(context, batches.size(), totalImages);

        // ⭐ 选择处理方式：并行或串行
        if (visionLlmExecutor != null && batches.size() > 1) {
            log.info("🚀 并行处理 {} 个批次", batches.size());
            processImageBatchesInParallel(batches, context);
        } else {
            log.info("🔄 串行处理 {} 个批次", batches.size());
            processImageBatchesSequentially(batches, context);
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
        return """
                请描述这张图片的内容，包括：
                1. 主要内容和对象
                2. 图表数据（如果有）
                3. 文字信息（如果有）
                4. 整体含义和作用
                
                请用简洁的语言描述，便于理解。""";
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
                .toList();

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
                .toList();

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
                .toList();

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
                .toList();

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
                .toList();

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

    // ====================== 批处理方法 ======================

    /**
     * 智能分批：根据配置动态决定批次大小
     */
    protected List<List<ContentBlock>> smartBatchingForImages(List<ContentBlock> imageBlocks) {
        // 如果没有配置或未启用批处理，使用默认批次大小
        int batchSize = 5; // 默认值
        if (batchProcessingConfig != null && batchProcessingConfig.isEnabled()) {
            batchSize = batchProcessingConfig.getMaxBatchSize();
        }

        List<List<ContentBlock>> batches = new ArrayList<>();
        List<ContentBlock> currentBatch = new ArrayList<>();

        for (ContentBlock block : imageBlocks) {
            if (currentBatch.size() < batchSize) {
                currentBatch.add(block);
            } else {
                if (!currentBatch.isEmpty()) {
                    batches.add(new ArrayList<>(currentBatch));
                    currentBatch.clear();
                }
                currentBatch.add(block);
            }
        }

        if (!currentBatch.isEmpty()) {
            batches.add(currentBatch);
        }

        log.debug("📦 智能分批: {} 个图片块 -> {} 个批次，每批最多 {} 个",
                imageBlocks.size(), batches.size(), batchSize);

        return batches;
    }

    /**
     * 发送批次信息（流式模式）
     */
    protected void sendBatchInfo(ProcessingContext context, int totalBatches, int totalImages) {
        if (context == null || context.getOptions() == null) {
            return;
        }

        boolean isStreaming = Boolean.TRUE.equals(context.getOptions().get("streaming"));
        Object callbackObj = context.getOptions().get("streamCallback");

        if (isStreaming && callbackObj instanceof java.util.function.Consumer) {
            @SuppressWarnings("unchecked")
            java.util.function.Consumer<String> callback =
                    (java.util.function.Consumer<String>) callbackObj;

            String batchInfo = String.format(
                    "BATCH_INFO:{\"totalBatches\":%d,\"totalImages\":%d}\n",
                    totalBatches, totalImages
            );
            callback.accept(batchInfo);
            log.debug("📤 已发送批次信息: {} 批次, {} 张图片", totalBatches, totalImages);
        }
    }

    /**
     * 并行处理图片批次
     */
    protected void processImageBatchesInParallel(List<List<ContentBlock>> batches, ProcessingContext context) {
        log.info("🚀 开始并行处理 {} 个批次", batches.size());
        long startTime = System.currentTimeMillis();

        List<CompletableFuture<Void>> futures = new ArrayList<>();

        for (int i = 0; i < batches.size(); i++) {
            final int batchIndex = i;
            final List<ContentBlock> batch = batches.get(i);

            CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
                try {
                    log.debug("⚙️ [Thread: {}] 处理批次 #{}",
                            Thread.currentThread().getName(), batchIndex + 1);

                    // 发送批次开始标记
                    sendBatchStartMarker(context, batchIndex, batches.size());

                    // 处理批次中的所有图片
                    processImageBatch(batch, context, batchIndex);

                    // 发送批次结束标记
                    sendBatchEndMarker(context, batchIndex);

                    log.debug("✅ [Thread: {}] 批次 #{} 完成",
                            Thread.currentThread().getName(), batchIndex + 1);

                } catch (Exception e) {
                    log.error("❌ 批次 #{} 处理失败", batchIndex + 1, e);
                }
            }, visionLlmExecutor);

            futures.add(future);
        }

        // 等待所有批次完成
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();

        long elapsed = System.currentTimeMillis() - startTime;
        log.info("✅ 并行处理完成: 耗时 {}ms, 平均每批次 {}ms",
                elapsed, elapsed / batches.size());
    }

    /**
     * 串行处理图片批次
     */
    protected void processImageBatchesSequentially(List<List<ContentBlock>> batches, ProcessingContext context) {
        log.info("🔄 开始串行处理 {} 个批次", batches.size());
        long startTime = System.currentTimeMillis();

        for (int i = 0; i < batches.size(); i++) {
            try {
                log.debug("⚙️ 处理批次 #{}/{}", i + 1, batches.size());

                // 发送批次开始标记
                sendBatchStartMarker(context, i, batches.size());

                // 处理批次
                processImageBatch(batches.get(i), context, i);

                // 发送批次结束标记
                sendBatchEndMarker(context, i);

                log.debug("✅ 批次 #{} 完成", i + 1);

            } catch (Exception e) {
                log.error("❌ 批次 #{} 处理失败", i + 1, e);
            }
        }

        long elapsed = System.currentTimeMillis() - startTime;
        log.info("✅ 串行处理完成: 耗时 {}ms, 平均每批次 {}ms",
                elapsed, elapsed / batches.size());
    }

    /**
     * 处理单个图片批次
     */
    protected void processImageBatch(List<ContentBlock> batch, ProcessingContext context, int batchIndex) {
        for (ContentBlock block : batch) {
            for (ExtractedImage image : block.getImages()) {
                try {
                    // 先应用 ImageHandler
                    ExtractedImage processedImage = applyImageHandlers(image, context);

                    // 调用 Vision LLM 分析图片
                    String imageDescription = analyzeImageWithRetry(processedImage, context, batchIndex);

                    // 将描述保存到图片元数据
                    if (processedImage.getMetadata() == null) {
                        processedImage.setMetadata(new HashMap<>());
                    }
                    processedImage.getMetadata().put("visionDescription", imageDescription);
                    processedImage.getMetadata().put("batchIndex", batchIndex);

                    log.debug("🖼️ 图片分析完成: {} 字符 (批次 {})",
                            imageDescription.length(), batchIndex);

                } catch (Exception e) {
                    log.error("❌ 图片分析失败 (批次 {}): {}", batchIndex, e.getMessage());
                    if (image.getMetadata() == null) {
                        image.setMetadata(new HashMap<>());
                    }
                    image.getMetadata().put("visionDescription", "[图片分析失败: " + e.getMessage() + "]");
                }
            }
        }
    }

    // ====================== 提取结果存储方法 ======================

    /**
     * 保存成功的提取结果
     */
    protected void saveExtractionResult(ProcessingContext context, ProcessingResult result, long startTime) {
        if (extractionResultService == null) {
            log.debug("⚠️ [Storage] DocumentExtractionResultService 未配置，跳过保存");
            return;
        }

        try {
            DocumentExtractionResult extractionResult = buildExtractionResult(
                    context, result, startTime, "COMPLETED", null);

            if (extractionResult != null) {
                extractionResultService.save(extractionResult);
                log.debug("✅ [Storage] 提取结果已保存: documentId={}", extractionResult.getDocumentId());
            }

        } catch (Exception e) {
            log.warn("⚠️ [Storage] 保存提取结果失败: {}", e.getMessage());
            // 不抛出异常，避免影响主流程
        }
    }

    /**
     * 保存失败的提取结果
     */
    protected void saveFailedExtractionResult(ProcessingContext context, Exception error, long startTime) {
        if (extractionResultService == null) {
            return;
        }

        try {
            DocumentExtractionResult extractionResult = buildExtractionResult(
                    context, null, startTime, "FAILED", error.getMessage());

            if (extractionResult != null) {
                extractionResultService.save(extractionResult);
                log.debug("✅ [Storage] 失败记录已保存: documentId={}", extractionResult.getDocumentId());
            }

        } catch (Exception e) {
            log.warn("⚠️ [Storage] 保存失败记录失败: {}", e.getMessage());
        }
    }

    /**
     * 构建 DocumentExtractionResult 对象
     */
    protected DocumentExtractionResult buildExtractionResult(
            ProcessingContext context,
            ProcessingResult result,
            long startTime,
            String status,
            String errorMessage) {

        try {
            long completedTime = System.currentTimeMillis();
            long duration = completedTime - startTime;

            DocumentExtractionResult.DocumentExtractionResultBuilder builder = DocumentExtractionResult.builder()
                    .documentId(getDocumentId(context))
                    .fileName(context.getOriginalFileName())
                    .fileExtension(context.getFileExtension())
                    .extractionMethod(getName())
                    .status(status)
                    .startTime(startTime)
                    .completedTime(completedTime)
                    .duration(duration)
                    .createdAt(completedTime)
                    .updatedAt(completedTime);

            // 文件大小
            if (context.getFileSize() > 0) {
                builder.fileSize(context.getFileSize());
            }

            // 文件MD5
            if (context.getOptions() != null && context.getOptions().containsKey("fileMd5")) {
                Object md5 = context.getOptions().get("fileMd5");
                if (md5 != null) {
                    builder.fileMd5(md5.toString());
                }
            }

            // 提取的文本内容
            if (result != null && result.getContent() != null) {
                builder.extractedText(result.getContent());
            }

            // 提取模型
            if (visionAIService != null) {
                builder.extractionModel("vision-llm");
            }

            // 错误信息
            if (errorMessage != null) {
                builder.errorMessage(errorMessage);
            }

            // 页数和图片数
            if (result != null && result.getMetadata() != null) {
                Map<String, Object> metadata = result.getMetadata();

                if (metadata.containsKey("totalPages")) {
                    Object pages = metadata.get("totalPages");
                    if (pages instanceof Number) {
                        builder.pageCount(((Number) pages).intValue());
                    }
                }

                if (metadata.containsKey("totalSlides")) {
                    Object slides = metadata.get("totalSlides");
                    if (slides instanceof Number) {
                        builder.pageCount(((Number) slides).intValue());
                    }
                }

                if (result.getImages() != null) {
                    builder.imageCount(result.getImages().size());
                }

                // 元数据（转换为JSON字符串）
                try {
                    String metadataJson = convertMapToJson(metadata);
                    builder.metadata(metadataJson);
                } catch (Exception e) {
                    log.debug("元数据转换失败: {}", e.getMessage());
                }
            }

            return builder.build();

        } catch (Exception e) {
            log.error("❌ 构建 DocumentExtractionResult 失败", e);
            return null;
        }
    }

    /**
     * 获取文档ID（从 context.options 中获取，或使用文件名哈希）
     */
    protected String getDocumentId(ProcessingContext context) {
        if (context.getOptions() != null && context.getOptions().containsKey("documentId")) {
            Object docId = context.getOptions().get("documentId");
            if (docId != null) {
                return docId.toString();
            }
        }

        // 使用文件路径的哈希作为默认ID
        if (context.getFilePath() != null && !context.getFilePath().isEmpty()) {
            return String.valueOf(Math.abs(context.getFilePath().hashCode()));
        }

        // 使用文件名的哈希
        if (context.getOriginalFileName() != null) {
            return String.valueOf(Math.abs(context.getOriginalFileName().hashCode()));
        }

        // 使用时间戳作为最后的备选
        return String.valueOf(System.currentTimeMillis());
    }

    /**
     * 简单的 Map 转 JSON 字符串（避免依赖 Jackson）
     */
    protected String convertMapToJson(Map<String, Object> map) {
        if (map == null || map.isEmpty()) {
            return "{}";
        }

        StringBuilder json = new StringBuilder("{");
        boolean first = true;
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (!first) {
                json.append(",");
            }
            first = false;

            json.append("\"").append(entry.getKey()).append("\":");

            Object value = entry.getValue();
            if (value == null) {
                json.append("null");
            } else if (value instanceof String) {
                json.append("\"").append(escapeJson((String) value)).append("\"");
            } else if (value instanceof Number || value instanceof Boolean) {
                json.append(value);
            } else {
                json.append("\"").append(escapeJson(value.toString())).append("\"");
            }
        }
        json.append("}");

        return json.toString();
    }

    /**
     * 转义 JSON 字符串
     */
    protected String escapeJson(String str) {
        if (str == null) {
            return "";
        }
        return str.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }

    /**
     * 分析图片（带重试机制）
     */
    protected String analyzeImageWithRetry(ExtractedImage image, ProcessingContext context, int batchIndex) {
        int maxRetries = 3;
        Exception lastException = null;

        for (int attempt = 1; attempt <= maxRetries; attempt++) {
            try {
                if (attempt > 1) {
                    log.debug("🔄 重试图片分析 (第 {}/{} 次，批次 {})",
                            attempt, maxRetries, batchIndex);
                    Thread.sleep(2000L * attempt); // 递增等待：2s, 4s, 6s
                }

                String result = analyzeImage(image);

                // 流式模式下发送内容
                sendBatchContent(context, batchIndex, result);

                return result;

            } catch (UnsupportedOperationException e) {
                log.error("❌ 当前AI服务不支持Vision功能: {}", e.getMessage());
                return "[当前AI服务不支持Vision功能]";

            } catch (Exception e) {
                lastException = e;

                boolean isTimeout = e.getMessage() != null &&
                        (e.getMessage().contains("timeout") ||
                         e.getMessage().contains("Connection timed out"));

                if (isTimeout && attempt < maxRetries) {
                    log.warn("⚠️ 图片分析超时，将重试... (尝试 {}/{})", attempt, maxRetries);
                    continue;
                } else {
                    break;
                }
            }
        }

        log.error("❌ 图片分析失败（已重试{}次）: {}", maxRetries,
                lastException.getMessage());
        return "[图片分析失败: " + lastException.getMessage() + "]";
    }

    /**
     * 发送批次开始标记
     */
    protected void sendBatchStartMarker(ProcessingContext context, int batchIndex, int totalBatches) {
        if (context == null || context.getOptions() == null) {
            return;
        }

        Object callbackObj = context.getOptions().get("streamCallback");
        if (callbackObj instanceof java.util.function.Consumer) {
            @SuppressWarnings("unchecked")
            java.util.function.Consumer<String> callback =
                    (java.util.function.Consumer<String>) callbackObj;

            String marker = String.format(
                    "BATCH_START:{\"batchIndex\":%d,\"batchNumber\":%d,\"totalBatches\":%d}\n",
                    batchIndex, batchIndex + 1, totalBatches
            );
            callback.accept(marker);
            log.debug("📤 批次 {} 开始", batchIndex + 1);
        }
    }

    /**
     * 发送批次结束标记
     */
    protected void sendBatchEndMarker(ProcessingContext context, int batchIndex) {
        if (context == null || context.getOptions() == null) {
            return;
        }

        Object callbackObj = context.getOptions().get("streamCallback");
        if (callbackObj instanceof java.util.function.Consumer) {
            @SuppressWarnings("unchecked")
            java.util.function.Consumer<String> callback =
                    (java.util.function.Consumer<String>) callbackObj;

            String marker = String.format(
                    "BATCH_END:{\"batchIndex\":%d,\"batchNumber\":%d}\n",
                    batchIndex, batchIndex + 1
            );
            callback.accept(marker);
            log.debug("✅ 批次 {} 结束", batchIndex + 1);
        }
    }

    /**
     * 发送批次内容
     */
    protected void sendBatchContent(ProcessingContext context, int batchIndex, String content) {
        if (context == null || context.getOptions() == null) {
            return;
        }

        boolean isStreaming = Boolean.TRUE.equals(context.getOptions().get("streaming"));
        Object callbackObj = context.getOptions().get("streamCallback");

        if (isStreaming && callbackObj instanceof java.util.function.Consumer) {
            @SuppressWarnings("unchecked")
            java.util.function.Consumer<String> callback =
                    (java.util.function.Consumer<String>) callbackObj;

            // 使用 BATCH_CONTENT 格式发送
            callback.accept("BATCH_CONTENT:" + batchIndex + ":" + content);
        }
    }
}


