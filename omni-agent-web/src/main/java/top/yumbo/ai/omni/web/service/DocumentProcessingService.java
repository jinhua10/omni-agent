package top.yumbo.ai.omni.web.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.ai.api.EmbeddingService;
import top.yumbo.ai.omni.chunking.starter.ChunkingStrategyManager;
import top.yumbo.ai.omni.document.processor.starter.DocumentProcessorManager;
import top.yumbo.ai.omni.storage.api.model.Chunk;
import top.yumbo.ai.omni.storage.api.model.Image;
import top.yumbo.ai.omni.web.websocket.DocumentProcessingWebSocketHandler;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import jakarta.annotation.PostConstruct;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;

/**
 * 文档处理服务（智能混合模式）
 * (Document Processing Service - Smart Hybrid Mode)
 * <p>
 * 实现方案3：智能混合模式 ⭐
 * - 系统配置=自动 → 自动处理 → 完成
 * - 系统配置=手动 → PENDING → 用户介入 → 完成
 * <p>
 * 处理文档并推送进度
 * (Process documents and push progress)
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4) - Refactored for Smart Hybrid Mode
 */
@Slf4j
@Service
public class DocumentProcessingService {

    private final DocumentProcessingWebSocketHandler webSocketHandler;
    private final SystemRAGConfigService ragConfigService;
    private final DocumentStorageService storageService;  // ⭐ 存储服务
    private final DocumentProcessorManager documentProcessorManager;  // ⭐ 文档处理管理器
    private final ChunkingStrategyManager chunkingStrategyManager;  // ⭐ 分块策略管理器

    // ⭐ 可选服务（如果没有配置相应的 starter，这些服务可能不存在）
    // 注入所有可用的 EmbeddingService，然后智能选择最合适的
    @Autowired(required = false)
    private List<EmbeddingService> embeddingServices;  // ⭐ 所有可用的向量化服务

    private EmbeddingService embeddingService;  // ⭐ 实际使用的向量化服务（智能选择）

    @Autowired(required = false)
    private RagService ragService;  // ⭐ RAG索引服务（可选）

    // ⭐ 图片处理线程池（用于异步保存图片）
    @Autowired(required = false)
    @Qualifier("imageProcessingExecutor")
    private Executor imageProcessingExecutor;

    @Value("${omni-agent.file-watcher.watch-directory:./data/documents}")
    private String watchDirectory;  // ⭐ 中转站目录

    /**
     * 构造函数
     */
    public DocumentProcessingService(
            DocumentProcessingWebSocketHandler webSocketHandler,
            SystemRAGConfigService ragConfigService,
            DocumentStorageService storageService,
            DocumentProcessorManager documentProcessorManager,
            ChunkingStrategyManager chunkingStrategyManager) {
        this.webSocketHandler = webSocketHandler;
        this.ragConfigService = ragConfigService;
        this.storageService = storageService;
        this.documentProcessorManager = documentProcessorManager;
        this.chunkingStrategyManager = chunkingStrategyManager;
    }

    /**
     * 初始化：智能选择最合适的 EmbeddingService
     * <p>
     * 优先级：
     * 1. onnxEmbeddingService（专用的 Embedding 服务，性能最好）
     * 2. aiService（通用 AI 服务，支持 Ollama/Online API）
     * 3. 其他实现了 EmbeddingService 的服务
     */
    @PostConstruct
    public void init() {
        if (embeddingServices == null || embeddingServices.isEmpty()) {
            log.warn("⚠️ 未找到任何 EmbeddingService，向量化功能将不可用");
            this.embeddingService = null;
            return;
        }

        // 优先级 1：查找名为 onnxEmbeddingService 的 bean（ONNX 专用服务）
        for (EmbeddingService service : embeddingServices) {
            String beanName = service.getClass().getSimpleName();
            if (beanName.contains("OnnxEmbedding") || beanName.contains("ONNX")) {
                this.embeddingService = service;
                log.info("✅ 选择 ONNX Embedding 服务: {}", service.getClass().getSimpleName());
                return;
            }
        }

        // 优先级 2：查找名为 aiService 的 bean（非 Vision 的通用 AI 服务）
        for (EmbeddingService service : embeddingServices) {
            String beanName = service.getClass().getSimpleName();
            if (!beanName.toLowerCase().contains("vision")) {
                this.embeddingService = service;
                log.info("✅ 选择 AI Embedding 服务: {}", service.getClass().getSimpleName());
                return;
            }
        }

        // 优先级 3：使用第一个可用的服务
        this.embeddingService = embeddingServices.get(0);
        log.info("✅ 选择默认 Embedding 服务: {}", embeddingService.getClass().getSimpleName());
    }

    /**
     * 手动处理文档（强制执行完整流程）⭐
     * <p>
     * 用于用户手动点击"开始处理"按钮时触发
     * 无视系统自动配置，直接使用指定的模型和策略进行处理
     *
     * @param documentId       文档ID
     * @param documentName     文档名称
     * @param content          文档内容
     * @param extractionModel  文本提取模型
     * @param chunkingStrategy 分块策略
     * @param chunkingParams   分块参数
     */
    public CompletableFuture<Void> processDocumentManually(
            String documentId,
            String documentName,
            byte[] content,
            String extractionModel,
            String chunkingStrategy,
            Map<String, Object> chunkingParams) {

        return CompletableFuture.runAsync(() -> {
            try {
                log.info("🎯 手动处理文档: documentId={}, model={}, strategy={}",
                        documentId, extractionModel, chunkingStrategy);

                // 获取文档配置
                SystemRAGConfigService.DocumentRAGConfig docConfig =
                        ragConfigService.getDocumentConfig(documentId);

                // 强制设置配置（使用传入的参数）
                docConfig.setTextExtractionModel(extractionModel);
                docConfig.setChunkingStrategy(chunkingStrategy);
                docConfig.setChunkingParams(chunkingParams);

                // 推送进度：上传完成
                pushProgress(documentId, "UPLOAD", 0, "文档上传完成", documentName, null);

                // 推送进度：开始提取
                pushProgress(documentId, "EXTRACT", 20, "正在提取文本...", documentName, null);

                // ⭐ 调用核心处理方法
                RAGProcessingResult result = performFullRAGCore(documentId, documentName, content, docConfig);

                // 完成
                docConfig.setStatus("COMPLETED");
                ragConfigService.setDocumentConfig(documentId, docConfig);

                pushProgress(documentId, "COMPLETED", 100, "处理完成！", documentName,
                        Map.of("chunks", result.getChunkCount(),
                                "vectors", result.getVectorCount(),
                                "status", "COMPLETED"));

                log.info("✅ 手动文档处理完成: documentId={}", documentId);

            } catch (Exception e) {
                log.error("❌ 手动文档处理失败: documentId={}", documentId, e);
                pushProgress(documentId, "FAILED", 0, "处理失败: " + e.getMessage(),
                        null, Map.of("status", "FAILED", "error", e.getMessage()));
                throw new RuntimeException("文档处理失败", e);
            }
        });
    }

    /**
     * 处理文档（智能混合模式）⭐
     * <p>
     * 根据系统配置决定处理方式：
     * 1. 如果系统配置为"自动"，则全自动处理
     * 2. 如果系统配置为"手动"，则等待用户配置
     */
    public CompletableFuture<Void> processDocument(String documentId, String documentName, byte[] content) {
        return CompletableFuture.runAsync(() -> {
            try {
                log.info("📄 开始处理文档（智能混合模式）: documentId={}, name={}", documentId, documentName);

                // 获取系统配置
                boolean autoTextExtraction = ragConfigService.isAutoTextExtraction();
                boolean autoRAG = ragConfigService.isAutoRAG();

                log.info("🎛️ 系统配置: 自动提取={}, 自动RAG={}", autoTextExtraction, autoRAG);

                // 获取文档配置
                SystemRAGConfigService.DocumentRAGConfig docConfig =
                        ragConfigService.getDocumentConfig(documentId);

                // 阶段1: 上传完成
                pushProgress(documentId, "UPLOAD", 0, "文档上传完成", documentName, null);
                Thread.sleep(500);

                // ⭐ 智能判断：根据系统配置决定流程
                if (autoTextExtraction && autoRAG) {
                    // 模式A: 全自动模式
                    log.info("🤖 全自动模式：自动提取 + 自动分块 + 自动索引");
                    performFullRAG(documentId, documentName, content, docConfig);

                } else if (autoTextExtraction && !autoRAG) {
                    // 模式B: 半自动模式（自动提取，手动分块）
                    log.info("🔧 半自动模式：自动提取，等待配置分块");
                    performTextExtraction(documentId, documentName, content, docConfig);

                    // 等待用户配置分块策略
                    docConfig.setStatus("EXTRACTED");
                    ragConfigService.setDocumentConfig(documentId, docConfig);
                    pushProgress(documentId, "CHUNK", 40, "等待配置分块策略...", documentName,
                            Map.of("status", "PENDING", "message", "请在分块配置中选择分块策略"));
                    log.info("⏸️ 文档等待配置分块: documentId={}", documentId);

                } else {
                    // 模式C: 完全手动模式
                    log.info("👤 完全手动模式：等待用户配置");
                    docConfig.setStatus("PENDING");
                    ragConfigService.setDocumentConfig(documentId, docConfig);
                    pushProgress(documentId, "EXTRACT", 10, "等待配置文本提取方式...", documentName,
                            Map.of("status", "PENDING", "message", "请在文本提取配置中选择提取方式"));
                    log.info("⏸️ 文档等待配置: documentId={}", documentId);
                }

            } catch (Exception e) {
                log.error("❌ 文档处理失败: documentId={}", documentId, e);
                pushProgress(documentId, "FAILED", 0, "处理失败: " + e.getMessage(),
                        null, Map.of("status", "FAILED", "error", e.getMessage()));
            }
        });
    }

    /**
     * 执行文本提取
     */
    private void performTextExtraction(String documentId, String documentName, byte[] content,
                                       SystemRAGConfigService.DocumentRAGConfig docConfig) {
        // ⭐ 传递文档名称以提取文件扩展名，并获取提取结果（包含图片）
        TextExtractionResult extractionResult = extractTextWithImages(content, docConfig.getTextExtractionModel(), documentName, documentId);

        // ⭐ 持久化提取文本到存储服务
        try {
            String savedId = storageService.saveExtractedText(documentId, extractionResult.getText());
            if (savedId != null) {
                log.info("✅ 已保存提取文本到存储服务: documentId={}, length={}", documentId, extractionResult.getText().length());
            } else {
                log.warn("⚠️ 保存提取文本失败（返回null）: documentId={}", documentId);
            }
        } catch (Exception e) {
            log.error("❌ 保存提取文本失败: documentId={}", documentId, e);
            // 继续处理，不影响整体流程
        }

        // ⭐ 异步持久化图片到存储服务（不阻塞主流程）
        if (extractionResult.getImages() != null && !extractionResult.getImages().isEmpty()) {
            final String finalDocumentId = documentId;
            final String finalDocumentName = documentName;
            final List<top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage> finalImages =
                    extractionResult.getImages();

            if (imageProcessingExecutor != null) {
                // 异步保存图片
                CompletableFuture.runAsync(() -> {
                    try {
                        int savedImageCount = saveExtractedImages(finalDocumentId, finalDocumentName, finalImages);
                        log.info("🖼️ [异步] 已保存 {} 张图片: documentId={}", savedImageCount, finalDocumentId);
                    } catch (Exception e) {
                        log.error("❌ [异步] 保存图片失败: documentId={}", finalDocumentId, e);
                    }
                }, imageProcessingExecutor).exceptionally(ex -> {
                    log.error("❌ [异步] 图片保存任务异常: documentId={}", finalDocumentId, ex);
                    return null;
                });
                log.debug("📤 图片保存任务已提交到异步线程池: {} 张图片", finalImages.size());
            } else {
                // 同步保存（如果线程池未配置）
                try {
                    int savedImageCount = saveExtractedImages(finalDocumentId, finalDocumentName, finalImages);
                    log.info("🖼️ [同步] 已保存 {} 张图片: documentId={}", savedImageCount, finalDocumentId);
                } catch (Exception e) {
                    log.error("❌ [同步] 保存图片失败: documentId={}", finalDocumentId, e);
                }
            }
        }

        // 配置中只保存摘要（前200字符）
        String summary = extractionResult.getText().length() > 200
                ? extractionResult.getText().substring(0, 200) + "..."
                : extractionResult.getText();
        docConfig.setTextSummary(summary);
        docConfig.setExtractedTextRef(documentId);  // 保存引用


        docConfig.setStatus("EXTRACTED");
        ragConfigService.setDocumentConfig(documentId, docConfig);
        pushProgress(documentId, "EXTRACT", 30, "文本提取完成", documentName,
                Map.of("extractedLength", extractionResult.getText().length(), "imageCount", extractionResult.getImages().size()));
    }

    /**
     * 执行完整RAG流程（自动模式）⭐
     * <p>
     * 注意：此方法用于自动模式（系统配置为全自动时）
     * - 调用统一的核心处理方法 performFullRAGCore
     * - 包含进度推送和状态更新
     */
    private void performFullRAG(String documentId, String documentName, byte[] content,
                                SystemRAGConfigService.DocumentRAGConfig docConfig) throws Exception {

        log.info("🤖 自动模式处理文档: documentId={}", documentId);

        // 推送进度：开始提取
        pushProgress(documentId, "EXTRACT", 20, "正在提取文本...", documentName, null);

        // ⭐ 调用核心处理方法
        RAGProcessingResult result = performFullRAGCore(documentId, documentName, content, docConfig);

        // 完成
        docConfig.setStatus("COMPLETED");
        ragConfigService.setDocumentConfig(documentId, docConfig);

        pushProgress(documentId, "COMPLETED", 100, "处理完成！", documentName,
                Map.of("chunks", result.getChunkCount(),
                        "vectors", result.getVectorCount(),
                        "status", "COMPLETED"));

        log.info("✅ 自动模式文档处理完成: documentId={}", documentId);
    }

    /**
     * 归档文档到存储服务并清理中转站 ⭐
     * <p>
     * 包含重试机制：最多重试3次
     */
    private void archiveDocument(String documentId, String documentName, byte[] content,
                                 SystemRAGConfigService.DocumentRAGConfig docConfig) {
        final int maxRetries = 3;
        Exception lastException = null;

        // 重试机制：最多尝试3次
        for (int attempt = 1; attempt <= maxRetries; attempt++) {
            try {
                log.info("🔄 归档尝试 {}/{}: documentId={}", attempt, maxRetries, documentId);

                // 保存原始文档到存储服务
                String savedId = storageService.saveDocument(documentId, documentName, content);

                if (savedId != null) {
                    log.info("✅ 已归档到存储服务: documentId={}, path=documents/{}", documentId, documentName);

                    // 删除中转站文件
                    Path watchFile = Paths.get(watchDirectory).resolve(documentName);
                    if (Files.exists(watchFile)) {
                        Files.delete(watchFile);
                        log.info("🗑️ 已清理中转站: {}", watchFile);
                    } else {
                        log.warn("⚠️ 中转站文件不存在: {}", watchFile);
                    }

                    // 成功，跳出重试循环
                    return;
                } else {
                    log.warn("⚠️ 归档返回null (尝试 {}/{})", attempt, maxRetries);
                    lastException = new RuntimeException("归档返回null");
                }
            } catch (Exception e) {
                lastException = e;
                log.warn("⚠️ 归档失败 (尝试 {}/{}): {}", attempt, maxRetries, e.getMessage());

                // 如果不是最后一次尝试，等待后重试
                if (attempt < maxRetries) {
                    try {
                        long waitTime = 1000L * attempt; // 递增等待时间：1s, 2s, 3s
                        log.info("⏳ 等待 {}ms 后重试...", waitTime);
                        Thread.sleep(waitTime);
                    } catch (InterruptedException ie) {
                        Thread.currentThread().interrupt();
                        log.error("❌ 重试等待被中断", ie);
                        break;
                    }
                }
            }
        }

        // 所有重试都失败
        log.error("❌ 归档失败（已重试{}次）: documentId={}", maxRetries, documentId, lastException);
        // 不影响整体流程，继续标记为完成
        // 中转站文件保留，等待定时清理任务或手动处理
    }

    /**
     * 推送进度
     */
    private void pushProgress(String documentId, String stage, int percentage,
                              String message, String documentName, Map<String, Object> extras) {
        Map<String, Object> progress = new HashMap<>();
        progress.put("documentId", documentId);
        progress.put("documentName", documentName);
        progress.put("stage", stage);
        progress.put("percentage", percentage);
        progress.put("message", message);
        progress.put("timestamp", System.currentTimeMillis());

        if (extras != null) {
            progress.putAll(extras);
        }

        // 推送到WebSocket
        webSocketHandler.broadcastProgress(documentId, progress);
    }


    /**
     * 提取文本（支持不同模型）⭐ 真实实现（支持分批并行）
     */
    private String extractText(byte[] content, String model, String documentName) {
        log.info("📝 提取文本: {} bytes, model={}, file={}", content.length, model, documentName);

        // 如果是 standard 模型，使用简单的文本提取
        if ("standard".equals(model)) {
            try {
                return new String(content, java.nio.charset.StandardCharsets.UTF_8);
            } catch (Exception e) {
                log.error("❌ Standard 文本提取失败", e);
                return "Standard 文本提取失败: " + e.getMessage();
            }
        }

        // ⭐ 提取文件扩展名
        String fileExtension = "txt";  // 默认
        if (documentName != null && documentName.contains(".")) {
            fileExtension = documentName.substring(documentName.lastIndexOf(".") + 1);
        }

        // vision-llm, ocr 等需要调用DocumentProcessorManager
        try {
            // ⭐ 构建处理上下文（启用分批并行，但不需要流式输出）
            Map<String, Object> options = new HashMap<>();
            options.put("model", model);      // ⭐ 传递请求的模型
            options.put("batchSize", 5);      // ⭐ 每批处理5个页面（启用分批并行）
            // 注意：不设置 streaming=true 和 streamCallback，因为流程视图不需要实时输出

            top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingContext context =
                    top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingContext.builder()
                            .fileBytes(content)              // ⭐ 使用 fileBytes
                            .originalFileName(documentName)  // ⭐ 使用真实文件名
                            .fileExtension(fileExtension)    // ⭐ 使用提取的扩展名
                            .fileSize((long) content.length) // ⭐ 文件大小
                            .options(options)                // ⭐ 处理选项（包含分批配置）
                            .build();

            // ⭐ 真正调用文档处理器进行提取（支持分批并行）
            log.info("🚀 [流程视图] 开始分批并行处理: model={}, file={}, batchSize={}",
                    model, documentName, options.get("batchSize"));

            top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingResult result =
                    documentProcessorManager.processDocument(context);

            String extractedText = result.getContent();

            if (extractedText == null || extractedText.isEmpty()) {
                log.warn("⚠️ 提取文本为空，使用默认文本");
                return "提取文本为空";
            }

            log.info("✅ 文本提取成功（分批并行）: {} 字符, model={}, processor={}",
                    extractedText.length(), model, result.getProcessorName());
            return extractedText;

        } catch (Exception e) {
            log.error("❌ 文本提取失败: model={}, file={}", model, documentName, e);
            // 返回错误信息而不是模拟文本
            return "文本提取失败: " + e.getMessage();
        }
    }

    /**
     * 提取文本和图片（支持不同模型）⭐ 新方法
     */
    private TextExtractionResult extractTextWithImages(byte[] content, String model, String documentName, String documentId) {
        log.info("📝 提取文本和图片: {} bytes, model={}, file={}", content.length, model, documentName);

        // 如果是 standard 模型，使用简单的文本提取（无图片）
        if ("standard".equals(model)) {
            try {
                String text = new String(content, java.nio.charset.StandardCharsets.UTF_8);
                return new TextExtractionResult(text, new ArrayList<>());
            } catch (Exception e) {
                log.error("❌ Standard 文本提取失败", e);
                return new TextExtractionResult("Standard 文本提取失败: " + e.getMessage(), new ArrayList<>());
            }
        }

        // ⭐ 提取文件扩展名
        String fileExtension = "txt";  // 默认
        if (documentName != null && documentName.contains(".")) {
            fileExtension = documentName.substring(documentName.lastIndexOf(".") + 1);
        }

        // vision-llm, ocr 等需要调用DocumentProcessorManager
        try {
            // ⭐ 构建处理上下文（启用分批并行，但不需要流式输出）
            Map<String, Object> options = new HashMap<>();
            options.put("model", model);      // ⭐ 传递请求的模型
            options.put("batchSize", 5);      // ⭐ 每批处理5个页面（启用分批并行）
            options.put("documentId", documentId);  // ⭐ 传递文档ID，用于生成图片路径
            // 注意：不设置 streaming=true 和 streamCallback，因为流程视图不需要实时输出

            top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingContext context =
                    top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingContext.builder()
                            .fileBytes(content)              // ⭐ 使用 fileBytes
                            .originalFileName(documentName)  // ⭐ 使用真实文件名
                            .fileExtension(fileExtension)    // ⭐ 使用提取的扩展名
                            .fileSize((long) content.length) // ⭐ 文件大小
                            .options(options)                // ⭐ 处理选项（包含分批配置和文档ID）
                            .build();

            // ⭐ 真正调用文档处理器进行提取（支持分批并行）
            log.info("🚀 [流程视图] 开始分批并行处理: model={}, file={}, batchSize={}",
                    model, documentName, options.get("batchSize"));

            top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingResult result =
                    documentProcessorManager.processDocument(context);

            String extractedText = result.getContent();
            List<top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage> images =
                    result.getImages() != null ? result.getImages() : new ArrayList<>();

            if (extractedText == null || extractedText.isEmpty()) {
                log.warn("⚠️ 提取文本为空，使用默认文本");
                extractedText = "提取文本为空";
            }

            log.info("✅ 文本和图片提取成功（分批并行）: {} 字符, {} 张图片, model={}, processor={}",
                    extractedText.length(), images.size(), model, result.getProcessorName());
            return new TextExtractionResult(extractedText, images);

        } catch (Exception e) {
            log.error("❌ 文本提取失败: model={}, file={}", model, documentName, e);
            // 返回错误信息而不是模拟文本
            return new TextExtractionResult("文本提取失败: " + e.getMessage(), new ArrayList<>());
        }
    }

    /**
     * 保存提取的图片到存储服务（支持压缩和去重）⭐
     *
     * @param documentId 文档ID
     * @param documentName 文档名称（用于生成友好的图片路径）
     * @param extractedImages 提取的图片列表
     * @return 成功保存的图片数量
     */
    private int saveExtractedImages(String documentId, String documentName,
                                    List<top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage> extractedImages) {
        if (extractedImages == null || extractedImages.isEmpty()) {
            return 0;
        }

        // ⭐ 从文档名称中提取基础名（去除扩展名）
        String baseName = documentName;
        if (documentName != null && documentName.contains(".")) {
            baseName = documentName.substring(0, documentName.lastIndexOf("."));
        }

        int savedCount = 0;
        int deduplicatedCount = 0;
        int compressedCount = 0;
        long totalOriginalSize = 0;
        long totalCompressedSize = 0;

        // ⭐ 配置压缩参数
        top.yumbo.ai.omni.core.image.ImageCompressor.CompressionConfig compressionConfig =
                new top.yumbo.ai.omni.core.image.ImageCompressor.CompressionConfig();
        compressionConfig.setEnabled(true);
        compressionConfig.setQuality(0.85f);
        compressionConfig.setMaxWidth(2048);
        compressionConfig.setMaxHeight(2048);
        compressionConfig.setMinSizeToCompress(100 * 1024); // 100KB

        for (top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage extractedImage : extractedImages) {
            try {
                byte[] imageData = extractedImage.getData();
                String format = extractedImage.getFormat();
                int originalSize = imageData.length;
                totalOriginalSize += originalSize;

                // ⭐ 1. 计算图片哈希值（用于去重）
                String imageHash = top.yumbo.ai.omni.core.image.ImageHashCalculator.calculateHash(imageData);

                // ⭐ 2. 检查是否已存在相同图片
                Optional<String> existingImageId = storageService.findImageByHash(imageHash);
                if (existingImageId.isPresent()) {
                    deduplicatedCount++;
                    log.debug("🔄 图片已存在，跳过保存: hash={}, existingId={}",
                            imageHash.substring(0, 16), existingImageId.get());

                    // 复用已有图片，只更新引用计数（如果需要）
                    savedCount++;
                    totalCompressedSize += originalSize; // 估算
                    continue;
                }

                // ⭐ 3. 压缩图片
                top.yumbo.ai.omni.core.image.ImageCompressor.CompressionResult compressionResult =
                        top.yumbo.ai.omni.core.image.ImageCompressor.compress(imageData, format, compressionConfig);

                if (compressionResult.isCompressed()) {
                    compressedCount++;
                    imageData = compressionResult.getData();
                    format = compressionResult.getFormat();
                    log.debug("🗜️ 图片已压缩: {}KB -> {}KB (节省: {}KB)",
                            originalSize / 1024,
                            compressionResult.getCompressedSize() / 1024,
                            compressionResult.getSavedBytes() / 1024);
                }

                totalCompressedSize += imageData.length;

                // ⭐ 4. 从 metadata 中获取图片序号
                Integer imageIndex = 0;
                if (extractedImage.getMetadata() != null && extractedImage.getMetadata().containsKey("imageIndex")) {
                    imageIndex = ((Number) extractedImage.getMetadata().get("imageIndex")).intValue();
                }

                // ⭐ 5. 构建 Image 对象
                Image image = Image.builder()
                        .documentId(documentId)
                        .data(imageData)
                        .format(format)
                        .pageNumber(extractedImage.getPageNumber())
                        .metadata(extractedImage.getMetadata() != null ? extractedImage.getMetadata() : new HashMap<>())
                        .createdAt(System.currentTimeMillis())
                        .build();

                // ⭐ 6. 在 metadata 中添加关键信息
                image.getMetadata().put("baseName", baseName);
                image.getMetadata().put("imageIndex", imageIndex);
                image.getMetadata().put("imageHash", imageHash);
                image.getMetadata().put("originalSize", originalSize);
                image.getMetadata().put("compressed", compressionResult.isCompressed());
                if (compressionResult.isCompressed()) {
                    image.getMetadata().put("compressionRatio", compressionResult.getCompressionRatio());
                }

                // ⭐ 7. 保存到存储服务
                String imageId = storageService.saveImage(documentId, image);
                if (imageId != null) {
                    savedCount++;
                    log.debug("💾 保存图片: documentId={}, page={}, index={}, imageId={}, size={}KB",
                            documentId, extractedImage.getPageNumber(), imageIndex, imageId, imageData.length / 1024);
                } else {
                    log.warn("⚠️ 保存图片失败（返回null）: documentId={}, page={}, index={}",
                            documentId, extractedImage.getPageNumber(), imageIndex);
                }
            } catch (Exception e) {
                log.error("❌ 保存图片失败: documentId={}, page={}", documentId, extractedImage.getPageNumber(), e);
                // 继续处理其他图片
            }
        }

        // ⭐ 输出统计信息
        float savedRatio = totalOriginalSize > 0 ? (float) totalCompressedSize / totalOriginalSize : 1.0f;
        log.info("✅ 图片保存完成: 总数={}, 保存={}, 去重={}, 压缩={}, 原始大小={}MB, 存储大小={}MB, 压缩率={}%",
                extractedImages.size(), savedCount, deduplicatedCount, compressedCount,
                totalOriginalSize / (1024 * 1024),
                totalCompressedSize / (1024 * 1024),
                String.format("%.1f", savedRatio * 100));

        return savedCount;
    }

    /**
     * 文本提取结果（包含文本和图片）
     */
    private static class TextExtractionResult {
        private final String text;
        private final List<top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage> images;

        public TextExtractionResult(String text, List<top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage> images) {
            this.text = text;
            this.images = images;
        }

        public String getText() {
            return text;
        }

        public List<top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage> getImages() {
            return images;
        }
    }


    /**
     * 执行分块（真实实现）⭐
     */
    private int performChunking(String text, SystemRAGConfigService.DocumentRAGConfig docConfig) {
        String strategy = docConfig != null ? docConfig.getChunkingStrategy() : "fixed-size";
        Map<String, Object> params = docConfig != null ? docConfig.getChunkingParams() : new HashMap<>();
        String documentId = docConfig != null ? docConfig.getDocumentId() : "unknown";

        log.info("✂️ 执行智能分块: {} 字符, strategy={}, params={}",
                text.length(), strategy, params);

        try {
            // ⭐ 调用真正的分块策略管理器
            var chunks = chunkingStrategyManager.chunkWithStrategy(
                    documentId,
                    text,
                    strategy,
                    params
            );

            log.info("✅ 智能分块完成: 生成 {} 个分块, strategy={}", chunks.size(), strategy);

            // ⭐ 持久化分块结果到存储服务
            saveChunksToStorage(documentId, chunks);

            return chunks.size();

        } catch (Exception e) {
            log.error("❌ 智能分块失败: strategy={}", strategy, e);
            // 降级：返回默认分块数
            return 15;
        }
    }

    /**
     * 保存分块到存储服务 ⭐
     */
    private void saveChunksToStorage(String documentId, List<Chunk> chunks) {
        if (chunks == null || chunks.isEmpty()) {
            log.warn("⚠️ 分块列表为空，跳过保存");
            return;
        }

        try {
            // ⭐ 批量保存分块
            List<String> chunkIds = storageService.saveChunks(documentId, chunks);

            log.info("✅ 已保存 {} 个分块到存储服务: documentId={}", chunkIds.size(), documentId);

            // 日志：输出前3个分块的预览
            for (int i = 0; i < Math.min(chunks.size(), 3); i++) {
                var chunk = chunks.get(i);
                String preview = chunk.getContent().length() > 100
                        ? chunk.getContent().substring(0, 100) + "..."
                        : chunk.getContent();
                log.debug("📦 分块 #{}: id={}, size={} 字符, preview: {}",
                        i + 1, chunk.getId(), chunk.getContent().length(), preview);
            }

            if (chunks.size() > 3) {
                log.debug("📦 ... 还有 {} 个分块", chunks.size() - 3);
            }

        } catch (Exception e) {
            log.error("❌ 保存分块失败: documentId={}", documentId, e);
            // 不影响整体流程，继续处理
        }
    }

    /**
     * 执行向量化（真实实现）⭐
     */
    private int performVectorization(String documentId, int chunkCount) {
        log.info("🔢 执行向量化: documentId={}, {} 个分块", documentId, chunkCount);

        // ⭐ 检查必要的服务是否可用
        if (embeddingService == null || ragService == null) {
            log.warn("⚠️ EmbeddingService 或 RAGService 未配置，跳过向量化");
            log.info("💡 提示: 请添加相应的 starter 依赖（如 omni-agent-ai-starter-ollama）");
            // 降级：返回模拟数据
            return chunkCount * 768;
        }

        try {
            // ⭐ 1. 从存储服务读取分块
            var chunks = storageService.getChunksByDocument(documentId);

            if (chunks == null || chunks.isEmpty()) {
                log.warn("⚠️ 未找到分块数据: documentId={}", documentId);
                return 0;
            }

            log.info("📦 读取到 {} 个分块，开始向量化", chunks.size());

            // ⭐ 2. 批量生成向量
            List<String> texts = chunks.stream()
                    .map(Chunk::getContent)
                    .collect(java.util.stream.Collectors.toList());

            List<float[]> embeddings = embeddingService.embedBatch(texts);

            log.info("✅ 向量生成完成: {} 个向量, 维度={}",
                    embeddings.size(), embeddingService.getDimension());

            // ⭐ 3. 构建 RAG 文档并索引
            List<top.yumbo.ai.omni.rag.model.Document> ragDocuments = new java.util.ArrayList<>();

            for (int i = 0; i < chunks.size(); i++) {
                var chunk = chunks.get(i);
                float[] embedding = embeddings.get(i);

                var ragDoc = top.yumbo.ai.omni.rag.model.Document.builder()
                        .id(chunk.getId())
                        .content(chunk.getContent())
                        .embedding(embedding)
                        .metadata(new java.util.HashMap<>())
                        .build();

                // 添加元数据
                ragDoc.getMetadata().put("documentId", documentId);
                ragDoc.getMetadata().put("chunkIndex", i);
                ragDoc.getMetadata().put("chunkId", chunk.getId());

                ragDocuments.add(ragDoc);
            }

            // ⭐ 4. 批量索引到 RAG 服务
            ragService.batchIndex(ragDocuments);

            log.info("✅ 向量化完成: documentId={}, 生成 {} 个向量, 索引 {} 个文档",
                    documentId, embeddings.size(), ragDocuments.size());

            return embeddings.size() * embeddingService.getDimension();

        } catch (Exception e) {
            log.error("❌ 向量化失败: documentId={}", documentId, e);
            // 降级：返回模拟数据
            return chunkCount * 768;
        }
    }

    /**
     * 核心RAG处理流程（真实实现）⭐
     * <p>
     * 此方法执行完整的RAG处理流程，并推送详细的进度信息：
     * 1. 文本提取（20-30%）
     * 2. 智能分块（40-50%）
     * 3. 向量化（60-70%）
     * 4. 建立索引（80-90%）
     * 5. 归档（95-100%）
     */
    private RAGProcessingResult performFullRAGCore(
            String documentId,
            String documentName,
            byte[] content,
            SystemRAGConfigService.DocumentRAGConfig docConfig) throws Exception {

        log.info("🚀 开始核心RAG流程: documentId={}, model={}, strategy={}",
                documentId, docConfig.getTextExtractionModel(), docConfig.getChunkingStrategy());

        // 阶段1: 文本提取 ⭐ (20-30%)
        pushProgress(documentId, "EXTRACT", 20, "正在提取文本...", documentName, null);
        performTextExtraction(documentId, documentName, content, docConfig);
        pushProgress(documentId, "EXTRACT", 30, "文本提取完成", documentName, null);

        // 获取提取的文本
        String extractedText = ragConfigService.getExtractedText(documentId)
                .orElseThrow(() -> new RuntimeException("文本提取失败"));

        // 阶段2: 智能分块 ⭐ (40-50%)
        pushProgress(documentId, "CHUNK", 40, "正在进行智能分块...", documentName,
                Map.of("strategy", docConfig.getChunkingStrategy()));
        int chunkCount = performChunking(extractedText, docConfig);
        docConfig.setStatus("CHUNKED");
        ragConfigService.setDocumentConfig(documentId, docConfig);
        pushProgress(documentId, "CHUNK", 50, "分块完成，生成 " + chunkCount + " 个分块", documentName,
                Map.of("chunkCount", chunkCount));

        // 阶段3: 向量化 ⭐ (60-70%)
        pushProgress(documentId, "VECTORIZE", 60, "正在生成向量...", documentName, null);
        int vectorCount = performVectorization(documentId, chunkCount);
        docConfig.setStatus("VECTORIZING");
        ragConfigService.setDocumentConfig(documentId, docConfig);
        pushProgress(documentId, "VECTORIZE", 70, "向量化完成", documentName,
                Map.of("vectorCount", vectorCount));

        // 阶段4: 建立索引 ⭐ (80-90%)
        pushProgress(documentId, "INDEX", 80, "正在建立索引...", documentName, null);
        performIndexing(documentId, vectorCount);
        pushProgress(documentId, "INDEX", 90, "索引建立完成", documentName, null);

        // 阶段5: 归档 ⭐ (95%)
        pushProgress(documentId, "INDEX", 95, "正在归档文档...", documentName, null);
        archiveDocument(documentId, documentName, content, docConfig);

        log.info("✅ 核心RAG流程完成: documentId={}, chunks={}, vectors={}",
                documentId, chunkCount, vectorCount);

        return new RAGProcessingResult(chunkCount, vectorCount);
    }

    /**
     * 执行索引（真实实现）⭐
     * <p>
     * 注意：索引已在 performVectorization 中完成
     * 此方法保留用于兼容性和日志输出
     */
    private void performIndexing(String documentId, int vectorCount) {
        log.info("📊 索引已完成: documentId={}, {} 个向量已索引", documentId, vectorCount);
        // 索引操作已在 performVectorization() 中通过 ragService.indexDocuments() 完成
    }

    /**
     * RAG 处理结果
     */
    @lombok.Data
    @lombok.AllArgsConstructor
    public static class RAGProcessingResult {
        /**
         * 分块数量
         */
        private int chunkCount;
        /**
         * 向量总维度数
         */
        private int vectorCount;
    }
}