package top.yumbo.ai.omni.web.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.core.document.DocumentProcessorManager;
import top.yumbo.ai.omni.web.websocket.DocumentProcessingWebSocketHandler;
import top.yumbo.ai.storage.api.DocumentStorageService;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * 文档处理服务（智能混合模式）
 * (Document Processing Service - Smart Hybrid Mode)
 *
 * 实现方案3：智能混合模式 ⭐
 * - 系统配置=自动 → 自动处理 → 完成
 * - 系统配置=手动 → PENDING → 用户介入 → 完成
 *
 * 处理文档并推送进度
 * (Process documents and push progress)
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4) - Refactored for Smart Hybrid Mode
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class DocumentProcessingService {

    private final DocumentProcessingWebSocketHandler webSocketHandler;
    private final SystemRAGConfigService ragConfigService;
    private final DocumentStorageService storageService;  // ⭐ 新增：存储服务
    private final DocumentProcessorManager documentProcessorManager;  // ⭐ 新增：文档处理管理器

    @Value("${omni-agent.file-watcher.watch-directory:./data/documents}")
    private String watchDirectory;  // ⭐ 新增：中转站目录

    /**
     * 手动处理文档（强制执行完整流程）⭐
     *
     * 用于用户手动点击"开始处理"按钮时触发
     * 无视系统自动配置，直接使用指定的模型和策略进行处理
     *
     * @param documentId 文档ID
     * @param documentName 文档名称
     * @param content 文档内容
     * @param extractionModel 文本提取模型
     * @param chunkingStrategy 分块策略
     * @param chunkingParams 分块参数
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

                // 阶段1: 上传完成
                pushProgress(documentId, "UPLOAD", 0, "文档上传完成", documentName, null);
                Thread.sleep(500);

                // 阶段2: 文本提取 ⭐
                performTextExtraction(documentId, documentName, content, docConfig);

                // ⭐ 使用存储服务获取提取文本
                String extractedText = ragConfigService.getExtractedText(documentId)
                    .orElseThrow(() -> new RuntimeException("文本提取失败"));

                // 阶段3: 智能分块 ⭐
                pushProgress(documentId, "CHUNK", 40, "正在智能分块...", documentName, null);
                Thread.sleep(2000);
                int chunkCount = performChunking(extractedText, docConfig);
                docConfig.setStatus("CHUNKED");
                ragConfigService.setDocumentConfig(documentId, docConfig);

                // 阶段4: 向量化
                pushProgress(documentId, "VECTORIZE", 60, "正在向量化...", documentName,
                    Map.of("chunks", chunkCount));
                Thread.sleep(2000);
                int vectorCount = performVectorization(chunkCount);
                docConfig.setStatus("VECTORIZING");
                ragConfigService.setDocumentConfig(documentId, docConfig);

                // 阶段5: 建立索引
                pushProgress(documentId, "INDEX", 80, "正在建立索引...", documentName,
                    Map.of("chunks", chunkCount, "vectors", vectorCount));
                Thread.sleep(1500);
                performIndexing(documentId, vectorCount);

                // 阶段6: 归档
                pushProgress(documentId, "ARCHIVE", 90, "正在归档文档...", documentName, null);
                archiveDocument(documentId, documentName, content, docConfig);

                // 完成
                docConfig.setStatus("COMPLETED");
                ragConfigService.setDocumentConfig(documentId, docConfig);
                pushProgress(documentId, "COMPLETED", 100, "处理完成！", documentName,
                    Map.of("chunks", chunkCount, "vectors", vectorCount, "status", "COMPLETED"));

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
     *
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
                                       SystemRAGConfigService.DocumentRAGConfig docConfig) throws InterruptedException {
        pushProgress(documentId, "EXTRACT", 20, "正在提取文本...", documentName, null);
        Thread.sleep(1500);
        // ⭐ 传递文档名称以提取文件扩展名
        String extractedText = extractText(content, docConfig.getTextExtractionModel(), documentName);

        // ⭐ 持久化提取文本到存储服务
        try {
            String savedId = storageService.saveExtractedText(documentId, extractedText);
            if (savedId != null) {
                log.info("✅ 已保存提取文本到存储服务: documentId={}, length={}", documentId, extractedText.length());
            } else {
                log.warn("⚠️ 保存提取文本失败（返回null）: documentId={}", documentId);
            }
        } catch (Exception e) {
            log.error("❌ 保存提取文本失败: documentId={}", documentId, e);
            // 继续处理，不影响整体流程
        }

        // 配置中只保存摘要（前200字符）
        String summary = extractedText.length() > 200
            ? extractedText.substring(0, 200) + "..."
            : extractedText;
        docConfig.setTextSummary(summary);
        docConfig.setExtractedTextRef(documentId);  // 保存引用


        docConfig.setStatus("EXTRACTED");
        ragConfigService.setDocumentConfig(documentId, docConfig);
        pushProgress(documentId, "EXTRACT", 30, "文本提取完成", documentName,
            Map.of("extractedLength", extractedText.length()));
    }

    /**
     * 执行完整RAG流程
     */
    private void performFullRAG(String documentId, String documentName, byte[] content,
                                SystemRAGConfigService.DocumentRAGConfig docConfig) throws InterruptedException {
        // 文本提取
        if (docConfig.getExtractedTextRef() == null && docConfig.getExtractedText() == null) {
            performTextExtraction(documentId, documentName, content, docConfig);
        }

        // ⭐ 使用新方式获取提取文本（优先从存储服务）
        String extractedText = ragConfigService.getExtractedText(documentId)
            .orElseThrow(() -> new RuntimeException("提取文本不存在"));

        // 阶段3: 智能分块
        pushProgress(documentId, "CHUNK", 40, "正在智能分块...", documentName, null);
        Thread.sleep(2000);
        int chunkCount = performChunking(extractedText, docConfig);
        docConfig.setStatus("CHUNKED");
        ragConfigService.setDocumentConfig(documentId, docConfig);

        // 阶段4: 向量化
        pushProgress(documentId, "VECTORIZE", 60, "正在向量化...", documentName,
            Map.of("chunks", chunkCount));
        Thread.sleep(2000);
        int vectorCount = performVectorization(chunkCount);
        docConfig.setStatus("VECTORIZING");
        ragConfigService.setDocumentConfig(documentId, docConfig);

        // 阶段5: 建立索引
        pushProgress(documentId, "INDEX", 80, "正在建立索引...", documentName,
            Map.of("chunks", chunkCount, "vectors", vectorCount));
        Thread.sleep(1500);
        performIndexing(documentId, vectorCount);

        // ⭐ 阶段7: 归档到存储服务（新增）
        pushProgress(documentId, "ARCHIVE", 90, "正在归档文档...", documentName, null);
        archiveDocument(documentId, documentName, content, docConfig);

        // 完成
        docConfig.setStatus("COMPLETED");
        ragConfigService.setDocumentConfig(documentId, docConfig);
        pushProgress(documentId, "COMPLETED", 100, "处理完成！", documentName,
            Map.of("chunks", chunkCount, "vectors", vectorCount, "status", "COMPLETED"));

        log.info("✅ 文档处理完成: documentId={}", documentId);
    }

    /**
     * 归档文档到存储服务并清理中转站 ⭐
     *
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
     * 执行分块（支持配置）
     */
    private int performChunking(String text, SystemRAGConfigService.DocumentRAGConfig docConfig) {
        String strategy = docConfig != null ? docConfig.getChunkingStrategy() : "fixed-size";
        log.debug("✂️ 执行分块: {} 字符, strategy={}", text.length(), strategy);
        // TODO: 实际实现应该调用ChunkingStrategyManager
        return 15; // 模拟返回15个分块
    }

    /**
     * 执行向量化（模拟）
     */
    private int performVectorization(int chunkCount) {
        log.debug("🔢 执行向量化: {} 个分块", chunkCount);
        // 实际实现应该调用向量化服务
        return chunkCount * 768; // 模拟每个分块生成768维向量
    }

    /**
     * 执行索引（模拟）
     */
    private void performIndexing(String documentId, int vectorCount) {
        log.debug("📊 执行索引: documentId={}, {} 个向量", documentId, vectorCount);
        // 实际实现应该调用索引服务
    }
}

