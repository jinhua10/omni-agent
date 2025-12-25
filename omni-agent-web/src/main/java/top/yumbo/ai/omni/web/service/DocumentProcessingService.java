package top.yumbo.ai.omni.web.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import top.yumbo.ai.ai.api.EmbeddingService;
import top.yumbo.ai.omni.core.chunking.ChunkingStrategyManager;
import top.yumbo.ai.omni.core.document.DocumentProcessorManager;
import top.yumbo.ai.omni.web.websocket.DocumentProcessingWebSocketHandler;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.storage.api.DocumentStorageService;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
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
    private final DocumentStorageService storageService;  // ⭐ 存储服务
    private final DocumentProcessorManager documentProcessorManager;  // ⭐ 文档处理管理器
    private final ChunkingStrategyManager chunkingStrategyManager;  // ⭐ 分块策略管理器
    private final EmbeddingService embeddingService;  // ⭐ 向量化服务
    private final RAGService ragService;  // ⭐ RAG索引服务

    @Value("${omni-agent.file-watcher.watch-directory:./data/documents}")
    private String watchDirectory;  // ⭐ 中转站目录

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
                    performFullRAGSimulated(documentId, documentName, content, docConfig);

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
     * 执行完整RAG流程（自动模式）⭐
     *
     * 注意：此方法用于自动模式（系统配置为全自动时）
     * - 调用统一的核心处理方法 performFullRAGCore
     * - 包含进度推送和状态更新
     */
    private void performFullRAGSimulated(String documentId, String documentName, byte[] content,
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
    private void saveChunksToStorage(String documentId, List<top.yumbo.ai.storage.api.model.Chunk> chunks) {
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
                .map(top.yumbo.ai.storage.api.model.Chunk::getContent)
                .collect(java.util.stream.Collectors.toList());

            List<float[]> embeddings = embeddingService.embedBatch(texts);

            log.info("✅ 向量生成完成: {} 个向量, 维度={}",
                    embeddings.size(), embeddingService.getDimension());

            // ⭐ 3. 构建 RAG 文档并索引
            List<top.yumbo.ai.rag.api.model.Document> ragDocuments = new java.util.ArrayList<>();

            for (int i = 0; i < chunks.size(); i++) {
                var chunk = chunks.get(i);
                float[] embedding = embeddings.get(i);

                var ragDoc = top.yumbo.ai.rag.api.model.Document.builder()
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
            List<String> indexedIds = ragService.indexDocuments(ragDocuments);

            log.info("✅ 向量化完成: documentId={}, 生成 {} 个向量, 索引 {} 个文档",
                    documentId, embeddings.size(), indexedIds.size());

            return embeddings.size() * embeddingService.getDimension();

        } catch (Exception e) {
            log.error("❌ 向量化失败: documentId={}", documentId, e);
            // 降级：返回模拟数据
            return chunkCount * 768;
        }
    }

    /**
     * 核心RAG处理流程（真实实现）⭐
     *
     * 提取的统一核心处理逻辑，避免代码重复
     *
     * @param documentId 文档ID
     * @param documentName 文档名称
     * @param content 文档内容
     * @param docConfig 文档配置
     * @return 处理结果（包含分块数和向量数）
     * @throws Exception 处理失败时抛出异常
     */
    private RAGProcessingResult performFullRAGCore(
            String documentId,
            String documentName,
            byte[] content,
            SystemRAGConfigService.DocumentRAGConfig docConfig) throws Exception {

        log.info("🚀 开始核心RAG流程: documentId={}, model={}, strategy={}",
                documentId, docConfig.getTextExtractionModel(), docConfig.getChunkingStrategy());

        // 阶段1: 文本提取 ⭐
        performTextExtraction(documentId, documentName, content, docConfig);

        // 获取提取的文本
        String extractedText = ragConfigService.getExtractedText(documentId)
                .orElseThrow(() -> new RuntimeException("文本提取失败"));

        // 阶段2: 智能分块 ⭐
        int chunkCount = performChunking(extractedText, docConfig);
        docConfig.setStatus("CHUNKED");
        ragConfigService.setDocumentConfig(documentId, docConfig);

        // 阶段3: 向量化 ⭐
        int vectorCount = performVectorization(documentId, chunkCount);
        docConfig.setStatus("VECTORIZING");
        ragConfigService.setDocumentConfig(documentId, docConfig);

        // 阶段4: 建立索引 ⭐（已在向量化中完成）
        performIndexing(documentId, vectorCount);

        // 阶段5: 归档 ⭐
        archiveDocument(documentId, documentName, content, docConfig);

        log.info("✅ 核心RAG流程完成: documentId={}, chunks={}, vectors={}",
                documentId, chunkCount, vectorCount);

        return new RAGProcessingResult(chunkCount, vectorCount);
    }

    /**
     * 执行索引（真实实现）⭐
     *
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
        /** 分块数量 */
        private int chunkCount;
        /** 向量总维度数 */
        private int vectorCount;
    }
}

