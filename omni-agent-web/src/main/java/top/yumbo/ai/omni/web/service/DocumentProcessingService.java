package top.yumbo.ai.omni.web.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.web.websocket.DocumentProcessingWebSocketHandler;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * 文档处理服务
 * (Document Processing Service)
 *
 * 处理文档并推送进度
 * (Process documents and push progress)
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class DocumentProcessingService {

    private final DocumentProcessingWebSocketHandler webSocketHandler;
    private final SystemRAGConfigService ragConfigService;

    /**
     * 处理文档（检查配置决定是否自动执行）
     */
    public CompletableFuture<Void> processDocument(String documentId, String documentName, byte[] content) {
        return CompletableFuture.runAsync(() -> {
            try {
                log.info("📄 开始处理文档: documentId={}, name={}", documentId, documentName);

                // 获取文档配置
                SystemRAGConfigService.DocumentRAGConfig docConfig =
                    ragConfigService.getDocumentConfig(documentId);

                // 阶段1: 上传
                pushProgress(documentId, "UPLOAD", 0, "文档上传完成", documentName, null);
                Thread.sleep(500);

                // 检查是否自动文本提取
                if (ragConfigService.isAutoTextExtraction()) {
                    // 自动文本提取
                    performTextExtraction(documentId, documentName, content, docConfig);
                } else {
                    // 等待用户配置
                    docConfig.setStatus("PENDING");
                    ragConfigService.setDocumentConfig(documentId, docConfig);
                    pushProgress(documentId, "EXTRACT", 10, "等待配置文本提取方式...", documentName,
                        Map.of("status", "PENDING", "message", "请在文本提取配置中选择提取方式"));
                    log.info("⏸️ 文档等待配置: documentId={}", documentId);
                    return; // 暂停，等待用户配置
                }

                // 检查是否自动RAG
                if (!ragConfigService.isAutoRAG()) {
                    // 等待用户配置分块策略
                    docConfig.setStatus("EXTRACTED");
                    ragConfigService.setDocumentConfig(documentId, docConfig);
                    pushProgress(documentId, "CHUNK", 40, "等待配置分块策略...", documentName,
                        Map.of("status", "PENDING", "message", "请在分块配置中选择分块策略"));
                    log.info("⏸️ 文档等待配置分块: documentId={}", documentId);
                    return; // 暂停，等待用户配置
                }

                // 自动执行完整流程
                performFullRAG(documentId, documentName, content, docConfig);

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
        String extractedText = extractText(content, docConfig.getTextExtractionModel());
        docConfig.setExtractedText(extractedText);
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
        if (docConfig.getExtractedText() == null) {
            performTextExtraction(documentId, documentName, content, docConfig);
        }

        String extractedText = docConfig.getExtractedText();

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

        // 完成
        docConfig.setStatus("COMPLETED");
        ragConfigService.setDocumentConfig(documentId, docConfig);
        pushProgress(documentId, "COMPLETED", 100, "处理完成！", documentName,
            Map.of("chunks", chunkCount, "vectors", vectorCount, "status", "COMPLETED"));

        log.info("✅ 文档处理完成: documentId={}", documentId);
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
     * 提取文本（模拟）
     */
    private String extractText(byte[] content) {
        return extractText(content, "standard");
    }

    /**
     * 提取文本（支持不同模型）
     */
    private String extractText(byte[] content, String model) {
        log.debug("📝 提取文本: {} bytes, model={}", content.length, model);
        // TODO: 实际实现应该根据model调用不同的提取服务
        // standard - 标准文本提取
        // vision-llm - Vision LLM提取（用于图片、PPT等）
        // ocr - OCR提取
        return "模拟提取的文本内容...";
    }

    /**
     * 执行分块（模拟）
     */
    private int performChunking(String text) {
        return performChunking(text, null);
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

