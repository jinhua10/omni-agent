package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;
import top.yumbo.ai.omni.web.model.ApiResponse;
import top.yumbo.ai.omni.web.service.SystemRAGConfigService;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * 文档处理控制器
 * (Document Processing Controller)
 *
 * 职责：
 * - 触发文本提取
 * - 触发智能分块
 * - 触发向量化索引
 * - 重建文档
 *
 * 不负责配置管理（由SystemRAGConfigController负责）
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/documents/processing")
@RequiredArgsConstructor
public class DocumentProcessingController {

    private final SystemRAGConfigService configService;
    private final top.yumbo.ai.omni.web.service.DocumentProcessingService processingService;
    private final top.yumbo.ai.omni.core.document.DocumentProcessorManager documentProcessorManager;
    private final top.yumbo.ai.omni.core.chunking.ChunkingStrategyManager chunkingStrategyManager;
    private final top.yumbo.ai.storage.api.DocumentStorageService storageService;

    /**
     * 触发文本提取（流式SSE）
     * POST /api/documents/processing/{documentId}/extract
     */
    @PostMapping(value = "/{documentId}/extract", produces = "text/event-stream;charset=UTF-8")
    public SseEmitter extractText(
            @PathVariable String documentId,
            @RequestBody ExtractRequest request) {

        SseEmitter emitter = new SseEmitter(5 * 60 * 1000L); // 5分钟超时

        CompletableFuture.runAsync(() -> {
            try {
                log.info("🔍 开始文本提取: documentId={}, model={}", documentId, request.getModel());

                // 更新配置
                SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);
                config.setTextExtractionModel(request.getModel());
                config.setStatus("EXTRACTING");
                config.setUpdatedAt(System.currentTimeMillis());
                configService.setDocumentConfig(documentId, config);

                // 发送进度：开始
                sendProgress(emitter, 10, "正在读取文档...");

                // 读取中转站文件
                byte[] content = readDocumentFile(documentId);
                if (content == null) {
                    sendError(emitter, "文档文件不存在");
                    return;
                }

                sendProgress(emitter, 30, "正在解析文档格式...");

                // 调用实际的文本提取服务
                String extractedText = extractTextWithProcessor(documentId, content, request.getModel());

                sendProgress(emitter, 80, "文本提取完成");

                // 保存提取结果
                config.setExtractedText(extractedText);
                config.setStatus("EXTRACTED");
                config.setUpdatedAt(System.currentTimeMillis());
                configService.setDocumentConfig(documentId, config);

                // 流式发送提取的文本
                sendTextContent(emitter, extractedText);

                sendComplete(emitter, "提取完成");
                log.info("✅ 文本提取完成: documentId={}", documentId);

            } catch (Exception e) {
                log.error("❌ 文本提取失败: documentId={}", documentId, e);
                sendError(emitter, "提取失败: " + e.getMessage());
            }
        });

        setupEmitterCallbacks(emitter, documentId);
        return emitter;
    }

    /**
     * 触发智能分块（流式SSE）
     * POST /api/documents/processing/{documentId}/chunk
     */
    @PostMapping(value = "/{documentId}/chunk", produces = "text/event-stream;charset=UTF-8")
    public SseEmitter chunkDocument(
            @PathVariable String documentId,
            @RequestBody ChunkRequest request) {

        SseEmitter emitter = new SseEmitter(5 * 60 * 1000L);

        CompletableFuture.runAsync(() -> {
            try {
                log.info("✂️ 开始智能分块: documentId={}, strategy={}", documentId, request.getStrategy());

                // 更新配置
                SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);
                config.setChunkingStrategy(request.getStrategy());
                config.setChunkingParams(request.getParams());
                config.setStatus("CHUNKING");
                config.setUpdatedAt(System.currentTimeMillis());
                configService.setDocumentConfig(documentId, config);

                sendProgress(emitter, 20, "正在分析文档结构...");

                // 获取提取的文本
                String extractedText = config.getExtractedText();
                if (extractedText == null || extractedText.isEmpty()) {
                    sendError(emitter, "未找到提取的文本，请先执行文本提取");
                    return;
                }

                sendProgress(emitter, 60, "正在智能分块...");

                // TODO: 调用实际的分块服务
                // List<Chunk> chunks = chunkingStrategyManager.chunk(documentId, extractedText, request.getStrategy());
                int chunkCount = simulateChunking(extractedText, request.getStrategy());

                sendProgress(emitter, 90, String.format("分块完成，共 %d 个分块", chunkCount));

                // 保存分块结果
                // storageService.saveChunks(documentId, chunks);

                config.setStatus("CHUNKED");
                config.setUpdatedAt(System.currentTimeMillis());
                configService.setDocumentConfig(documentId, config);

                sendComplete(emitter, Map.of("chunkCount", chunkCount));
                log.info("✅ 智能分块完成: documentId={}, chunks={}", documentId, chunkCount);

            } catch (Exception e) {
                log.error("❌ 智能分块失败: documentId={}", documentId, e);
                sendError(emitter, "分块失败: " + e.getMessage());
            }
        });

        setupEmitterCallbacks(emitter, documentId);
        return emitter;
    }

    /**
     * 触发完整处理（提取+分块+索引）
     * POST /api/documents/processing/{documentId}/process
     */
    @PostMapping("/{documentId}/process")
    public ApiResponse<Void> processDocument(
            @PathVariable String documentId,
            @RequestBody ProcessRequest request) {

        try {
            log.info("🚀 开始完整处理: documentId={}", documentId);

            // 读取文档内容
            byte[] content = readDocumentFile(documentId);
            if (content == null) {
                return ApiResponse.error("文档文件不存在");
            }

            // 触发异步处理
            processingService.processDocument(documentId, documentId, content)
                    .exceptionally(throwable -> {
                        log.error("❌ 文档处理失败: documentId={}", documentId, throwable);
                        return null;
                    });

            return ApiResponse.success(null, "处理已启动");

        } catch (Exception e) {
            log.error("❌ 启动处理失败: documentId={}", documentId, e);
            return ApiResponse.error("启动失败: " + e.getMessage());
        }
    }

    /**
     * 重建文档（重新处理）
     * POST /api/documents/processing/{documentId}/rebuild
     */
    @PostMapping("/{documentId}/rebuild")
    public ApiResponse<Void> rebuildDocument(
            @PathVariable String documentId,
            @RequestBody RebuildRequest request) {

        try {
            log.info("🔄 重建文档: documentId={}", documentId);

            // 重置状态
            SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);
            config.setStatus("PENDING");
            config.setExtractedText(null);

            if (request.getTextExtractionModel() != null) {
                config.setTextExtractionModel(request.getTextExtractionModel());
            }
            if (request.getChunkingStrategy() != null) {
                config.setChunkingStrategy(request.getChunkingStrategy());
            }
            if (request.getChunkingParams() != null) {
                config.setChunkingParams(request.getChunkingParams());
            }

            config.setUpdatedAt(System.currentTimeMillis());
            configService.setDocumentConfig(documentId, config);

            // 读取文档并触发处理
            byte[] content = readDocumentFile(documentId);
            if (content == null) {
                return ApiResponse.error("文档文件不存在");
            }

            processingService.processDocument(documentId, documentId, content);

            return ApiResponse.success(null, "重建已启动");

        } catch (Exception e) {
            log.error("❌ 重建文档失败: documentId={}", documentId, e);
            return ApiResponse.error("重建失败: " + e.getMessage());
        }
    }

    // ========== 辅助方法 ==========

    private byte[] readDocumentFile(String documentId) {
        try {
            Path documentPath = Paths.get("data/documents", documentId);
            if (Files.exists(documentPath)) {
                return Files.readAllBytes(documentPath);
            }
            log.warn("⚠️ 文档文件不存在: {}", documentPath);
            return null;
        } catch (Exception e) {
            log.error("❌ 读取文档文件失败: {}", documentId, e);
            return null;
        }
    }

    private void sendProgress(SseEmitter emitter, int percent, String message) {
        try {
            emitter.send(SseEmitter.event()
                    .name("message")
                    .data(String.format("{\"type\":\"progress\",\"percent\":%d,\"message\":\"%s\"}",
                            percent, message)));
        } catch (Exception e) {
            log.error("发送进度失败", e);
        }
    }

    private void sendTextContent(SseEmitter emitter, String text) {
        try {
            int chunkSize = 500;
            for (int i = 0; i < text.length(); i += chunkSize) {
                int end = Math.min(i + chunkSize, text.length());
                String chunk = text.substring(i, end)
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"")
                        .replace("\n", "\\n");
                emitter.send(SseEmitter.event()
                        .name("message")
                        .data("{\"type\":\"content\",\"content\":\"" + chunk + "\"}"));
                Thread.sleep(50);
            }
        } catch (Exception e) {
            log.error("发送文本内容失败", e);
        }
    }

    private void sendComplete(SseEmitter emitter, String message) {
        try {
            emitter.send(SseEmitter.event()
                    .name("message")
                    .data("{\"type\":\"complete\",\"message\":\"" + message + "\"}"));
            emitter.complete();
        } catch (Exception e) {
            log.error("发送完成消息失败", e);
        }
    }

    private void sendComplete(SseEmitter emitter, Map<String, Object> data) {
        try {
            String json = new com.fasterxml.jackson.databind.ObjectMapper().writeValueAsString(data);
            emitter.send(SseEmitter.event()
                    .name("message")
                    .data("{\"type\":\"complete\",\"data\":" + json + "}"));
            emitter.complete();
        } catch (Exception e) {
            log.error("发送完成消息失败", e);
        }
    }

    private void sendError(SseEmitter emitter, String message) {
        try {
            emitter.send(SseEmitter.event()
                    .name("message")
                    .data("{\"type\":\"error\",\"message\":\"" + message + "\"}"));
            emitter.complete();
        } catch (Exception e) {
            log.error("发送错误消息失败", e);
        }
    }

    private void setupEmitterCallbacks(SseEmitter emitter, String documentId) {
        emitter.onTimeout(() -> {
            log.warn("⚠️ SSE超时: documentId={}", documentId);
            emitter.complete();
        });
        emitter.onError(e -> {
            log.error("❌ SSE错误: documentId={}", documentId, e);
        });
    }

    // 模拟方法（TODO: 替换为实际实现）
    private String simulateTextExtraction(byte[] content, String model) {
        return "这是模拟提取的文本内容，使用模型: " + model + "\\n文档大小: " + content.length + " 字节";
    }

    /**
     * 使用DocumentProcessorManager提取文本
     */
    private String extractTextWithProcessor(String documentId, byte[] content, String model) {
        try {
            // 从documentId获取文件扩展名
            String fileExtension = getFileExtension(documentId);

            // 创建处理上下文
            top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingContext context =
                    top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingContext.builder()
                            .fileBytes(content)
                            .fileExtension(fileExtension)
                            .originalFileName(documentId)
                            .fileSize(content.length)
                            .options(Map.of("model", model))
                            .build();

            // 调用文档处理器
            top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingResult result =
                    documentProcessorManager.processDocument(context);

            if (result.isSuccess() && result.getContent() != null) {
                log.info("✅ 文档处理成功: documentId={}, contentLength={}",
                        documentId, result.getContent().length());
                return result.getContent();
            } else {
                log.warn("⚠️ 文档处理未返回内容: documentId={}, error={}",
                        documentId, result.getError());
                return "文档处理失败: " + (result.getError() != null ? result.getError() : "未知错误");
            }
        } catch (Exception e) {
            log.error("❌ 文档处理异常: documentId={}", documentId, e);
            return "文档处理异常: " + e.getMessage();
        }
    }

    /**
     * 从文件名获取扩展名
     */
    private String getFileExtension(String filename) {
        if (filename == null || filename.isEmpty()) {
            return "";
        }
        int lastDot = filename.lastIndexOf('.');
        if (lastDot > 0 && lastDot < filename.length() - 1) {
            return filename.substring(lastDot + 1).toLowerCase();
        }
        return "";
    }

    private int simulateChunking(String text, String strategy) {
        return text.length() / 200; // 模拟分块数量
    }

    // ========== 请求对象 ==========

    @Data
    public static class ExtractRequest {
        private String model;
        private Boolean streaming = true;
    }

    @Data
    public static class ChunkRequest {
        private String strategy;
        private Map<String, Object> params;
    }

    @Data
    public static class ProcessRequest {
        private String textExtractionModel;
        private String chunkingStrategy;
        private Map<String, Object> chunkingParams;
    }

    @Data
    public static class RebuildRequest {
        private String textExtractionModel;
        private String chunkingStrategy;
        private Map<String, Object> chunkingParams;
    }
}

