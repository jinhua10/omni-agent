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
import java.util.HashMap;
import java.util.List;
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
    private final top.yumbo.ai.omni.core.document.service.DocumentExtractionResultService extractionResultService;

    /**
     * 触发文本提取（流式SSE）
     * POST /api/documents/processing/{documentId}/extract
     *
     * 支持缓存：如果之前已提取且文件未变化，直接返回缓存结果
     */
    @PostMapping(value = "/{documentId}/extract", produces = "text/event-stream;charset=UTF-8")
    public SseEmitter extractText(
            @PathVariable String documentId,
            @RequestBody ExtractRequest request) {

        SseEmitter emitter = new SseEmitter(5 * 60 * 1000L); // 5分钟超时

        CompletableFuture.runAsync(() -> {
            try {
                log.info("🔍 开始文本提取: documentId={}, model={}, forceReExtract={}",
                        documentId, request.getModel(), request.isForceReExtract());

                // ⭐ 1. 检查缓存：如果已提取且未强制重新提取
                if (!request.isForceReExtract()) {
                    var cachedResult = extractionResultService.findByDocumentId(documentId);
                    if (cachedResult.isPresent() && "COMPLETED".equals(cachedResult.get().getStatus())) {
                        var cached = cachedResult.get();
                        log.info("✅ 使用缓存的提取结果: documentId={}, cachedAt={}",
                                documentId, new java.util.Date(cached.getCompletedTime()));

                        sendProgress(emitter, 50, "使用缓存的提取结果...");
                        sendTextContent(emitter, cached.getExtractedText());
                        sendComplete(emitter, "从缓存加载完成");

                        // 同步到内存配置
                        SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);
                        config.setExtractedText(cached.getExtractedText());
                        config.setTextExtractionModel(cached.getExtractionModel());
                        config.setStatus("EXTRACTED");
                        configService.setDocumentConfig(documentId, config);

                        return;
                    }
                }

                // ⭐ 2. 创建提取记录
                long startTime = System.currentTimeMillis();
                var extractionResult = top.yumbo.ai.omni.core.document.model.DocumentExtractionResult.builder()
                        .documentId(documentId)
                        .fileName(documentId)
                        .fileExtension(getFileExtension(documentId))
                        .extractionModel(request.getModel())
                        .extractionMethod(request.getModel() != null && request.getModel().contains("vision") ? "vision-llm" : "text-only")
                        .status("EXTRACTING")
                        .startTime(startTime)
                        .build();

                extractionResultService.save(extractionResult);

                // 更新内存配置
                SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);
                config.setTextExtractionModel(request.getModel());
                config.setStatus("EXTRACTING");
                config.setUpdatedAt(System.currentTimeMillis());
                configService.setDocumentConfig(documentId, config);

                // 发送进度：开始
                sendProgress(emitter, 10, "正在读取文档...");

                // ⭐ 3. 读取中转站文件并计算MD5
                byte[] content = readDocumentFile(documentId);
                if (content == null) {
                    extractionResult.setStatus("FAILED");
                    extractionResult.setErrorMessage("文档文件不存在");
                    extractionResult.setCompletedTime(System.currentTimeMillis());
                    extractionResultService.save(extractionResult);

                    sendError(emitter, "文档文件不存在");
                    return;
                }

                // 计算MD5
                String md5 = calculateMd5(content);
                extractionResult.setFileSize((long) content.length);
                extractionResult.setFileMd5(md5);

                sendProgress(emitter, 30, "正在解析文档格式...");

                // ⭐ 4. 调用实际的文本提取服务（支持真正 streaming）
                String extractedText;
                if (Boolean.TRUE.equals(request.getStreaming())) {
                    sendProgress(emitter, 35, "正在实时提取文本...");
                    extractedText = extractTextWithProcessorStreaming(
                            documentId,
                            content,
                            request.getModel(),
                            chunk -> {
                                try {
                                    // 直接把增量内容发给前端（不做 500 字符二次切分，避免延迟）
                                    String safe = (chunk == null ? "" : chunk)
                                            .replace("\\", "\\\\")
                                            .replace("\"", "\\\"")
                                            .replace("\n", "\\n");
                                    emitter.send(SseEmitter.event()
                                            .name("message")
                                            .data("{\"type\":\"content\",\"content\":\"" + safe + "\"}"));
                                } catch (Exception sendEx) {
                                    log.error("发送流式内容失败", sendEx);
                                }
                            }
                    );
                } else {
                    extractedText = extractTextWithProcessor(documentId, content, request.getModel());
                }

                sendProgress(emitter, 80, "文本提取完成");

                // ⭐ 5. 保存提取结果到持久化存储
                long completedTime = System.currentTimeMillis();
                extractionResult.setExtractedText(extractedText);
                extractionResult.setStatus("COMPLETED");
                extractionResult.setCompletedTime(completedTime);
                extractionResult.setDuration(completedTime - startTime);
                extractionResultService.save(extractionResult);

                // 同步到内存配置（保持向后兼容）
                config.setExtractedText(extractedText);
                config.setStatus("EXTRACTED");
                config.setUpdatedAt(System.currentTimeMillis());
                configService.setDocumentConfig(documentId, config);

                // 非 streaming 模式才在这里统一发送
                if (!Boolean.TRUE.equals(request.getStreaming())) {
                    sendTextContent(emitter, extractedText);
                }

                sendComplete(emitter, "提取完成并已保存");
                log.info("✅ 文本提取完成并持久化: documentId={}, textLength={}, duration={}ms",
                        documentId, extractedText.length(), extractionResult.getDuration());

            } catch (Exception e) {
                log.error("❌ 文本提取失败: documentId={}", documentId, e);

                // 更新失败状态
                try {
                    var failedResult = extractionResultService.findByDocumentId(documentId);
                    failedResult.ifPresent(result -> {
                        result.setStatus("FAILED");
                        result.setErrorMessage(e.getMessage());
                        result.setCompletedTime(System.currentTimeMillis());
                        extractionResultService.save(result);
                    });
                } catch (Exception saveEx) {
                    log.error("保存失败状态失败", saveEx);
                }

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
     * 获取文档提取结果
     * GET /api/documents/processing/{documentId}/extraction-result
     *
     * @return 提取结果信息（不包含完整文本，需要调用extract接口获取）
     */
    @GetMapping("/{documentId}/extraction-result")
    public ApiResponse<Map<String, Object>> getExtractionResult(@PathVariable String documentId) {
        try {
            var result = extractionResultService.findByDocumentId(documentId);

            if (result.isEmpty()) {
                return ApiResponse.success(Map.of(
                        "exists", false,
                        "message", "未找到提取记录"
                ));
            }

            var extraction = result.get();
            Map<String, Object> info = new HashMap<>();
            info.put("exists", true);
            info.put("documentId", extraction.getDocumentId());
            info.put("fileName", extraction.getFileName());
            info.put("fileExtension", extraction.getFileExtension());
            info.put("fileSize", extraction.getFileSize());
            info.put("extractionModel", extraction.getExtractionModel());
            info.put("extractionMethod", extraction.getExtractionMethod());
            info.put("status", extraction.getStatus());
            info.put("completedTime", extraction.getCompletedTime());
            info.put("duration", extraction.getDuration());
            info.put("textLength", extraction.getExtractedText() != null ? extraction.getExtractedText().length() : 0);
            info.put("textPreview", extraction.getSummary());

            return ApiResponse.success(info);

        } catch (Exception e) {
            log.error("获取提取结果失败: documentId={}", documentId, e);
            return ApiResponse.error("获取提取结果失败: " + e.getMessage());
        }
    }

    /**
     * 获取所有提取结果列表
     * GET /api/documents/processing/extraction-results
     */
    @GetMapping("/extraction-results")
    public ApiResponse<List<Map<String, Object>>> listExtractionResults() {
        try {
            var results = extractionResultService.findAll();

            List<Map<String, Object>> list = results.stream().map(extraction -> {
                Map<String, Object> info = new HashMap<>();
                info.put("documentId", extraction.getDocumentId());
                info.put("fileName", extraction.getFileName());
                info.put("status", extraction.getStatus());
                info.put("completedTime", extraction.getCompletedTime());
                info.put("textLength", extraction.getExtractedText() != null ? extraction.getExtractedText().length() : 0);
                return info;
            }).collect(java.util.stream.Collectors.toList());

            return ApiResponse.success(list);

        } catch (Exception e) {
            log.error("获取提取结果列表失败", e);
            return ApiResponse.error("获取提取结果列表失败: " + e.getMessage());
        }
    }

    /**
     * 删除提取结果
     * DELETE /api/documents/processing/{documentId}/extraction-result
     */
    @DeleteMapping("/{documentId}/extraction-result")
    public ApiResponse<Void> deleteExtractionResult(@PathVariable String documentId) {
        try {
            extractionResultService.delete(documentId);
            return ApiResponse.success(null, "提取结果已删除");
        } catch (Exception e) {
            log.error("删除提取结果失败: documentId={}", documentId, e);
            return ApiResponse.error("删除提取结果失败: " + e.getMessage());
        }
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
     * 使用DocumentProcessorManager提取文本（支持 streaming SSE）
     */
    private String extractTextWithProcessorStreaming(String documentId,
                                                    byte[] content,
                                                    String model,
                                                    java.util.function.Consumer<String> streamCallback) {
        try {
            String fileExtension = getFileExtension(documentId);

            Map<String, Object> options = new HashMap<>();
            options.put("model", model);
            options.put("streaming", true);
            if (streamCallback != null) {
                // VisionLLMDocumentProcessor 会读取该回调并增量输出
                options.put("streamCallback", streamCallback);
            }

            top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingContext context =
                    top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingContext.builder()
                            .fileBytes(content)
                            .fileExtension(fileExtension)
                            .originalFileName(documentId)
                            .fileSize(content.length)
                            .options(options)
                            .build();

            top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingResult result =
                    documentProcessorManager.processDocument(context);

            return result.isSuccess() && result.getContent() != null ? result.getContent() : "";
        } catch (Exception e) {
            log.error("❌ 文档处理失败(Streaming): documentId={}", documentId, e);
            return "";
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

    /**
     * 计算文件MD5
     */
    private String calculateMd5(byte[] content) {
        try {
            java.security.MessageDigest md = java.security.MessageDigest.getInstance("MD5");
            byte[] digest = md.digest(content);
            StringBuilder sb = new StringBuilder();
            for (byte b : digest) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (Exception e) {
            log.warn("计算MD5失败", e);
            return null;
        }
    }

    private int simulateChunking(String text, String strategy) {
        return text.length() / 200; // 模拟分块数量
    }

    // ========== 请求对象 ==========

    @Data
    public static class ExtractRequest {
        private String model;
        private Boolean streaming = true;
        /**
         * 是否强制重新提取（忽略缓存）
         */
        private boolean forceReExtract = false;
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

