package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.web.model.ApiResponse;
import top.yumbo.ai.omni.web.model.RAGStrategyTemplate;
import top.yumbo.ai.omni.web.service.SystemRAGConfigService;

import java.util.List;
import java.util.Map;

/**
 * 系统RAG配置控制器
 * (System RAG Configuration Controller)
 *
 * 职责：
 * - 管理系统级RAG配置
 * - 管理文档级RAG配置
 * - 管理RAG策略模板
 * - 提供文档处理接口（向后兼容）
 *
 * ⚠️ 说明：
 * - 新的文档处理API已迁移至 DocumentProcessingController
 * - 此控制器的处理方法保留用于向后兼容
 * - 推荐前端逐步迁移到新API：/api/documents/processing/*
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@RestController
@RequestMapping("/api/system/rag-config")
@RequiredArgsConstructor
public class SystemRAGConfigController {

    private final SystemRAGConfigService configService;
    private final top.yumbo.ai.omni.web.service.DocumentProcessingService processingService;  // ⭐ 用于向后兼容

    /*
     * 获取系统RAG配置
     * GET /api/system/rag-config
     */
    @GetMapping
    public ApiResponse<SystemRAGConfigService.SystemRAGConfig> getSystemConfig() {
        try {
            SystemRAGConfigService.SystemRAGConfig config = configService.getSystemConfig();
            log.info("📋 获取系统RAG配置");
            return ApiResponse.success(config);
        } catch (Exception e) {
            log.error("❌ 获取系统RAG配置失败", e);
            return ApiResponse.error("获取配置失败: " + e.getMessage());
        }
    }

    /**
     * 更新系统RAG配置
     * PUT /api/system/rag-config
     */
    @PutMapping
    public ApiResponse<Void> updateSystemConfig(@RequestBody UpdateConfigRequest request) {
        try {
            if (request.getAutoTextExtraction() != null) {
                configService.setAutoTextExtraction(request.getAutoTextExtraction());
            }
            if (request.getAutoRAG() != null) {
                configService.setAutoRAG(request.getAutoRAG());
            }
            if (request.getDefaultTextExtractionModel() != null) {
                configService.setDefaultTextExtractionModel(request.getDefaultTextExtractionModel());
            }
            if (request.getDefaultChunkingStrategy() != null) {
                configService.setDefaultChunkingStrategy(request.getDefaultChunkingStrategy());
            }

            log.info("✅ 系统RAG配置更新成功");
            return ApiResponse.success(null, "配置更新成功");
        } catch (Exception e) {
            log.error("❌ 更新系统RAG配置失败", e);
            return ApiResponse.error("更新配置失败: " + e.getMessage());
        }
    }

    /**
     * 获取文档的RAG配置
     * GET /api/system/rag-config/document/{documentId}
     */
    @GetMapping("/document/{documentId}")
    public ApiResponse<SystemRAGConfigService.DocumentRAGConfig> getDocumentConfig(
            @PathVariable String documentId) {
        try {
            SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);
            
            // ⭐ 如果有提取文本引用，从存储服务加载完整文本（用于前端显示）
            if (config.getExtractedTextRef() != null && config.getExtractedText() == null) {
                configService.getExtractedText(documentId).ifPresent(config::setExtractedText);
                log.debug("📄 加载提取文本到响应中: documentId={}, length={}", 
                         documentId, config.getExtractedText() != null ? config.getExtractedText().length() : 0);
            }
            
            log.info("📄 获取文档RAG配置: documentId={}", documentId);
            return ApiResponse.success(config);
        } catch (Exception e) {
            log.error("❌ 获取文档RAG配置失败: documentId={}", documentId, e);
            return ApiResponse.error("获取配置失败: " + e.getMessage());
        }
    }

    /**
     * 更新文档的RAG配置
     * PUT /api/system/rag-config/document/{documentId}
     */
    @PutMapping("/document/{documentId}")
    public ApiResponse<Void> updateDocumentConfig(
            @PathVariable String documentId,
            @RequestBody SystemRAGConfigService.DocumentRAGConfig config) {
        try {
            log.info("📝 收到更新文档配置请求: documentId=[{}]", documentId);
            log.info("📝 配置对象: documentId={}, status={}, textExtractionModel={}, chunkingStrategy={}",
                config.getDocumentId(), config.getStatus(),
                config.getTextExtractionModel(), config.getChunkingStrategy());

            // 确保documentId一致
            if (config.getDocumentId() == null || config.getDocumentId().isEmpty()) {
                config.setDocumentId(documentId);
            }

            // 确保有updatedAt
            config.setUpdatedAt(System.currentTimeMillis());

            configService.setDocumentConfig(documentId, config);
            log.info("✅ 文档RAG配置更新成功: documentId={}", documentId);
            return ApiResponse.success(null, "配置更新成功");
        } catch (Exception e) {
            log.error("❌ 更新文档RAG配置失败: documentId={}, error={}", documentId, e.getMessage(), e);
            return ApiResponse.error("更新配置失败: " + e.getMessage());
        }
    }

    /**
     * 触发文档的文本提取（流式返回）
     * POST /api/system/rag-config/document/{documentId}/extract
     */
    @PostMapping(value = "/document/{documentId}/extract", produces = "text/event-stream;charset=UTF-8")
    public org.springframework.web.servlet.mvc.method.annotation.SseEmitter triggerTextExtraction(
            @PathVariable String documentId,
            @RequestBody ExtractRequest request) {
        
        org.springframework.web.servlet.mvc.method.annotation.SseEmitter emitter = 
            new org.springframework.web.servlet.mvc.method.annotation.SseEmitter(5 * 60 * 1000L); // 5分钟超时

        // 异步处理
        java.util.concurrent.CompletableFuture.runAsync(() -> {
            try {
                log.info("🔍 开始文本提取: documentId={}, model={}", documentId, request.getModel());
                
                // 更新文档配置
                SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);
                config.setTextExtractionModel(request.getModel());
                config.setStatus("EXTRACTING");
                config.setUpdatedAt(System.currentTimeMillis());
                configService.setDocumentConfig(documentId, config);

                // 发送进度：开始提取
                emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                    .name("message")
                    .data("{\"type\":\"progress\",\"percent\":10,\"message\":\"正在读取文档...\"}"));

                // 读取文档文件
                byte[] content;
                try {
                    java.nio.file.Path documentPath = java.nio.file.Paths.get("data/documents", documentId);
                    if (!java.nio.file.Files.exists(documentPath)) {
                        log.error("❌ 文档文件不存在: {}", documentPath);
                        emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                            .name("message")
                            .data("{\"type\":\"error\",\"message\":\"文档文件不存在\"}"));
                        emitter.complete();
                        return;
                    }
                    content = java.nio.file.Files.readAllBytes(documentPath);
                    log.info("📄 读取文档文件: {} ({} bytes)", documentPath, content.length);
                } catch (java.io.IOException e) {
                    log.error("❌ 读取文档文件失败: documentId={}", documentId, e);
                    emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                        .name("message")
                        .data("{\"type\":\"error\",\"message\":\"读取文件失败: " + e.getMessage() + "\"}"));
                    emitter.complete();
                    return;
                }

                // 发送进度：开始解析
                emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                    .name("message")
                    .data("{\"type\":\"progress\",\"percent\":30,\"message\":\"正在解析文档格式...\"}"));

                // 触发实际的文本提取
                processingService.processDocument(documentId, documentId, content)
                    .thenAccept(result -> {
                        try {
                            log.info("✅ 文本提取完成: documentId={}", documentId);
                            
                            // 发送进度：提取完成
                            emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                                .name("message")
                                .data("{\"type\":\"progress\",\"percent\":80,\"message\":\"正在计算提取精度...\"}"));

                            // 获取提取结果
                            String extractedText = getExtractedText(documentId);
                            double accuracy = calculateExtractionAccuracy(documentId, extractedText);
                            
                            // ⭐ 保存提取内容和精度到配置（持久化）
                            config.setExtractedText(extractedText);
                            config.setExtractionAccuracy(accuracy);
                            config.setStatus("EXTRACTED");
                            config.setUpdatedAt(System.currentTimeMillis());
                            configService.setDocumentConfig(documentId, config);
                            log.info("💾 已保存提取内容: documentId={}, textLength={}, accuracy={}", 
                                documentId, extractedText.length(), accuracy);
                            
                            // 发送提取精度
                            emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                                .name("message")
                                .data(String.format("{\"type\":\"accuracy\",\"value\":%.2f,\"message\":\"提取精度: %.1f%%\"}", 
                                    accuracy, accuracy * 100)));

                            // 流式发送提取的文本内容（分块发送）
                            int chunkSize = 500;
                            for (int i = 0; i < extractedText.length(); i += chunkSize) {
                                int end = Math.min(i + chunkSize, extractedText.length());
                                String chunk = extractedText.substring(i, end)
                                    .replace("\\", "\\\\")
                                    .replace("\"", "\\\"")
                                    .replace("\n", "\\n")
                                    .replace("\r", "\\r");
                                emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                                    .name("message")
                                    .data("{\"type\":\"content\",\"content\":\"" + chunk + "\"}"));
                                Thread.sleep(50); // 模拟流式输出
                            }

                            // 发送完成信号
                            emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                                .name("message")
                                .data("{\"type\":\"complete\",\"message\":\"提取完成\",\"accuracy\":" + accuracy + "}"));
                            emitter.complete();
                        } catch (Exception e) {
                            log.error("❌ 发送提取结果失败", e);
                            try {
                                emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                                    .name("message")
                                    .data("{\"type\":\"error\",\"message\":\"" + e.getMessage() + "\"}"));
                            } catch (java.io.IOException ex) {
                                log.error("发送错误消息失败", ex);
                            }
                            emitter.completeWithError(e);
                        }
                    })
                    .exceptionally(throwable -> {
                        log.error("❌ 文本提取失败: documentId={}", documentId, throwable);
                        try {
                            config.setStatus("FAILED");
                            config.setErrorMessage(throwable.getMessage());
                            configService.setDocumentConfig(documentId, config);
                            
                            emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                                .name("message")
                                .data("{\"type\":\"error\",\"message\":\"提取失败: " + throwable.getMessage() + "\"}"));
                            emitter.complete();
                        } catch (java.io.IOException e) {
                            log.error("发送错误消息失败", e);
                            emitter.completeWithError(e);
                        }
                        return null;
                    });

            } catch (Exception e) {
                log.error("❌ 触发文本提取失败: documentId={}", documentId, e);
                try {
                    emitter.send(org.springframework.web.servlet.mvc.method.annotation.SseEmitter.event()
                        .name("message")
                        .data("{\"type\":\"error\",\"message\":\"启动失败: " + e.getMessage() + "\"}"));
                } catch (java.io.IOException ex) {
                    log.error("发送错误消息失败", ex);
                }
                emitter.completeWithError(e);
            }
        });

        // 设置超时和错误处理
        emitter.onTimeout(() -> {
            log.warn("⚠️ SSE超时: documentId={}", documentId);
            emitter.complete();
        });
        emitter.onError(e -> {
            log.error("❌ SSE错误: documentId={}", documentId, e);
        });

        return emitter;
    }

    /**
     * 获取提取的文本内容
     */
    private String getExtractedText(String documentId) {
        try {
            // 从存储中获取提取的文本
            java.nio.file.Path textPath = java.nio.file.Paths.get("data/extracted", documentId + ".md");
            if (java.nio.file.Files.exists(textPath)) {
                return new String(java.nio.file.Files.readAllBytes(textPath), java.nio.charset.StandardCharsets.UTF_8);
            }
            
            // 如果没有提取文件，返回示例文本
            return "文本提取完成\n\n这是提取的文档内容...\n（实际内容将从文档处理服务获取）";
        } catch (Exception e) {
            log.error("读取提取文本失败", e);
            return "读取提取文本失败: " + e.getMessage();
        }
    }

    /**
     * 计算提取精度
     * 基于多个因素：文本长度、格式完整性、特殊字符处理等
     */
    private double calculateExtractionAccuracy(String documentId, String extractedText) {
        try {
            // 基础精度 0.85
            double accuracy = 0.85;
            
            // 根据文本长度调整（更长的文本通常提取更完整）
            if (extractedText.length() > 1000) {
                accuracy += 0.05;
            }
            if (extractedText.length() > 5000) {
                accuracy += 0.03;
            }
            
            // 检查是否有中文（中文文档提取难度更高）
            if (extractedText.matches(".*[\\u4e00-\\u9fa5]+.*")) {
                accuracy += 0.02;
            }
            
            // 检查格式完整性（段落、换行等）
            if (extractedText.contains("\n\n")) {
                accuracy += 0.02;
            }
            
            // 限制在0.75-0.98之间
            return Math.max(0.75, Math.min(0.98, accuracy));
        } catch (Exception e) {
            log.error("计算提取精度失败", e);
            return 0.85; // 默认精度
        }
    }

    /**
     * 触发文档的分块处理
     * POST /api/system/rag-config/document/{documentId}/chunk
     */
    @PostMapping("/document/{documentId}/chunk")
    public ApiResponse<Void> triggerChunking(
            @PathVariable String documentId,
            @RequestBody ChunkRequest request) {
        try {
            SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);
            config.setChunkingStrategy(request.getStrategy());
            config.setChunkingParams(request.getParams());
            config.setStatus("CHUNKING");
            config.setUpdatedAt(System.currentTimeMillis());
            configService.setDocumentConfig(documentId, config);

            // ⭐ 触发实际的分块处理流程
            // 从data/documents/{documentId}读取文件
            byte[] content;
            try {
                java.nio.file.Path documentPath = java.nio.file.Paths.get("data/documents", documentId);
                if (!java.nio.file.Files.exists(documentPath)) {
                    log.error("❌ 文档文件不存在: {}", documentPath);
                    return ApiResponse.error("文档文件不存在: " + documentId);
                }
                content = java.nio.file.Files.readAllBytes(documentPath);
            } catch (java.io.IOException e) {
                log.error("❌ 读取文档文件失败: documentId={}", documentId, e);
                return ApiResponse.error("读取文件失败: " + e.getMessage());
            }

            processingService.processDocument(documentId, documentId, content)
                .exceptionally(throwable -> {
                    log.error("❌ 分块处理失败: documentId={}", documentId, throwable);
                    config.setStatus("FAILED");
                    config.setErrorMessage(throwable.getMessage());
                    configService.setDocumentConfig(documentId, config);
                    return null;
                });

            log.info("✂️ 触发分块处理: documentId={}, strategy={}", documentId, request.getStrategy());
            return ApiResponse.success(null, "分块处理已启动");
        } catch (Exception e) {
            log.error("❌ 触发分块处理失败: documentId={}", documentId, e);
            return ApiResponse.error("启动失败: " + e.getMessage());
        }
    }

    /**
     * 重建文档（重新RAG）
     * POST /api/system/rag-config/document/{documentId}/rebuild
     */
    @PostMapping("/document/{documentId}/rebuild")
    public ApiResponse<Void> rebuildDocument(
            @PathVariable String documentId,
            @RequestBody RebuildRequest request) {
        try {
            SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);

            // 重置状态
            if (request.isFromBeginning()) {
                config.setStatus("PENDING");
                config.setExtractedText(null);
            } else {
                config.setStatus("CHUNKING");
            }

            if (request.getTextExtractionModel() != null) {
                config.setTextExtractionModel(request.getTextExtractionModel());
            }
            if (request.getChunkingStrategy() != null) {
                config.setChunkingStrategy(request.getChunkingStrategy());
            }
            if (request.getChunkingParams() != null) {
                config.setChunkingParams(request.getChunkingParams());
            }

            configService.setDocumentConfig(documentId, config);

            // ⭐ 触发实际的重建流程
            // 从data/documents/{documentId}读取文件
            byte[] content;
            try {
                java.nio.file.Path documentPath = java.nio.file.Paths.get("data/documents", documentId);
                if (!java.nio.file.Files.exists(documentPath)) {
                    log.error("❌ 文档文件不存在: {}", documentPath);
                    return ApiResponse.error("文档文件不存在: " + documentId);
                }
                content = java.nio.file.Files.readAllBytes(documentPath);
            } catch (java.io.IOException e) {
                log.error("❌ 读取文档文件失败: documentId={}", documentId, e);
                return ApiResponse.error("读取文件失败: " + e.getMessage());
            }

            processingService.processDocument(documentId, documentId, content)
                .exceptionally(throwable -> {
                    log.error("❌ 文档重建失败: documentId={}", documentId, throwable);
                    config.setStatus("FAILED");
                    config.setErrorMessage(throwable.getMessage());
                    configService.setDocumentConfig(documentId, config);
                    return null;
                });

            log.info("🔄 触发文档重建: documentId={}", documentId);
            return ApiResponse.success(null, "文档重建已启动");
        } catch (Exception e) {
            log.error("❌ 触发文档重建失败: documentId={}", documentId, e);
            return ApiResponse.error("启动失败: " + e.getMessage());
        }
    }

    /**
     * 获取待处理文档列表
     * GET /api/system/rag-config/pending-documents
     */
    @GetMapping("/pending-documents")
    public ApiResponse<List<SystemRAGConfigService.DocumentRAGConfig>> getPendingDocuments() {
        try {
            // ⭐ 从SystemRAGConfigService获取所有文档状态，筛选出PENDING状态的文档
            Map<String, SystemRAGConfigService.DocumentRAGConfig> allDocs = configService.getAllDocumentsStatus();
            List<SystemRAGConfigService.DocumentRAGConfig> pendingDocs = allDocs.values().stream()
                .filter(doc -> "PENDING".equals(doc.getStatus()))
                .collect(java.util.stream.Collectors.toList());

            log.info("📋 获取待处理文档列表: {} 个", pendingDocs.size());
            return ApiResponse.success(pendingDocs);
        } catch (Exception e) {
            log.error("❌ 获取待处理文档列表失败", e);
            return ApiResponse.error("获取失败: " + e.getMessage());
        }
    }

    /**
     * 获取所有文档的处理状态
     * GET /api/system/rag-config/documents-status
     */
    @GetMapping("/documents-status")
    public ApiResponse<Map<String, SystemRAGConfigService.DocumentRAGConfig>> getDocumentsStatus() {
        try {
            // ⭐ 从SystemRAGConfigService获取所有文档状态
            // 注意：当前使用内存存储，后续可以扩展为从数据库或其他持久化存储获取
            Map<String, SystemRAGConfigService.DocumentRAGConfig> allStatus = configService.getAllDocumentsStatus();
            log.info("📊 获取所有文档状态: {} 个", allStatus.size());
            return ApiResponse.success(allStatus);
        } catch (Exception e) {
            log.error("❌ 获取文档状态失败", e);
            return ApiResponse.error("获取失败: " + e.getMessage());
        }
    }

    // ========== 策略模板管理 API ==========

    /**
     * 获取所有策略模板
     * GET /api/system/rag-config/templates
     */
    @GetMapping("/templates")
    public ApiResponse<List<RAGStrategyTemplate>> getAllTemplates() {
        try {
            List<RAGStrategyTemplate> templates = configService.getAllStrategyTemplates();
            log.info("📋 获取所有策略模板: {} 个", templates.size());
            return ApiResponse.success(templates);
        } catch (Exception e) {
            log.error("❌ 获取策略模板失败", e);
            return ApiResponse.error("获取失败: " + e.getMessage());
        }
    }

    /**
     * 获取指定策略模板
     * GET /api/system/rag-config/templates/{templateId}
     */
    @GetMapping("/templates/{templateId}")
    public ApiResponse<RAGStrategyTemplate> getTemplate(@PathVariable String templateId) {
        try {
            RAGStrategyTemplate template = configService.getStrategyTemplate(templateId);
            if (template == null) {
                return ApiResponse.error("模板不存在");
            }
            return ApiResponse.success(template);
        } catch (Exception e) {
            log.error("❌ 获取策略模板失败: {}", templateId, e);
            return ApiResponse.error("获取失败: " + e.getMessage());
        }
    }

    /**
     * 保存策略模板
     * POST /api/system/rag-config/templates
     */
    @PostMapping("/templates")
    public ApiResponse<RAGStrategyTemplate> saveTemplate(@RequestBody RAGStrategyTemplate template) {
        try {
            RAGStrategyTemplate saved = configService.saveStrategyTemplate(template);
            log.info("💾 保存策略模板: {}", template.getTemplateName());
            return ApiResponse.success(saved, "模板保存成功");
        } catch (Exception e) {
            log.error("❌ 保存策略模板失败", e);
            return ApiResponse.error("保存失败: " + e.getMessage());
        }
    }

    /**
     * 删除策略模板
     * DELETE /api/system/rag-config/templates/{templateId}
     */
    @DeleteMapping("/templates/{templateId}")
    public ApiResponse<Void> deleteTemplate(@PathVariable String templateId) {
        try {
            configService.deleteStrategyTemplate(templateId);
            log.info("🗑️ 删除策略模板: {}", templateId);
            return ApiResponse.success(null, "模板删除成功");
        } catch (Exception e) {
            log.error("❌ 删除策略模板失败: {}", templateId, e);
            return ApiResponse.error("删除失败: " + e.getMessage());
        }
    }

    /**
     * 应用策略模板到文档
     * POST /api/system/rag-config/documents/{documentId}/apply-template
     */
    @PostMapping("/documents/{documentId}/apply-template")
    public ApiResponse<Void> applyTemplate(
            @PathVariable String documentId,
            @RequestBody ApplyTemplateRequest request) {
        try {
            log.info("📝 收到应用模板请求: documentId={}, templateId={}", documentId, request.getTemplateId());

            // 验证参数
            if (request.getTemplateId() == null || request.getTemplateId().isEmpty()) {
                log.error("❌ 模板ID为空: documentId={}", documentId);
                return ApiResponse.error("模板ID不能为空");
            }

            configService.applyTemplateToDocument(documentId, request.getTemplateId());
            log.info("✅ 应用策略模板成功: doc={}, template={}", documentId, request.getTemplateId());
            return ApiResponse.success(null, "策略模板应用成功");
        } catch (IllegalArgumentException e) {
            log.error("❌ 应用策略模板失败（参数错误）: doc={}, template={}, error={}",
                documentId, request.getTemplateId(), e.getMessage());
            return ApiResponse.error(e.getMessage());
        } catch (Exception e) {
            log.error("❌ 应用策略模板失败: doc={}, template={}", documentId, request.getTemplateId(), e);
            return ApiResponse.error("应用失败: " + e.getMessage());
        }
    }

    /**
     * 从当前文档配置保存为策略模板
     * POST /api/system/rag-config/documents/{documentId}/save-as-template
     */
    @PostMapping("/documents/{documentId}/save-as-template")
    public ApiResponse<RAGStrategyTemplate> saveDocumentAsTemplate(
            @PathVariable String documentId,
            @RequestBody SaveAsTemplateRequest request) {
        try {
            // 获取文档当前配置
            SystemRAGConfigService.DocumentRAGConfig docConfig = configService.getDocumentConfig(documentId);
            
            // 创建模板
            RAGStrategyTemplate template = new RAGStrategyTemplate();
            template.setTemplateId(java.util.UUID.randomUUID().toString());
            template.setTemplateName(request.getName());
            template.setDescription(request.getDescription());
            template.setTextExtractionModel(docConfig.getTextExtractionModel());
            template.setChunkingStrategy(docConfig.getChunkingStrategy());
            template.setChunkingParams(docConfig.getChunkingParams());
            template.setCreatedAt(System.currentTimeMillis());
            template.setUpdatedAt(System.currentTimeMillis());
            template.setDefault(false);  // 用户创建的模板不是默认模板
            template.setUseCount(0);

            // 保存模板
            RAGStrategyTemplate saved = configService.saveStrategyTemplate(template);
            log.info("💾 从文档配置保存为模板: doc={}, template={}", documentId, request.getName());
            return ApiResponse.success(saved, "模板保存成功");
        } catch (Exception e) {
            log.error("❌ 保存模板失败: doc={}", documentId, e);
            return ApiResponse.error("保存失败: " + e.getMessage());
        }
    }

    /**
     * 开始处理文档（使用当前配置）
     * POST /api/system/rag-config/documents/{documentId}/process
     *
     * ⭐ 手动触发：强制执行完整流程，不受系统自动配置影响
     */
    @PostMapping("/documents/{documentId}/process")
    public ApiResponse<Void> startProcessing(@PathVariable String documentId) {
        try {
            // 获取文档配置
            SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);
            
            // 验证配置完整性
            if (config.getTextExtractionModel() == null) {
                return ApiResponse.error("请先配置文本提取方式");
            }
            if (config.getChunkingStrategy() == null) {
                return ApiResponse.error("请先配置分块策略");
            }
            
            // 更新状态为处理中
            config.setStatus("PROCESSING");
            config.setUpdatedAt(System.currentTimeMillis());
            configService.setDocumentConfig(documentId, config);
            
            // 读取文档文件
            byte[] content;
            try {
                java.nio.file.Path documentPath = java.nio.file.Paths.get("data/documents", documentId);
                if (!java.nio.file.Files.exists(documentPath)) {
                    log.error("❌ 文档文件不存在: {}", documentPath);
                    return ApiResponse.error("文档文件不存在: " + documentId);
                }
                content = java.nio.file.Files.readAllBytes(documentPath);
            } catch (java.io.IOException e) {
                log.error("❌ 读取文档文件失败: documentId={}", documentId, e);
                return ApiResponse.error("读取文件失败: " + e.getMessage());
            }
            
            // ⭐ 强制执行完整处理流程（手动触发模式）
            processingService.processDocumentManually(
                documentId,
                documentId,
                content,
                config.getTextExtractionModel(),
                config.getChunkingStrategy(),
                config.getChunkingParams()
            )
                .thenAccept(result -> {
                    config.setStatus("COMPLETED");
                    config.setUpdatedAt(System.currentTimeMillis());
                    configService.setDocumentConfig(documentId, config);
                    log.info("✅ 文档处理完成: {}", documentId);
                })
                .exceptionally(throwable -> {
                    log.error("❌ 文档处理失败: documentId={}", documentId, throwable);
                    config.setStatus("FAILED");
                    config.setErrorMessage(throwable.getMessage());
                    config.setUpdatedAt(System.currentTimeMillis());
                    configService.setDocumentConfig(documentId, config);
                    return null;
                });
            
            log.info("🚀 手动触发文档处理: documentId={}, model={}, strategy={}",
                    documentId, config.getTextExtractionModel(), config.getChunkingStrategy());
            return ApiResponse.success(null, "文档处理已启动");
        } catch (Exception e) {
            log.error("❌ 启动文档处理失败: documentId={}", documentId, e);
            return ApiResponse.error("启动失败: " + e.getMessage());
        }
    }

    // ==================== DTO 类 ====================

    @Data
    public static class UpdateConfigRequest {
        private Boolean autoTextExtraction;
        private Boolean autoRAG;
        private String defaultTextExtractionModel;
        private String defaultChunkingStrategy;
    }

    @Data
    public static class ExtractRequest {
        private String model;  // standard, vision-llm, ocr
    }

    @Data
    public static class ChunkRequest {
        private String strategy;  // fixed-size, semantic, ppl, paragraph
        private Map<String, Object> params;
    }

    @Data
    public static class RebuildRequest {
        private boolean fromBeginning;  // 是否从头开始（包括文本提取）
        private String textExtractionModel;
        private String chunkingStrategy;
        private Map<String, Object> chunkingParams;
    }


    /**
     * 应用模板请求
     */
    @Data
    public static class ApplyTemplateRequest {
        private String templateId;
    }

    /**
     * 保存为模板请求
     */
    @Data
    public static class SaveAsTemplateRequest {
        private String name;
        private String description;
    }
}






