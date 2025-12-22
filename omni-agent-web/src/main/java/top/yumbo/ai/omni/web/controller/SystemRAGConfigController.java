package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.web.model.RAGStrategyTemplate;
import top.yumbo.ai.omni.web.service.DocumentProcessingService;
import top.yumbo.ai.omni.web.service.SystemRAGConfigService;

import java.util.List;
import java.util.Map;

/**
 * 系统RAG配置控制器
 * (System RAG Configuration Controller)
 *
 * 管理RAG流程的系统配置
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
    private final DocumentProcessingService processingService;  // ⭐ 新增

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
            configService.setDocumentConfig(documentId, config);
            log.info("✅ 文档RAG配置更新成功: documentId={}", documentId);
            return ApiResponse.success(null, "配置更新成功");
        } catch (Exception e) {
            log.error("❌ 更新文档RAG配置失败: documentId={}", documentId, e);
            return ApiResponse.error("更新配置失败: " + e.getMessage());
        }
    }

    /**
     * 触发文档的文本提取
     * POST /api/system/rag-config/document/{documentId}/extract
     */
    @PostMapping("/document/{documentId}/extract")
    public ApiResponse<Void> triggerTextExtraction(
            @PathVariable String documentId,
            @RequestBody ExtractRequest request) {
        try {
            SystemRAGConfigService.DocumentRAGConfig config = configService.getDocumentConfig(documentId);
            config.setTextExtractionModel(request.getModel());
            config.setStatus("EXTRACTING");
            config.setUpdatedAt(System.currentTimeMillis());
            configService.setDocumentConfig(documentId, config);

            // ⭐ 触发实际的文本提取流程
            // 从data/documents/{documentId}读取文件
            byte[] content;
            try {
                java.nio.file.Path documentPath = java.nio.file.Paths.get("data/documents", documentId);
                if (!java.nio.file.Files.exists(documentPath)) {
                    log.error("❌ 文档文件不存在: {}", documentPath);
                    return ApiResponse.error("文档文件不存在: " + documentId);
                }
                content = java.nio.file.Files.readAllBytes(documentPath);
                log.info("📄 读取文档文件: {} ({} bytes)", documentPath, content.length);
            } catch (java.io.IOException e) {
                log.error("❌ 读取文档文件失败: documentId={}", documentId, e);
                return ApiResponse.error("读取文件失败: " + e.getMessage());
            }

            processingService.processDocument(documentId, documentId, content)
                .exceptionally(throwable -> {
                    log.error("❌ 文本提取失败: documentId={}", documentId, throwable);
                    config.setStatus("FAILED");
                    config.setErrorMessage(throwable.getMessage());
                    configService.setDocumentConfig(documentId, config);
                    return null;
                });

            log.info("🔍 触发文本提取: documentId={}, model={}", documentId, request.getModel());
            return ApiResponse.success(null, "文本提取已启动");
        } catch (Exception e) {
            log.error("❌ 触发文本提取失败: documentId={}", documentId, e);
            return ApiResponse.error("启动失败: " + e.getMessage());
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
            configService.applyTemplateToDocument(documentId, request.getTemplateId());
            log.info("📋 应用策略模板到文档: doc={}, template={}", documentId, request.getTemplateId());
            return ApiResponse.success(null, "策略模板应用成功");
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
            
            // 触发处理流程
            processingService.processDocument(documentId, documentId, content)
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
            
            log.info("🚀 开始处理文档: documentId={}", documentId);
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

    @Data
    public static class ApiResponse<T> {
        private Boolean success;
        private String message;
        private T data;

        public static <T> ApiResponse<T> success(T data) {
            ApiResponse<T> response = new ApiResponse<>();
            response.setSuccess(true);
            response.setData(data);
            return response;
        }

        public static <T> ApiResponse<T> success(T data, String message) {
            ApiResponse<T> response = new ApiResponse<>();
            response.setSuccess(true);
            response.setMessage(message);
            response.setData(data);
            return response;
        }

        public static <T> ApiResponse<T> error(String message) {
            ApiResponse<T> response = new ApiResponse<>();
            response.setSuccess(false);
            response.setMessage(message);
            return response;
        }
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

