package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.web.service.SystemRAGConfigService;

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

    /**
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
            configService.setDocumentConfig(documentId, config);

            // TODO: 触发实际的文本提取流程

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
            configService.setDocumentConfig(documentId, config);

            // TODO: 触发实际的分块处理流程

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

            // TODO: 触发实际的重建流程

            log.info("🔄 触发文档重建: documentId={}", documentId);
            return ApiResponse.success(null, "文档重建已启动");
        } catch (Exception e) {
            log.error("❌ 触发文档重建失败: documentId={}", documentId, e);
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
}

