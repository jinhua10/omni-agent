package top.yumbo.ai.omni.core.ppl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.core.optimization.RAGOptimizationService;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.OptimizationData;
import top.yumbo.ai.storage.api.model.OptimizationType;
import top.yumbo.ai.storage.api.model.PPLData;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * PPL 存储服务 - 负责 PPL (Prompt Programming Language) 数据的存储和管理
 * (PPL Storage Service - Responsible for PPL data storage and management)
 *
 * <p>
 * 重构说明 (Refactoring Notes v4.0):
 * - ✅ v1.0: 硬编码文件存储
 * - ✅ v2.0: 使用 DocumentStorageService 接口
 * - ✅ v3.0: 重构为可插拔架构
 * - ✅ v4.0: 委托给 RAGOptimizationService 通用框架，保持向后兼容
 * </p>
 *
 * <p>
 * 向后兼容策略 (Backward Compatibility Strategy):
 * - 保留原有的 PPL 相关方法
 * - 内部委托给新的 RAGOptimizationService
 * - 新项目推荐直接使用 RAGOptimizationService
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 4.0.0 - 委托给通用优化框架
 * @see RAGOptimizationService 推荐使用的通用框架
 * @deprecated 推荐使用 {@link RAGOptimizationService}，本类保留用于向后兼容
 */
@Slf4j
@Service
public class PPLStorageService {

    private final DocumentStorageService storageService;
    private final RAGOptimizationService optimizationService;

    @Autowired
    public PPLStorageService(DocumentStorageService storageService,
                            RAGOptimizationService optimizationService) {
        this.storageService = storageService;
        this.optimizationService = optimizationService;
        log.info("PPLStorageService initialized (delegating to RAGOptimizationService)");
    }

    /**
     * 保存 PPL 数据
     *
     * @deprecated 推荐使用 {@link RAGOptimizationService#savePPLData}
     */
    @Deprecated
    public String savePPLData(String documentId, String content, String metadata) {
        if (content == null || content.trim().isEmpty()) {
            log.warn("Empty PPL content for document: {}", documentId);
            return null;
        }

        try {
            // 委托给新的通用优化服务
            Map<String, Object> data = Map.of(
                "content", content,
                "metadata", metadata != null ? metadata : ""
            );

            return optimizationService.saveOptimizationData(
                documentId,
                OptimizationType.PPL.getCode(),
                data
            );
        } catch (Exception e) {
            log.error("Failed to save PPL data for document: {}", documentId, e);
            return null;
        }
    }

    /**
     * 保存 PPL 数据（新版本 - 使用 PPLData 对象）
     *
     * @deprecated 推荐使用 {@link RAGOptimizationService#savePPLData}
     */
    @Deprecated
    public String savePPLData(String documentId,
                             List<String> probablePoints,
                             Map<String, Float> scores,
                             String modelVersion) {
        return optimizationService.savePPLData(documentId, probablePoints, scores, modelVersion);
    }

    /**
     * 获取 PPL 数据
     *
     * @deprecated 推荐使用 {@link RAGOptimizationService#getOptimizationData}
     */
    @Deprecated
    public Optional<PPLData> getPPLData(String documentId) {
        try {
            // 先尝试从新系统获取
            Optional<OptimizationData> optData = optimizationService.getOptimizationData(
                documentId,
                OptimizationType.PPL.getCode()
            );

            if (optData.isPresent()) {
                // 转换为 PPLData 格式
                OptimizationData data = optData.get();
                return Optional.of(convertToPPLData(data));
            }

            // 降级到旧系统（向后兼容）
            return storageService.getPPLData(documentId);
        } catch (Exception e) {
            log.error("Failed to get PPL data for document: {}", documentId, e);
            return Optional.empty();
        }
    }

    /**
     * 删除 PPL 数据
     *
     * @deprecated 推荐使用 {@link RAGOptimizationService#deleteOptimizationData}
     */
    @Deprecated
    public void deletePPLData(String documentId) {
        try {
            optimizationService.deleteOptimizationData(documentId, OptimizationType.PPL.getCode());
        } catch (Exception e) {
            log.error("Failed to delete PPL data for document: {}", documentId, e);
        }
    }

    // ========== 工具方法 (Utility Methods) ==========

    /**
     * 转换 OptimizationData 到 PPLData（向后兼容）
     */
    @SuppressWarnings("unchecked")
    private PPLData convertToPPLData(OptimizationData optData) {
        return PPLData.builder()
            .documentId(optData.getDocumentId())
            .probablePoints((List<String>) optData.getData("probablePoints", List.class))
            .scores((Map<String, Float>) optData.getData("scores", Map.class))
            .modelVersion((String) optData.getData("modelVersion", String.class))
            .analyzedAt(optData.getProcessedAt())
            .metadata(optData.getMetadata())
            .build();
    }
}

