package top.yumbo.ai.omni.core.optimization;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.api.model.OptimizationData;
import top.yumbo.ai.omni.storage.api.model.OptimizationType;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * RAG优化服务 - 负责各种RAG优化算法数据的存储和管理（通用框架）
 * (RAG Optimization Service - Generic Framework for RAG Optimization Algorithms)
 *
 * <p>
 * 设计理念 (Design Philosophy):
 * - 通用化：支持多种RAG优化算法（PPL, HyDE, Rerank等）
 * - 可扩展：用户可以自定义优化算法
 * - 可插拔：基于DocumentStorageService实现，支持多种存储后端
 * </p>
 *
 * <p>
 * 支持的优化算法类型 (Supported Optimization Types):
 * - PPL: Prompt Programming Language
 * - HyDE: Hypothetical Document Embeddings
 * - RERANK: Semantic Reranking
 * - QUERY_EXPANSION: Query Expansion
 * - METADATA_FILTER: Metadata Filtering
 * - CONTEXT_COMPRESSION: Context Compression
 * - HYBRID_SEARCH: Hybrid Search
 * - KNOWLEDGE_GRAPH: Knowledge Graph Enhancement
 * - HOPE_ROUTING: HOPE Intelligent Routing
 * - BEHAVIOR_ANALYSIS: Behavior Analysis Enhancement
 * - MULTI_MODEL_VOTING: Multi-Model Voting
 * - CUSTOM: User-defined algorithms
 * </p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 * @version 2.0.0 - 重构为通用优化框架
 */
@Slf4j
@Service
public class RAGOptimizationService {

    private final DocumentStorageService storageService;

    @Autowired
    public RAGOptimizationService(DocumentStorageService storageService) {
        this.storageService = storageService;
        log.info("RAGOptimizationService initialized with storage: {}",
                 storageService.getClass().getSimpleName());
    }

    // ========== 通用优化数据操作 (Generic Optimization Data Operations) ==========

    /**
     * 保存优化数据
     *
     * @param documentId 文档ID
     * @param optimizationType 优化类型
     * @param data 优化数据内容
     * @return 优化数据ID
     */
    public String saveOptimizationData(String documentId,
                                      String optimizationType,
                                      Map<String, Object> data) {
        return saveOptimizationData(documentId, optimizationType, data, null, null);
    }

    /**
     * 保存优化数据（完整版）
     *
     * @param documentId 文档ID
     * @param optimizationType 优化类型
     * @param data 优化数据内容
     * @param metadata 元数据
     * @param metrics 性能指标
     * @return 优化数据ID
     */
    public String saveOptimizationData(String documentId,
                                      String optimizationType,
                                      Map<String, Object> data,
                                      Map<String, Object> metadata,
                                      Map<String, Double> metrics) {
        if (data == null || data.isEmpty()) {
            log.warn("Empty optimization data for document: {} type: {}", documentId, optimizationType);
            return null;
        }

        try {
            OptimizationData optimizationData = OptimizationData.builder()
                .documentId(documentId)
                .optimizationType(optimizationType)
                .processedAt(System.currentTimeMillis())
                .data(data)
                .metadata(metadata)
                .metrics(metrics)
                .build();

            String dataId = storageService.saveOptimizationData(documentId, optimizationData);
            log.info("Saved {} optimization data for document: {}", optimizationType, documentId);
            return dataId;
        } catch (Exception e) {
            log.error("Failed to save {} optimization data for document: {}",
                     optimizationType, documentId, e);
            return null;
        }
    }

    /**
     * 获取指定类型的优化数据
     */
    public Optional<OptimizationData> getOptimizationData(String documentId, String optimizationType) {
        try {
            return storageService.getOptimizationData(documentId, optimizationType);
        } catch (Exception e) {
            log.error("Failed to get {} optimization data for document: {}",
                     optimizationType, documentId, e);
            return Optional.empty();
        }
    }

    /**
     * 获取文档的所有优化数据
     */
    public List<OptimizationData> getAllOptimizationData(String documentId) {
        try {
            return storageService.getAllOptimizationData(documentId);
        } catch (Exception e) {
            log.error("Failed to get all optimization data for document: {}", documentId, e);
            return List.of();
        }
    }

    /**
     * 删除指定类型的优化数据
     */
    public void deleteOptimizationData(String documentId, String optimizationType) {
        try {
            storageService.deleteOptimizationData(documentId, optimizationType);
            log.info("Deleted {} optimization data for document: {}", optimizationType, documentId);
        } catch (Exception e) {
            log.error("Failed to delete {} optimization data for document: {}",
                     optimizationType, documentId, e);
        }
    }

    /**
     * 删除文档的所有优化数据
     */
    public void deleteAllOptimizationData(String documentId) {
        try {
            storageService.deleteAllOptimizationData(documentId);
            log.info("Deleted all optimization data for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete all optimization data for document: {}", documentId, e);
        }
    }

    // ========== 特定算法的便捷方法 (Algorithm-Specific Convenience Methods) ==========

    /**
     * 保存PPL优化数据
     */
    public String savePPLData(String documentId,
                             List<String> probablePoints,
                             Map<String, Float> scores,
                             String modelVersion) {
        Map<String, Object> data = Map.of(
            "probablePoints", probablePoints,
            "scores", scores,
            "modelVersion", modelVersion
        );
        return saveOptimizationData(documentId, OptimizationType.PPL.getCode(), data);
    }

    /**
     * 保存HyDE优化数据
     */
    public String saveHyDEData(String documentId,
                              String hypotheticalDoc,
                              float[] embedding,
                              double similarity) {
        Map<String, Object> data = Map.of(
            "hypotheticalDoc", hypotheticalDoc,
            "embedding", embedding,
            "similarity", similarity
        );
        return saveOptimizationData(documentId, OptimizationType.HYDE.getCode(), data);
    }

    /**
     * 保存Rerank优化数据
     */
    public String saveRerankData(String documentId,
                                List<Integer> rerankedIndices,
                                List<Double> scores,
                                String model) {
        Map<String, Object> data = Map.of(
            "rerankedIndices", rerankedIndices,
            "scores", scores,
            "model", model
        );
        return saveOptimizationData(documentId, OptimizationType.RERANK.getCode(), data);
    }

    /**
     * 保存查询扩展数据
     */
    public String saveQueryExpansionData(String documentId,
                                        List<String> expandedQueries,
                                        Map<String, Double> weights) {
        Map<String, Object> data = Map.of(
            "expandedQueries", expandedQueries,
            "weights", weights
        );
        return saveOptimizationData(documentId, OptimizationType.QUERY_EXPANSION.getCode(), data);
    }

    /**
     * 保存元数据过滤配置
     */
    public String saveMetadataFilterData(String documentId,
                                        Map<String, Object> filters,
                                        List<String> extractedFilters) {
        Map<String, Object> data = Map.of(
            "filters", filters,
            "extractedFilters", extractedFilters
        );
        return saveOptimizationData(documentId, OptimizationType.METADATA_FILTER.getCode(), data);
    }

    /**
     * 保存上下文压缩数据
     */
    public String saveContextCompressionData(String documentId,
                                           String originalContext,
                                           String compressedContext,
                                           double compressionRatio) {
        Map<String, Object> data = Map.of(
            "originalContext", originalContext,
            "compressedContext", compressedContext,
            "originalLength", originalContext.length(),
            "compressedLength", compressedContext.length()
        );
        Map<String, Double> metrics = Map.of(
            "compressionRatio", compressionRatio
        );
        return saveOptimizationData(documentId,
                                   OptimizationType.CONTEXT_COMPRESSION.getCode(),
                                   data, null, metrics);
    }

    // ========== 工具方法 (Utility Methods) ==========

    /**
     * 检查文档是否有指定类型的优化数据
     */
    public boolean hasOptimizationData(String documentId, String optimizationType) {
        return getOptimizationData(documentId, optimizationType).isPresent();
    }

    /**
     * 获取文档的优化类型列表
     */
    public List<String> getOptimizationTypes(String documentId) {
        return getAllOptimizationData(documentId).stream()
            .map(OptimizationData::getOptimizationType)
            .distinct()
            .toList();
    }

    /**
     * 批量保存优化数据
     */
    public void batchSaveOptimizationData(List<OptimizationData> dataList) {
        dataList.forEach(data ->
            storageService.saveOptimizationData(data.getDocumentId(), data)
        );
        log.info("Batch saved {} optimization data items", dataList.size());
    }
}

