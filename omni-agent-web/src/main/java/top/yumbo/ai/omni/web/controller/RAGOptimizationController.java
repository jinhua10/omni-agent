package top.yumbo.ai.omni.web.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.core.optimization.AutoOptimizationSelector;
import top.yumbo.ai.omni.core.optimization.AutoOptimizationSelector.*;
import top.yumbo.ai.omni.core.optimization.RAGOptimizationService;
import top.yumbo.ai.omni.storage.api.model.OptimizationData;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * RAG优化算法控制器
 *
 * 提供RAG优化算法的管理和查询API
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/rag")
@CrossOrigin(origins = "*")
@Tag(name = "RAG Optimization", description = "RAG优化算法管理API")
public class RAGOptimizationController {

    @Autowired
    private RAGOptimizationService optimizationService;

    @Autowired
    private AutoOptimizationSelector autoSelector;

    /**
     * 自动选择最佳算法
     */
    @PostMapping("/auto-select")
    @Operation(summary = "自动选择算法", description = "根据查询上下文自动推荐最佳算法组合")
    public ResponseEntity<OptimizationRecommendation> autoSelectAlgorithms(
            @RequestBody QueryContext context) {
        log.info("Auto-selecting algorithms for query: {}", context.getQuery());
        OptimizationRecommendation recommendation = autoSelector.selectOptimalAlgorithms(context);
        return ResponseEntity.ok(recommendation);
    }

    /**
     * 批量评估场景
     */
    @PostMapping("/evaluate-scenarios")
    @Operation(summary = "批量评估场景", description = "批量评估多个查询场景的最佳算法")
    public ResponseEntity<Map<String, OptimizationRecommendation>> evaluateScenarios(
            @RequestBody List<QueryContext> contexts) {
        log.info("Evaluating {} scenarios", contexts.size());
        Map<String, OptimizationRecommendation> results = autoSelector.evaluateScenarios(contexts);
        return ResponseEntity.ok(results);
    }

    /**
     * 保存优化数据
     */
    @PostMapping("/optimization-data")
    @Operation(summary = "保存优化数据", description = "保存算法优化数据")
    public ResponseEntity<String> saveOptimizationData(@RequestBody SaveOptimizationRequest request) {
        log.info("Saving optimization data for document: {}, type: {}",
                request.getDocumentId(), request.getOptimizationType());

        String resultId = optimizationService.saveOptimizationData(
                request.getDocumentId(),
                request.getOptimizationType(),
                request.getData(),
                request.getMetadata(),
                request.getMetrics()
        );

        return ResponseEntity.ok(resultId);
    }

    /**
     * 获取优化数据
     */
    @GetMapping("/optimization-data/{documentId}/{optimizationType}")
    @Operation(summary = "获取优化数据", description = "获取指定文档的特定算法优化数据")
    public ResponseEntity<OptimizationData> getOptimizationData(
            @PathVariable String documentId,
            @PathVariable String optimizationType) {
        log.info("Getting optimization data for document: {}, type: {}", documentId, optimizationType);

        Optional<OptimizationData> data = optimizationService.getOptimizationData(
                documentId, optimizationType);

        return data.map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    /**
     * 获取文档的所有优化数据
     */
    @GetMapping("/optimization-data/{documentId}")
    @Operation(summary = "获取所有优化数据", description = "获取指定文档的所有优化数据")
    public ResponseEntity<List<OptimizationData>> getAllOptimizationData(
            @PathVariable String documentId) {
        log.info("Getting all optimization data for document: {}", documentId);
        List<OptimizationData> dataList = optimizationService.getAllOptimizationData(documentId);
        return ResponseEntity.ok(dataList);
    }

    /**
     * 获取文档的优化类型列表
     */
    @GetMapping("/optimization-types/{documentId}")
    @Operation(summary = "获取优化类型", description = "获取文档已应用的所有优化算法类型")
    public ResponseEntity<List<String>> getOptimizationTypes(
            @PathVariable String documentId) {
        log.info("Getting optimization types for document: {}", documentId);
        List<String> types = optimizationService.getOptimizationTypes(documentId);
        return ResponseEntity.ok(types);
    }

    /**
     * 检查是否存在优化数据
     */
    @GetMapping("/optimization-data/{documentId}/{optimizationType}/exists")
    @Operation(summary = "检查优化数据", description = "检查是否存在指定的优化数据")
    public ResponseEntity<Boolean> hasOptimizationData(
            @PathVariable String documentId,
            @PathVariable String optimizationType) {
        log.info("Checking optimization data for document: {}, type: {}", documentId, optimizationType);
        boolean exists = optimizationService.hasOptimizationData(documentId, optimizationType);
        return ResponseEntity.ok(exists);
    }

    /**
     * 删除优化数据
     */
    @DeleteMapping("/optimization-data/{documentId}/{optimizationType}")
    @Operation(summary = "删除优化数据", description = "删除指定的优化数据")
    public ResponseEntity<String> deleteOptimizationData(
            @PathVariable String documentId,
            @PathVariable String optimizationType) {
        log.info("Deleting optimization data for document: {}, type: {}", documentId, optimizationType);
        optimizationService.deleteOptimizationData(documentId, optimizationType);
        return ResponseEntity.ok("Optimization data deleted successfully");
    }

    /**
     * 删除文档的所有优化数据
     */
    @DeleteMapping("/optimization-data/{documentId}")
    @Operation(summary = "删除所有优化数据", description = "删除文档的所有优化数据")
    public ResponseEntity<String> deleteAllOptimizationData(
            @PathVariable String documentId) {
        log.info("Deleting all optimization data for document: {}", documentId);
        optimizationService.deleteAllOptimizationData(documentId);
        return ResponseEntity.ok("All optimization data deleted successfully");
    }

    // ========== 请求/响应对象 ==========

    @Data
    public static class SaveOptimizationRequest {
        private String documentId;
        private String optimizationType;
        private Map<String, Object> data;
        private Map<String, Object> metadata;
        private Map<String, Double> metrics;
    }
}






