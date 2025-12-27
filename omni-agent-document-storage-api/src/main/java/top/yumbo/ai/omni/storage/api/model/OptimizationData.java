package top.yumbo.ai.omni.storage.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;

import java.io.Serial;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * RAG优化数据模型 (通用)
 * (RAG Optimization Data Model - Generic)
 *
 * <p>用于存储各种RAG优化算法的分析结果，包括但不限于：</p>
 * <ul>
 *   <li>PPL - Prompt Programming Language</li>
 *   <li>HyDE - Hypothetical Document Embeddings</li>
 *   <li>Reranking - 语义重排序</li>
 *   <li>Query Expansion - 查询扩展</li>
 *   <li>Metadata Filtering - 元数据过滤</li>
 *   <li>Context Compression - 上下文压缩</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class OptimizationData implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 文档ID
     */
    @NotBlank(message = "文档ID不能为空")
    private String documentId;

    /**
     * 优化算法类型
     * (Optimization Algorithm Type)
     * <p>
     * 例如: PPL, HYDE, RERANK, QUERY_EXPANSION, etc.
     */
    @NotBlank(message = "优化类型不能为空")
    private String optimizationType;

    /**
     * 算法版本
     * (Algorithm Version)
     */
    private String algorithmVersion;

    /**
     * 分析/处理时间
     * (Analysis/Processing Timestamp)
     */
    private Long processedAt;

    /**
     * 优化数据内容
     * (Optimization Data Content)
     *
     * <p>灵活的键值对存储，根据不同算法类型存储不同的数据结构：</p>
     * <ul>
     *   <li>PPL: probablePoints, scores, modelVersion</li>
     *   <li>HyDE: hypotheticalDoc, embedding, similarity</li>
     *   <li>Rerank: rerankedIndices, scores, model</li>
     *   <li>QueryExpansion: expandedQueries, weights</li>
     * </ul>
     */
    @Builder.Default
    private Map<String, Object> data = new HashMap<>();

    /**
     * 额外元数据
     * (Additional Metadata)
     */
    @Builder.Default
    private Map<String, Object> metadata = new HashMap<>();

    /**
     * 优化效果统计
     * (Optimization Performance Metrics)
     * <p>
     * 例如: precisionGain, recallGain, processingTime
     */
    @Builder.Default
    private Map<String, Double> metrics = new HashMap<>();

    // ========== 便捷方法 (Convenience Methods) ==========

    /**
     * 添加数据项
     */
    public void putData(String key, Object value) {
        if (this.data == null) {
            this.data = new HashMap<>();
        }
        this.data.put(key, value);
    }

    /**
     * 获取数据项
     */
    @SuppressWarnings("unchecked")
    public <T> T getData(String key, Class<T> type) {
        if (this.data == null) {
            return null;
        }
        Object value = this.data.get(key);
        return value != null ? (T) value : null;
    }

    /**
     * 添加元数据
     */
    public void putMetadata(String key, Object value) {
        if (this.metadata == null) {
            this.metadata = new HashMap<>();
        }
        this.metadata.put(key, value);
    }

    /**
     * 添加性能指标
     */
    public void putMetric(String key, Double value) {
        if (this.metrics == null) {
            this.metrics = new HashMap<>();
        }
        this.metrics.put(key, value);
    }

    /**
     * 获取性能指标
     */
    public Double getMetric(String key) {
        return this.metrics != null ? this.metrics.get(key) : null;
    }
}

