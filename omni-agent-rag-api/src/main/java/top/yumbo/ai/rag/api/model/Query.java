package top.yumbo.ai.rag.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;

import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

/**
 * 查询模型
 * (Query Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Query implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 查询文本
     */
    @NotBlank(message = "查询文本不能为空")
    private String text;

    /**
     * 向量表示（用于向量搜索）
     */
    private float[] embedding;

    /**
     * 返回结果数量
     */
    @Builder.Default
    private int topK = 5;

    /**
     * 搜索模式
     */
    @Builder.Default
    private SearchMode mode = SearchMode.TEXT;

    /**
     * 文本权重（混合搜索时使用）
     */
    @Builder.Default
    private float textWeight = 0.5f;

    /**
     * 向量权重（混合搜索时使用）
     */
    @Builder.Default
    private float vectorWeight = 0.5f;

    /**
     * 最小相似度阈值
     */
    @Builder.Default
    private float minScore = 0.0f;

    /**
     * 过滤条件
     */
    private Map<String, Object> filters;

    /**
     * 高亮设置
     */
    @Builder.Default
    private boolean highlight = false;

    /**
     * 是否包含向量
     */
    @Builder.Default
    private boolean includeEmbedding = false;

    /**
     * 搜索模式枚举
     */
    public enum SearchMode {
        /** 文本搜索 */
        TEXT,
        /** 向量搜索 */
        VECTOR,
        /** 混合搜索 */
        HYBRID,
        /** 语义搜索 */
        SEMANTIC
    }
}

