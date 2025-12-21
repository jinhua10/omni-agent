package top.yumbo.ai.omni.core.query.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.Map;

/**
 * 查询请求模型
 * (Query Request Model)
 *
 * <p>
 * 封装所有查询参数，支持高级查询功能
 * (Encapsulates all query parameters, supports advanced query features)
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class QueryRequest {

    /**
     * 查询文本
     * (Query text)
     */
    private String queryText;

    /**
     * 查询字段
     * (Query fields)
     */
    @Builder.Default
    private String[] fields = new String[]{"title", "content"};

    /**
     * 返回结果数量限制
     * (Result count limit)
     */
    @Builder.Default
    private int limit = 10;

    /**
     * 结果偏移量（用于分页）
     * (Result offset for pagination)
     */
    @Builder.Default
    private int offset = 0;

    /**
     * 过滤条件
     * (Filter conditions)
     */
    @Builder.Default
    private Map<String, String> filters = new HashMap<>();

    /**
     * 排序字段
     * (Sort field)
     */
    private String sortField;

    /**
     * 排序方向
     * (Sort order)
     */
    @Builder.Default
    private SortOrder sortOrder = SortOrder.RELEVANCE;

    /**
     * 是否启用模糊查询
     * (Whether to enable fuzzy query)
     */
    @Builder.Default
    private boolean enableFuzzy = false;

    /**
     * 最小分数阈值
     * (Minimum score threshold)
     */
    @Builder.Default
    private float minScore = 0.0f;

    /**
     * 是否启用查询扩展
     * (Whether to enable query expansion)
     */
    @Builder.Default
    private boolean enableExpansion = false;

    /**
     * 是否启用重排序
     * (Whether to enable reranking)
     */
    @Builder.Default
    private boolean enableRerank = false;

    /**
     * 排序方向枚举
     * (Sort order enumeration)
     */
    public enum SortOrder {
        /**
         * 升序 (Ascending order)
         */
        ASC,

        /**
         * 降序 (Descending order)
         */
        DESC,

        /**
         * 按相关性排序 (Sort by relevance)
         */
        RELEVANCE
    }

    /**
     * 生成缓存键
     * (Generate cache key)
     *
     * @return 缓存键 (Cache key)
     */
    public String getCacheKey() {
        return String.format("query:%s:limit:%d:offset:%d:minScore:%.2f",
                queryText, limit, offset, minScore);
    }
}
