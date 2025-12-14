package top.yumbo.ai.rag.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * 搜索结果模型
 * (Search Result Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SearchResult implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 文档对象
     */
    private Document document;

    /**
     * 相关性分数
     */
    private float score;

    /**
     * 文本相关性分数（混合搜索时）
     */
    private Float textScore;

    /**
     * 向量相似度分数（混合搜索时）
     */
    private Float vectorScore;

    /**
     * 高亮片段
     */
    private List<String> highlights;

    /**
     * 匹配原因
     */
    private String reason;

    /**
     * 排名
     */
    private Integer rank;

    /**
     * 距离（向量搜索时）
     */
    private Float distance;
}

