package top.yumbo.ai.omni.rag.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

/**
 * 搜索结果模型
 * (Search Result Model)
 *
 * 包装 Document 并添加搜索相关的元数据
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SearchResult implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 文档对象
     */
    private Document document;

    /**
     * 相关性得分（0-1）
     */
    private Double score;

    /**
     * 高亮片段
     */
    private Map<String, String> highlights;

    /**
     * 匹配字段
     */
    private String matchedField;

    /**
     * 排名位置
     */
    private Integer rank;

    /**
     * 便捷方法：从 Document 创建
     */
    public static SearchResult fromDocument(Document document) {
        return SearchResult.builder()
                .document(document)
                .score(document.getScore())
                .build();
    }

    /**
     * 便捷方法：从 Document 创建并设置分数
     */
    public static SearchResult fromDocument(Document document, Double score) {
        return SearchResult.builder()
                .document(document)
                .score(score)
                .build();
    }

    // ========== 便捷访问方法 ==========

    /**
     * 获取文档ID
     */
    public String getDocumentId() {
        return document != null ? document.getId() : null;
    }

    /**
     * 获取文档标题
     */
    public String getTitle() {
        return document != null ? document.getTitle() : null;
    }

    /**
     * 获取文档内容
     */
    public String getContent() {
        return document != null ? document.getContent() : null;
    }

    /**
     * 获取文档摘要
     */
    public String getSummary() {
        return document != null ? document.getSummary() : null;
    }
}

