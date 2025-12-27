package top.yumbo.ai.rag.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

/**
 * RAG 文档模型
 * (RAG Document Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Document implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 文档ID
     */
    private String id;

    /**
     * 文档标题
     */
    private String title;

    /**
     * 文档内容
     */
    private String content;

    /**
     * 文档摘要
     */
    private String summary;

    /**
     * 向量表示（Embedding）
     */
    private float[] embedding;

    /**
     * 元数据
     */
    private Map<String, Object> metadata;

    /**
     * 来源
     */
    private String source;

    /**
     * 文档类型
     */
    private String type;

    /**
     * 作者
     */
    private String author;

    /**
     * 标签
     */
    private java.util.List<String> tags;

    /**
     * 创建时间
     */
    private Long createdAt;

    /**
     * 更新时间
     */
    private Long updatedAt;

    /**
     * 索引时间
     */
    private Long indexedAt;
}

