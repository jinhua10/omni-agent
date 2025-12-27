package top.yumbo.ai.omni.core.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.Map;

/**
 * 知识文档模型
 *
 * <p>用于在知识域之间传递文档数据</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeDocument {

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
     * 来源域ID
     */
    private String sourceDomainId;

    /**
     * 文档类型
     */
    private String documentType;

    /**
     * 元数据
     */
    @Builder.Default
    private Map<String, Object> metadata = new HashMap<>();

    /**
     * 相关性得分（用于排序）
     */
    private Double relevanceScore;
}


