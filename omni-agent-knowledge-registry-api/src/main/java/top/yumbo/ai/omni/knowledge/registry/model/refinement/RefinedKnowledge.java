package top.yumbo.ai.omni.knowledge.registry.model.refinement;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * 提炼的知识模型
 *
 * <p>表示经过AI或规则处理后的精炼知识</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RefinedKnowledge implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 知识ID
     */
    private String knowledgeId;

    /**
     * 知识标题
     */
    private String title;

    /**
     * 提炼后的知识内容
     */
    private String refinedContent;

    /**
     * 原始文档ID
     */
    private String sourceDocumentId;

    /**
     * 来源域ID
     */
    private String sourceDomainId;

    /**
     * 角色ID
     */
    private String roleId;

    /**
     * 知识类型
     */
    private String knowledgeType;

    /**
     * 重要性等级（0.0-1.0）
     */
    private Double importance;

    /**
     * 创建时间
     */
    private LocalDateTime createdAt;

    /**
     * 更新时间
     */
    private LocalDateTime updatedAt;
}

