package top.yumbo.ai.omni.core.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 提炼的知识模型
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RefinedKnowledge {

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
     * 重要性等级（1-5）
     */
    private Integer importance;
}

