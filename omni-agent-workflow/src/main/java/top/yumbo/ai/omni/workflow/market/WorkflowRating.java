package top.yumbo.ai.omni.workflow.market;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 工作流评分
 * (Workflow Rating)
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WorkflowRating {

    /**
     * 评分ID
     */
    private String id;

    /**
     * 工作流ID
     */
    private String workflowId;

    /**
     * 用户ID
     */
    private String userId;

    /**
     * 用户名称
     */
    private String userName;

    /**
     * 评分（1-5星）
     */
    private Integer rating;

    /**
     * 评论
     */
    private String comment;

    /**
     * 创建时间
     */
    private Long createdAt;
}

