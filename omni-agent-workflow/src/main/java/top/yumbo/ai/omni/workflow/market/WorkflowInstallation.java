package top.yumbo.ai.omni.workflow.market;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 工作流安装记录
 * (Workflow Installation)
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WorkflowInstallation {

    /**
     * 安装ID
     */
    private String id;

    /**
     * 工作流ID
     */
    private String workflowId;

    /**
     * 工作流版本
     */
    private String workflowVersion;

    /**
     * 用户ID
     */
    private String userId;

    /**
     * 安装时间
     */
    private Long installedAt;

    /**
     * 是否启用
     */
    @Builder.Default
    private Boolean enabled = true;
}

