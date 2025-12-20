package top.yumbo.ai.omni.workflow.market;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.workflow.Workflow;

import java.util.List;
import java.util.Map;

/**
 * 市场工作流
 * (Market Workflow)
 *
 * <p>用于工作流市场的工作流数据模型，包含市场相关的元数据</p>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MarketWorkflow {

    // ========== 基本信息 ==========

    /**
     * 唯一ID
     */
    private String id;

    /**
     * 工作流名称
     */
    private String name;

    /**
     * 版本号
     */
    private String version;

    /**
     * 描述
     */
    private String description;

    /**
     * 分类
     */
    private String category;

    /**
     * 标签
     */
    private List<String> tags;

    // ========== 作者信息 ==========

    /**
     * 作者ID
     */
    private String authorId;

    /**
     * 作者名称
     */
    private String authorName;

    // ========== 工作流定义 ==========

    /**
     * 工作流定义
     */
    private Workflow workflowDefinition;

    // ========== 市场信息 ==========

    /**
     * 状态（draft/published/deprecated）
     */
    @Builder.Default
    private String status = "draft";

    /**
     * 是否公开
     */
    @Builder.Default
    private boolean isPublic = true;

    /**
     * 许可证（MIT/Apache/GPL等）
     */
    @Builder.Default
    private String license = "MIT";

    // ========== 统计信息 ==========

    /**
     * 下载次数
     */
    @Builder.Default
    private Long downloadCount = 0L;

    /**
     * 安装次数
     */
    @Builder.Default
    private Long installCount = 0L;

    /**
     * 收藏次数
     */
    @Builder.Default
    private Long favoriteCount = 0L;

    /**
     * 平均评分（0-5）
     */
    @Builder.Default
    private Double rating = 0.0;

    /**
     * 评分人数
     */
    @Builder.Default
    private Long ratingCount = 0L;

    // ========== 时间信息 ==========

    /**
     * 创建时间
     */
    private Long createdAt;

    /**
     * 更新时间
     */
    private Long updatedAt;

    /**
     * 发布时间
     */
    private Long publishedAt;

    // ========== 元数据 ==========

    /**
     * 元数据
     */
    private Map<String, Object> metadata;

    /**
     * 依赖的其他工作流
     */
    private List<String> dependencies;

    /**
     * 需要的 Agent
     */
    private List<String> requiredAgents;
}

