package top.yumbo.ai.omni.knowledge.registry.model;

/**
 * 角色状态枚举
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public enum RoleStatus {

    /**
     * 活跃状态
     */
    ACTIVE("活跃"),

    /**
     * 学习中
     */
    LEARNING("学习中"),

    /**
     * 暂停
     */
    PAUSED("暂停"),

    /**
     * 已归档
     */
    ARCHIVED("已归档");

    private final String description;

    RoleStatus(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}

