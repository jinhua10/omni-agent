package top.yumbo.ai.knowledge.registry.model;

/**
 * 知识域状态枚举
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public enum DomainStatus {

    /**
     * 活跃状态
     */
    ACTIVE("活跃"),

    /**
     * 非活跃状态
     */
    INACTIVE("非活跃"),

    /**
     * 已归档
     */
    ARCHIVED("已归档"),

    /**
     * 错误状态
     */
    ERROR("错误");

    private final String description;

    DomainStatus(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}

