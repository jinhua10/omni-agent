package top.yumbo.ai.omni.knowledge.registry.model;

/**
 * 知识域类型枚举
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public enum DomainType {

    /**
     * 文档知识域
     */
    DOCUMENT("文档知识域"),

    /**
     * 源码知识域
     */
    SOURCE_CODE("源码知识域"),

    /**
     * 角色知识域
     */
    ROLE_KNOWLEDGE("角色知识域"),

    /**
     * API文档域
     */
    API_DOCUMENTATION("API文档域"),

    /**
     * 混合域
     */
    MIXED("混合域");

    private final String description;

    DomainType(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}

