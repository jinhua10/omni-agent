package top.yumbo.ai.omni.knowledge.registry.role;

import lombok.Builder;
import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * 角色模型 (Role Model)
 *
 * 表示系统中的一个角色配置
 * (Represents a role configuration in the system)
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@Data
@Builder
public class Role {

    /**
     * 角色ID (Role ID)
     */
    private String id;

    /**
     * 角色名称 (Role name)
     */
    private String name;

    /**
     * 角色描述 (Role description)
     */
    private String description;

    /**
     * 角色关键词 (Role keywords)
     */
    private List<String> keywords;

    /**
     * 是否启用 (Is enabled)
     */
    private boolean enabled;

    /**
     * 优先级 (Priority)
     */
    private int priority;

    /**
     * 扩展属性 (Extended properties)
     */
    private Map<String, Object> properties;
}


