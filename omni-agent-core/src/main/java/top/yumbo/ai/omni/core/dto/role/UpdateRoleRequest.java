package top.yumbo.ai.omni.core.dto.role;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.knowledge.registry.model.RoleStatus;

import java.util.List;

/**
 * 更新知识角色请求
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UpdateRoleRequest {

    /**
     * 角色名称
     */
    private String roleName;

    /**
     * 角色描述
     */
    private String description;

    /**
     * 角色职责
     */
    private String responsibilities;

    /**
     * 角色状态
     */
    private RoleStatus status;

    /**
     * 学习源域ID列表
     */
    private List<String> sourceDomainIds;
}

