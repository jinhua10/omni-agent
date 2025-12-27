package top.yumbo.ai.omni.core.dto.role;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 创建知识角色请求
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreateRoleRequest {

    /**
     * 角色名称（必填）
     */
    private String roleName;

    /**
     * 角色描述
     */
    private String description;

    /**
     * 角色职责（必填）
     */
    private String responsibilities;

    /**
     * 学习源域ID列表（可选）
     */
    @Builder.Default
    private List<String> sourceDomainIds = new ArrayList<>();

    /**
     * 配置信息（可选）
     */
    @Builder.Default
    private Map<String, Object> config = new HashMap<>();
}


