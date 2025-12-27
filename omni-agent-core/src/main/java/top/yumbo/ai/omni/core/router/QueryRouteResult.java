package top.yumbo.ai.omni.core.router;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;

import java.util.ArrayList;
import java.util.List;

/**
 * 查询路由结果
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class QueryRouteResult {

    /**
     * 匹配的域ID列表（按相关度排序）
     */
    @Builder.Default
    private List<String> domainIds = new ArrayList<>();

    /**
     * 匹配的角色ID列表
     */
    @Builder.Default
    private List<String> roleIds = new ArrayList<>();

    /**
     * 推荐的域类型
     */
    private DomainType suggestedDomainType;

    /**
     * 路由置信度（0-1）
     */
    private Double confidence;

    /**
     * 是否需要跨域���询
     */
    @Builder.Default
    private Boolean crossDomain = false;
}

