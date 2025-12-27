package top.yumbo.ai.omni.core.dto.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;

/**
 * 更新知识域请求
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UpdateDomainRequest {

    /**
     * 域名称
     */
    private String domainName;

    /**
     * 描述
     */
    private String description;

    /**
     * 状态
     */
    private DomainStatus status;
}

