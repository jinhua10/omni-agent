package top.yumbo.ai.omni.knowledge.registry.dto.domain;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;

import java.util.HashMap;
import java.util.Map;

/**
 * 创建知识域请求
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreateDomainRequest {

    /**
     * 域名称（必填）
     */
    private String domainName;

    /**
     * 域类型（必填）
     */
    private DomainType domainType;

    /**
     * 描述
     */
    private String description;

    /**
     * 关联的实体ID（可选）
     */
    private String linkedEntityId;

    /**
     * 配置信息（可选）
     */
    @Builder.Default
    private Map<String, Object> config = new HashMap<>();
}


