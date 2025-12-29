package top.yumbo.ai.omni.knowledge.registry.dto.role;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 从域学习知识请求
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class LearnFromDomainsRequest {

    /**
     * 源域ID列表（必填）
     */
    private List<String> sourceDomainIds;

    /**
     * 是否使用AI提炼知识（默认true）
     */
    @Builder.Default
    private Boolean useAIRefinement = true;

    /**
     * 最大文档数量限制（避免一次处理太多）
     */
    @Builder.Default
    private Integer maxDocuments = 100;
}


