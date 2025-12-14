package top.yumbo.ai.storage.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * PPL 数据模型
 * (PPL - Probable Point of Loss Data Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PPLData implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 文档ID
     */
    @NotBlank(message = "文档ID不能为空")
    private String documentId;

    /**
     * 可能的关键点列表
     */
    private List<String> probablePoints;

    /**
     * 每个点的得分
     */
    private Map<String, Float> scores;

    /**
     * 模型版本
     */
    private String modelVersion;

    /**
     * 分析时间
     */
    private Long analyzedAt;

    /**
     * 额外元数据
     */
    private Map<String, Object> metadata;
}

