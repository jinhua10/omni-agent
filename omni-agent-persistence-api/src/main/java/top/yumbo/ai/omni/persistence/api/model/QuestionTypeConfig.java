package top.yumbo.ai.omni.persistence.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;

/**
 * 问题类型配置
 * (Question Type Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class QuestionTypeConfig implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 类型ID (Type ID)
     */
    @NotBlank(message = "类型ID不能为空")
    private String id;

    /**
     * 类型名称（中文）(Type name - Chinese)
     */
    @NotBlank(message = "类型名称不能为空")
    private String name;

    /**
     * 类型名称（英文）(Type name - English)
     */
    private String nameEn;

    /**
     * 优先级（数字越小优先级越高）(Priority - lower number means higher priority)
     */
    @Builder.Default
    private int priority = 100;

    /**
     * 复杂度 (Complexity)
     * 可选值: LOW, MEDIUM, HIGH
     */
    private String complexity;

    /**
     * 建议的知识层 (Suggested knowledge layer)
     * 可选值: HIGH_FREQUENCY, ORDINARY, PERMANENT
     */
    private String suggestedLayer;

    /**
     * 是否启用 (Enabled flag)
     */
    @Builder.Default
    private boolean enabled = true;

    /**
     * 描述信息 (Description)
     */
    private String description;

    /**
     * 创建时间 (Created timestamp)
     */
    private Long createdAt;

    /**
     * 更新时间 (Updated timestamp)
     */
    private Long updatedAt;
}

