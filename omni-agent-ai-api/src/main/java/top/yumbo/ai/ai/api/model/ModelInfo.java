package top.yumbo.ai.ai.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

/**
 * 模型信息
 * (Model Information)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ModelInfo implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 模型名称
     */
    private String name;

    /**
     * 模型显示名称
     */
    private String displayName;

    /**
     * 模型描述
     */
    private String description;

    /**
     * 模型大小
     */
    private String size;

    /**
     * 上下文长度
     */
    private Integer contextLength;

    /**
     * 是否可用
     */
    @Builder.Default
    private boolean available = true;

    /**
     * 模型类型（chat, completion, embedding）
     */
    private String type;

    /**
     * 供应商
     */
    private String provider;
}

