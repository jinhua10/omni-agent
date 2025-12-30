package top.yumbo.ai.omni.hope.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 问题类型配置
 * HOPE 系统使用的问题分类配置模型
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class QuestionTypeConfig implements Serializable {

    /**
     * 类型ID（唯一标识）
     */
    private String id;

    /**
     * 类型名称（中文）
     */
    private String name;

    /**
     * 类型名称（英文）
     */
    private String nameEn;

    /**
     * 优先级（数字越大优先级越高）
     */
    @Builder.Default
    private Integer priority = 0;

    /**
     * 复杂度（simple/medium/complex）
     */
    @Builder.Default
    private String complexity = "medium";

    /**
     * 建议使用的层级（permanent/ordinary/high_frequency）
     */
    @Builder.Default
    private String suggestedLayer = "ordinary";

    /**
     * 是否启用
     */
    @Builder.Default
    private Boolean enabled = true;

    /**
     * 关键词列表
     */
    @Builder.Default
    private List<String> keywords = new ArrayList<>();

    /**
     * 正则表达式模式列表
     */
    @Builder.Default
    private List<String> patterns = new ArrayList<>();

    /**
     * 扩展属性
     */
    @Builder.Default
    private Map<String, Object> metadata = new HashMap<>();

    /**
     * 描述信息
     */
    private String description;
}

