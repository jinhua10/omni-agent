package top.yumbo.ai.omni.core.qa.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * 意图分析结果
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class IntentAnalysisResult {

    /**
     * 核心意图
     */
    private String intent;

    /**
     * 关键实体
     */
    @Builder.Default
    private List<String> entities = new ArrayList<>();

    /**
     * 技术栈
     */
    @Builder.Default
    private List<String> techStack = new ArrayList<>();

    /**
     * 约束条件
     */
    @Builder.Default
    private List<String> constraints = new ArrayList<>();

    /**
     * 缺失信息
     */
    @Builder.Default
    private List<String> missingInfo = new ArrayList<>();

    /**
     * 置信度 (0-1)
     */
    @Builder.Default
    private Double confidence = 0.0;

    /**
     * 创建默认结果
     */
    public static IntentAnalysisResult createDefault() {
        return IntentAnalysisResult.builder()
                .intent("unknown")
                .confidence(0.0)
                .build();
    }

    /**
     * 是否有缺失信息
     */
    public boolean hasMissingInfo() {
        return missingInfo != null && !missingInfo.isEmpty();
    }
}

