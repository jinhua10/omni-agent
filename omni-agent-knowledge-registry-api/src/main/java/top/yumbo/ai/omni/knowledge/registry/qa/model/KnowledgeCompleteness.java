package top.yumbo.ai.omni.knowledge.registry.qa.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * 知识完整性评估
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeCompleteness {

    /**
     * 完整性评分 (0-1)
     */
    @Builder.Default
    private Double score = 0.0;

    /**
     * 缺失内容列表
     */
    @Builder.Default
    private List<String> missing = new ArrayList<>();

    /**
     * 评估原因
     */
    private String reason;

    /**
     * 创建默认评估
     */
    public static KnowledgeCompleteness createDefault() {
        return KnowledgeCompleteness.builder()
                .score(0.0)
                .reason("未评估")
                .build();
    }
}

