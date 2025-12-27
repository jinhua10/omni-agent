package top.yumbo.ai.omni.core.qa.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.rag.model.Document;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 知识缺口检测结果
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeGapResult {

    /**
     * 是否有知识
     */
    @Builder.Default
    private boolean hasKnowledge = false;

    /**
     * 知识完整性评估
     */
    private KnowledgeCompleteness completeness;

    /**
     * 检索到的知识（按域分组）
     */
    @Builder.Default
    private Map<String, List<Document>> retrievedKnowledge = new HashMap<>();

    /**
     * 知识缺口列表
     */
    @Builder.Default
    private List<String> gaps = new ArrayList<>();

    /**
     * 是否需要用户输入
     */
    @Builder.Default
    private boolean needsUserInput = false;

    /**
     * 向用户提出的问题
     */
    @Builder.Default
    private List<String> questionsForUser = new ArrayList<>();

    /**
     * 是否知识充足（可以直接回答）
     */
    public boolean isKnowledgeSufficient() {
        return hasKnowledge &&
               completeness != null &&
               completeness.getScore() >= 0.7 &&
               !needsUserInput;
    }
}

