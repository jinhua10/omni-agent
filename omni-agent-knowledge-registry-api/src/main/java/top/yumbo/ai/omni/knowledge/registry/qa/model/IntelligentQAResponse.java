package top.yumbo.ai.omni.knowledge.registry.qa.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.rag.model.Document;

import java.util.ArrayList;
import java.util.List;

/**
 * 智能问答响应
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class IntelligentQAResponse {

    /**
     * 对话ID
     */
    private String conversationId;

    /**
     * 用户问题
     */
    private String question;

    /**
     * 回答
     */
    private String answer;

    /**
     * 意图分析结果
     */
    private IntentAnalysisResult intent;

    /**
     * 是否找到知识
     */
    @Builder.Default
    private Boolean hasKnowledge = false;

    /**
     * 知识是否充足
     */
    @Builder.Default
    private Boolean knowledgeSufficient = false;

    /**
     * 是否需要更多信息
     */
    @Builder.Default
    private Boolean needsMoreInfo = false;

    /**
     * 参考文档
     */
    @Builder.Default
    private List<Document> references = new ArrayList<>();
}

