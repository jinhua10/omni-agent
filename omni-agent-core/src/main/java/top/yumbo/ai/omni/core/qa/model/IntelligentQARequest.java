package top.yumbo.ai.omni.core.qa.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 智能问答请求
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class IntelligentQARequest {

    /**
     * 用户问题
     */
    private String question;

    /**
     * 对话ID（可选，用于多轮对话）
     */
    private String conversationId;

    /**
     * 用户ID（可选）
     */
    private String userId;

    /**
     * 是否启用知识学习（默认true）
     */
    @Builder.Default
    private Boolean enableLearning = true;
}

