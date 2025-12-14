package top.yumbo.ai.ai.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * AI 请求模型
 * (AI Request Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AIRequest implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 提示词
     */
    @NotBlank(message = "提示词不能为空")
    private String prompt;

    /**
     * 系统提示（System Prompt）
     */
    private String systemPrompt;

    /**
     * 对话历史
     */
    private List<ChatMessage> messages;

    /**
     * 使用的模型
     */
    private String model;

    /**
     * 温度参数（0-1，控制随机性）
     */
    @Builder.Default
    private Float temperature = 0.7f;

    /**
     * Top P 参数（核采样）
     */
    @Builder.Default
    private Float topP = 0.9f;

    /**
     * Top K 参数
     */
    private Integer topK;

    /**
     * 最大生成token数
     */
    @Builder.Default
    private Integer maxTokens = 2048;

    /**
     * 停止词
     */
    private List<String> stopWords;

    /**
     * 是否流式输出
     */
    @Builder.Default
    private boolean stream = false;

    /**
     * 额外参数
     */
    private Map<String, Object> options;

    /**
     * 请求ID（用于追踪）
     */
    private String requestId;

    /**
     * 超时时间（秒）
     */
    @Builder.Default
    private Integer timeout = 60;
}

