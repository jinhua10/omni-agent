package top.yumbo.ai.ai.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

/**
 * AI 响应模型
 * (AI Response Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AIResponse implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 生成的文本
     */
    private String text;

    /**
     * 使用的模型
     */
    private String model;

    /**
     * 完成原因（stop, length, error, etc.）
     */
    private String finishReason;

    /**
     * 输入token数
     */
    private Integer promptTokens;

    /**
     * 输出token数
     */
    private Integer completionTokens;

    /**
     * 总token数
     */
    private Integer totalTokens;

    /**
     * 生成耗时（毫秒）
     */
    private Long duration;

    /**
     * 请求ID
     */
    private String requestId;

    /**
     * 是否成功
     */
    @Builder.Default
    private boolean success = true;

    /**
     * 错误信息
     */
    private String error;

    /**
     * 额外元数据
     */
    private Map<String, Object> metadata;

    /**
     * 响应时间戳
     */
    private Long timestamp;
}

