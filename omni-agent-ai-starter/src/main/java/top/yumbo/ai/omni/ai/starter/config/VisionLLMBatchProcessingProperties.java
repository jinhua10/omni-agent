package top.yumbo.ai.omni.ai.starter.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * Vision LLM 批处理配置属性
 * (Vision LLM Batch Processing Configuration Properties)
 *
 * <p>智能批处理：根据上下文大小动态决定每批处理多少张幻灯片</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "omni-agent.vision-llm.batch-processing")
public class VisionLLMBatchProcessingProperties {

    /**
     * 是否启用智能批处理
     */
    private boolean enabled = true;

    /**
     * 最大上下文token数
     * <p>
     * 根据模型限制调整：
     * - qwen-vl-plus: 8000
     * - qwen-vl-max: 32000
     * - gpt-4o: 128000
     * </p>
     */
    private int maxContextTokens = 8000;

    /**
     * 单张幻灯片预估token数
     * <p>
     * 包括：图片token + 文字token + 提示词token
     * </p>
     */
    private int estimatedTokensPerSlide = 1500;

    /**
     * 预留token数
     * <p>
     * 用于系统提示词和响应
     * </p>
     */
    private int reservedTokens = 2000;

    /**
     * 最小批次大小（至少处理多少张）
     */
    private int minBatchSize = 1;

    /**
     * 最大批次大小（最多处理多少张）
     */
    private int maxBatchSize = 5;

    /**
     * 计算可容纳的幻灯片数量
     *
     * @return 可容纳的幻灯片数量
     */
    public int calculateBatchSize() {
        int availableTokens = maxContextTokens - reservedTokens;
        int calculatedSize = availableTokens / estimatedTokensPerSlide;

        // 限制在最小和最大范围内
        return Math.max(minBatchSize, Math.min(maxBatchSize, calculatedSize));
    }

    /**
     * 预判断是否可以添加更多幻灯片到当前批次
     *
     * @param currentSlideCount 当前批次已有的幻灯片数
     * @return true 如果可以添加更多
     */
    public boolean canAddMoreSlides(int currentSlideCount) {
        if (!enabled) {
            return false;
        }

        int usedTokens = reservedTokens + (currentSlideCount * estimatedTokensPerSlide);
        int remainingTokens = maxContextTokens - usedTokens;

        return remainingTokens >= estimatedTokensPerSlide && currentSlideCount < maxBatchSize;
    }
}

