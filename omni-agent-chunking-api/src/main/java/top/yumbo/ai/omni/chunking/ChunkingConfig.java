package top.yumbo.ai.omni.chunking;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 分块配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ChunkingConfig {

    /**
     * 分块策略
     */
    private ChunkingStrategy strategy;

    /**
     * 固定长度分块 - 块大小
     */
    private Integer fixedLengthSize;

    /**
     * 固定长度分块 - 重叠大小
     */
    private Integer overlap;

    /**
     * 语义分块 - 相似度阈值
     */
    private Double semanticThreshold;

    /**
     * 最大分块大小
     */
    private Integer maxChunkSize;

    /**
     * 最小分块大小
     */
    private Integer minChunkSize;

    /**
     * 创建默认配置
     *
     * @return 默认配置
     */
    public static ChunkingConfig defaults() {
        return ChunkingConfig.builder()
                .strategy(ChunkingStrategy.PPL)
                .fixedLengthSize(512)
                .overlap(50)
                .semanticThreshold(0.7)
                .maxChunkSize(1024)
                .minChunkSize(100)
                .build();
    }
}

