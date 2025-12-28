package top.yumbo.ai.omni.chunking.starter.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;

/**
 * 分块配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.chunking")
public class ChunkingProperties {

    /**
     * 是否启用分块服务
     */
    private boolean enabled = true;

    /**
     * 默认分块策略
     */
    private ChunkingStrategy strategy = ChunkingStrategy.PPL;

    /**
     * 固定长度分块配置
     */
    private FixedLength fixedLength = new FixedLength();

    /**
     * 语义分块配置
     */
    private Semantic semantic = new Semantic();

    /**
     * 通用配置
     */
    private General general = new General();

    /**
     * 固定长度分块配置
     */
    @Data
    public static class FixedLength {
        /**
         * 块大小
         */
        private int size = 512;

        /**
         * 重叠大小
         */
        private int overlap = 50;
    }

    /**
     * 语义分块配置
     */
    @Data
    public static class Semantic {
        /**
         * 相似度阈值
         */
        private double threshold = 0.7;
    }

    /**
     * 通用配置
     */
    @Data
    public static class General {
        /**
         * 最大分块大小
         */
        private int maxChunkSize = 1024;

        /**
         * 最小分块大小
         */
        private int minChunkSize = 100;
    }
}



