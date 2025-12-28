package top.yumbo.ai.omni.chunking.starter.config;
}
    }
        private int minChunkSize = 100;
         */
         * 最小分块大小
        /**

        private int maxChunkSize = 1024;
         */
         * 最大分块大小
        /**
    public static class General {
    @Data
     */
     * 通用配置
    /**

    }
        private double threshold = 0.7;
         */
         * 相似度阈值
        /**
    public static class Semantic {
    @Data
     */
     * 语义分块配置
    /**

    }
        private int overlap = 50;
         */
         * 重叠大小
        /**

        private int size = 512;
         */
         * 块大小
        /**
    public static class FixedLength {
    @Data
     */
     * 固定长度分块配置
    /**

    private General general = new General();
     */
     * 通用配置
    /**

    private Semantic semantic = new Semantic();
     */
     * 语义分块配置
    /**

    private FixedLength fixedLength = new FixedLength();
     */
     * 固定长度分块配置
    /**

    private ChunkingStrategy strategy = ChunkingStrategy.PPL;
     */
     * 默认分块策略
    /**

    private boolean enabled = true;
     */
     * 是否启用分块服务
    /**

public class ChunkingProperties {
@ConfigurationProperties(prefix = "omni-agent.chunking")
@Data
 */
 * @since 1.0.0
 * @author OmniAgent Team
 *
 * 分块配置属性
/**

import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import org.springframework.boot.context.properties.ConfigurationProperties;
import lombok.Data;


