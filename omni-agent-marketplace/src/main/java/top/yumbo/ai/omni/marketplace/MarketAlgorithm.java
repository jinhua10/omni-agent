package top.yumbo.ai.omni.marketplace;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * 算法市场 - 算法定义
 *
 * 支持三种类型：
 * 1. Pipeline - 配置化组合（最安全）
 * 2. Script - 脚本实现（需要沙箱）
 * 3. Remote - 远程服务（需要鉴权）
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MarketAlgorithm {

    /**
     * 算法ID
     */
    private String algorithmId;

    /**
     * 算法名称
     */
    private String name;

    /**
     * 算法版本
     */
    private String version;

    /**
     * 作者信息
     */
    private String author;

    /**
     * 算法描述
     */
    private String description;

    /**
     * 算法类型
     */
    private AlgorithmType type;

    /**
     * 配置化算法：步骤配置
     */
    private PipelineConfig pipelineConfig;

    /**
     * 脚本算法：脚本内容
     */
    private String script;

    /**
     * 脚本语言：javascript / python（暂只支持javascript）
     */
    private String scriptLanguage;

    /**
     * 远程算法：服务端点
     */
    private String remoteEndpoint;

    /**
     * 远程算法：认证token
     */
    private String remoteAuthToken;

    /**
     * 算法标签
     */
    private List<String> tags;

    /**
     * 性能指标
     */
    private AlgorithmMetrics metrics;

    /**
     * 使用次数
     */
    private Long usageCount;

    /**
     * 评分（1-5星）
     */
    private Double rating;

    /**
     * 是否已审核通过
     */
    private Boolean approved;

    /**
     * 创建时间
     */
    private Long createdAt;

    /**
     * 更新时间
     */
    private Long updatedAt;

    /**
     * 算法类型枚举
     */
    public enum AlgorithmType {
        PIPELINE,   // 配置化组合（最安全）
        SCRIPT,     // 脚本实现（需要沙箱隔离）
        REMOTE      // 远程服务（需要网络鉴权）
    }

    /**
     * 配置化算法的步骤配置
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PipelineConfig {
        /**
         * 步骤列表
         */
        private List<PipelineStep> steps;

        /**
         * 最大执行时间（毫秒）
         */
        private Integer maxExecutionTimeMs;
    }

    /**
     * 单个步骤
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PipelineStep {
        /**
         * 步骤类型（对应已注册的组件）
         */
        private String type;

        /**
         * 步骤参数
         */
        private Map<String, Object> params;

        /**
         * 条件执行（可选）
         */
        private String condition;
    }

    /**
     * 算法性能指标
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AlgorithmMetrics {
        /**
         * 平均精度提升
         */
        private Double avgPrecisionGain;

        /**
         * 平均延迟（ms）
         */
        private Double avgLatency;

        /**
         * 成功率
         */
        private Double successRate;
    }
}

