package top.yumbo.ai.omni.marketplace;

import java.util.Map;

/**
 * 算法组件接口
 *
 * 所有可组合的算法组件都需要实现此接口
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface AlgorithmComponent {

    /**
     * 执行算法
     *
     * @param input 输入数据
     * @param params 参数配置
     * @return 输出结果
     */
    Object execute(Object input, Map<String, Object> params);

    /**
     * 获取性能指标
     *
     * @return 指标Map（如：precisionGain, latency）
     */
    Map<String, Double> getMetrics();

    /**
     * 获取组件名称
     */
    default String getName() {
        return this.getClass().getSimpleName();
    }
}

