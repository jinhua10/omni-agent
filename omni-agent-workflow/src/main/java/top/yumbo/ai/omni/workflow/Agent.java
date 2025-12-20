package top.yumbo.ai.omni.workflow;

import java.util.Map;

/**
 * Agent 接口
 * (Agent Interface)
 *
 * <p>工作流中的执行单元，每个Agent负责完成特定的任务</p>
 *
 * <p>示例实现:</p>
 * <pre>{@code
 * @Component("SymptomExtractor")
 * public class SymptomExtractorAgent implements Agent {
 *     @Override
 *     public Object execute(Object input, WorkflowContext context) {
 *         String userDescription = (String) input;
 *         // 提取症状...
 *         return symptoms;
 *     }
 *
 *     @Override
 *     public String getName() {
 *         return "SymptomExtractor";
 *     }
 * }
 * }</pre>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
public interface Agent {

    /**
     * 执行 Agent 任务
     *
     * @param input 输入数据（来自前序步骤或工作流输入）
     * @param context 工作流上下文（包含所有步骤的结果）
     * @return 输出结果
     * @throws Exception 执行异常
     */
    Object execute(Object input, WorkflowContext context) throws Exception;

    /**
     * 获取 Agent 名称（必须唯一）
     *
     * @return Agent 名称
     */
    String getName();

    /**
     * 获取 Agent 描述
     *
     * @return Agent 描述
     */
    default String getDescription() {
        return "";
    }

    /**
     * 获取 Agent 支持的输入类型
     *
     * @return 输入类型（Java 类名）
     */
    default String getInputType() {
        return "Object";
    }

    /**
     * 获取 Agent 支持的输出类型
     *
     * @return 输出类型（Java 类名）
     */
    default String getOutputType() {
        return "Object";
    }

    /**
     * 获取 Agent 配置schema（用于验证config参数）
     *
     * @return 配置schema（JSON Schema）
     */
    default Map<String, Object> getConfigSchema() {
        return Map.of();
    }

    /**
     * 验证输入数据
     *
     * @param input 输入数据
     * @return 是否有效
     */
    default boolean validateInput(Object input) {
        return input != null;
    }
}

