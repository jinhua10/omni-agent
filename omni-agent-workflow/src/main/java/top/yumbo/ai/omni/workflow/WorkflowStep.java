package top.yumbo.ai.omni.workflow;

import lombok.Data;
import java.util.List;
import java.util.Map;

/**
 * 工作流步骤定义
 * (Workflow Step Definition)
 *
 * <p>工作流中的单个步骤，每个步骤由一个 Agent 执行</p>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Data
@lombok.Builder
@lombok.NoArgsConstructor
@lombok.AllArgsConstructor
public class WorkflowStep {

    /**
     * 步骤ID（在工作流内唯一）
     */
    private String id;

    /**
     * 步骤名称
     */
    private String name;

    /**
     * 执行此步骤的 Agent 名称
     */
    private String agent;

    /**
     * 输入配置
     * - 可以是固定值
     * - 可以引用前序步骤的输出: "${step_id.output}"
     * - 可以引用工作流输入: "${workflow.input}"
     */
    private Object input;

    /**
     * 步骤配置参数
     */
    private Map<String, Object> config;

    /**
     * 依赖的步骤ID列表（此步骤必须在依赖步骤完成后才能执行）
     */
    private List<String> dependencies;

    /**
     * 是否允许失败（如果为true，即使此步骤失败，工作流也会继续执行）
     */
    @lombok.Builder.Default
    private boolean allowFailure = false;

    /**
     * 超时时间（毫秒）
     */
    @lombok.Builder.Default
    private long timeout = 60000;  // 默认60秒

    /**
     * 重试次数
     */
    @lombok.Builder.Default
    private int retries = 0;

    /**
     * 条件执行表达式（SpEL表达式，返回boolean）
     * 例如: "${symptom_extraction.severity == 'HIGH'}"
     */
    private String condition;
}

