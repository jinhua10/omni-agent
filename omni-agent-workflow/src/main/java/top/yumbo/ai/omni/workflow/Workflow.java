package top.yumbo.ai.omni.workflow;

import lombok.Data;
import java.util.List;
import java.util.Map;

/**
 * 工作流定义
 * (Workflow Definition)
 *
 * <p>定义一个完整的工作流，包含多个步骤</p>
 *
 * <p>示例:</p>
 * <pre>{@code
 * Workflow workflow = Workflow.builder()
 *     .name("TechDoc-Diagnosis")
 *     .description("技术文档问题诊断工作流")
 *     .steps(List.of(
 *         WorkflowStep.builder()
 *             .id("symptom_extraction")
 *             .name("症状提取")
 *             .agent("SymptomExtractor")
 *             .build(),
 *         WorkflowStep.builder()
 *             .id("solution_generation")
 *             .name("解决方案生成")
 *             .agent("SolutionGenerator")
 *             .dependencies(List.of("symptom_extraction"))
 *             .build()
 *     ))
 *     .build();
 * }</pre>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Data
@lombok.Builder
@lombok.NoArgsConstructor
@lombok.AllArgsConstructor
public class Workflow {

    /**
     * 工作流名称（唯一标识）
     */
    private String name;

    /**
     * 工作流描述
     */
    private String description;

    /**
     * 工作流步骤列表
     */
    private List<WorkflowStep> steps;

    /**
     * 工作流全局配置
     */
    private Map<String, Object> config;

    /**
     * 工作流版本
     */
    private String version;

    /**
     * 工作流标签（用于分类和搜索）
     */
    private List<String> tags;

    /**
     * 工作流分类（用于市场分类）
     */
    private String category;

    /**
     * 工作流元数据
     */
    private Map<String, Object> metadata;

    /**
     * 创建时间
     */
    private Long createdAt;

    /**
     * 更新时间
     */
    private Long updatedAt;

    /**
     * 作者
     */
    private String author;

    /**
     * 状态（draft, active, deprecated）
     */
    @lombok.Builder.Default
    private String status = "active";

    /**
     * 输入参数 Schema（JSON Schema格式）
     */
    private Map<String, Object> inputSchema;

    /**
     * 输出结果 Schema（JSON Schema格式）
     */
    private Map<String, Object> outputSchema;
}

