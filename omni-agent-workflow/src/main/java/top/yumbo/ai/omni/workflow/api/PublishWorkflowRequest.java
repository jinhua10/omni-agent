package top.yumbo.ai.omni.workflow.api;

import lombok.Data;
import lombok.Builder;
import java.util.Map;

/**
 * 工作流发布请求
 */
@Data
@Builder
public class PublishWorkflowRequest {
    private String name;
    private String version;
    private String description;
    private String category;
    private String[] tags;
    private String license;
    private Map<String, Object> workflowDefinition;
}

