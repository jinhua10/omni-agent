package top.yumbo.ai.omni.workflow.agents;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.workflow.Agent;
import top.yumbo.ai.omni.workflow.WorkflowContext;

import java.util.Map;

/**
 * Echo Agent - 简单的回显 Agent（用于测试）
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@Component("EchoAgent")
public class EchoAgent implements Agent {

    @Override
    public Object execute(Object input, WorkflowContext context) throws Exception {
        log.info("📢 EchoAgent: input={}", input);

        if (input instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) input;
            return Map.of(
                "echo", input,
                "timestamp", System.currentTimeMillis(),
                "message", "Echo: " + map
            );
        }

        return Map.of(
            "echo", input,
            "timestamp", System.currentTimeMillis(),
            "message", "Echo: " + input
        );
    }

    @Override
    public String getName() {
        return "EchoAgent";
    }

    @Override
    public String getDescription() {
        return "简单的回显 Agent，用于测试工作流";
    }
}

