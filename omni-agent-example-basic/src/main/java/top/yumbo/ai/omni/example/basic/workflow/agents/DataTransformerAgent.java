package top.yumbo.ai.omni.example.basic.workflow.agents;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.workflow.Agent;
import top.yumbo.ai.omni.workflow.WorkflowContext;

import java.util.Map;

/**
 * 数据转换 Agent 示例
 *
 * @author OmniAgent Team
 */
@Slf4j
@Component("DataTransformer")
public class DataTransformerAgent implements Agent {

    @Override
    public Object execute(Object input, WorkflowContext context) throws Exception {
        log.info("🔄 DataTransformer: 开始转换数据");

        if (input instanceof String) {
            String data = (String) input;
            String transformed = data.toUpperCase();

            log.info("  原始数据: {}", data);
            log.info("  转换后: {}", transformed);

            return Map.of(
                "original", data,
                "transformed", transformed,
                "length", data.length(),
                "timestamp", System.currentTimeMillis()
            );
        } else if (input instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> dataMap = (Map<String, Object>) input;

            log.info("  转换 Map 数据: {} 个字段", dataMap.size());

            return Map.of(
                "input", dataMap,
                "size", dataMap.size(),
                "keys", dataMap.keySet(),
                "timestamp", System.currentTimeMillis()
            );
        }

        return Map.of(
            "input", input,
            "type", input.getClass().getSimpleName(),
            "timestamp", System.currentTimeMillis()
        );
    }

    @Override
    public String getName() {
        return "DataTransformer";
    }

    @Override
    public String getDescription() {
        return "数据转换 Agent - 将输入数据转换为大写，并提取元数据";
    }

    @Override
    public String getInputType() {
        return "String or Map";
    }

    @Override
    public String getOutputType() {
        return "Map<String, Object>";
    }
}

