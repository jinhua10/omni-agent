package top.yumbo.ai.omni.example.basic.workflow.agents;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.workflow.Agent;
import top.yumbo.ai.omni.workflow.WorkflowContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * 数据过滤 Agent 示例
 *
 * @author OmniAgent Team
 */
@Slf4j
@Component("DataFilter")
public class DataFilterAgent implements Agent {

    @Override
    public Object execute(Object input, WorkflowContext context) throws Exception {
        log.info("🔍 DataFilter: 开始过滤数据");

        if (input instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> dataMap = (Map<String, Object>) input;

            // 过滤掉 null 值和空字符串
            List<String> filtered = new ArrayList<>();
            List<String> kept = new ArrayList<>();

            dataMap.forEach((key, value) -> {
                if (value == null || (value instanceof String && ((String) value).isEmpty())) {
                    filtered.add(key);
                } else {
                    kept.add(key);
                }
            });

            log.info("  总字段: {}, 保留: {}, 过滤: {}",
                     dataMap.size(), kept.size(), filtered.size());

            return Map.of(
                "totalFields", dataMap.size(),
                "keptFields", kept,
                "filteredFields", filtered,
                "keptCount", kept.size(),
                "filteredCount", filtered.size()
            );
        }

        return Map.of(
            "input", input,
            "message", "输入类型不支持过滤"
        );
    }

    @Override
    public String getName() {
        return "DataFilter";
    }

    @Override
    public String getDescription() {
        return "数据过滤 Agent - 过滤掉 null 值和空字符串";
    }

    @Override
    public String getInputType() {
        return "Map<String, Object>";
    }

    @Override
    public String getOutputType() {
        return "Map<String, Object>";
    }
}


