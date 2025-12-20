package top.yumbo.ai.omni.example.basic.workflow.agents;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.workflow.Agent;
import top.yumbo.ai.omni.workflow.WorkflowContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * 数据验证 Agent 示例
 *
 * @author OmniAgent Team
 */
@Slf4j
@Component("DataValidator")
public class DataValidatorAgent implements Agent {

    @Override
    public Object execute(Object input, WorkflowContext context) throws Exception {
        log.info("✅ DataValidator: 开始验证数据");

        List<String> errors = new ArrayList<>();
        boolean isValid = true;

        if (input instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> dataMap = (Map<String, Object>) input;

            // 验证必填字段
            if (!dataMap.containsKey("name") || dataMap.get("name") == null) {
                errors.add("缺少必填字段: name");
                isValid = false;
            }

            // 验证数据类型
            if (dataMap.containsKey("age")) {
                Object age = dataMap.get("age");
                if (!(age instanceof Number)) {
                    errors.add("字段 age 必须是数字");
                    isValid = false;
                } else {
                    int ageValue = ((Number) age).intValue();
                    if (ageValue < 0 || ageValue > 150) {
                        errors.add("字段 age 必须在 0-150 之间");
                        isValid = false;
                    }
                }
            }

            // 验证邮箱格式
            if (dataMap.containsKey("email")) {
                String email = (String) dataMap.get("email");
                if (email != null && !email.matches("^[A-Za-z0-9+_.-]+@(.+)$")) {
                    errors.add("邮箱格式不正确");
                    isValid = false;
                }
            }

            log.info("  验证结果: {}, 错误数: {}", isValid ? "通过" : "失败", errors.size());

            return Map.of(
                "isValid", isValid,
                "errors", errors,
                "errorCount", errors.size(),
                "checkedFields", dataMap.keySet()
            );
        }

        errors.add("输入必须是 Map 类型");
        return Map.of(
            "isValid", false,
            "errors", errors
        );
    }

    @Override
    public String getName() {
        return "DataValidator";
    }

    @Override
    public String getDescription() {
        return "数据验证 Agent - 验证数据格式和必填字段";
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

