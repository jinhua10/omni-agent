package top.yumbo.ai.omni.marketplace.strategy.examples;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.marketplace.strategy.AbstractMarketplaceStrategy;
import top.yumbo.ai.omni.marketplace.strategy.StrategyExecutionException;
import top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.*;

import java.util.List;
import java.util.Map;

/**
 * 示例：简单的文本大写转换策略
 *
 * 展示如何实现一个市场策略
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public class SimpleUppercaseStrategy extends AbstractMarketplaceStrategy {

    @Override
    public String getStrategyName() {
        return "Simple Uppercase Converter";
    }

    @Override
    public StrategyCategory getCategory() {
        return StrategyCategory.CUSTOM;
    }

    @Override
    public String getDescription() {
        return "A simple strategy that converts text to uppercase";
    }

    @Override
    public AuthorInfo getAuthor() {
        return AuthorInfo.builder()
                .name("OmniAgent Team")
                .email("team@omnigen.ai")
                .organization("OmniAgent")
                .build();
    }

    @Override
    public String getParameterSchema() {
        return """
            {
              "type": "object",
              "properties": {
                "trimWhitespace": {
                  "type": "boolean",
                  "description": "是否去除首尾空格",
                  "default": false
                },
                "removeSpecialChars": {
                  "type": "boolean",
                  "description": "是否移除特殊字符",
                  "default": false
                }
              }
            }
            """;
    }

    @Override
    public Map<String, Object> getDefaultParameters() {
        return Map.of(
            "trimWhitespace", false,
            "removeSpecialChars", false
        );
    }

    @Override
    public List<String> getTags() {
        return List.of("text", "transform", "example", "simple");
    }

    @Override
    protected <I, O> O doExecute(I input, Map<String, Object> params, ExecutionContext context)
            throws StrategyExecutionException {

        // 验证输入
        if (!(input instanceof String)) {
            throw new StrategyExecutionException(
                    getStrategyId(),
                    StrategyExecutionException.ExecutionErrorCode.INVALID_INPUT,
                    "Input must be a String, got: " + input.getClass().getName()
            );
        }

        String text = (String) input;

        // 获取参数
        boolean trimWhitespace = (boolean) params.getOrDefault("trimWhitespace", false);
        boolean removeSpecialChars = (boolean) params.getOrDefault("removeSpecialChars", false);

        // 处理文本
        if (trimWhitespace) {
            text = text.trim();
        }

        if (removeSpecialChars) {
            text = text.replaceAll("[^a-zA-Z0-9\\s]", "");
        }

        String result = text.toUpperCase();

        log.debug("转换完成: {} -> {}", input, result);

        return (O) result;
    }

    @Override
    public List<UsageExample> getExamples() {
        return List.of(
            UsageExample.builder()
                .title("基本用法")
                .description("将文本转换为大写")
                .input("hello world")
                .parameters(Map.of())
                .expectedOutput("HELLO WORLD")
                .codeExample("""
                    String result = strategy.execute("hello world", Map.of());
                    """)
                .build(),

            UsageExample.builder()
                .title("去除空格")
                .description("转换为大写并去除首尾空格")
                .input("  hello world  ")
                .parameters(Map.of("trimWhitespace", true))
                .expectedOutput("HELLO WORLD")
                .codeExample("""
                    Map<String, Object> params = Map.of("trimWhitespace", true);
                    String result = strategy.execute("  hello world  ", params);
                    """)
                .build(),

            UsageExample.builder()
                .title("移除特殊字符")
                .description("转换为大写并移除特殊字符")
                .input("hello, world!")
                .parameters(Map.of("removeSpecialChars", true))
                .expectedOutput("HELLO WORLD")
                .codeExample("""
                    Map<String, Object> params = Map.of("removeSpecialChars", true);
                    String result = strategy.execute("hello, world!", params);
                    """)
                .build()
        );
    }
}

