package top.yumbo.ai.omni.marketplace.strategy.adapters;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.strategy.PPLChunkingStrategy;
import top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.UsageExample;
import top.yumbo.ai.omni.marketplace.strategy.adapters.model.ChunkingInput;

import java.util.List;
import java.util.Map;

/**
 * PPL 智能分块策略 - 市场适配器
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Component
@ConditionalOnClass(name = "top.yumbo.ai.omni.ppl.onnx.PPLService")
public class PPLChunkingMarketAdapter extends ChunkingStrategyAdapter {

    public PPLChunkingMarketAdapter(PPLChunkingStrategy executor) {
        super(executor, ChunkingStrategy.PPL);
    }

    @Override
    public String getParameterSchema() {
        return """
            {
              "type": "object",
              "properties": {
                "pplThreshold": {
                  "type": "number",
                  "description": "困惑度阈值",
                  "default": 2.0,
                  "minimum": 0.5,
                  "maximum": 10.0
                },
                "minChunkSize": {
                  "type": "integer",
                  "description": "最小分块大小（字符数）",
                  "default": 200,
                  "minimum": 50,
                  "maximum": 1000
                },
                "maxChunkSize": {
                  "type": "integer",
                  "description": "最大分块大小（字符数）",
                  "default": 1500,
                  "minimum": 500,
                  "maximum": 5000
                }
              }
            }
            """;
    }

    @Override
    public List<UsageExample> getExamples() {
        return List.of(
            UsageExample.builder()
                .title("高精度代码分块")
                .description("使用 PPL 模型智能识别代码边界")
                .input(new ChunkingInput("code_java", "public class Main { ... }"))
                .parameters(Map.of("pplThreshold", 2.0))
                .expectedOutput("高精度分块列表")
                .build()
        );
    }

    @Override
    public List<String> getTags() {
        return List.of("chunking", "ppl", "ai-powered", "high-precision", "built-in");
    }
}

