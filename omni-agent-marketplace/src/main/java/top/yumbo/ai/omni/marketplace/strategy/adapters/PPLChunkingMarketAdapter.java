package top.yumbo.ai.omni.marketplace.strategy.adapters;

import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.core.chunking.strategy.PPLChunkingStrategy;
import top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.UsageExample;

import java.util.List;
import java.util.Map;

/**
 * PPL 困惑度分块策略 - 市场适配器
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Component
public class PPLChunkingMarketAdapter extends ChunkingStrategyAdapter {

    public PPLChunkingMarketAdapter(PPLChunkingStrategy delegate) {
        super(delegate);
    }

    @Override
    public String getParameterSchema() {
        return """
            {
              "type": "object",
              "properties": {
                "minChunkSize": {
                  "type": "integer",
                  "description": "最小分块大小（字符数）",
                  "default": 200,
                  "minimum": 100,
                  "maximum": 2000
                },
                "maxChunkSize": {
                  "type": "integer",
                  "description": "最大分块大小（字符数）",
                  "default": 800,
                  "minimum": 500,
                  "maximum": 5000
                },
                "threshold": {
                  "type": "number",
                  "description": "困惑度阈值（0-1）",
                  "default": 0.3,
                  "minimum": 0,
                  "maximum": 1
                }
              }
            }
            """;
    }

    @Override
    public String getDescription() {
        return "基于困惑度的智能分块策略，在主题转换点切分。" +
               "支持简化版（词汇重叠度）和ONNX版（真实语言模型）两种模式。";
    }

    @Override
    public List<UsageExample> getExamples() {
        return List.of(
            UsageExample.builder()
                .title("API文档分块")
                .description("检测接口边界，在主题转换处切分")
                .input(new ChunkingInput("api_doc", "POST /api/users\n\n创建用户...\n\nGET /api/users\n\n获取用户..."))
                .parameters(Map.of("threshold", 0.3))
                .expectedOutput("在接口边界处切分的分块列表")
                .build(),

            UsageExample.builder()
                .title("长篇文章分块")
                .description("检测主题转换，保持主题完整性")
                .input(new ChunkingInput("article", "第一章...\n\n第二章..."))
                .parameters(Map.of("minChunkSize", 200, "maxChunkSize", 800))
                .expectedOutput("主题完整的分块列表")
                .build()
        );
    }

    @Override
    public List<String> getTags() {
        return List.of("chunking", "ppl", "perplexity", "ai-powered", "smart", "onnx-optional", "built-in");
    }
}

