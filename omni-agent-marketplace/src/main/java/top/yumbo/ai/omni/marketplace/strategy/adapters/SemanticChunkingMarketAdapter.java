package top.yumbo.ai.omni.marketplace.strategy.adapters;

import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.strategy.SemanticStrategy;
import top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.UsageExample;
import top.yumbo.ai.omni.marketplace.strategy.adapters.model.ChunkingInput;

import java.util.List;
import java.util.Map;

/**
 * 语义分块策略 - 市场适配器
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Component
public class SemanticChunkingMarketAdapter extends ChunkingStrategyAdapter {

    public SemanticChunkingMarketAdapter(SemanticStrategy executor) {
        super(executor, ChunkingStrategy.SEMANTIC);
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
                  "default": 300,
                  "minimum": 100,
                  "maximum": 2000
                },
                "maxChunkSize": {
                  "type": "integer",
                  "description": "最大分块大小（字符数）",
                  "default": 1000,
                  "minimum": 500,
                  "maximum": 5000
                },
                "similarityThreshold": {
                  "type": "number",
                  "description": "语义相似度阈值（0-1）",
                  "default": 0.5,
                  "minimum": 0,
                  "maximum": 1
                }
              }
            }
            """;
    }

    @Override
    public List<UsageExample> getExamples() {
        return List.of(
            UsageExample.builder()
                .title("技术文档分块")
                .description("基于语义相似度智能分块，保持主题连贯")
                .input(new ChunkingInput("tech_doc", "## 安装\n\n步骤1...\n\n## 配置\n\n..."))
                .parameters(Map.of("similarityThreshold", 0.5))
                .expectedOutput("语义连贯的分块列表")
                .build()
        );
    }

    @Override
    public List<String> getTags() {
        return List.of("chunking", "semantic", "ai-powered", "smart", "built-in");
    }
}

