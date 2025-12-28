package top.yumbo.ai.omni.marketplace.strategy.adapters;

import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.UsageExample;
import top.yumbo.ai.omni.marketplace.strategy.adapters.model.ChunkingInput;

import java.util.List;
import java.util.Map;

/**
 * 段落分块策略 - 市场适配器
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Component
public class ParagraphChunkingMarketAdapter extends ChunkingStrategyAdapter {

    public ParagraphChunkingMarketAdapter(ParagraphChunkingStrategy delegate) {
        super(delegate);
    }

    @Override
    public String getParameterSchema() {
        return """
            {
              "type": "object",
              "properties": {
                "maxParagraphsPerChunk": {
                  "type": "integer",
                  "description": "每个分块最多包含的段落数",
                  "default": 3,
                  "minimum": 1,
                  "maximum": 10
                }
              }
            }
            """;
    }

    @Override
    public List<UsageExample> getExamples() {
        return List.of(
            UsageExample.builder()
                .title("Markdown文档分块")
                .description("按段落分块，保持段落完整性")
                .input(new ChunkingInput("readme_md", "# 标题\n\n段落1...\n\n段落2..."))
                .parameters(Map.of("maxParagraphsPerChunk", 3))
                .expectedOutput("段落完整的分块列表")
                .build()
        );
    }
}

