package top.yumbo.ai.omni.marketplace.strategy.adapters;

import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.core.chunking.strategy.SentenceBoundaryChunkingStrategy;
import top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.UsageExample;
import top.yumbo.ai.omni.marketplace.strategy.adapters.model.ChunkingInput;

import java.util.List;
import java.util.Map;

/**
 * 句子边界分块策略 - 市场适配器
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Component
public class SentenceBoundaryChunkingMarketAdapter extends ChunkingStrategyAdapter {

    public SentenceBoundaryChunkingMarketAdapter(SentenceBoundaryChunkingStrategy delegate) {
        super(delegate);
    }

    @Override
    public String getParameterSchema() {
        return """
            {
              "type": "object",
              "properties": {
                "targetSize": {
                  "type": "integer",
                  "description": "目标分块大小（字符数）",
                  "default": 500,
                  "minimum": 100,
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
                .title("FAQ文档分块")
                .description("按句子边界分块，保持问答完整性")
                .input(new ChunkingInput("faq_1", "Q: 如何使用？A: 很简单..."))
                .parameters(Map.of("targetSize", 500))
                .expectedOutput("句子完整的分块列表")
                .build()
        );
    }
}

