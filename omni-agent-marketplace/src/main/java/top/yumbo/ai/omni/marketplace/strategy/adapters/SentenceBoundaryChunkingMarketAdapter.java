package top.yumbo.ai.omni.marketplace.strategy.adapters;

import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.strategy.SentenceBoundaryStrategy;
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

    public SentenceBoundaryChunkingMarketAdapter(SentenceBoundaryStrategy executor) {
        super(executor, ChunkingStrategy.SENTENCE);
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
                  "maximum": 2000
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
                .description("按句子边界分块，避免破坏句子完整性")
                .input(new ChunkingInput("faq", "问：如何安装？答：...问：如何配置？答：..."))
                .parameters(Map.of("targetSize", 500))
                .expectedOutput("句子完整的分块列表")
                .build()
        );
    }
}

