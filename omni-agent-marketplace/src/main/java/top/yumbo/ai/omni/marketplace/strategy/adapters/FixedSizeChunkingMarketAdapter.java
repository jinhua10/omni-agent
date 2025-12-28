package top.yumbo.ai.omni.marketplace.strategy.adapters;

import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.strategy.FixedLengthStrategy;
import top.yumbo.ai.omni.marketplace.strategy.adapters.model.ChunkingInput;
import top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.UsageExample;

import java.util.List;
import java.util.Map;

/**
 * 固定大小分块策略 - 市场适配器
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Component
public class FixedSizeChunkingMarketAdapter extends ChunkingStrategyAdapter {

    public FixedSizeChunkingMarketAdapter(FixedLengthStrategy executor) {
        super(executor, ChunkingStrategy.FIXED_LENGTH);
    }

    @Override
    public String getParameterSchema() {
        return """
            {
              "type": "object",
              "properties": {
                "chunkSize": {
                  "type": "integer",
                  "description": "分块大小（字符数）",
                  "default": 500,
                  "minimum": 100,
                  "maximum": 5000
                },
                "overlapSize": {
                  "type": "integer",
                  "description": "重叠大小（字符数）",
                  "default": 50,
                  "minimum": 0,
                  "maximum": 500
                }
              }
            }
            """;
    }

    @Override
    public List<UsageExample> getExamples() {
        return List.of(
            UsageExample.builder()
                .title("基本用法")
                .description("使用默认参数进行固定大小分块")
                .input(new ChunkingInput("doc_1", "这是一段很长的文本内容..."))
                .parameters(null)
                .expectedOutput("分块列表")
                .codeExample("""
                    ChunkingInput input = new ChunkingInput("doc_1", content);
                    ExecutionResult<ChunkingOutput> result = strategy.execute(input, null, context);
                    List<Chunk> chunks = result.getData().getChunks();
                    """)
                .build(),

            UsageExample.builder()
                .title("自定义参数")
                .description("自定义分块大小和重叠大小")
                .input(new ChunkingInput("doc_2", "文本内容..."))
                .parameters(Map.of("chunkSize", 1000, "overlapSize", 100))
                .expectedOutput("分块列表")
                .codeExample("""
                    Map<String, Object> params = Map.of(
                        "chunkSize", 1000,
                        "overlapSize", 100
                    );
                    ExecutionResult<ChunkingOutput> result = strategy.execute(input, params, context);
                    """)
                .build()
        );
    }
}

