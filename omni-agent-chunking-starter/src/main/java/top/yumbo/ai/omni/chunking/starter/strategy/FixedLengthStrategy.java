package top.yumbo.ai.omni.chunking.starter.strategy;

import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;
import top.yumbo.ai.omni.chunking.starter.util.ChunkingParamUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * 固定长度分块策略
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class FixedLengthStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;

    public FixedLengthStrategy(ChunkingProperties properties) {
        this.properties = properties;
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        List<Chunk> chunks = new ArrayList<>();

        // 使用 ChunkingParamUtils 获取参数
        int chunkSize = ChunkingParamUtils.getFixedLengthSize(config,
                properties.getFixedLength().getSize());
        int overlap = ChunkingParamUtils.getOverlap(config,
                properties.getFixedLength().getOverlap());

        int index = 0;
        int position = 0;

        while (position < content.length()) {
            int endPosition = Math.min(position + chunkSize, content.length());
            String chunkContent = content.substring(position, endPosition);

            Chunk chunk = Chunk.builder()
                    .id(UUID.randomUUID().toString())
                    .documentId(documentId)
                    .content(chunkContent)
                    .sequence(index++)
                    .startPosition(position)
                    .endPosition(endPosition)
                    .strategy(ChunkingStrategy.FIXED_LENGTH)
                    .createdAt(System.currentTimeMillis())
                    .build();

            chunks.add(chunk);

            // 移动位置，考虑重叠
            position += (chunkSize - overlap);

            // 避免无限循环
            if (position <= chunk.getStartPosition()) {
                position = chunk.getEndPosition();
            }
        }

        return chunks;
    }
}



