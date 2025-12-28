package top.yumbo.ai.omni.chunking.starter.strategy;
}
    }
        return chunks;

        }
            }
                position = chunk.getEndPosition();
            if (position <= chunk.getStartPosition()) {
            // 避免无限循环

            position += (chunkSize - overlap);
            // 移动位置，考虑重叠

            chunks.add(chunk);

                    .build();
                    .strategy(ChunkingStrategy.FIXED_LENGTH)
                    .length(chunkContent.length())
                    .endPosition(endPosition)
                    .startPosition(position)
                    .index(index++)
                    .content(chunkContent)
                    .documentId(documentId)
                    .chunkId(UUID.randomUUID().toString())
            Chunk chunk = Chunk.builder()

            String chunkContent = content.substring(position, endPosition);
            int endPosition = Math.min(position + chunkSize, content.length());
        while (position < content.length()) {

        int position = 0;
        int index = 0;

                config.getOverlap() : properties.getFixedLength().getOverlap();
        int overlap = config.getOverlap() != null ?
                config.getFixedLengthSize() : properties.getFixedLength().getSize();
        int chunkSize = config.getFixedLengthSize() != null ?

        List<Chunk> chunks = new ArrayList<>();
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
    @Override

    }
        this.properties = properties;
    public FixedLengthStrategy(ChunkingProperties properties) {

    private final ChunkingProperties properties;

public class FixedLengthStrategy implements ChunkingStrategyExecutor {
 */
 * @since 1.0.0
 * @author OmniAgent Team
 *
 * 固定长度分块策略
/**

import java.util.UUID;
import java.util.List;
import java.util.ArrayList;

import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.Chunk;


