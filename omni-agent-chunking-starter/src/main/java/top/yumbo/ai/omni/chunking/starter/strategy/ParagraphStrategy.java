package top.yumbo.ai.omni.chunking.starter.strategy;

import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * 段落分块策略
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class ParagraphStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;

    public ParagraphStrategy(ChunkingProperties properties) {
        this.properties = properties;
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        List<Chunk> chunks = new ArrayList<>();

        // 按段落分割（空行分割）
        String[] paragraphs = content.split("\\n\\s*\\n");

        int index = 0;
        int position = 0;

        for (String paragraph : paragraphs) {
            if (paragraph.trim().isEmpty()) {
                continue;
            }

            int startPos = position;
            int endPos = startPos + paragraph.length();

            Chunk chunk = Chunk.builder()
                    .chunkId(UUID.randomUUID().toString())
                    .documentId(documentId)
                    .content(paragraph.trim())
                    .index(index++)
                    .startPosition(startPos)
                    .endPosition(endPos)
                    .length(paragraph.trim().length())
                    .strategy(ChunkingStrategy.PARAGRAPH)
                    .build();

            chunks.add(chunk);

            // 更新位置（考虑分隔符）
            position = content.indexOf(paragraph, position) + paragraph.length();
        }

        return chunks;
    }
}

