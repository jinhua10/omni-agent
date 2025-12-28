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
 * 段落分块策略
 *
 * <p>按段落分块，可配置每个分块包含的最大段落数</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class ParagraphStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;
    private static final int DEFAULT_MAX_PARAGRAPHS_PER_CHUNK = 3;

    public ParagraphStrategy(ChunkingProperties properties) {
        this.properties = properties;
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        List<Chunk> chunks = new ArrayList<>();

        if (content == null || content.isEmpty()) {
            return chunks;
        }

        // 使用 ChunkingParamUtils 获取最大段落数（从 maxChunkSize 推算）
        int maxParagraphs = DEFAULT_MAX_PARAGRAPHS_PER_CHUNK;
        if (config.getMaxChunkSize() != null) {
            // 估算：假设每段落平均 300 字符
            maxParagraphs = Math.max(1, config.getMaxChunkSize() / 300);
        }

        // 按段落分割（双换行符）
        String[] paragraphs = content.split("\\n\\s*\\n");

        int index = 0;
        int startPosition = 0;
        StringBuilder currentChunk = new StringBuilder();
        int paragraphCount = 0;

        for (String paragraph : paragraphs) {
            String trimmed = paragraph.trim();
            if (trimmed.isEmpty()) {
                continue;
            }

            // 达到最大段落数，保存当前分块
            if (paragraphCount >= maxParagraphs && currentChunk.length() > 0) {
                String chunkContent = currentChunk.toString().trim();
                chunks.add(Chunk.builder()
                        .id(UUID.randomUUID().toString())
                        .documentId(documentId)
                        .content(chunkContent)
                        .sequence(index++)
                        .startPosition(startPosition)
                        .endPosition(startPosition + chunkContent.length())
                        .strategy(ChunkingStrategy.PARAGRAPH)
                        .createdAt(System.currentTimeMillis())
                        .build());

                startPosition += chunkContent.length();
                currentChunk = new StringBuilder();
                paragraphCount = 0;
            }

            if (currentChunk.length() > 0) {
                currentChunk.append("\n\n");
            }
            currentChunk.append(trimmed);
            paragraphCount++;
        }

        // 处理最后一个分块
        if (currentChunk.length() > 0) {
            String chunkContent = currentChunk.toString().trim();
            chunks.add(Chunk.builder()
                    .id(UUID.randomUUID().toString())
                    .documentId(documentId)
                    .content(chunkContent)
                    .sequence(index)
                    .startPosition(startPosition)
                    .endPosition(startPosition + chunkContent.length())
                    .strategy(ChunkingStrategy.PARAGRAPH)
                    .createdAt(System.currentTimeMillis())
                    .build());
        }

        return chunks;
    }
}

