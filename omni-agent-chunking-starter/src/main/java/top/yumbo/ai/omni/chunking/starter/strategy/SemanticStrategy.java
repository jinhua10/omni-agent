package top.yumbo.ai.omni.chunking.starter.strategy;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * 语义分块策略
 *
 * <p>基于语义相似度进行分块（当前使用段落边界的简化实现）</p>
 * <p>未来可扩展为基于向量相似度的实现</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class SemanticStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;

    public SemanticStrategy(ChunkingProperties properties) {
        this.properties = properties;
        log.info("✅ 语义分块策略已初始化（使用段落边界简化版）");
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        if (content == null || content.isEmpty()) {
            return new ArrayList<>();
        }

        List<Chunk> chunks = new ArrayList<>();

        // 按段落分割（双换行符）
        String[] paragraphs = content.split("\\n\\s*\\n");

        int index = 0;
        int position = 0;
        StringBuilder currentChunk = new StringBuilder();
        int chunkStart = 0;

        int maxSize = config.getMaxChunkSize() != null ?
                config.getMaxChunkSize() : properties.getGeneral().getMaxChunkSize();
        int minSize = config.getMinChunkSize() != null ?
                config.getMinChunkSize() : properties.getGeneral().getMinChunkSize();

        for (String paragraph : paragraphs) {
            String trimmed = paragraph.trim();
            if (trimmed.isEmpty()) {
                continue;
            }

            // 如果当前块 + 新段落超过最大长度，则创建新块
            if (currentChunk.length() > 0 &&
                currentChunk.length() + trimmed.length() > maxSize) {

                // 保存当前块
                if (currentChunk.length() >= minSize) {
                    chunks.add(createChunk(documentId, currentChunk.toString(),
                            index++, chunkStart, position));
                }

                // 开始新块
                currentChunk = new StringBuilder(trimmed);
                chunkStart = position;
            } else {
                // 添加到当前块
                if (currentChunk.length() > 0) {
                    currentChunk.append("\n\n");
                }
                currentChunk.append(trimmed);
            }

            position += paragraph.length();
        }

        // 保存最后一块
        if (currentChunk.length() > 0) {
            chunks.add(createChunk(documentId, currentChunk.toString(),
                    index, chunkStart, position));
        }

        return chunks;
    }

    private Chunk createChunk(String documentId, String content, int index,
                            int start, int end) {
        return Chunk.builder()
                .chunkId(UUID.randomUUID().toString())
                .documentId(documentId)
                .content(content)
                .index(index)
                .startPosition(start)
                .endPosition(end)
                .length(content.length())
                .strategy(ChunkingStrategy.SEMANTIC)
                .build();
    }
}


