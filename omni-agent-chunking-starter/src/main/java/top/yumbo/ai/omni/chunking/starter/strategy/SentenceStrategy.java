package top.yumbo.ai.omni.chunking.starter.strategy;

import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 句子分块策略
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class SentenceStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;
    private static final Pattern SENTENCE_PATTERN = Pattern.compile("[^.!?。！？]+[.!?。！？]+");

    public SentenceStrategy(ChunkingProperties properties) {
        this.properties = properties;
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        List<Chunk> chunks = new ArrayList<>();

        Matcher matcher = SENTENCE_PATTERN.matcher(content);
        int index = 0;

        while (matcher.find()) {
            String sentence = matcher.group().trim();
            if (sentence.isEmpty()) {
                continue;
            }

            Chunk chunk = Chunk.builder()
                    .chunkId(UUID.randomUUID().toString())
                    .documentId(documentId)
                    .content(sentence)
                    .index(index++)
                    .startPosition(matcher.start())
                    .endPosition(matcher.end())
                    .length(sentence.length())
                    .strategy(ChunkingStrategy.SENTENCE)
                    .build();

            chunks.add(chunk);
        }

        return chunks;
    }
}

