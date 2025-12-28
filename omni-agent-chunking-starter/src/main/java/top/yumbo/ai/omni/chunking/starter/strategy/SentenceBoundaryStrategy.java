package top.yumbo.ai.omni.chunking.starter.strategy;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 句子边界分块策略
 *
 * <p>按句子边界分块，避免破坏句子完整性</p>
 * <p>从 core/old/chunking 迁移而来，适配新架构</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class SentenceBoundaryStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;

    private static final int DEFAULT_TARGET_SIZE = 500;
    private static final Pattern SENTENCE_PATTERN = Pattern.compile("[。！？.!?]+[\\s\"'）】]*");

    public SentenceBoundaryStrategy(ChunkingProperties properties) {
        this.properties = properties;
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        if (content == null || content.isEmpty()) {
            return new ArrayList<>();
        }

        int targetSize = config.getMaxChunkSize() != null ?
                config.getMaxChunkSize() : DEFAULT_TARGET_SIZE;

        // 按句子分割
        List<String> sentences = splitIntoSentences(content);

        List<Chunk> chunks = new ArrayList<>();
        StringBuilder currentChunk = new StringBuilder();
        int index = 0;
        int startPosition = 0;

        for (String sentence : sentences) {
            // 如果当前分块加上新句子会超过目标大小，先保存当前分块
            if (currentChunk.length() > 0 &&
                currentChunk.length() + sentence.length() > targetSize) {

                String chunkContent = currentChunk.toString();
                chunks.add(Chunk.builder()
                        .chunkId(UUID.randomUUID().toString())
                        .documentId(documentId)
                        .content(chunkContent)
                        .index(index++)
                        .startPosition(startPosition)
                        .endPosition(startPosition + chunkContent.length())
                        .length(chunkContent.length())
                        .strategy(ChunkingStrategy.SENTENCE)
                        .build());

                // 重置
                startPosition += chunkContent.length();
                currentChunk = new StringBuilder();
            }

            currentChunk.append(sentence);
        }

        // 处理最后一个分块
        if (currentChunk.length() > 0) {
            String chunkContent = currentChunk.toString();
            chunks.add(Chunk.builder()
                    .chunkId(UUID.randomUUID().toString())
                    .documentId(documentId)
                    .content(chunkContent)
                    .index(index)
                    .startPosition(startPosition)
                    .endPosition(startPosition + chunkContent.length())
                    .length(chunkContent.length())
                    .strategy(ChunkingStrategy.SENTENCE)
                    .build());
        }

        log.debug("📋 句子边界分块完成: {} chunks", chunks.size());
        return chunks;
    }

    /**
     * 按句子分割文本
     */
    private List<String> splitIntoSentences(String content) {
        List<String> sentences = new ArrayList<>();
        Matcher matcher = SENTENCE_PATTERN.matcher(content);

        int lastEnd = 0;
        while (matcher.find()) {
            sentences.add(content.substring(lastEnd, matcher.end()));
            lastEnd = matcher.end();
        }

        // 添加最后一部分
        if (lastEnd < content.length()) {
            sentences.add(content.substring(lastEnd));
        }

        return sentences;
    }
}

