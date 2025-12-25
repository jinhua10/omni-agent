package top.yumbo.ai.omni.core.chunking.strategy;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.storage.api.model.Chunk;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 句子边界分块策略
 *
 * 按句子边界分块，避免破坏句子完整性
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
public class SentenceBoundaryChunkingStrategy implements ChunkingStrategy {

    private static final int DEFAULT_TARGET_SIZE = 500;
    private static final Pattern SENTENCE_PATTERN = Pattern.compile("[。！？.!?]+[\\s\"'）】]*");

    @Override
    public List<Chunk> chunk(String documentId, String content, Map<String, Object> params) {
        List<Chunk> chunks = new ArrayList<>();

        if (content == null || content.isEmpty()) {
            return chunks;
        }

        int targetSize = ChunkingParamUtils.getParam(params, "targetSize", DEFAULT_TARGET_SIZE);

        // 按句子分割
        List<String> sentences = splitIntoSentences(content);

        StringBuilder currentChunk = new StringBuilder();
        int sequence = 0;
        int startPosition = 0;

        for (String sentence : sentences) {
            // 如果当前分块加上新句子会超过目标大小，先保存当前分块
            if (currentChunk.length() > 0 &&
                currentChunk.length() + sentence.length() > targetSize) {

                String chunkContent = currentChunk.toString();
                Chunk chunk = Chunk.builder()
                        .documentId(documentId)
                        .content(chunkContent)
                        .sequence(sequence)
                        .startPosition(startPosition)
                        .endPosition(startPosition + chunkContent.length())
                        .metadata(Map.of(
                                "strategy", getStrategyName(),
                                "targetSize", targetSize,
                                "sentences", countSentences(chunkContent)
                        ))
                        .createdAt(System.currentTimeMillis())
                        .build();

                chunks.add(chunk);

                // 重置
                startPosition += chunkContent.length();
                currentChunk = new StringBuilder();
                sequence++;
            }

            currentChunk.append(sentence);
        }

        // 处理最后一个分块
        if (currentChunk.length() > 0) {
            String chunkContent = currentChunk.toString();
            Chunk chunk = Chunk.builder()
                    .documentId(documentId)
                    .content(chunkContent)
                    .sequence(sequence)
                    .startPosition(startPosition)
                    .endPosition(startPosition + chunkContent.length())
                    .metadata(Map.of(
                            "strategy", getStrategyName(),
                            "targetSize", targetSize,
                            "sentences", countSentences(chunkContent)
                    ))
                    .createdAt(System.currentTimeMillis())
                    .build();

            chunks.add(chunk);
        }

        log.debug("Sentence boundary chunking: {} chunks created", chunks.size());
        return chunks;
    }

    @Override
    public String getStrategyName() {
        return "sentence_boundary";
    }

    @Override
    public String getDescription() {
        return "句子边界分块策略 - 按句子边界分块，保持句子完整性";
    }

    @Override
    public Map<String, Object> getDefaultParams() {
        Map<String, Object> params = new HashMap<>();
        params.put("targetSize", DEFAULT_TARGET_SIZE);
        return params;
    }

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

    private int countSentences(String content) {
        Matcher matcher = SENTENCE_PATTERN.matcher(content);
        int count = 0;
        while (matcher.find()) {
            count++;
        }
        return Math.max(1, count);
    }
}

