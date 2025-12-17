package top.yumbo.ai.omni.core.chunking.strategy;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.storage.api.model.Chunk;

import java.util.*;

/**
 * 段落分块策略
 *
 * 按段落分块，保持段落完整性
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
public class ParagraphChunkingStrategy implements ChunkingStrategy {

    private static final int DEFAULT_MAX_PARAGRAPHS_PER_CHUNK = 3;

    @Override
    public List<Chunk> chunk(String documentId, String content, Map<String, Object> params) {
        List<Chunk> chunks = new ArrayList<>();

        if (content == null || content.isEmpty()) {
            return chunks;
        }

        int maxParagraphs = getParam(params, "maxParagraphsPerChunk", DEFAULT_MAX_PARAGRAPHS_PER_CHUNK);

        // 按段落分割（双换行符）
        String[] paragraphs = content.split("\\n\\s*\\n");

        int sequence = 0;
        int startPosition = 0;
        StringBuilder currentChunk = new StringBuilder();
        int paragraphCount = 0;

        for (String paragraph : paragraphs) {
            String trimmed = paragraph.trim();
            if (trimmed.isEmpty()) {
                continue;
            }

            // 达到最大段落数，保存当前分块
            if (paragraphCount >= maxParagraphs) {
                String chunkContent = currentChunk.toString().trim();
                Chunk chunk = Chunk.builder()
                        .documentId(documentId)
                        .content(chunkContent)
                        .sequence(sequence)
                        .startPosition(startPosition)
                        .endPosition(startPosition + chunkContent.length())
                        .metadata(Map.of(
                                "strategy", getStrategyName(),
                                "paragraphCount", paragraphCount
                        ))
                        .createdAt(System.currentTimeMillis())
                        .build();

                chunks.add(chunk);

                startPosition += chunkContent.length();
                currentChunk = new StringBuilder();
                paragraphCount = 0;
                sequence++;
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
            Chunk chunk = Chunk.builder()
                    .documentId(documentId)
                    .content(chunkContent)
                    .sequence(sequence)
                    .startPosition(startPosition)
                    .endPosition(startPosition + chunkContent.length())
                    .metadata(Map.of(
                            "strategy", getStrategyName(),
                            "paragraphCount", paragraphCount
                    ))
                    .createdAt(System.currentTimeMillis())
                    .build();

            chunks.add(chunk);
        }

        log.debug("Paragraph chunking: {} chunks created", chunks.size());
        return chunks;
    }

    @Override
    public String getStrategyName() {
        return "paragraph";
    }

    @Override
    public String getDescription() {
        return "段落分块策略 - 按段落分块，保持段落完整性";
    }

    @Override
    public Map<String, Object> getDefaultParams() {
        Map<String, Object> params = new HashMap<>();
        params.put("maxParagraphsPerChunk", DEFAULT_MAX_PARAGRAPHS_PER_CHUNK);
        return params;
    }

    @SuppressWarnings("unchecked")
    private <T> T getParam(Map<String, Object> params, String key, T defaultValue) {
        if (params == null || !params.containsKey(key)) {
            return defaultValue;
        }
        return (T) params.get(key);
    }
}

