package top.yumbo.ai.omni.core.chunking.strategy;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.storage.api.model.Chunk;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 固定大小分块策略
 *
 * 按固定字符数分块，支持重叠
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
public class FixedSizeChunkingStrategy implements ChunkingStrategy {

    private static final int DEFAULT_CHUNK_SIZE = 500;
    private static final int DEFAULT_OVERLAP_SIZE = 50;

    @Override
    public List<Chunk> chunk(String documentId, String content, Map<String, Object> params) {
        List<Chunk> chunks = new ArrayList<>();

        if (content == null || content.isEmpty()) {
            return chunks;
        }

        // 从参数获取配置
        int chunkSize = getParam(params, "chunkSize", DEFAULT_CHUNK_SIZE);
        int overlapSize = getParam(params, "overlapSize", DEFAULT_OVERLAP_SIZE);

        int contentLength = content.length();
        int position = 0;
        int sequence = 0;

        while (position < contentLength) {
            // 计算当前分块的结束位置
            int endPosition = Math.min(position + chunkSize, contentLength);

            // 提取分块内容
            String chunkContent = content.substring(position, endPosition);

            // 创建分块对象
            Chunk chunk = Chunk.builder()
                    .documentId(documentId)
                    .content(chunkContent)
                    .sequence(sequence)
                    .startPosition(position)
                    .endPosition(endPosition)
                    .metadata(Map.of(
                            "strategy", getStrategyName(),
                            "chunkSize", chunkSize,
                            "overlapSize", overlapSize
                    ))
                    .createdAt(System.currentTimeMillis())
                    .build();

            chunks.add(chunk);

            // 移动到下一个分块（考虑重叠）
            int nextPosition = endPosition - overlapSize;

            // 确保position能够前进
            if (nextPosition <= position || nextPosition >= contentLength) {
                break;
            }

            position = nextPosition;
            sequence++;
        }

        log.debug("Fixed-size chunking: {} chunks created", chunks.size());
        return chunks;
    }

    @Override
    public String getStrategyName() {
        return "fixed_size";
    }

    @Override
    public String getDescription() {
        return "固定大小分块策略 - 按固定字符数分块，支持重叠";
    }

    @Override
    public Map<String, Object> getDefaultParams() {
        Map<String, Object> params = new HashMap<>();
        params.put("chunkSize", DEFAULT_CHUNK_SIZE);
        params.put("overlapSize", DEFAULT_OVERLAP_SIZE);
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

