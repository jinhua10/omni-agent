package top.yumbo.ai.omni.core.chunking.strategy;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.storage.api.model.Chunk;

import java.util.*;

/**
 * 语义分块策略
 * <p>
 * 基于语义相似度的智能分块，保持语义连贯性
 * <p>
 * 原理：
 * 1. 将文档分割成段落
 * 2. 计算相邻段落的语义相似度
 * 3. 相似度低于阈值时切分（主题转换）
 * 4. 保证每个分块内语义连贯
 * <p>
 * 简化实现（不依赖向量模型）：
 * - 使用 TF-IDF + 余弦相似度
 * - 计算段落间的词汇分布相似度
 * - 相似度低 = 主题转换 = 分块边界
 * <p>
 * 适用场景：
 * - 技术文档（保持代码示例完整）
 * - 长篇文章（保持主题连贯）
 * - 多主题文档（在主题转换处切分）
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
public class SemanticChunkingStrategy implements ChunkingStrategy {

    private static final int DEFAULT_MIN_CHUNK_SIZE = 300;
    private static final int DEFAULT_MAX_CHUNK_SIZE = 1000;
    private static final double DEFAULT_SIMILARITY_THRESHOLD = 0.5;

    @Override
    public List<Chunk> chunk(String documentId, String content, Map<String, Object> params) {
        List<Chunk> chunks = new ArrayList<>();

        if (content == null || content.isEmpty()) {
            return chunks;
        }

        int minChunkSize = getParam(params, "minChunkSize", DEFAULT_MIN_CHUNK_SIZE);
        int maxChunkSize = getParam(params, "maxChunkSize", DEFAULT_MAX_CHUNK_SIZE);
        double threshold = getParam(params, "similarityThreshold", DEFAULT_SIMILARITY_THRESHOLD);

        // 1. 按段落分割
        List<Paragraph> paragraphs = splitIntoParagraphs(content);
        if (paragraphs.isEmpty()) {
            return chunks;
        }

        // 2. 计算段落的词频向量（简化的 TF-IDF）
        List<Map<String, Integer>> vectors = calculateWordVectors(paragraphs);

        // 3. 计算相邻段落的语义相似度
        List<Double> similarities = calculateSimilarities(vectors);

        // 4. 找到分块边界（相似度低于阈值的位置）
        List<Integer> boundaries = findSemanticBoundaries(similarities, paragraphs,
                minChunkSize, maxChunkSize, threshold);

        // 5. 在边界处切分
        int sequence = 0;
        int start = 0;

        for (int boundary : boundaries) {
            if (boundary > start) {
                StringBuilder chunkContent = new StringBuilder();
                int chunkStart = paragraphs.get(start).startPosition;
                int chunkEnd = paragraphs.get(boundary - 1).endPosition;

                for (int i = start; i < boundary; i++) {
                    if (!chunkContent.isEmpty()) {
                        chunkContent.append("\n\n");
                    }
                    chunkContent.append(paragraphs.get(i).text);
                }

                double avgSimilarity = calculateAvgSimilarity(similarities, start, boundary);

                Chunk chunk = Chunk.builder()
                        .documentId(documentId)
                        .content(chunkContent.toString().trim())
                        .sequence(sequence)
                        .startPosition(chunkStart)
                        .endPosition(chunkEnd)
                        .metadata(Map.of(
                                "strategy", getStrategyName(),
                                "paragraphs", boundary - start,
                                "avgSimilarity", avgSimilarity,
                                "semanticCoherence", avgSimilarity > threshold ? "high" : "medium"
                        ))
                        .createdAt(System.currentTimeMillis())
                        .build();

                chunks.add(chunk);

                start = boundary;
                sequence++;
            }
        }

        // 处理最后一个分块
        if (start < paragraphs.size()) {
            StringBuilder chunkContent = new StringBuilder();
            int chunkStart = paragraphs.get(start).startPosition;
            int chunkEnd = paragraphs.getLast().endPosition;

            for (int i = start; i < paragraphs.size(); i++) {
                if (!chunkContent.isEmpty()) {
                    chunkContent.append("\n\n");
                }
                chunkContent.append(paragraphs.get(i).text);
            }

            Chunk chunk = Chunk.builder()
                    .documentId(documentId)
                    .content(chunkContent.toString().trim())
                    .sequence(sequence)
                    .startPosition(chunkStart)
                    .endPosition(chunkEnd)
                    .metadata(Map.of(
                            "strategy", getStrategyName(),
                            "paragraphs", paragraphs.size() - start,
                            "avgSimilarity", calculateAvgSimilarity(similarities, start, paragraphs.size())
                    ))
                    .createdAt(System.currentTimeMillis())
                    .build();

            chunks.add(chunk);
        }

        log.debug("Semantic chunking: {} chunks created based on semantic boundaries", chunks.size());
        return chunks;
    }

    @Override
    public String getStrategyName() {
        return "semantic";
    }

    @Override
    public String getDescription() {
        return "语义分块策略 - 基于段落语义相似度智能切分，保持主题连贯性";
    }

    @Override
    public Map<String, Object> getDefaultParams() {
        Map<String, Object> params = new HashMap<>();
        params.put("minChunkSize", DEFAULT_MIN_CHUNK_SIZE);
        params.put("maxChunkSize", DEFAULT_MAX_CHUNK_SIZE);
        params.put("similarityThreshold", DEFAULT_SIMILARITY_THRESHOLD);
        return params;
    }

    /**
     * 按段落分割（双换行符）
     */
    private List<Paragraph> splitIntoParagraphs(String content) {
        List<Paragraph> paragraphs = new ArrayList<>();

        String[] parts = content.split("\\n\\s*\\n");

        int position = 0;
        for (String part : parts) {
            String trimmed = part.trim();
            if (!trimmed.isEmpty()) {
                int start = position;
                int end = start + trimmed.length();
                paragraphs.add(new Paragraph(trimmed, start, end));
                position = end;
            }
        }

        return paragraphs;
    }

    /**
     * 计算段落的词频向量（简化的 TF-IDF）
     */
    private List<Map<String, Integer>> calculateWordVectors(List<Paragraph> paragraphs) {
        List<Map<String, Integer>> vectors = new ArrayList<>();

        for (Paragraph paragraph : paragraphs) {
            Map<String, Integer> wordCount = new HashMap<>();

            // 分词并统计词频
            String[] words = paragraph.text.toLowerCase()
                    .replaceAll("[^a-zA-Z0-9\\s\u4e00-\u9fa5]", " ")
                    .split("\\s+");

            for (String word : words) {
                if (word.length() > 1) {  // 忽略单字符
                    wordCount.merge(word, 1, Integer::sum);
                }
            }

            vectors.add(wordCount);
        }

        return vectors;
    }

    /**
     * 计算相邻段落的余弦相似度
     */
    private List<Double> calculateSimilarities(List<Map<String, Integer>> vectors) {
        List<Double> similarities = new ArrayList<>();

        for (int i = 0; i < vectors.size() - 1; i++) {
            double similarity = cosineSimilarity(vectors.get(i), vectors.get(i + 1));
            similarities.add(similarity);
        }

        return similarities;
    }

    /**
     * 余弦相似度计算
     */
    private double cosineSimilarity(Map<String, Integer> vec1, Map<String, Integer> vec2) {
        if (vec1.isEmpty() || vec2.isEmpty()) {
            return 0.0;
        }

        // 计算点积
        double dotProduct = 0.0;
        Set<String> commonKeys = new HashSet<>(vec1.keySet());
        commonKeys.retainAll(vec2.keySet());

        for (String key : commonKeys) {
            dotProduct += vec1.get(key) * vec2.get(key);
        }

        // 计算向量长度
        double magnitude1 = 0.0;
        for (int count : vec1.values()) {
            magnitude1 += count * count;
        }
        magnitude1 = Math.sqrt(magnitude1);

        double magnitude2 = 0.0;
        for (int count : vec2.values()) {
            magnitude2 += count * count;
        }
        magnitude2 = Math.sqrt(magnitude2);

        if (magnitude1 == 0.0 || magnitude2 == 0.0) {
            return 0.0;
        }

        return dotProduct / (magnitude1 * magnitude2);
    }

    /**
     * 找到语义边界（相似度低于阈值的位置）
     */
    private List<Integer> findSemanticBoundaries(List<Double> similarities,
                                                 List<Paragraph> paragraphs,
                                                 int minChunkSize,
                                                 int maxChunkSize,
                                                 double threshold) {
        List<Integer> boundaries = new ArrayList<>();
        boundaries.add(0);  // 起始边界

        int currentChunkSize = 0;
        int lastBoundary = 0;

        for (int i = 0; i < similarities.size(); i++) {
            currentChunkSize += paragraphs.get(i).text.length();

            // 在以下情况切分：
            // 1. 语义相似度低于阈值 + 已达到最小分块大小
            // 2. 达到最大分块大小（强制切分）
            boolean isSemanticBoundary = similarities.get(i) < threshold;

            if ((isSemanticBoundary && currentChunkSize >= minChunkSize) ||
                    currentChunkSize >= maxChunkSize) {

                boundaries.add(i + 1);
                currentChunkSize = 0;
                lastBoundary = i + 1;
            }
        }

        // 最后一个边界
        if (lastBoundary < paragraphs.size()) {
            boundaries.add(paragraphs.size());
        }

        return boundaries;
    }

    /**
     * 计算平均相似度
     */
    private double calculateAvgSimilarity(List<Double> similarities, int start, int end) {
        if (start >= end || similarities.isEmpty()) {
            return 0.0;
        }

        double sum = 0.0;
        int count = 0;

        for (int i = start; i < Math.min(end, similarities.size()); i++) {
            sum += similarities.get(i);
            count++;
        }

        return count > 0 ? sum / count : 0.0;
    }

    @SuppressWarnings("unchecked")
    private <T> T getParam(Map<String, Object> params, String key, T defaultValue) {
        if (params == null || !params.containsKey(key)) {
            return defaultValue;
        }
        return (T) params.get(key);
    }

    /**
     * 段落数据结构
     */
    private static class Paragraph {
        String text;
        int startPosition;
        int endPosition;

        Paragraph(String text, int start, int end) {
            this.text = text;
            this.startPosition = start;
            this.endPosition = end;
        }
    }
}

