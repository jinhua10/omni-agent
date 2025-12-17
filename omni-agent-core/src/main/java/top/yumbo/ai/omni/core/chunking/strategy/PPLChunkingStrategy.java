package top.yumbo.ai.omni.core.chunking.strategy;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import top.yumbo.ai.storage.api.model.Chunk;

import java.util.*;

/**
 * PPL 困惑度分块策略
 * <p>
 * 基于困惑度（Perplexity/Probable Point of Loss）的智能分块
 * 在困惑度峰值点（语义边界）切分，保持语义完整性
 * <p>
 * 原理：
 * 1. 计算文本每个位置的"困惑度"（语义不连贯程度）
 * 2. 困惑度高的位置 = 主题转换点 = 分块边界
 * 3. 在这些边界处切分，保证每个分块语义完整
 * <p>
 * 简化实现（不依赖语言模型）：
 * - 使用句子相似度作为困惑度的近似
 * - 相似度低 = 困惑度高 = 主题转换
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
public class PPLChunkingStrategy implements ChunkingStrategy {

    private static final int DEFAULT_MIN_CHUNK_SIZE = 200;
    private static final int DEFAULT_MAX_CHUNK_SIZE = 800;
    private static final double DEFAULT_THRESHOLD = 0.3;  // 困惑度阈值

    @Override
    public List<Chunk> chunk(String documentId, String content, Map<String, Object> params) {
        List<Chunk> chunks = new ArrayList<>();

        if (content == null || content.isEmpty()) {
            return chunks;
        }

        int minChunkSize = getParam(params, "minChunkSize", DEFAULT_MIN_CHUNK_SIZE);
        int maxChunkSize = getParam(params, "maxChunkSize", DEFAULT_MAX_CHUNK_SIZE);
        double threshold = getParam(params, "threshold", DEFAULT_THRESHOLD);

        // 1. 按句子分割
        List<Sentence> sentences = splitIntoSentences(content);
        if (sentences.isEmpty()) {
            return chunks;
        }

        // 2. 计算句子间的"困惑度"（简化：使用词汇重叠度的倒数）
        List<Double> perplexities = calculatePerplexities(sentences);

        // 3. 找到困惑度峰值点（分块边界）
        List<Integer> boundaries = findBoundaries(perplexities, sentences,
                minChunkSize, maxChunkSize, threshold);

        // 4. 在边界处切分
        int sequence = 0;
        int start = 0;

        for (int boundary : boundaries) {
            if (boundary > start) {
                // 合并句子形成分块
                StringBuilder chunkContent = new StringBuilder();
                int chunkStart = sentences.get(start).startPosition;
                int chunkEnd = sentences.get(boundary - 1).endPosition;

                for (int i = start; i < boundary; i++) {
                    chunkContent.append(sentences.get(i).text);
                }

                Chunk chunk = Chunk.builder()
                        .documentId(documentId)
                        .content(chunkContent.toString().trim())
                        .sequence(sequence)
                        .startPosition(chunkStart)
                        .endPosition(chunkEnd)
                        .metadata(Map.of(
                                "strategy", getStrategyName(),
                                "sentences", boundary - start,
                                "avgPerplexity", calculateAvgPerplexity(perplexities, start, boundary)
                        ))
                        .createdAt(System.currentTimeMillis())
                        .build();

                chunks.add(chunk);

                start = boundary;
                sequence++;
            }
        }

        // 处理最后一个分块
        if (start < sentences.size()) {
            StringBuilder chunkContent = new StringBuilder();
            int chunkStart = sentences.get(start).startPosition;
            int chunkEnd = sentences.getLast().endPosition;

            for (int i = start; i < sentences.size(); i++) {
                chunkContent.append(sentences.get(i).text);
            }

            Chunk chunk = Chunk.builder()
                    .documentId(documentId)
                    .content(chunkContent.toString().trim())
                    .sequence(sequence)
                    .startPosition(chunkStart)
                    .endPosition(chunkEnd)
                    .metadata(Map.of(
                            "strategy", getStrategyName(),
                            "sentences", sentences.size() - start,
                            "avgPerplexity", calculateAvgPerplexity(perplexities, start, sentences.size())
                    ))
                    .createdAt(System.currentTimeMillis())
                    .build();

            chunks.add(chunk);
        }

        log.debug("PPL chunking: {} chunks created with avg perplexity boundaries", chunks.size());
        return chunks;
    }

    @Override
    public String getStrategyName() {
        return "ppl";
    }

    @Override
    public String getDescription() {
        return "PPL困惑度分块策略 - 基于语义边界智能切分，保持主题完整性";
    }

    @Override
    public Map<String, Object> getDefaultParams() {
        Map<String, Object> params = new HashMap<>();
        params.put("minChunkSize", DEFAULT_MIN_CHUNK_SIZE);
        params.put("maxChunkSize", DEFAULT_MAX_CHUNK_SIZE);
        params.put("threshold", DEFAULT_THRESHOLD);
        return params;
    }

    /**
     * 分割成句子
     */
    private List<Sentence> splitIntoSentences(String content) {
        List<Sentence> sentences = new ArrayList<>();

        // 简单的句子分割（按标点符号）
        String[] parts = content.split("(?<=[。！？.!?])\\s*");

        int position = 0;
        for (String part : parts) {
            String trimmed = part.trim();
            if (!trimmed.isEmpty()) {
                int start = position;
                int end = start + trimmed.length();
                sentences.add(new Sentence(trimmed, start, end));
                position = end;
            }
        }

        return sentences;
    }

    /**
     * 计算句子间的困惑度
     * <p>
     * 简化实现：使用词汇重叠度的倒数作为困惑度
     * - 重叠度高 = 主题相关 = 困惑度低
     * - 重叠度低 = 主题转换 = 困惑度高
     */
    private List<Double> calculatePerplexities(List<Sentence> sentences) {
        List<Double> perplexities = new ArrayList<>();

        for (int i = 0; i < sentences.size() - 1; i++) {
            // 计算相邻句子的词汇重叠度
            double overlap = calculateWordOverlap(sentences.get(i).text,
                    sentences.get(i + 1).text);

            // 困惑度 = 1 - 重叠度（重叠度越低，困惑度越高）
            double perplexity = 1.0 - overlap;
            perplexities.add(perplexity);
        }

        return perplexities;
    }

    /**
     * 计算两个句子的词汇重叠度（Jaccard相似度）
     */
    private double calculateWordOverlap(String sent1, String sent2) {
        Set<String> words1 = new HashSet<>(Arrays.asList(sent1.toLowerCase().split("\\s+")));
        Set<String> words2 = new HashSet<>(Arrays.asList(sent2.toLowerCase().split("\\s+")));

        // 计算交集
        Set<String> intersection = new HashSet<>(words1);
        intersection.retainAll(words2);

        // 计算并集
        Set<String> union = new HashSet<>(words1);
        union.addAll(words2);

        if (union.isEmpty()) {
            return 0.0;
        }

        // Jaccard 相似度 = |交集| / |并集|
        return (double) intersection.size() / union.size();
    }

    /**
     * 找到分块边界（困惑度峰值点）
     */
    private List<Integer> findBoundaries(List<Double> perplexities,
                                         List<Sentence> sentences,
                                         int minChunkSize,
                                         int maxChunkSize,
                                         double threshold) {
        List<Integer> boundaries = new ArrayList<>();
        boundaries.add(0);  // 起始边界

        int currentChunkSize = 0;
        int lastBoundary = 0;

        for (int i = 0; i < perplexities.size(); i++) {
            currentChunkSize += sentences.get(i).text.length();

            // 判断是否是困惑度峰值（局部最大值）
            boolean isPeak = isPeakPoint(perplexities, i, threshold);

            // 在以下情况切分：
            // 1. 困惑度峰值 + 已达到最小分块大小
            // 2. 达到最大分块大小（强制切分）
            if ((isPeak && currentChunkSize >= minChunkSize) ||
                    currentChunkSize >= maxChunkSize) {

                boundaries.add(i + 1);
                currentChunkSize = 0;
                lastBoundary = i + 1;
            }
        }

        // 最后一个边界
        if (lastBoundary < sentences.size()) {
            boundaries.add(sentences.size());
        }

        return boundaries;
    }

    /**
     * 判断是否是困惑度峰值点
     */
    private boolean isPeakPoint(List<Double> perplexities, int index, double threshold) {
        if (index == 0 || index == perplexities.size() - 1) {
            return false;
        }

        double current = perplexities.get(index);
        double prev = perplexities.get(index - 1);
        double next = perplexities.get(index + 1);

        // 是局部最大值 && 超过阈值
        return current > prev && current > next && current > threshold;
    }

    /**
     * 计算平均困惑度
     */
    private double calculateAvgPerplexity(List<Double> perplexities, int start, int end) {
        if (start >= end || perplexities.isEmpty()) {
            return 0.0;
        }

        double sum = 0.0;
        int count = 0;

        for (int i = start; i < Math.min(end, perplexities.size()); i++) {
            sum += perplexities.get(i);
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
     * 句子数据结构
     */
    private static class Sentence {
        String text;
        int startPosition;
        int endPosition;

        Sentence(String text, int start, int end) {
            this.text = text;
            this.startPosition = start;
            this.endPosition = end;
        }
    }
}

