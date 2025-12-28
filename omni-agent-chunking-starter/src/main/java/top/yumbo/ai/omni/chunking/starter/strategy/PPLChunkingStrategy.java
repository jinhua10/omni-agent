package top.yumbo.ai.omni.chunking.starter.strategy;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;

import java.util.*;

/**
 * PPL 困惑度分块策略
 *
 * <p>使用词汇重叠度近似困惑度，在语义边界处智能切分</p>
 * <p>从 core/old/chunking 迁移而来</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class PPLChunkingStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;

    private static final int DEFAULT_MIN_CHUNK_SIZE = 200;
    private static final int DEFAULT_MAX_CHUNK_SIZE = 800;
    private static final double DEFAULT_THRESHOLD = 0.3;

    public PPLChunkingStrategy(ChunkingProperties properties) {
        this.properties = properties;
        log.info("✅ PPL 分块策略已初始化（使用简化版词汇重叠度算法）");
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        if (content == null || content.isEmpty()) {
            return new ArrayList<>();
        }

        // 1. 按句子分割
        List<Sentence> sentences = splitIntoSentences(content);
        if (sentences.isEmpty()) {
            return new ArrayList<>();
        }

        // 2. 计算句子间的困惑度
        List<Double> perplexities = calculatePerplexities(sentences);

        // 3. 找到分块边界
        int minChunkSize = config.getMinChunkSize() != null ?
                config.getMinChunkSize() : DEFAULT_MIN_CHUNK_SIZE;
        int maxChunkSize = config.getMaxChunkSize() != null ?
                config.getMaxChunkSize() : DEFAULT_MAX_CHUNK_SIZE;

        List<Integer> boundaries = findBoundaries(perplexities, sentences,
                minChunkSize, maxChunkSize, DEFAULT_THRESHOLD);

        // 4. 在边界处切分
        return createChunks(documentId, sentences, boundaries);
    }

    /**
     * 分割成句子
     */
    private List<Sentence> splitIntoSentences(String content) {
        List<Sentence> sentences = new ArrayList<>();
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
     * 计算句子间的困惑度（使用词汇重叠度近似）
     */
    private List<Double> calculatePerplexities(List<Sentence> sentences) {
        List<Double> perplexities = new ArrayList<>();

        for (int i = 0; i < sentences.size() - 1; i++) {
            double overlap = calculateWordOverlap(
                    sentences.get(i).text,
                    sentences.get(i + 1).text
            );
            // 困惑度 = 1 - 重叠度
            perplexities.add(1.0 - overlap);
        }

        return perplexities;
    }

    /**
     * 计算词汇重叠度（Jaccard 相似度）
     */
    private double calculateWordOverlap(String sent1, String sent2) {
        Set<String> words1 = new HashSet<>(Arrays.asList(sent1.toLowerCase().split("\\s+")));
        Set<String> words2 = new HashSet<>(Arrays.asList(sent2.toLowerCase().split("\\s+")));

        Set<String> intersection = new HashSet<>(words1);
        intersection.retainAll(words2);

        Set<String> union = new HashSet<>(words1);
        union.addAll(words2);

        return union.isEmpty() ? 0.0 : (double) intersection.size() / union.size();
    }

    /**
     * 找到分块边界
     */
    private List<Integer> findBoundaries(List<Double> perplexities,
                                        List<Sentence> sentences,
                                        int minChunkSize,
                                        int maxChunkSize,
                                        double threshold) {
        List<Integer> boundaries = new ArrayList<>();
        boundaries.add(0);

        int currentChunkSize = 0;
        int lastBoundary = 0;

        for (int i = 0; i < perplexities.size(); i++) {
            currentChunkSize += sentences.get(i).text.length();

            boolean isPeak = isPeakPoint(perplexities, i, threshold);

            if ((isPeak && currentChunkSize >= minChunkSize) ||
                currentChunkSize >= maxChunkSize) {
                boundaries.add(i + 1);
                currentChunkSize = 0;
                lastBoundary = i + 1;
            }
        }

        if (lastBoundary < sentences.size()) {
            boundaries.add(sentences.size());
        }

        return boundaries;
    }

    /**
     * 判断是否是困惑度峰值点
     */
    private boolean isPeakPoint(List<Double> perplexities, int index, double threshold) {
        if (index == 0 || index >= perplexities.size() - 1) {
            return false;
        }

        double current = perplexities.get(index);
        double prev = perplexities.get(index - 1);
        double next = perplexities.get(index + 1);

        return current > prev && current > next && current > threshold;
    }

    /**
     * 创建分块
     */
    private List<Chunk> createChunks(String documentId, List<Sentence> sentences, List<Integer> boundaries) {
        List<Chunk> chunks = new ArrayList<>();

        for (int i = 0; i < boundaries.size() - 1; i++) {
            int start = boundaries.get(i);
            int end = boundaries.get(i + 1);

            if (end > start) {
                StringBuilder content = new StringBuilder();
                int chunkStart = sentences.get(start).startPosition;
                int chunkEnd = sentences.get(end - 1).endPosition;

                for (int j = start; j < end; j++) {
                    content.append(sentences.get(j).text);
                }

                chunks.add(Chunk.builder()
                        .chunkId(UUID.randomUUID().toString())
                        .documentId(documentId)
                        .content(content.toString().trim())
                        .index(i)
                        .startPosition(chunkStart)
                        .endPosition(chunkEnd)
                        .length(content.length())
                        .strategy(ChunkingStrategy.PPL)
                        .build());
            }
        }

        return chunks;
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


