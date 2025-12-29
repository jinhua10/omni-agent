package top.yumbo.ai.omni.chunking.starter.strategy;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import top.yumbo.ai.omni.ai.starter.impl.PPLOnnxService;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;

import java.util.*;
import java.util.stream.Collectors;

/**
 * PPL 困惑度分块策略（支持 ONNX 和简化版）
 *
 * <p>支持两种实现：</p>
 * <ul>
 *   <li>简化版 - 快速、零依赖，使用词汇重叠度近似困惑度（默认）</li>
 *   <li>ONNX 版 - 精度高、使用真实语言模型计算困惑度（可选）</li>
 * </ul>
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

    // ONNX 服务（可选依赖）
    @Autowired(required = false)
    private PPLOnnxService pplOnnxService;

    public PPLChunkingStrategy(ChunkingProperties properties) {
        this.properties = properties;
        log.info("✅ PPL 分块策略已初始化");
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

        // 2. 选择 PPL 计算器并计算困惑度
        PPLCalculator calculator = selectCalculator();
        List<Double> perplexities = calculator.calculate(content, sentences);

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
     * 根据配置选择 PPL 计算器
     */
    private PPLCalculator selectCalculator() {
        // 如果 ONNX 服务可用，使用 ONNX 计算器
        if (pplOnnxService != null) {
            try {
                if (pplOnnxService.isHealthy()) {
                    log.info("✅ 使用 ONNX PPL 计算器（精度更高）");
                    return new OnnxPPLCalculator(pplOnnxService);
                }
            } catch (Exception e) {
                log.warn("⚠️ ONNX 服务检查失败: {}", e.getMessage());
            }
        }

        // 默认使用简化版
        log.info("✅ 使用简化版 PPL 计算器（词汇重叠度算法）");
        return new SimplifiedPPLCalculator();
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
                        .id(UUID.randomUUID().toString())
                        .documentId(documentId)
                        .content(content.toString().trim())
                        .sequence(i)
                        .startPosition(chunkStart)
                        .endPosition(chunkEnd)
                        .strategy(ChunkingStrategy.PPL)
                        .createdAt(System.currentTimeMillis())
                        .build());
            }
        }

        return chunks;
    }

    // ========== PPL 计算器接口和实现 ==========

    /**
     * PPL 计算器接口
     */
    interface PPLCalculator {
        /**
         * 计算文本的困惑度序列
         *
         * @param content 原始文本内容
         * @param sentences 已分割的句子列表
         * @return 困惑度序列
         */
        List<Double> calculate(String content, List<Sentence> sentences);
    }

    /**
     * 简化版 PPL 计算器
     * 使用词汇重叠度近似困惑度
     */
    class SimplifiedPPLCalculator implements PPLCalculator {
        @Override
        public List<Double> calculate(String content, List<Sentence> sentences) {
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
    }

    /**
     * ONNX PPL 计算器
     * 使用真实语言模型计算困惑度
     */
    class OnnxPPLCalculator implements PPLCalculator {
        private final PPLOnnxService pplService;

        OnnxPPLCalculator(PPLOnnxService pplService) {
            this.pplService = pplService;
        }

        @Override
        public List<Double> calculate(String content, List<Sentence> sentences) {
            // 使用 ONNX 服务计算每个句子的困惑度
            List<String> sentenceTexts = sentences.stream()
                    .map(s -> s.text)
                    .collect(Collectors.toList());

            List<Double> perplexities = new ArrayList<>();

            for (int i = 0; i < sentenceTexts.size() - 1; i++) {
                try {
                    // 计算相邻句子的困惑度差异
                    double ppl1 = pplService.calculatePerplexity(sentenceTexts.get(i));
                    double ppl2 = pplService.calculatePerplexity(sentenceTexts.get(i + 1));

                    // 困惑度差异越大，说明语义跳跃越大
                    double perplexityDiff = Math.abs(ppl2 - ppl1);
                    perplexities.add(perplexityDiff);

                } catch (Exception e) {
                    log.warn("⚠️ ONNX 计算困惑度失败: {}", e.getMessage());
                    // 降级到简化版算法
                    double overlap = calculateWordOverlap(
                            sentenceTexts.get(i),
                            sentenceTexts.get(i + 1)
                    );
                    perplexities.add(1.0 - overlap);
                }
            }

            return perplexities;
        }
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


