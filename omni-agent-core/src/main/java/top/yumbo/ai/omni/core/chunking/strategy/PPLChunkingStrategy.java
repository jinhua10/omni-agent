package top.yumbo.ai.omni.core.chunking.strategy;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.storage.api.model.Chunk;

import java.util.*;
import java.util.stream.Collectors;

/**
 * PPL 困惑度分块策略（配置驱动）⭐
 * <p>
 * 支持两种实现，用户通过配置自由选择：
 * 1. 简化版 - 快速、零依赖，使用词汇重叠度近似困惑度（默认）
 * 2. ONNX 版 - 精度高、使用真实语言模型计算困惑度（可选）
 * <p>
 * 配置方式：
 * <pre>
 * # application.yml
 * chunking:
 *   ppl:
 *     mode: simplified  # simplified | onnx | auto
 *     prefer-accuracy: false  # 自动模式时，是否优先精度
 * </pre>
 * <p>
 * 各模式说明：
 * - simplified: 强制使用简化版（<1ms，零依赖）
 * - onnx: 强制使用 ONNX 版（30-150ms，精度+15-20%）
 * - auto: 自动选择（ONNX 可用且 prefer-accuracy=true 时用 ONNX）
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

    // ========== 配置参数 ==========

    @Value("${chunking.ppl.mode:simplified}")
    private String pplMode;  // simplified | onnx | auto

    @Value("${chunking.ppl.prefer-accuracy:false}")
    private boolean preferAccuracy;

    // ========== 可选依赖（ONNX 服务）==========

    @Autowired(required = false)
    private top.yumbo.ai.omni.ppl.onnx.PPLOnnxService pplOnnxService;

    @Override
    public List<Chunk> chunk(String documentId, String content, Map<String, Object> params) {
        if (content == null || content.isEmpty()) {
            return new ArrayList<>();
        }

        // 根据配置选择 PPL 计算方式
        PPLCalculator calculator = selectCalculator();

        // 使用选定的计算器计算困惑度
        List<Double> perplexities = calculator.calculate(content);

        // 基于困惑度创建分块
        return createChunksFromPerplexities(documentId, content, perplexities, params);
    }

    /**
     * 根据配置选择 PPL 计算器
     */
    private PPLCalculator selectCalculator() {
        String mode = pplMode != null ? pplMode.toLowerCase() : "simplified";

        switch (mode) {
            case "onnx":
                // 强制使用 ONNX
                if (pplOnnxService != null && pplOnnxService.isHealthy()) {
                    log.info("✅ 使用 ONNX PPL 计算器（配置指定: mode=onnx）");
                    return new OnnxPPLCalculator(pplOnnxService);
                } else {
                    log.warn("⚠️ ONNX 服务不可用，降级到简化版");
                    return new SimplifiedPPLCalculator();
                }

            case "auto":
                // 自动选择
                if (pplOnnxService != null && pplOnnxService.isHealthy() && preferAccuracy) {
                    log.info("✅ 使用 ONNX PPL 计算器（自动选择 - 优先精度）");
                    return new OnnxPPLCalculator(pplOnnxService);
                } else {
                    log.info("✅ 使用简化版 PPL 计算器（自动选择 - 优先速度）");
                    return new SimplifiedPPLCalculator();
                }

            case "simplified":
            default:
                // 强制使用简化版
                log.info("✅ 使用简化版 PPL 计算器（配置指定: mode=simplified）");
                return new SimplifiedPPLCalculator();
        }
    }

    /**
     * 基于困惑度创建分块
     */
    private List<Chunk> createChunksFromPerplexities(String documentId, String content,
                                                     List<Double> perplexities,
                                                     Map<String, Object> params) {
        List<Chunk> chunks = new ArrayList<>();

        int minChunkSize = ChunkingParamUtils.getParam(params, "minChunkSize", DEFAULT_MIN_CHUNK_SIZE);
        int maxChunkSize = ChunkingParamUtils.getParam(params, "maxChunkSize", DEFAULT_MAX_CHUNK_SIZE);
        double threshold = ChunkingParamUtils.getParam(params, "threshold", DEFAULT_THRESHOLD);

        // 1. 按句子分割
        List<Sentence> sentences = splitIntoSentences(content);
        if (sentences.isEmpty()) {
            return chunks;
        }


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

    // ========== PPL 计算器接口和实现 ==========

    /**
     * PPL 计算器接口
     */
    interface PPLCalculator {
        /**
         * 计算文本的困惑度序列
         *
         * @param content 文本内容
         * @return 困惑度序列
         */
        List<Double> calculate(String content);
    }

    /**
     * 简化版 PPL 计算器
     * 使用词汇重叠度近似困惑度
     */
    class SimplifiedPPLCalculator implements PPLCalculator {
        @Override
        public List<Double> calculate(String content) {
            List<Sentence> sentences = splitIntoSentences(content);
            return calculatePerplexities(sentences);
        }
    }

    /**
     * ONNX PPL 计算器
     * 使用真实语言模型计算困惑度
     */
    class OnnxPPLCalculator implements PPLCalculator {
        private final top.yumbo.ai.omni.ppl.onnx.PPLOnnxService pplService;

        OnnxPPLCalculator(top.yumbo.ai.omni.ppl.onnx.PPLOnnxService pplService) {
            this.pplService = pplService;
        }

        @Override
        public List<Double> calculate(String content) {
            List<String> sentences = Arrays.stream(
                            content.split("(?<=[。！？.!?])\\s*")
                    ).filter(s -> !s.trim().isEmpty())
                    .toList();

            return sentences.stream()
                    .map(s -> {
                        try {
                            return pplService.calculatePerplexity(s);
                        } catch (Exception e) {
                            log.warn("ONNX 计算困惑度失败: {}", e.getMessage());
                            return Double.MAX_VALUE;
                        }
                    })
                    .collect(Collectors.toList());
        }
    }
}

