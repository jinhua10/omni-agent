package top.yumbo.ai.omni.chunking.starter.strategy;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 语义分块策略（支持 ONNX 和 AI 服务）
 *
 * <p>支持三种实现：</p>
 * <ul>
 *   <li>简化版 - 基于段落边界，零依赖（默认）</li>
 *   <li>ONNX 版 - 基于向量相似度，使用 ONNX Embedding 模型</li>
 *   <li>AI 版 - 基于大语言模型的语义理解</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class SemanticStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;

    // ONNX Embedding 服务（可选依赖）
    @Autowired(required = false)
    private top.yumbo.ai.omni.ai.onnx.OnnxEmbeddingService onnxEmbeddingService;

    // AI 服务（可选依赖）
    @Autowired(required = false)
    private top.yumbo.ai.omni.ai.api.AIService aiService;

    public SemanticStrategy(ChunkingProperties properties) {
        this.properties = properties;
        log.info("✅ 语义分块策略已初始化");
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        if (content == null || content.isEmpty()) {
            return new ArrayList<>();
        }

        // 选择语义计算器
        SemanticCalculator calculator = selectCalculator();

        // 按段落分割
        List<Paragraph> paragraphs = splitIntoParagraphs(content);
        if (paragraphs.isEmpty()) {
            return new ArrayList<>();
        }

        // 计算段落间的语义相似度
        List<Double> similarities = calculator.calculateSimilarities(paragraphs);

        // 根据相似度找到分块边界
        int maxSize = config.getMaxChunkSize() != null ?
                config.getMaxChunkSize() : properties.getGeneral().getMaxChunkSize();
        int minSize = config.getMinChunkSize() != null ?
                config.getMinChunkSize() : properties.getGeneral().getMinChunkSize();

        double threshold = properties.getSemantic().getThreshold();
        List<Integer> boundaries = findBoundaries(similarities, paragraphs, minSize, maxSize, threshold);

        // 创建分块
        return createChunks(documentId, paragraphs, boundaries);
    }

    /**
     * 选择语义计算器
     */
    private SemanticCalculator selectCalculator() {
        // 优先使用 ONNX Embedding（精度高、速度快）
        if (onnxEmbeddingService != null) {
            try {
                log.info("✅ 使用 ONNX Embedding 语义计算器（向量相似度）");
                return new OnnxSemanticCalculator(onnxEmbeddingService);
            } catch (Exception e) {
                log.warn("⚠️ ONNX Embedding 服务初始化失败: {}", e.getMessage());
            }
        }

        // 其次使用 AI 服务（精度高、速度较慢）
        if (aiService != null) {
            try {
                log.info("✅ 使用 AI 服务语义计算器（大语言模型）");
                return new AISemanticCalculator(aiService);
            } catch (Exception e) {
                log.warn("⚠️ AI 服务初始化失败: {}", e.getMessage());
            }
        }

        // 默认使用简化版
        log.info("✅ 使用简化版语义计算器（段落边界）");
        return new SimplifiedSemanticCalculator();
    }

    /**
     * 分割成段落
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
                position = content.indexOf(part, position) + part.length();
            }
        }

        return paragraphs;
    }

    /**
     * 根据相似度找到分块边界
     */
    private List<Integer> findBoundaries(List<Double> similarities,
                                        List<Paragraph> paragraphs,
                                        int minSize, int maxSize, double threshold) {
        List<Integer> boundaries = new ArrayList<>();
        boundaries.add(0);

        int currentChunkSize = 0;
        int lastBoundary = 0;

        for (int i = 0; i < similarities.size(); i++) {
            currentChunkSize += paragraphs.get(i).text.length();

            // 相似度低于阈值表示语义跳跃，应该切分
            boolean isSemanticBoundary = similarities.get(i) < threshold;

            if ((isSemanticBoundary && currentChunkSize >= minSize) ||
                currentChunkSize >= maxSize) {
                boundaries.add(i + 1);
                currentChunkSize = 0;
                lastBoundary = i + 1;
            }
        }

        if (lastBoundary < paragraphs.size()) {
            boundaries.add(paragraphs.size());
        }

        return boundaries;
    }

    /**
     * 创建分块
     */
    private List<Chunk> createChunks(String documentId, List<Paragraph> paragraphs, List<Integer> boundaries) {
        List<Chunk> chunks = new ArrayList<>();

        for (int i = 0; i < boundaries.size() - 1; i++) {
            int start = boundaries.get(i);
            int end = boundaries.get(i + 1);

            if (end > start) {
                StringBuilder content = new StringBuilder();
                int chunkStart = paragraphs.get(start).startPosition;
                int chunkEnd = paragraphs.get(end - 1).endPosition;

                for (int j = start; j < end; j++) {
                    if (j > start) {
                        content.append("\n\n");
                    }
                    content.append(paragraphs.get(j).text);
                }

                chunks.add(Chunk.builder()
                        .chunkId(UUID.randomUUID().toString())
                        .documentId(documentId)
                        .content(content.toString())
                        .index(i)
                        .startPosition(chunkStart)
                        .endPosition(chunkEnd)
                        .length(content.length())
                        .strategy(ChunkingStrategy.SEMANTIC)
                        .build());
            }
        }

        return chunks;
    }

    // ========== 语义计算器接口和实现 ==========

    /**
     * 语义计算器接口
     */
    interface SemanticCalculator {
        /**
         * 计算段落间的语义相似度
         *
         * @param paragraphs 段落列表
         * @return 相似度序列（相邻段落的相似度）
         */
        List<Double> calculateSimilarities(List<Paragraph> paragraphs);
    }

    /**
     * 简化版语义计算器
     * 使用 TF-IDF + 余弦相似度计算段落间语义相似度
     */
    class SimplifiedSemanticCalculator implements SemanticCalculator {
        @Override
        public List<Double> calculateSimilarities(List<Paragraph> paragraphs) {
            // 1. 计算段落的词频向量（TF-IDF）
            List<Map<String, Integer>> vectors = calculateWordVectors(paragraphs);

            // 2. 计算相邻段落的余弦相似度
            List<Double> similarities = new ArrayList<>();
            for (int i = 0; i < vectors.size() - 1; i++) {
                double similarity = cosineSimilarity(vectors.get(i), vectors.get(i + 1));
                similarities.add(similarity);
            }

            return similarities;
        }

        /**
         * 计算段落的词频向量（简化的 TF-IDF）
         */
        private List<Map<String, Integer>> calculateWordVectors(List<Paragraph> paragraphs) {
            List<Map<String, Integer>> vectors = new ArrayList<>();

            for (Paragraph paragraph : paragraphs) {
                Map<String, Integer> wordCount = new HashMap<>();

                // 分词并统计词频（支持中英文）
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
    }

    /**
     * ONNX 语义计算器
     * 使用 ONNX Embedding 模型计算向量相似度
     */
    class OnnxSemanticCalculator implements SemanticCalculator {
        private final top.yumbo.ai.omni.ai.onnx.OnnxEmbeddingService embeddingService;

        OnnxSemanticCalculator(top.yumbo.ai.omni.ai.onnx.OnnxEmbeddingService embeddingService) {
            this.embeddingService = embeddingService;
        }

        @Override
        public List<Double> calculateSimilarities(List<Paragraph> paragraphs) {
            List<Double> similarities = new ArrayList<>();

            // 计算所有段落的向量
            List<float[]> embeddings = paragraphs.stream()
                    .map(p -> {
                        try {
                            return embeddingService.embed(p.text);
                        } catch (Exception e) {
                            log.warn("⚠️ ONNX Embedding 失败: {}", e.getMessage());
                            return null;
                        }
                    })
                    .collect(Collectors.toList());

            // 计算相邻段落的余弦相似度
            for (int i = 0; i < embeddings.size() - 1; i++) {
                float[] vec1 = embeddings.get(i);
                float[] vec2 = embeddings.get(i + 1);

                if (vec1 != null && vec2 != null) {
                    double similarity = cosineSimilarity(vec1, vec2);
                    similarities.add(similarity);
                } else {
                    // 降级：使用固定相似度
                    similarities.add(1.0);
                }
            }

            return similarities;
        }

        /**
         * 计算余弦相似度
         */
        private double cosineSimilarity(float[] vec1, float[] vec2) {
            if (vec1.length != vec2.length) {
                return 0.0;
            }

            double dotProduct = 0.0;
            double norm1 = 0.0;
            double norm2 = 0.0;

            for (int i = 0; i < vec1.length; i++) {
                dotProduct += vec1[i] * vec2[i];
                norm1 += vec1[i] * vec1[i];
                norm2 += vec2[i] * vec2[i];
            }

            if (norm1 == 0.0 || norm2 == 0.0) {
                return 0.0;
            }

            return dotProduct / (Math.sqrt(norm1) * Math.sqrt(norm2));
        }
    }

    /**
     * AI 语义计算器
     * 使用大语言模型判断段落间的语义相关性
     */
    class AISemanticCalculator implements SemanticCalculator {
        private final top.yumbo.ai.omni.ai.api.AIService aiService;

        AISemanticCalculator(top.yumbo.ai.omni.ai.api.AIService aiService) {
            this.aiService = aiService;
        }

        @Override
        public List<Double> calculateSimilarities(List<Paragraph> paragraphs) {
            List<Double> similarities = new ArrayList<>();

            for (int i = 0; i < paragraphs.size() - 1; i++) {
                try {
                    // 使用 AI 判断相邻段落的语义相关性
                    String prompt = String.format(
                            "判断以下两个段落的语义相关性，返回 0-1 之间的数值（1表示高度相关，0表示不相关）：\n\n" +
                            "段落1：%s\n\n段落2：%s\n\n只返回数字，不要其他内容。",
                            paragraphs.get(i).text,
                            paragraphs.get(i + 1).text
                    );

                    String response = aiService.chat(prompt);
                    double similarity = parseScore(response);
                    similarities.add(similarity);

                } catch (Exception e) {
                    log.warn("⚠️ AI 计算语义相似度失败: {}", e.getMessage());
                    // 降级：使用固定相似度
                    similarities.add(1.0);
                }
            }

            return similarities;
        }

        /**
         * 解析 AI 返回的分数
         */
        private double parseScore(String response) {
            try {
                // 提取数字
                String cleaned = response.replaceAll("[^0-9.]", "");
                double score = Double.parseDouble(cleaned);
                return Math.max(0.0, Math.min(1.0, score));
            } catch (Exception e) {
                return 1.0; // 默认高相似度
            }
        }
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


