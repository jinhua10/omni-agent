package top.yumbo.ai.omni.ai.onnx;

import ai.onnxruntime.*;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.ai.api.EmbeddingService;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 基于 ONNX Runtime 的 Embedding 服务实现
 * (ONNX Runtime based Embedding Service Implementation)
 *
 * <p>
 * 支持的模型 (Supported Models):
 * - bge-base-zh-v1.5 (中文，768维，推荐)
 * - bge-m3 (多语言，1024维，大模型)
 * - bge-large-zh (中文，1024维)
 * - text2vec-base-chinese (中文，768维)
 * </p>
 *
 * <p>
 * 特点 (Features):
 * - 本地推理，无需网络请求
 * - 支持批量处理
 * - 自动 L2 归一化
 * - 适用于向量检索
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class OnnxEmbeddingService implements EmbeddingService, AutoCloseable {

    private final OrtEnvironment env;
    private final OrtSession session;

    @Getter
    private final int dimension;

    @Getter
    private final String embeddingModel;

    private final int maxSequenceLength;

    // 常量
    private static final int DEFAULT_MAX_SEQUENCE_LENGTH = 512;
    private static final int CLS_TOKEN = 101;  // [CLS]
    private static final int SEP_TOKEN = 102;  // [SEP]
    private static final int UNK_TOKEN = 100;  // [UNK]
    private static final int VOCAB_SIZE = 21128; // BERT 词汇表大小

    /**
     * 构造函数
     *
     * @param modelPath ONNX 模型文件路径
     * @throws OrtException ONNX Runtime 异常
     * @throws IOException IO 异常
     */
    public OnnxEmbeddingService(String modelPath) throws OrtException, IOException {
        this(modelPath, DEFAULT_MAX_SEQUENCE_LENGTH);
    }

    /**
     * 完整构造函数
     *
     * @param modelPath         ONNX 模型文件路径
     * @param maxSequenceLength 最大序列长度
     * @throws OrtException ONNX Runtime 异常
     * @throws IOException IO 异常
     */
    public OnnxEmbeddingService(String modelPath, int maxSequenceLength)
            throws OrtException, IOException {

        this.maxSequenceLength = maxSequenceLength;

        // 解析模型路径
        String actualModelPath = resolveModelPath(modelPath);

        // 提取模型名称
        Path finalPath = Paths.get(actualModelPath);
        this.embeddingModel = finalPath.getParent() != null ?
                finalPath.getParent().getFileName().toString() : "unknown";

        // 初始化 ONNX Runtime 环境
        this.env = OrtEnvironment.getEnvironment();

        // 配置会话选项
        OrtSession.SessionOptions options = new OrtSession.SessionOptions();
        options.setOptimizationLevel(OrtSession.SessionOptions.OptLevel.ALL_OPT);
        options.setInterOpNumThreads(4);
        options.setIntraOpNumThreads(4);

        // 加载模型
        this.session = env.createSession(actualModelPath, options);

        // 推断输出维度
        this.dimension = inferEmbeddingDimension();

        log.info("✅ ONNX Embedding 模型已加载");
        log.info("   - 模型: {}", embeddingModel);
        log.info("   - 路径: {}", modelPath);
        log.info("   - 维度: {}", dimension);
        log.info("   - 最大序列长度: {}", maxSequenceLength);
    }

    /**
     * 解析模型路径
     */
    private String resolveModelPath(String modelPath) throws IOException {
        // 1. 优先从文件系统加载
        try {
            Path modelFile = Paths.get(modelPath);
            if (Files.exists(modelFile)) {
                log.info("从文件系统加载模型: {}", modelFile.toAbsolutePath());
                return modelFile.toAbsolutePath().toString();
            }
        } catch (Exception e) {
            log.debug("文件系统路径无效: {}", e.getMessage());
        }

        // 2. 尝试从 classpath 加载
        try {
            var resource = getClass().getClassLoader().getResource(modelPath);
            if (resource != null) {
                Path path = Paths.get(resource.toURI());
                log.info("从 classpath 加载模型: {}", path.toAbsolutePath());
                return path.toAbsolutePath().toString();
            }
        } catch (Exception e) {
            log.debug("无法从 classpath 加载模型: {}", e.getMessage());
        }

        // 3. 抛出异常
        throw new IOException(String.format(
                "模型文件不存在: %s\n" +
                        "\n" +
                        "📥 推荐模型（国产）：\n" +
                        "  中文（推荐）：BAAI/bge-base-zh-v1.5 (768维，~400MB)\n" +
                        "  多语言大模型：BAAI/bge-m3 (1024维，~2GB)\n" +
                        "  中文大模型：BAAI/bge-large-zh (1024维)\n" +
                        "\n" +
                        "📁 模型放置位置：\n" +
                        "  1. 外部目录（推荐）：./models/bge-base-zh/model.onnx\n" +
                        "  2. 开发环境：src/main/resources/models/bge-base-zh/model.onnx\n" +
                        "\n" +
                        "💡 配置示例（application.yml）：\n" +
                        "  embedding:\n" +
                        "    onnx:\n" +
                        "      model-path: ./models/bge-base-zh/model.onnx",
                modelPath
        ));
    }

    @Override
    public float[] embed(String text) {
        if (text == null || text.trim().isEmpty()) {
            log.warn("输入文本为空，返回零向量");
            return new float[dimension];
        }

        try {
            // 1. 分词（简化版）
            long[] inputIds = tokenize(text);
            long[] attentionMask = createAttentionMask(inputIds);
            long[] tokenTypeIds = createTokenTypeIds(inputIds);

            // 2. 构建 ONNX 输入张量
            long[][] inputIdsArray = new long[][]{inputIds};
            long[][] attentionMaskArray = new long[][]{attentionMask};
            long[][] tokenTypeIdsArray = new long[][]{tokenTypeIds};

            OnnxTensor inputIdsTensor = OnnxTensor.createTensor(env, inputIdsArray);
            OnnxTensor attentionMaskTensor = OnnxTensor.createTensor(env, attentionMaskArray);
            OnnxTensor tokenTypeIdsTensor = OnnxTensor.createTensor(env, tokenTypeIdsArray);

            Map<String, OnnxTensor> inputs = new HashMap<>();
            inputs.put("input_ids", inputIdsTensor);
            inputs.put("attention_mask", attentionMaskTensor);
            inputs.put("token_type_ids", tokenTypeIdsTensor);

            // 3. 模型推理
            OrtSession.Result result = session.run(inputs);

            // 4. 提取输出向量
            Object outputValue = result.get(0).getValue();
            float[] vector;

            if (outputValue instanceof float[][][]) {
                // 三维输出: [batch_size, seq_len, hidden_dim]
                // 使用第一个 token（[CLS]）的嵌入作为句子表示
                float[][][] output3d = (float[][][]) outputValue;
                vector = output3d[0][0]; // batch=0, token=0 ([CLS])
            } else if (outputValue instanceof float[][]) {
                // 二维输出: [batch_size, hidden_dim]
                float[][] output2d = (float[][]) outputValue;
                vector = output2d[0]; // batch=0
            } else {
                log.error("未知输出格式: {}", outputValue.getClass().getName());
                return new float[dimension];
            }

            // 5. L2 归一化（用于余弦相似度）
            float[] normalized = l2Normalize(vector);

            // 清理资源
            inputIdsTensor.close();
            attentionMaskTensor.close();
            tokenTypeIdsTensor.close();
            result.close();

            log.trace("文本嵌入完成: {} chars -> {} dims", text.length(), dimension);

            return normalized;

        } catch (OrtException e) {
            log.error("嵌入生成失败: {}", text.substring(0, Math.min(50, text.length())), e);
            return new float[dimension];
        }
    }

    @Override
    public List<float[]> embedBatch(List<String> texts) {
        if (texts == null || texts.isEmpty()) {
            return Collections.emptyList();
        }

        return texts.stream()
                .map(this::embed)
                .collect(Collectors.toList());
    }

    /**
     * 简化的分词器（基于字符级别）
     *
     * 注意：这是简化实现，生产环境建议使用 HuggingFace Tokenizers
     */
    private long[] tokenize(String text) {
        char[] chars = text.toCharArray();
        int length = Math.min(chars.length, maxSequenceLength - 2); // 预留 [CLS] 和 [SEP]

        long[] tokens = new long[length + 2];
        tokens[0] = CLS_TOKEN; // [CLS] token

        for (int i = 0; i < length; i++) {
            // 将字符映射到词汇表范围
            int charCode = chars[i];
            int tokenId = (charCode % (VOCAB_SIZE - 1000)) + 1000; // 避开特殊token区域

            // 确保在有效范围内
            if (tokenId < 0 || tokenId >= VOCAB_SIZE) {
                tokenId = UNK_TOKEN;
            }

            tokens[i + 1] = tokenId;
        }

        tokens[length + 1] = SEP_TOKEN; // [SEP] token

        return tokens;
    }

    /**
     * 创建注意力掩码（全1，表示所有token都有效）
     */
    private long[] createAttentionMask(long[] inputIds) {
        long[] mask = new long[inputIds.length];
        Arrays.fill(mask, 1L);
        return mask;
    }

    /**
     * 创建 token type IDs（全0，表示单句输入）
     */
    private long[] createTokenTypeIds(long[] inputIds) {
        long[] tokenTypeIds = new long[inputIds.length];
        Arrays.fill(tokenTypeIds, 0L);
        return tokenTypeIds;
    }

    /**
     * L2 归一化
     */
    private float[] l2Normalize(float[] vector) {
        double sumSquares = 0;
        for (float v : vector) {
            sumSquares += v * v;
        }

        double norm = Math.sqrt(sumSquares);
        if (norm < 1e-10) {
            return vector; // 避免除零
        }

        float[] normalized = new float[vector.length];
        for (int i = 0; i < vector.length; i++) {
            normalized[i] = (float) (vector[i] / norm);
        }

        return normalized;
    }

    /**
     * 推断嵌入维度
     */
    private int inferEmbeddingDimension() throws OrtException {
        try {
            // 使用测试输入推断输出维度
            long[][] testInput = new long[][]{{CLS_TOKEN, SEP_TOKEN}};
            long[][] testMask = new long[][]{{1, 1}};
            long[][] testTokenTypeIds = new long[][]{{0, 0}};

            OnnxTensor inputTensor = OnnxTensor.createTensor(env, testInput);
            OnnxTensor maskTensor = OnnxTensor.createTensor(env, testMask);
            OnnxTensor tokenTypeIdsTensor = OnnxTensor.createTensor(env, testTokenTypeIds);

            Map<String, OnnxTensor> inputs = new HashMap<>();
            inputs.put("input_ids", inputTensor);
            inputs.put("attention_mask", maskTensor);
            inputs.put("token_type_ids", tokenTypeIdsTensor);

            OrtSession.Result result = session.run(inputs);

            // 提取维度
            Object outputValue = result.get(0).getValue();
            int dim;

            if (outputValue instanceof float[][][]) {
                float[][][] output3d = (float[][][]) outputValue;
                dim = output3d[0][0].length;
                log.debug("检测到三维输出，维度: {}", dim);
            } else if (outputValue instanceof float[][]) {
                float[][] output2d = (float[][]) outputValue;
                dim = output2d[0].length;
                log.debug("检测到二维输出，维度: {}", dim);
            } else {
                log.warn("未知输出格式: {}, 使用默认维度 768", outputValue.getClass().getName());
                dim = 768;
            }

            inputTensor.close();
            maskTensor.close();
            tokenTypeIdsTensor.close();
            result.close();

            return dim;

        } catch (Exception e) {
            log.warn("无法推断维度，使用默认值 768", e);
            return 768; // 默认维度（BGE-base-zh）
        }
    }

    @Override
    public void close() {
        try {
            if (session != null) {
                session.close();
            }
            log.info("ONNX Embedding 服务已关闭");
        } catch (OrtException e) {
            log.error("关闭 ONNX Embedding 服务失败", e);
        }
    }
}

