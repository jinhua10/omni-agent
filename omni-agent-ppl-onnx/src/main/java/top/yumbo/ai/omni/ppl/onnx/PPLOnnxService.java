package top.yumbo.ai.omni.ppl.onnx;

import ai.djl.huggingface.tokenizers.Encoding;
import ai.djl.huggingface.tokenizers.HuggingFaceTokenizer;
import ai.onnxruntime.*;
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.ai.onnx.SharedOnnxModelManager;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;

import java.nio.file.Paths;
import java.time.Duration;
import java.util.*;

/**
 * 基于 ONNX Runtime 的 PPL 服务实现（简化版）
 * <p>
 * 用于计算文本的困惑度（Perplexity）
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
@ConditionalOnProperty(prefix = "omni-agent.chunking.ppl.onnx", name = "enabled", havingValue = "true")
public class PPLOnnxService {

    @Value("${omni-agent.chunking.ppl.onnx.model-path}")
    private String modelPath;

    @Value("${omni-agent.chunking.ppl.onnx.tokenizer-path}")
    private String tokenizerPath;

    @Value("${omni-agent.chunking.ppl.onnx.use-cache:true}")
    private boolean useCache;

    @Value("${omni-agent.chunking.ppl.onnx.cache-size:1000}")
    private int cacheSize;

    @Value("${omni-agent.chunking.ppl.onnx.cache-ttl:3600}")
    private int cacheTtl;

    @Autowired(required = false)
    private SharedOnnxModelManager sharedModelManager;

    // ONNX Runtime 组件
    private OrtEnvironment env;
    private OrtSession session;
    private HuggingFaceTokenizer tokenizer;
    private boolean useSharedModel;

    // PPL 缓存
    private Cache<String, Double> pplCache;

    @PostConstruct
    public void init() {
        log.info("🚀 初始化 ONNX PPL 服务");

        try {
            // 1. 检查是否使用共享模型管理器
            if (sharedModelManager != null) {
                useSharedModel = true;
                SharedOnnxModelManager.ModelInfo modelInfo = sharedModelManager.getOrCreateSession(modelPath);
                this.env = sharedModelManager.getEnvironment();
                this.session = modelInfo.getSession();
                log.info("✅ ONNX 模型加载成功（共享模式）: {}", modelPath);
            } else {
                useSharedModel = false;
                // 独立模式：自己创建环境和session
                this.env = OrtEnvironment.getEnvironment();
                log.info("✅ ONNX Environment 创建成功");

                // 2. 加载 ONNX 模型
                OrtSession.SessionOptions sessionOptions = new OrtSession.SessionOptions();
                sessionOptions.setOptimizationLevel(OrtSession.SessionOptions.OptLevel.BASIC_OPT);

                this.session = env.createSession(modelPath, sessionOptions);
                log.info("✅ ONNX 模型加载成功（独立模式）: {}", modelPath);
            }

            // 3. 加载 Tokenizer
            this.tokenizer = HuggingFaceTokenizer.newInstance(Paths.get(tokenizerPath));
            log.info("✅ Tokenizer 加载成功: {}", tokenizerPath);

            // 4. 初始化缓存
            if (useCache) {
                this.pplCache = Caffeine.newBuilder()
                        .maximumSize(cacheSize)
                        .expireAfterWrite(Duration.ofSeconds(cacheTtl))
                        .recordStats()
                        .build();
                log.info("✅ PPL 缓存初始化: size={}, ttl={}s", cacheSize, cacheTtl);
            }

            log.info("🎉 ONNX PPL 服务初始化完成（共享模式: {}）", useSharedModel);

        } catch (Exception e) {
            log.error("❌ ONNX PPL 服务初始化失败", e);
            throw new RuntimeException("ONNX PPL 服务初始化失败", e);
        }
    }

    /**
     * 计算文本的困惑度
     */
    public double calculatePerplexity(String text) {
        if (text == null || text.trim().isEmpty()) {
            return Double.MAX_VALUE;
        }

        // 检查缓存
        if (pplCache != null) {
            Double cached = pplCache.getIfPresent(text);
            if (cached != null) {
                return cached;
            }
        }

        List<OnnxTensor> tensorsToClose = new ArrayList<>();

        try {
            // 1. Tokenize
            Encoding encoding = tokenizer.encode(text);
            long[] inputIds = encoding.getIds();
            long[] attentionMask = encoding.getAttentionMask();

            if (inputIds.length == 0) {
                return Double.MAX_VALUE;
            }

            // 2. 准备 ONNX 输入
            Map<String, OnnxTensor> inputs = new HashMap<>();
            int seqLen = inputIds.length;

            // 转换为 [1, seq_len] 的张量
            long[][] inputIdsArray = new long[1][seqLen];
            inputIdsArray[0] = inputIds;

            long[][] attentionMaskArray = new long[1][seqLen];
            attentionMaskArray[0] = attentionMask;

            OnnxTensor inputIdsTensor = OnnxTensor.createTensor(env, inputIdsArray);
            OnnxTensor attentionMaskTensor = OnnxTensor.createTensor(env, attentionMaskArray);

            tensorsToClose.add(inputIdsTensor);
            tensorsToClose.add(attentionMaskTensor);

            inputs.put("input_ids", inputIdsTensor);
            inputs.put("attention_mask", attentionMaskTensor);

            // 3. 模型推理
            try (OrtSession.Result results = session.run(inputs)) {
                // 获取 logits
                OnnxValue logitsValue = results.get(0);
                float[][][] logits = (float[][][]) logitsValue.getValue();

                // 4. 计算困惑度
                double totalLoss = 0.0;
                int validTokens = 0;

                // 对每个位置计算 cross-entropy loss
                for (int i = 0; i < inputIds.length - 1; i++) {
                    int targetId = (int) inputIds[i + 1];
                    float[] probs = logits[0][i];

                    // Softmax 归一化
                    float maxLogit = Float.NEGATIVE_INFINITY;
                    for (float logit : probs) {
                        maxLogit = Math.max(maxLogit, logit);
                    }

                    double sumExp = 0.0;
                    for (float logit : probs) {
                        sumExp += Math.exp(logit - maxLogit);
                    }

                    double logProb = probs[targetId] - maxLogit - Math.log(sumExp);
                    totalLoss -= logProb;
                    validTokens++;
                }

                // PPL = exp(average loss)
                double ppl = validTokens > 0 ? Math.exp(totalLoss / validTokens) : Double.MAX_VALUE;

                // 清理资源
                for (OnnxTensor tensor : tensorsToClose) {
                    try {
                        tensor.close();
                    } catch (Exception ignored) {
                    }
                }

                // 缓存结果
                if (pplCache != null) {
                    pplCache.put(text, ppl);
                }

                return ppl;
            }

        } catch (Exception e) {
            log.error("计算困惑度失败: {}", e.getMessage(), e);
            return Double.MAX_VALUE;
        }
    }

    /**
     * 健康检查
     */
    public boolean isHealthy() {
        return env != null && session != null && tokenizer != null;
    }

    @PreDestroy
    public void destroy() {
        try {
            if (useSharedModel && sharedModelManager != null) {
                // 共享模式：释放模型引用
                sharedModelManager.releaseSession(modelPath);
                log.info("✅ ONNX PPL 服务已关闭（共享模式）");
            } else {
                // 独立模式：直接关闭资源
                if (session != null) {
                    session.close();
                }
                if (env != null) {
                    env.close();
                }
                log.info("✅ ONNX PPL 服务已关闭（独立模式）");
            }
        } catch (Exception e) {
            log.error("关闭 ONNX PPL 服务失败", e);
        }
    }
}

