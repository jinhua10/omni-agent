package top.yumbo.ai.omni.marketplace.strategy.adapters;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.strategy.ChunkingStrategyExecutor;
import top.yumbo.ai.omni.marketplace.strategy.AbstractMarketplaceStrategy;
import top.yumbo.ai.omni.marketplace.strategy.StrategyExecutionException;
import top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.*;
import top.yumbo.ai.omni.marketplace.strategy.adapters.model.ChunkingInput;
import top.yumbo.ai.omni.marketplace.strategy.adapters.model.ChunkingOutput;

import java.util.List;
import java.util.Map;

/**
 * 分块策略适配器基类
 *
 * 将新的 ChunkingStrategyExecutor 适配到 MarketplaceStrategy 接口
 *
 * 使用适配器模式，保持现有分块策略不变，同时支持市场接口
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public abstract class ChunkingStrategyAdapter extends AbstractMarketplaceStrategy {

    /** 被适配的分块策略执行器 */
    protected final ChunkingStrategyExecutor executor;

    /** 策略类型 */
    protected final ChunkingStrategy strategyType;

    protected ChunkingStrategyAdapter(ChunkingStrategyExecutor executor, ChunkingStrategy strategyType) {
        this.executor = executor;
        this.strategyType = strategyType;
        log.debug("创建分块策略适配器: {}", strategyType);
    }

    @Override
    public StrategyCategory getCategory() {
        return StrategyCategory.CHUNKING;
    }

    @Override
    public String getStrategyId() {
        // 使用策略类型生成ID
        return "top.yumbo.ai.omni.chunking." + strategyType.name().toLowerCase() + ".v1";
    }

    @Override
    public String getStrategyName() {
        return strategyType.name().toLowerCase();
    }

    @Override
    public String getDescription() {
        return strategyType.getDescription();
    }

    @Override
    public AuthorInfo getAuthor() {
        return AuthorInfo.builder()
                .name("OmniAgent Team")
                .email("1015770492@qq.com")
                .organization("OmniAgent")
                .homepage("https://github.com/jinhua10/omni-agent")
                .build();
    }

    @Override
    public List<String> getTags() {
        return List.of("chunking", strategyType.name().toLowerCase(), "built-in", "text-processing");
    }

    @Override
    protected <I, O> O doExecute(I input, Map<String, Object> params, ExecutionContext context)
            throws StrategyExecutionException {

        try {
            // 验证输入类型
            if (!(input instanceof ChunkingInput)) {
                throw new StrategyExecutionException(
                        getStrategyId(),
                        StrategyExecutionException.ExecutionErrorCode.INVALID_INPUT,
                        "Input must be ChunkingInput, got: " + (input != null ? input.getClass().getName() : "null")
                );
            }

            ChunkingInput chunkingInput = (ChunkingInput) input;

            // 构建分块配置
            ChunkingConfig config = buildConfig(params);

            // 调用策略执行器
            List<Chunk> chunks = executor.execute(
                    chunkingInput.getDocumentId(),
                    chunkingInput.getContent(),
                    config
            );

            log.debug("分块完成: documentId={}, chunks={}", chunkingInput.getDocumentId(), chunks.size());

            // 返回结果
            return (O) new ChunkingOutput(chunks);

        } catch (Exception e) {
            throw new StrategyExecutionException(
                    getStrategyId(),
                    StrategyExecutionException.ExecutionErrorCode.INTERNAL_ERROR,
                    "分块执行失败: " + e.getMessage(),
                    e
            );
        }
    }

    /**
     * 从参数构建分块配置
     */
    protected ChunkingConfig buildConfig(Map<String, Object> params) {
        if (params == null || params.isEmpty()) {
            return ChunkingConfig.builder()
                    .strategy(strategyType)
                    .build();
        }

        ChunkingConfig.ChunkingConfigBuilder builder = ChunkingConfig.builder()
                .strategy(strategyType);

        // 通用参数
        if (params.containsKey("maxChunkSize")) {
            builder.maxChunkSize((Integer) params.get("maxChunkSize"));
        }
        if (params.containsKey("minChunkSize")) {
            builder.minChunkSize((Integer) params.get("minChunkSize"));
        }

        // 固定长度策略参数
        if (params.containsKey("chunkSize")) {
            builder.fixedLengthSize((Integer) params.get("chunkSize"));
        }
        if (params.containsKey("overlapSize") || params.containsKey("overlap")) {
            Object overlap = params.getOrDefault("overlapSize", params.get("overlap"));
            builder.overlap((Integer) overlap);
        }

        // 语义分块参数
        if (params.containsKey("similarityThreshold")) {
            builder.semanticThreshold(((Number) params.get("similarityThreshold")).doubleValue());
        }

        return builder.build();
    }

    @Override
    public SecurityLevel getSecurityLevel() {
        return SecurityLevel.SAFE;  // 内置策略，完全安全
    }
}

