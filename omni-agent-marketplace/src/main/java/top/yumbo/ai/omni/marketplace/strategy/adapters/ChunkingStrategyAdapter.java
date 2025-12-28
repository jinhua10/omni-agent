package top.yumbo.ai.omni.marketplace.strategy.adapters;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
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
 * 将现有的 ChunkingStrategy 适配到 MarketplaceStrategy 接口
 *
 * 使用适配器模式，保持现有分块策略不变，同时支持市场接口
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public abstract class ChunkingStrategyAdapter extends AbstractMarketplaceStrategy {

    /** 被适配的分块策略 */
    protected final ChunkingStrategy delegate;

    protected ChunkingStrategyAdapter(ChunkingStrategy delegate) {
        this.delegate = delegate;
        log.debug("创建分块策略适配器: {}", delegate.name());
    }

    @Override
    public StrategyCategory getCategory() {
        return StrategyCategory.CHUNKING;
    }

    @Override
    public String getStrategyId() {
        // 使用原策略名称生成ID
        return "top.yumbo.ai.omni.chunking." + delegate.name() + ".v1";
    }

    @Override
    public String getStrategyName() {
        return delegate.name();
    }

    @Override
    public String getDescription() {
        return delegate.getDescription();
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
    public Map<String, Object> getDefaultParameters() {
        return delegate.getDefaultParams();
    }

    @Override
    public List<String> getTags() {
        return List.of("chunking", delegate.name(), "built-in", "text-processing");
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

            // 调用原分块策略
            List<Chunk> chunks = delegate.chunk(
                    chunkingInput.getDocumentId(),
                    chunkingInput.getContent(),
                    params
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

    @Override
    public SecurityLevel getSecurityLevel() {
        return SecurityLevel.SAFE;  // 内置策略，完全安全
    }


}

