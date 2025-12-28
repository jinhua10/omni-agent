package top.yumbo.ai.omni.chunking.starter.strategy;

import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;

import java.util.Collections;
import java.util.List;

/**
 * 语义分块策略（占位实现）
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class SemanticStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;

    public SemanticStrategy(ChunkingProperties properties) {
        this.properties = properties;
        // TODO: 初始化语义模型
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        // TODO: 实现语义分块
        // 暂时返回空列表
        return Collections.emptyList();
    }
}

