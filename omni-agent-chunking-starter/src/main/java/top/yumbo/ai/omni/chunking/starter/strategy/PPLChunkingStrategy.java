package top.yumbo.ai.omni.chunking.starter.strategy;

import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;
import top.yumbo.ai.omni.chunking.starter.config.ChunkingProperties;

import java.util.Collections;
import java.util.List;

/**
 * PPL 分块策略（占位实现）
 *
 * <p>实际实现需要从 core/old/chunking 迁移</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class PPLChunkingStrategy implements ChunkingStrategyExecutor {

    private final ChunkingProperties properties;

    public PPLChunkingStrategy(ChunkingProperties properties) {
        this.properties = properties;
        // TODO: 初始化 PPL 模型
    }

    @Override
    public List<Chunk> execute(String documentId, String content, ChunkingConfig config) {
        // TODO: 从 core/old/chunking 迁移 PPL 分块实现
        // 暂时返回空列表
        return Collections.emptyList();
    }
}

