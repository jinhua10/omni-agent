package top.yumbo.ai.omni.chunking.starter.strategy;

import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.chunking.ChunkingConfig;

import java.util.List;

/**
 * 分块策略执行器接口
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface ChunkingStrategyExecutor {

    /**
     * 执行分块
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param config 分块配置
     * @return 分块列表
     */
    List<Chunk> execute(String documentId, String content, ChunkingConfig config);
}

