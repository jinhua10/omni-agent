package top.yumbo.ai.omni.chunking;

import java.util.List;

/**
 * 文档分块服务接口
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface ChunkingService {

    /**
     * 分块文档
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param config 分块配置
     * @return 分块列表
     */
    List<Chunk> chunk(String documentId, String content, ChunkingConfig config);

    /**
     * 获取支持的策略
     *
     * @return 支持的分块策略列表
     */
    List<ChunkingStrategy> getSupportedStrategies();

    /**
     * 获取默认策略
     *
     * @return 默认分块策略
     */
    ChunkingStrategy getDefaultStrategy();
}


