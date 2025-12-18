package top.yumbo.ai.omni.core.chunking.strategy;

import top.yumbo.ai.storage.api.model.Chunk;

import java.util.List;
import java.util.Map;

/**
 * 文档分块策略接口
 * <p>
 * 定义文档分块的统一接口，支持多种分块算法：
 * - 固定大小分块
 * - 语义感知分块
 * - PPL增强分块
 * - 结构化分块
 * - 算法市场自定义策略
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface ChunkingStrategy {

    /**
     * 对文档进行分块
     *
     * @param documentId 文档ID
     * @param content    文档内容
     * @param params     分块参数（可选）
     * @return 分块列表
     */
    List<Chunk> chunk(String documentId, String content, Map<String, Object> params);

    /**
     * 使用默认参数分块
     */
    default List<Chunk> chunk(String documentId, String content) {
        return chunk(documentId, content, null);
    }

    /**
     * 获取策略名称
     */
    String getStrategyName();

    /**
     * 获取策略描述
     */
    String getDescription();

    /**
     * 获取默认参数
     */
    Map<String, Object> getDefaultParams();
}

