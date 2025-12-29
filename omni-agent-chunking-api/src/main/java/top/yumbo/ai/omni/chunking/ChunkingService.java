package top.yumbo.ai.omni.chunking;

import java.util.List;

/**
 * 文档分块服务接口
 *
 * <p>提供完整的文档分块管理功能，包括：</p>
 * <ul>
 *   <li>智能分块：根据策略将文档切分为多个片段</li>
 *   <li>分块存储：将分块结果持久化</li>
 *   <li>分块查询：检索文档的分块数据</li>
 *   <li>分块管理：更新和删除分块</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface ChunkingService {

    // ========== 核心分块方法 ==========

    /**
     * 分块文档（不存储）
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param config 分块配置
     * @return 分块列表
     */
    List<Chunk> chunk(String documentId, String content, ChunkingConfig config);

    /**
     * 分块文档并存储
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @return 分块ID列表
     */
    List<String> chunkAndStore(String documentId, String content);

    /**
     * 分块文档并存储（使用指定配置）
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param config 分块配置
     * @return 分块ID列表
     */
    List<String> chunkAndStore(String documentId, String content, ChunkingConfig config);

    // ========== 智能分块方法 ==========

    /**
     * 自动选择策略分块（不存储）
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param fileName 文件名（用于推断文档类型）
     * @return 分块列表
     */
    List<Chunk> chunkWithAutoStrategy(String documentId, String content, String fileName);

    /**
     * 使用指定策略分块（不存储）
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param strategy 分块策略
     * @param config 分块配置
     * @return 分块列表
     */
    List<Chunk> chunkWithStrategy(String documentId, String content,
                                   ChunkingStrategy strategy, ChunkingConfig config);

    // ========== 查询方法 ==========

    /**
     * 获取文档的所有分块
     *
     * @param documentId 文档ID
     * @return 分块列表
     */
    List<Chunk> getChunks(String documentId);

    /**
     * 获取单个分块
     *
     * @param chunkId 分块ID
     * @return 分块对象，如果不存在则返回 null
     */
    Chunk getChunk(String chunkId);

    // ========== 删除方法 ==========

    /**
     * 删除文档的所有分块
     *
     * @param documentId 文档ID
     */
    void deleteChunks(String documentId);

    /**
     * 删除单个分块
     *
     * @param chunkId 分块ID
     */
    void deleteChunk(String chunkId);

    // ========== 更新方法 ==========

    /**
     * 重新切分和存储文档
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @return 分块ID列表
     */
    List<String> rechunkAndStore(String documentId, String content);

    /**
     * 重新切分和存储文档（使用指定配置）
     *
     * @param documentId 文档ID
     * @param content 文档内容
     * @param config 分块配置
     * @return 分块ID列表
     */
    List<String> rechunkAndStore(String documentId, String content, ChunkingConfig config);

    // ========== 策略管理 ==========

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


