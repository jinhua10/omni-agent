package top.yumbo.ai.omni.rag;

import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;
import top.yumbo.ai.omni.rag.model.IndexStatistics;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * RAG 服务接口（支持多域架构）
 * (RAG Service Interface - Multi-Domain Architecture)
 *
 * <p>提供文档检索、向量化和索引功能，支持知识网络多域隔离</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
public interface RagService {

    // ========== 核心检索 (Core Search) ==========

    /**
     * 语义搜索
     *
     * @param query 查询文本
     * @param maxResults 最大结果数
     * @return 匹配的文档列表（按相关性排序）
     */
    List<Document> semanticSearch(String query, int maxResults);

    /**
     * 向量搜索
     *
     * @param vector 查询向量
     * @param maxResults 最大结果数
     * @return 匹配的文档列表
     */
    List<Document> vectorSearch(Vector vector, int maxResults);

    // ========== 向量化 (Embedding) ==========

    /**
     * 文本向量化
     *
     * @param text 文本内容
     * @return 向量
     */
    Vector embed(String text);

    /**
     * 批量向量化
     *
     * @param texts 文本列表
     * @return 向量列表
     */
    List<Vector> batchEmbed(List<String> texts);

    // ========== 文档索引 (Document Indexing) ==========

    /**
     * 索引文档
     *
     * @param id 文档ID
     * @param vector 文档向量
     * @param metadata 元数据
     */
    void index(String id, Vector vector, Map<String, Object> metadata);

    /**
     * 批量索引
     *
     * @param documents 文档列表
     */
    void batchIndex(List<Document> documents);

    /**
     * 删除文档
     *
     * @param id 文档ID
     */
    void delete(String id);

    /**
     * 清空所有文档
     */
    default void clearAll() {
        throw new UnsupportedOperationException("clearAll() not implemented");
    }

    // ========== 域管理 (Domain Management) ==========

    /**
     * 获取域ID
     * ⭐ 核心方法：支持多域架构
     *
     * @return 域ID
     */
    String getDomainId();

    // ========== 文档管理 (Document Management) ==========

    /**
     * 获取文档
     *
     * @param documentId 文档ID
     * @return 文档对象
     */
    default Optional<Document> getDocument(String documentId) {
        return Optional.empty();
    }

    /**
     * 检查文档是否存在
     *
     * @param documentId 文档ID
     * @return 是否存在
     */
    default boolean documentExists(String documentId) {
        return getDocument(documentId).isPresent();
    }

    /**
     * 获取文档总数
     *
     * @return 文档总数
     */
    default long getDocumentCount() {
        return 0L;
    }

    /**
     * 获取所有文档（支持分页）
     *
     * @param offset 偏移量（从0开始）
     * @param limit 限制数量
     * @return 文档列表
     */
    default List<Document> getAllDocuments(int offset, int limit) {
        return List.of();
    }

    // ========== 统计与健康 (Statistics & Health) ==========

    /**
     * 获取索引统计信息
     *
     * @return 统计信息
     */
    default IndexStatistics getStatistics() {
        return IndexStatistics.builder()
                .totalDocuments(getDocumentCount())
                .healthy(isHealthy())
                .build();
    }

    /**
     * 健康检查
     *
     * @return 是否健康
     */
    default boolean isHealthy() {
        return true;
    }

    /**
     * 重建索引
     */
    default void rebuildIndex() {
        throw new UnsupportedOperationException("rebuildIndex() not implemented");
    }
}

