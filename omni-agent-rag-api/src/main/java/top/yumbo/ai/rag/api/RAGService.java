package top.yumbo.ai.rag.api;

import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.Query;
import top.yumbo.ai.rag.api.model.SearchResult;
import top.yumbo.ai.rag.api.model.IndexStatistics;

import java.util.List;

/**
 * RAG 服务核心接口
 * (RAG - Retrieval-Augmented Generation Service Interface)
 *
 * <p>用于文档索引、向量检索、全文搜索等功能</p>
 * <p>支持多种后端: File(Lucene), H2, SQLite, Redis, MongoDB, Elasticsearch</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface RAGService {

    // ========== 文档索引 (Document Indexing) ==========

    /**
     * 索引单个文档
     * @param document 文档对象
     * @return 文档ID
     */
    String indexDocument(Document document);

    /**
     * 批量索引文档
     * @param documents 文档列表
     * @return 成功索引的文档ID列表
     */
    List<String> indexDocuments(List<Document> documents);

    /**
     * 更新文档
     * @param document 文档对象
     * @return 是否成功
     */
    boolean updateDocument(Document document);

    /**
     * 删除文档
     * @param documentId 文档ID
     * @return 是否成功
     */
    boolean deleteDocument(String documentId);

    /**
     * 清空所有索引
     */
    void clearAll();

    // ========== 文本搜索 (Text Search) ==========

    /**
     * 全文搜索
     * @param query 查询对象
     * @return 搜索结果列表
     */
    List<SearchResult> search(Query query);

    /**
     * 简单文本搜索
     * @param text 查询文本
     * @param topK 返回前K个结果
     * @return 搜索结果列表
     */
    List<SearchResult> searchByText(String text, int topK);

    // ========== 向量搜索 (Vector Search) ==========

    /**
     * 向量相似度搜索
     * @param embedding 向量
     * @param topK 返回前K个结果
     * @return 搜索结果列表
     */
    List<SearchResult> vectorSearch(float[] embedding, int topK);

    /**
     * 向量相似度搜索（带过滤条件）
     * @param embedding 向量
     * @param topK 返回前K个结果
     * @param filters 过滤条件
     * @return 搜索结果列表
     */
    List<SearchResult> vectorSearch(float[] embedding, int topK,
                                    java.util.Map<String, Object> filters);

    // ========== 混合检索 (Hybrid Search) ==========

    /**
     * 混合检索（文本 + 向量）
     * @param query 查询对象（包含文本和向量）
     * @return 搜索结果列表
     */
    List<SearchResult> hybridSearch(Query query);

    /**
     * 混合检索（指定权重）
     * @param text 查询文本
     * @param embedding 向量
     * @param textWeight 文本权重 (0-1)
     * @param vectorWeight 向量权重 (0-1)
     * @param topK 返回前K个结果
     * @return 搜索结果列表
     */
    List<SearchResult> hybridSearch(String text, float[] embedding,
                                    float textWeight, float vectorWeight,
                                    int topK);

    // ========== 语义搜索 (Semantic Search) ==========

    /**
     * 语义搜索（自动生成向量）
     * @param text 查询文本
     * @param topK 返回前K个结果
     * @return 搜索结果列表
     */
    List<SearchResult> semanticSearch(String text, int topK);

    // ========== 文档管理 (Document Management) ==========

    /**
     * 获取文档
     * @param documentId 文档ID
     * @return 文档对象
     */
    java.util.Optional<Document> getDocument(String documentId);

    /**
     * 检查文档是否存在
     * @param documentId 文档ID
     * @return 是否存在
     */
    boolean documentExists(String documentId);

    /**
     * 获取文档总数
     * @return 文档总数
     */
    long getDocumentCount();

    /**
     * 获取所有文档（支持分页）
     * @param offset 偏移量（从0开始）
     * @param limit 限制数量
     * @return 文档列表
     */
    List<Document> getAllDocuments(int offset, int limit);

    // ========== 统计与健康 (Statistics & Health) ==========

    /**
     * 获取索引统计信息
     * @return 统计信息
     */
    IndexStatistics getStatistics();

    /**
     * 获取健康状态
     * @return 健康状态
     */
    boolean isHealthy();

    /**
     * 重建索引
     */
    void rebuildIndex();
}

