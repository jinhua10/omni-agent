package top.yumbo.ai.omni.rag;

import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;

import java.util.List;
import java.util.Map;

/**
 * RAG 服务接口
 *
 * <p>提供文档检索、向量化和索引功能</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface RagService {

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
     * 获取域ID
     *
     * @return 域ID
     */
    String getDomainId();
}

