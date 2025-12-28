package top.yumbo.ai.omni.knowledge.registry.service;

import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDocument;

import java.util.List;

/**
 * 知识提取服务接口
 *
 * <p>从知识域中提取文档和知识</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface KnowledgeExtractionService {

    /**
     * 从指定域提取文档
     *
     * @param domainId 域ID
     * @param maxResults 最大结果数
     * @return 文档列表
     */
    List<KnowledgeDocument> extractDocumentsFromDomain(
            String domainId,
            int maxResults
    );

    /**
     * 根据查询提取相关文档
     *
     * @param query 查询字符串
     * @param domainIds 要搜索的域ID列表
     * @param maxResults 最大结果数
     * @return 文档列表
     */
    List<KnowledgeDocument> extractDocumentsByQuery(
            String query,
            List<String> domainIds,
            int maxResults
    );

    /**
     * 提取指定文档的详细信息
     *
     * @param documentId 文档ID
     * @param domainId 域ID
     * @return 文档详情
     */
    KnowledgeDocument extractDocumentDetails(
            String documentId,
            String domainId
    );
}

