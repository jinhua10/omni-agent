package top.yumbo.ai.omni.knowledge.registry.network;

import top.yumbo.ai.omni.knowledge.registry.model.RefinedKnowledge;

import java.util.List;

/**
 * 知识关联服务接口
 *
 * <p>负责建立知识之间的关联关系</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface KnowledgeAssociationService {

    /**
     * 查找相关知识
     *
     * @param knowledgeId 知识ID
     * @param domainId 域ID
     * @param maxResults 最大结果数
     * @return 相关知识列表
     */
    List<RefinedKnowledge> findRelatedKnowledge(
            String knowledgeId,
            String domainId,
            int maxResults
    );

    /**
     * 跨域查找相关知识
     *
     * @param knowledgeId 知识ID
     * @param sourceDomainId 源域ID
     * @param targetDomainIds 目标域ID列表
     * @param maxResults 最大结果数
     * @return 相关知识列表
     */
    List<RefinedKnowledge> findCrossDomainRelatedKnowledge(
            String knowledgeId,
            String sourceDomainId,
            List<String> targetDomainIds,
            int maxResults
    );

    /**
     * 建立知识关联
     *
     * @param sourceKnowledgeId 源知识ID
     * @param targetKnowledgeId 目标知识ID
     * @param relationType 关联类型
     * @param strength 关联强度 (0.0-1.0)
     * @return 是否成功
     */
    boolean createAssociation(
            String sourceKnowledgeId,
            String targetKnowledgeId,
            String relationType,
            double strength
    );

    /**
     * 删除知识关联
     *
     * @param sourceKnowledgeId 源知识ID
     * @param targetKnowledgeId 目标知识ID
     * @return 是否成功
     */
    boolean removeAssociation(
            String sourceKnowledgeId,
            String targetKnowledgeId
    );
}

