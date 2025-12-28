package top.yumbo.ai.omni.knowledge.registry.network;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
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

    /**
     * 查找相关域
     *
     * @param domainId 域ID
     * @param topK 返回Top K个相关域
     * @return 相关域列表
     */
    List<DomainAssociation> findRelatedDomains(String domainId, int topK);

    /**
     * 推荐知识域
     *
     * @param query 查询文本
     * @param topK 返回Top K个推荐
     * @return 推荐域列表
     */
    List<DomainRecommendation> recommendDomains(String query, int topK);

    /**
     * 域关联信息
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class DomainAssociation {
        /** 关联域ID */
        private String domainId;

        /** 关联域名称 */
        private String domainName;

        /** 关联强度 (0.0-1.0) */
        private double strength;

        /** 关联类型 */
        private String relationType;

        /** 共享知识数量 */
        private int sharedKnowledgeCount;
    }

    /**
     * 域推荐信息
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class DomainRecommendation {
        /** 推荐域ID */
        private String domainId;

        /** 推荐域名称 */
        private String domainName;

        /** 推荐分数 (0.0-1.0) */
        private double score;

        /** 推荐原因 */
        private String reason;

        /** 匹配的关键词 */
        private List<String> matchedKeywords;
    }
}


