package top.yumbo.ai.omni.core.service.knowledge;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 知识关联服务
 * (Knowledge Association Service)
 *
 * <p>发现和管理知识域之间的关联关系</p>
 *
 * <p>核心功能：</p>
 * <ul>
 *     <li>发现相关域 - 基于内容相似度</li>
 *     <li>推荐知识域 - 基于查询历史</li>
 *     <li>知识迁移 - 在域之间迁移知识</li>
 *     <li>关联强度计算</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class KnowledgeAssociationService {

    private final KnowledgeRegistry knowledgeRegistry;

    /**
     * 发现与指定域相关的其他域
     *
     * @param domainId 源域ID
     * @param topK 返回最相关的K个域
     * @return 相关域列表
     */
    public List<DomainAssociation> findRelatedDomains(String domainId, int topK) {
        log.info("🔗 查找与域 {} 相关的其他域", domainId);

        KnowledgeDomain sourceDomain = knowledgeRegistry.findDomainById(domainId).orElse(null);
        if (sourceDomain == null) {
            log.warn("   源域不存在: {}", domainId);
            return Collections.emptyList();
        }

        // 获取所有活跃的域
        List<KnowledgeDomain> allDomains = knowledgeRegistry
                .findDomainsByStatus(DomainStatus.ACTIVE);

        // 计算关联分数
        List<DomainAssociation> associations = allDomains.stream()
                .filter(d -> !d.getDomainId().equals(domainId)) // 排除自己
                .map(targetDomain -> {
                    double score = calculateAssociationScore(sourceDomain, targetDomain);
                    return DomainAssociation.builder()
                            .sourceDomainId(domainId)
                            .targetDomainId(targetDomain.getDomainId())
                            .targetDomainName(targetDomain.getDomainName())
                            .associationScore(score)
                            .associationType(determineAssociationType(sourceDomain, targetDomain))
                            .build();
                })
                .filter(a -> a.getAssociationScore() > 0.1) // 过滤低分
                .sorted((a1, a2) -> Double.compare(a2.getAssociationScore(), a1.getAssociationScore()))
                .limit(topK)
                .collect(Collectors.toList());

        log.info("   找到 {} 个相关域", associations.size());

        return associations;
    }

    /**
     * 推荐知识域
     * 基于用户查询历史和域的活跃度
     *
     * @param query 查询文本
     * @param topK 返回Top K个推荐
     * @return 推荐的域列表
     */
    public List<DomainRecommendation> recommendDomains(String query, int topK) {
        log.info("💡 推荐知识域: query='{}', topK={}", query, topK);

        List<KnowledgeDomain> allDomains = knowledgeRegistry
                .findDomainsByStatus(DomainStatus.ACTIVE);

        // 计算推荐分数
        List<DomainRecommendation> recommendations = allDomains.stream()
                .map(domain -> {
                    double score = calculateRecommendationScore(domain, query);
                    return DomainRecommendation.builder()
                            .domainId(domain.getDomainId())
                            .domainName(domain.getDomainName())
                            .domainType(domain.getDomainType())
                            .score(score)
                            .reason(generateRecommendationReason(domain, query))
                            .build();
                })
                .filter(r -> r.getScore() > 0.0)
                .sorted((r1, r2) -> Double.compare(r2.getScore(), r1.getScore()))
                .limit(topK)
                .collect(Collectors.toList());

        log.info("   生成 {} 个推荐", recommendations.size());

        return recommendations;
    }

    /**
     * 计算域之间的关联分数
     */
    private double calculateAssociationScore(
            KnowledgeDomain source, KnowledgeDomain target) {

        double score = 0.0;

        // 1. 类型相似度（权重 0.3）
        if (source.getDomainType() == target.getDomainType()) {
            score += 0.3;
        }

        // 2. 关联实体（权重 0.4）
        if (hasLinkedEntity(source, target)) {
            score += 0.4;
        }

        // 3. 配置相似度（权重 0.3）
        score += calculateConfigSimilarity(source, target) * 0.3;

        return Math.min(score, 1.0);
    }

    /**
     * 检查是否有关联实体
     */
    private boolean hasLinkedEntity(KnowledgeDomain source, KnowledgeDomain target) {
        String sourceEntity = source.getLinkedEntityId();
        String targetEntity = target.getLinkedEntityId();

        return sourceEntity != null && sourceEntity.equals(targetEntity);
    }

    /**
     * 计算配置相似度
     */
    private double calculateConfigSimilarity(
            KnowledgeDomain source, KnowledgeDomain target) {

        Map<String, Object> sourceConfig = source.getConfig();
        Map<String, Object> targetConfig = target.getConfig();

        if (sourceConfig == null || targetConfig == null) {
            return 0.0;
        }

        // 简单的配置key重叠度
        Set<String> sourceKeys = sourceConfig.keySet();
        Set<String> targetKeys = targetConfig.keySet();

        Set<String> intersection = new HashSet<>(sourceKeys);
        intersection.retainAll(targetKeys);

        Set<String> union = new HashSet<>(sourceKeys);
        union.addAll(targetKeys);

        if (union.isEmpty()) {
            return 0.0;
        }

        return (double) intersection.size() / union.size();
    }

    /**
     * 确定关联类型
     */
    private AssociationType determineAssociationType(
            KnowledgeDomain source, KnowledgeDomain target) {

        // 如果有相同的关联实体
        if (hasLinkedEntity(source, target)) {
            return AssociationType.SHARED_ENTITY;
        }

        // 如果类型相同
        if (source.getDomainType() == target.getDomainType()) {
            return AssociationType.SAME_TYPE;
        }

        // 其他情况
        return AssociationType.CONTENT_RELATED;
    }

    /**
     * 计算推荐分数
     */
    private double calculateRecommendationScore(KnowledgeDomain domain, String query) {
        double score = 0.0;

        String lowerQuery = query.toLowerCase();

        // 1. 域名称匹配（权重 0.3）
        if (domain.getDomainName() != null &&
                domain.getDomainName().toLowerCase().contains(lowerQuery)) {
            score += 0.3;
        }

        // 2. 域描述匹配（权重 0.2）
        if (domain.getDescription() != null &&
                domain.getDescription().toLowerCase().contains(lowerQuery)) {
            score += 0.2;
        }

        // 3. 域活跃度（权重 0.5）
        if (domain.getStatus() == DomainStatus.ACTIVE) {
            score += 0.5;
        }

        return Math.min(score, 1.0);
    }

    /**
     * 生成推荐理由
     */
    private String generateRecommendationReason(KnowledgeDomain domain, String query) {
        List<String> reasons = new ArrayList<>();

        String lowerQuery = query.toLowerCase();

        if (domain.getDomainName() != null &&
                domain.getDomainName().toLowerCase().contains(lowerQuery)) {
            reasons.add("域名称匹配");
        }

        if (domain.getDescription() != null &&
                domain.getDescription().toLowerCase().contains(lowerQuery)) {
            reasons.add("域描述匹配");
        }

        if (domain.getStatus() == DomainStatus.ACTIVE) {
            reasons.add("活跃域");
        }

        return reasons.isEmpty() ? "其他" : String.join(", ", reasons);
    }

    /**
     * 域关联信息
     */
    @lombok.Data
    @lombok.Builder
    public static class DomainAssociation {
        /** 源域ID */
        private String sourceDomainId;

        /** 目标域ID */
        private String targetDomainId;

        /** 目标域名称 */
        private String targetDomainName;

        /** 关联分数（0.0 - 1.0） */
        private double associationScore;

        /** 关联类型 */
        private AssociationType associationType;
    }

    /**
     * 域推荐信息
     */
    @lombok.Data
    @lombok.Builder
    public static class DomainRecommendation {
        /** 域ID */
        private String domainId;

        /** 域名称 */
        private String domainName;

        /** 域类型 */
        private top.yumbo.ai.omni.knowledge.registry.model.DomainType domainType;

        /** 推荐分数（0.0 - 1.0） */
        private double score;

        /** 推荐理由 */
        private String reason;
    }

    /**
     * 关联类型
     */
    public enum AssociationType {
        /** 共享实体 - 关联同一个角色或项目 */
        SHARED_ENTITY,

        /** 相同类型 - 同类域 */
        SAME_TYPE,

        /** 内容相关 - 内容上有关联 */
        CONTENT_RELATED
    }
}

