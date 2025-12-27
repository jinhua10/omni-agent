package top.yumbo.ai.omni.core.router;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.RoleStatus;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 知识域路由器
 *
 * <p>负责根据查询意图将请求路由到合适的知识域</p>
 *
 * <p>核心功能：</p>
 * <ul>
 *     <li>意图识别 - 识别查询的领域意图</li>
 *     <li>域匹配 - 找到最相关的知识域</li>
 *     <li>角色匹配 - 找到合适的角色处理查询</li>
 *     <li>跨域查询 - 支持多域联合查询</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class DomainRouter {

    private final KnowledgeRegistry knowledgeRegistry;

    /**
     * 构造函数 - KnowledgeRegistry 为可选依赖
     */
    @Autowired(required = false)
    public DomainRouter(KnowledgeRegistry knowledgeRegistry) {
        this.knowledgeRegistry = knowledgeRegistry;
        if (knowledgeRegistry == null) {
            log.warn("⚠️ KnowledgeRegistry not available - DomainRouter will use fallback mode");
        } else {
            log.info("✅ DomainRouter initialized with KnowledgeRegistry");
        }
    }

    /**
     * 路由查询到合适的知识域
     *
     * @param query 用户查询
     * @return 路由结果
     */
    public QueryRouteResult route(String query) {
        log.info("路由查询: {}", query);

        // 1. 分析查询意图
        QueryIntent intent = analyzeIntent(query);

        // 2. 根据意图匹配域
        List<String> matchedDomains = matchDomains(intent);

        // 3. 匹配合适的角色
        List<String> matchedRoles = matchRoles(intent);

        // 4. 构建路由结果
        QueryRouteResult result = QueryRouteResult.builder()
                .domainIds(matchedDomains)
                .roleIds(matchedRoles)
                .suggestedDomainType(intent.getDomainType())
                .confidence(intent.getConfidence())
                .crossDomain(matchedDomains.size() > 1)
                .build();

        log.info("路由结果: {} 个域, {} 个角色",
                result.getDomainIds().size(),
                result.getRoleIds().size());

        return result;
    }

    /**
     * 分析查询意图
     *
     * @param query 查询文本
     * @return 查询意图
     */
    private QueryIntent analyzeIntent(String query) {
        String lowerQuery = query.toLowerCase();

        // 简单的关键词匹配（后续可扩展为AI模型）
        DomainType domainType = null;
        double confidence = 0.5;
        List<String> keywords = new ArrayList<>();

        // 源码相关
        if (containsAny(lowerQuery, "代码", "源码", "code", "安全漏洞", "bug", "重构")) {
            domainType = DomainType.SOURCE_CODE;
            confidence = 0.8;
            keywords.addAll(Arrays.asList("代码", "源码", "安全"));
        }
        // 文档相关
        else if (containsAny(lowerQuery, "文档", "doc", "说明", "教程", "指南")) {
            domainType = DomainType.DOCUMENT;
            confidence = 0.7;
            keywords.addAll(Arrays.asList("文档", "说明"));
        }
        // 角色知识相关
        else if (containsAny(lowerQuery, "分析", "评审", "审查", "建议")) {
            domainType = DomainType.ROLE_KNOWLEDGE;
            confidence = 0.6;
            keywords.addAll(Arrays.asList("分析", "评审"));
        }

        return QueryIntent.builder()
                .query(query)
                .domainType(domainType)
                .confidence(confidence)
                .keywords(keywords)
                .build();
    }

    /**
     * 匹配知识域
     *
     * @param intent 查询意图
     * @return 匹配的域ID列表
     */
    private List<String> matchDomains(QueryIntent intent) {
        // 如果 knowledgeRegistry 不可用，返回空列表
        if (knowledgeRegistry == null) {
            log.debug("KnowledgeRegistry not available, returning empty domain list");
            return Collections.emptyList();
        }

        // 获取所有活跃的域
        List<KnowledgeDomain> allDomains = knowledgeRegistry
                .findDomainsByStatus(DomainStatus.ACTIVE);

        // 如果有明确的域类型，优先匹配
        if (intent.getDomainType() != null) {
            List<String> typedDomains = allDomains.stream()
                    .filter(d -> d.getDomainType() == intent.getDomainType())
                    .map(KnowledgeDomain::getDomainId)
                    .collect(Collectors.toList());

            if (!typedDomains.isEmpty()) {
                return typedDomains;
            }
        }

        // 否则返回所有活跃域（支持跨域查询）
        return allDomains.stream()
                .map(KnowledgeDomain::getDomainId)
                .limit(5) // 限制最多5个域
                .collect(Collectors.toList());
    }

    /**
     * 匹配合适的角色
     *
     * @param intent 查询意图
     * @return 匹配的角色ID列表
     */
    private List<String> matchRoles(QueryIntent intent) {
        // 如果 knowledgeRegistry 不可用，返回空列表
        if (knowledgeRegistry == null) {
            log.debug("KnowledgeRegistry not available, returning empty role list");
            return Collections.emptyList();
        }

        // 获取所有活跃的角色
        List<KnowledgeRole> allRoles = knowledgeRegistry
                .findRolesByStatus(RoleStatus.ACTIVE);

        if (allRoles.isEmpty()) {
            return Collections.emptyList();
        }

        // 根据关键词匹配角色
        return allRoles.stream()
                .filter(role -> matchRoleResponsibilities(role, intent.getKeywords()))
                .map(KnowledgeRole::getRoleId)
                .limit(3) // 最多返回3个角色
                .collect(Collectors.toList());
    }

    /**
     * 匹配角色职责
     *
     * @param role 角色
     * @param keywords 关键词列表
     * @return 是否匹配
     */
    private boolean matchRoleResponsibilities(KnowledgeRole role, List<String> keywords) {
        if (role.getResponsibilities() == null || keywords.isEmpty()) {
            return false;
        }

        String responsibilities = role.getResponsibilities().toLowerCase();

        // 检查是否包含任何关键词
        return keywords.stream()
                .anyMatch(responsibilities::contains);
    }

    /**
     * 检查字符串是否包含任意关键词
     *
     * @param text 文本
     * @param keywords 关键词
     * @return 是否包含
     */
    private boolean containsAny(String text, String... keywords) {
        return Arrays.stream(keywords)
                .anyMatch(text::contains);
    }

    /**
     * 查询意图（内部类）
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    private static class QueryIntent {
        private String query;
        private DomainType domainType;
        private Double confidence;
        @Builder.Default
        private List<String> keywords = new ArrayList<>();
    }
}


