package top.yumbo.ai.omni.core.role;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 角色匹配服务 - 根据问题找到最合适的角色
 * (Role Matcher Service - Find the best role for a question)
 *
 * <p>
 * 核心功能：
 * - 基于领域的角色匹配
 * - 基于关键词的角色匹配
 * - 多角色推荐（用于复杂问题）
 * - 置信度评分
 * </p>
 *
 * <p>
 * 匹配策略：
 * 1. 领域匹配：问题领域 → 角色领域
 * 2. 关键词匹配：问题关键词 → 角色关键词
 * 3. 优先级加权：角色优先级越高，权重越大
 * 4. 综合评分：领域分数 × 0.6 + 关键词分数 × 0.4
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class RoleMatcherService {

    @Autowired
    private RoleService roleService;

    @Autowired
    private DomainAnalyzer domainAnalyzer;

    /**
     * 为问题找到最佳角色
     *
     * @param question 用户问题
     * @return 最佳角色，找不到则返回默认角色
     */
    public Role findBestRole(String question) {
        log.info("为问题匹配最佳角色: {}", question);

        List<RoleMatch> matches = matchRoles(question, 5);

        if (matches.isEmpty()) {
            log.warn("未找到匹配的角色，返回默认角色");
            return roleService.getRole("default");
        }

        RoleMatch bestMatch = matches.get(0);
        log.info("最佳角色: {} (置信度: {:.2f})",
                bestMatch.getRole().getName(), bestMatch.getScore());

        return bestMatch.getRole();
    }

    /**
     * 匹配多个角色（用于复杂问题的多角色协作）
     *
     * @param question 用户问题
     * @param topK 返回前K个角色
     * @return 角色匹配列表（按分数降序）
     */
    public List<RoleMatch> matchRoles(String question, int topK) {
        log.debug("匹配角色: question={}, topK={}", question, topK);

        // 1. 分析问题领域
        DomainAnalyzer.DomainAnalysisResult domainResult = domainAnalyzer.analyzeDomain(question);

        // 2. 获取所有启用的角色
        List<Role> enabledRoles = roleService.getEnabledRoles();

        if (enabledRoles.isEmpty()) {
            log.warn("没有启用的角色");
            return Collections.emptyList();
        }

        // 3. 计算每个角色的匹配分数
        List<RoleMatch> matches = new ArrayList<>();

        for (Role role : enabledRoles) {
            double score = calculateRoleScore(question, role, domainResult);

            if (score > 0.1) { // 最低阈值
                matches.add(RoleMatch.builder()
                        .role(role)
                        .score(score)
                        .reason(buildMatchReason(role, domainResult))
                        .build());
            }
        }

        // 4. 排序并返回前K个
        matches.sort((m1, m2) -> Double.compare(m2.getScore(), m1.getScore()));

        List<RoleMatch> topMatches = matches.stream()
                .limit(topK)
                .collect(Collectors.toList());

        log.info("匹配到 {} 个角色", topMatches.size());

        return topMatches;
    }

    /**
     * 计算角色匹配分数
     */
    private double calculateRoleScore(String question, Role role,
                                      DomainAnalyzer.DomainAnalysisResult domainResult) {
        double domainScore = calculateDomainScore(role, domainResult);
        double keywordScore = calculateKeywordScore(question, role);
        double priorityWeight = 1.0 + (role.getPriority() / 100.0); // 优先级加权

        // 综合评分：领域60% + 关键词40%，再乘以优先级权重
        double finalScore = (domainScore * 0.6 + keywordScore * 0.4) * priorityWeight;

        return Math.min(finalScore, 1.0); // 归一化到 [0, 1]
    }

    /**
     * 计算领域匹配分数
     */
    private double calculateDomainScore(Role role, DomainAnalyzer.DomainAnalysisResult domainResult) {
        if (domainResult.getDomains().isEmpty()) {
            return 0.0;
        }

        // 检查角色的属性中是否定义了领域
        Object domainsProp = role.getProperties().get("domains");
        if (!(domainsProp instanceof List)) {
            return 0.0;
        }

        @SuppressWarnings("unchecked")
        List<String> roleDomains = (List<String>) domainsProp;

        // 计算领域重叠度
        double maxScore = 0.0;
        for (DomainAnalyzer.DomainMatch domainMatch : domainResult.getDomains()) {
            if (roleDomains.contains(domainMatch.getDomainId())) {
                maxScore = Math.max(maxScore, domainMatch.getConfidence());
            }
        }

        return maxScore;
    }

    /**
     * 计算关键词匹配分数
     */
    private double calculateKeywordScore(String question, Role role) {
        if (role.getKeywords() == null || role.getKeywords().isEmpty()) {
            return 0.0;
        }

        String lowerQuestion = question.toLowerCase();
        int matchCount = 0;

        for (String keyword : role.getKeywords()) {
            if (lowerQuestion.contains(keyword.toLowerCase())) {
                matchCount++;
            }
        }

        return (double) matchCount / role.getKeywords().size();
    }

    /**
     * 构建匹配原因说明
     */
    private String buildMatchReason(Role role, DomainAnalyzer.DomainAnalysisResult domainResult) {
        StringBuilder reason = new StringBuilder();

        // 领域匹配原因
        if (domainResult.getPrimaryDomain() != null) {
            Object domainsProp = role.getProperties().get("domains");
            if (domainsProp instanceof List) {
                @SuppressWarnings("unchecked")
                List<String> roleDomains = (List<String>) domainsProp;

                if (roleDomains.contains(domainResult.getPrimaryDomain().getDomainId())) {
                    reason.append("领域匹配: ").append(domainResult.getPrimaryDomain().getDomainName());
                }
            }
        }

        // 角色描述
        if (reason.length() > 0) {
            reason.append(" | ");
        }
        reason.append(role.getDescription());

        return reason.toString();
    }

    /**
     * 角色匹配结果
     */
    @lombok.Data
    @lombok.Builder
    @lombok.NoArgsConstructor
    @lombok.AllArgsConstructor
    public static class RoleMatch {
        /** 角色对象 */
        private Role role;

        /** 匹配分数 (0.0 - 1.0) */
        private double score;

        /** 匹配原因 */
        private String reason;
    }
}

