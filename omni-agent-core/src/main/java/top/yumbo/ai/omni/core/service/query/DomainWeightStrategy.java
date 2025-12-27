package top.yumbo.ai.omni.core.service.query;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;

import java.util.HashMap;
import java.util.Map;

/**
 * 域权重策略
 * (Domain Weight Strategy)
 *
 * <p>根据不同的查询场景动态计算域的权重</p>
 *
 * <p>权重因素：</p>
 * <ul>
 *     <li>域类型权重 - 不同类型域在不同场景下的重要性</li>
 *     <li>查询意图匹配 - 查询与域的相关性</li>
 *     <li>域活跃度 - 域的使用频率和质量</li>
 *     <li>历史准确率 - 该域历史查询的准确性</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class DomainWeightStrategy {

    /**
     * 计算域权重
     *
     * @param domainId 域ID
     * @param domainType 域类型
     * @param query 查询文本
     * @param context 查询上下文
     * @return 权重值（0.0 - 2.0，1.0为基准）
     */
    public double calculateDomainWeight(
            String domainId,
            DomainType domainType,
            String query,
            QueryContext context) {

        double weight = 1.0; // 基准权重

        // 1. 域类型权重（0.5 - 1.5）
        weight *= calculateTypeWeight(domainType, query);

        // 2. 查询意图匹配权重（0.7 - 1.3）
        weight *= calculateIntentMatchWeight(query, context);

        // 3. 域质量权重（0.8 - 1.2）
        weight *= calculateQualityWeight(domainId);

        // 限制权重范围在 0.1 - 2.0
        weight = Math.max(0.1, Math.min(2.0, weight));

        log.debug("   域 {} 权重: {}", domainId, String.format("%.2f", weight));

        return weight;
    }

    /**
     * 计算域类型权重
     * 不同类型的域在不同查询场景下有不同的重要性
     */
    private double calculateTypeWeight(DomainType domainType, String query) {
        String lowerQuery = query.toLowerCase();

        // 源码相关查询
        if (containsAny(lowerQuery, "代码", "源码", "bug", "漏洞", "重构", "code")) {
            if (domainType == DomainType.SOURCE_CODE) {
                return 1.5; // 源码域权重提升
            } else if (domainType == DomainType.ROLE_KNOWLEDGE) {
                return 1.2; // 角色域也相关
            }
        }

        // 文档查询
        if (containsAny(lowerQuery, "文档", "教程", "指南", "说明", "doc", "guide")) {
            if (domainType == DomainType.DOCUMENT) {
                return 1.5;
            }
        }

        // 角色专业知识查询
        if (containsAny(lowerQuery, "分析", "评审", "建议", "优化", "review")) {
            if (domainType == DomainType.ROLE_KNOWLEDGE) {
                return 1.4;
            }
        }

        // API相关查询
        if (containsAny(lowerQuery, "api", "接口", "调用", "参数")) {
            if (domainType == DomainType.API_DOCUMENTATION) {
                return 1.5;
            }
        }

        return 1.0; // 默认权重
    }

    /**
     * 计算查询意图匹配权重
     */
    private double calculateIntentMatchWeight(String query, QueryContext context) {
        // 简化实现，后续可以用 AI 模型增强
        if (context != null && context.getIntent() != null) {
            // 如果有明确的意图，权重提升
            return 1.2;
        }

        // 查询越长，说明意图越明确，权重略微提升
        if (query.length() > 50) {
            return 1.1;
        }

        return 1.0;
    }

    /**
     * 计算域质量权重
     * 基于域的历史表现
     */
    private double calculateQualityWeight(String domainId) {
        // TODO: 从统计数据中获取域的历史准确率
        // 这里先返回默认值，后续可以实现域质量评分系统
        return 1.0;
    }

    /**
     * 检查字符串是否包含任一关键词
     */
    private boolean containsAny(String text, String... keywords) {
        for (String keyword : keywords) {
            if (text.contains(keyword)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 查询上下文
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class QueryContext {
        /** 查询意图 */
        private String intent;

        /** 用户偏好 */
        private Map<String, Object> preferences;

        /** 历史查询 */
        private String previousQuery;

        /** 扩展属性 */
        @Builder.Default
        private Map<String, Object> metadata = new HashMap<>();
    }
}

