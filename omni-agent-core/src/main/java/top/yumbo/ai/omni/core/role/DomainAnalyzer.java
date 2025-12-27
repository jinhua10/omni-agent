package top.yumbo.ai.omni.core.role;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 问题领域分析器 - 自动识别问题所属领域
 * (Domain Analyzer - Automatically identify question domain)
 *
 * <p>
 * 核心功能：
 * - 领域关键词匹配
 * - 多领域识别（复杂问题可能涉及多个领域）
 * - 置信度计算
 * - 领域优先级排序
 * </p>
 *
 * <p>
 * 使用场景：
 * - 自动选择合适的角色
 * - 多角色协作的任务分配
 * - 通用角色的智能转发
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class DomainAnalyzer {

    /**
     * 领域定义（简化版）
     */
    private static final Map<String, DomainDefinition> DOMAINS = new HashMap<>();

    static {
        // Java/Spring 领域
        DOMAINS.put("java", DomainDefinition.builder()
                .domainId("java")
                .name("Java开发")
                .keywords(Arrays.asList(
                        "Java", "JVM", "Spring", "Spring Boot", "Maven", "Gradle",
                        "Servlet", "JSP", "JPA", "Hibernate", "MyBatis", "JDBC"
                ))
                .build());

        // 前端领域
        DOMAINS.put("frontend", DomainDefinition.builder()
                .domainId("frontend")
                .name("前端开发")
                .keywords(Arrays.asList(
                        "JavaScript", "TypeScript", "React", "Vue", "Angular",
                        "HTML", "CSS", "前端", "浏览器", "DOM", "Ajax", "WebSocket"
                ))
                .build());

        // 数据库领域
        DOMAINS.put("database", DomainDefinition.builder()
                .domainId("database")
                .name("数据库")
                .keywords(Arrays.asList(
                        "MySQL", "PostgreSQL", "MongoDB", "Redis", "Oracle",
                        "SQL", "NoSQL", "数据库", "查询", "索引", "事务", "表"
                ))
                .build());

        // DevOps 领域
        DOMAINS.put("devops", DomainDefinition.builder()
                .domainId("devops")
                .name("DevOps运维")
                .keywords(Arrays.asList(
                        "Docker", "Kubernetes", "K8s", "Jenkins", "GitLab",
                        "CI/CD", "部署", "运维", "监控", "日志", "容器"
                ))
                .build());

        // 架构设计领域
        DOMAINS.put("architecture", DomainDefinition.builder()
                .domainId("architecture")
                .name("架构设计")
                .keywords(Arrays.asList(
                        "微服务", "分布式", "高并发", "负载均衡", "缓存",
                        "消息队列", "架构", "设计模式", "重构", "性能优化"
                ))
                .build());

        // 算法/数据结构领域
        DOMAINS.put("algorithm", DomainDefinition.builder()
                .domainId("algorithm")
                .name("算法与数据结构")
                .keywords(Arrays.asList(
                        "算法", "数据结构", "排序", "搜索", "树", "图",
                        "动态规划", "贪心", "回溯", "复杂度", "时间复杂度"
                ))
                .build());

        // 安全领域
        DOMAINS.put("security", DomainDefinition.builder()
                .domainId("security")
                .name("安全")
                .keywords(Arrays.asList(
                        "安全", "加密", "认证", "授权", "XSS", "CSRF",
                        "SQL注入", "OAuth", "JWT", "HTTPS", "SSL"
                ))
                .build());
    }

    /**
     * 分析问题领域
     *
     * @param question 用户问题
     * @return 领域分析结果
     */
    public DomainAnalysisResult analyzeDomain(String question) {
        log.debug("分析问题领域: {}", question);

        if (question == null || question.trim().isEmpty()) {
            return DomainAnalysisResult.builder()
                    .question(question)
                    .domains(Collections.emptyList())
                    .primaryDomain(null)
                    .build();
        }

        // 计算每个领域的匹配分数
        List<DomainMatch> matches = new ArrayList<>();

        for (DomainDefinition domain : DOMAINS.values()) {
            double score = calculateDomainScore(question, domain);
            if (score > 0) {
                matches.add(DomainMatch.builder()
                        .domainId(domain.getDomainId())
                        .domainName(domain.getName())
                        .confidence(score)
                        .matchedKeywords(findMatchedKeywords(question, domain))
                        .build());
            }
        }

        // 按置信度排序
        matches.sort((m1, m2) -> Double.compare(m2.getConfidence(), m1.getConfidence()));

        // 确定主要领域（置信度最高的）
        DomainMatch primary = matches.isEmpty() ? null : matches.getFirst();

        log.info("领域分析完成: 主要领域={}, 相关领域数={}",
                primary != null ? primary.getDomainName() : "未知", matches.size());

        return DomainAnalysisResult.builder()
                .question(question)
                .domains(matches)
                .primaryDomain(primary)
                .isMultiDomain(matches.size() > 1)
                .build();
    }

    /**
     * 计算领域匹配分数
     */
    private double calculateDomainScore(String question, DomainDefinition domain) {
        String lowerQuestion = question.toLowerCase();
        int matchCount = 0;
        double totalScore = 0.0;

        for (String keyword : domain.getKeywords()) {
            if (lowerQuestion.contains(keyword.toLowerCase())) {
                matchCount++;

                // 完全匹配权重更高
                int index = lowerQuestion.indexOf(keyword.toLowerCase());
                boolean isWholeWord = (index == 0 || !Character.isLetterOrDigit(lowerQuestion.charAt(index - 1))) &&
                                    (index + keyword.length() >= lowerQuestion.length() ||
                                     !Character.isLetterOrDigit(lowerQuestion.charAt(index + keyword.length())));

                double keywordScore = isWholeWord ? 1.0 : 0.7;

                // 位置权重（越靠前权重越高）
                double positionWeight = 1.0 - ((double) index / lowerQuestion.length()) * 0.3;

                totalScore += keywordScore * positionWeight;
            }
        }

        // 归一化
        if (matchCount > 0) {
            return Math.min(totalScore / domain.getKeywords().size(), 1.0);
        }

        return 0.0;
    }

    /**
     * 找出匹配的关键词
     */
    private List<String> findMatchedKeywords(String question, DomainDefinition domain) {
        String lowerQuestion = question.toLowerCase();
        return domain.getKeywords().stream()
                .filter(keyword -> lowerQuestion.contains(keyword.toLowerCase()))
                .collect(Collectors.toList());
    }

    /**
     * 获取所有支持的领域
     */
    public List<DomainDefinition> getAllDomains() {
        return new ArrayList<>(DOMAINS.values());
    }

    /**
     * 根据ID获取领域定义
     */
    public DomainDefinition getDomain(String domainId) {
        return DOMAINS.get(domainId);
    }

    /**
     * 领域定义
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DomainDefinition {
        /** 领域ID */
        private String domainId;

        /** 领域名称 */
        private String name;

        /** 关键词列表 */
        private List<String> keywords;
    }

    /**
     * 领域匹配结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DomainMatch {
        /** 领域ID */
        private String domainId;

        /** 领域名称 */
        private String domainName;

        /** 置信度 (0.0 - 1.0) */
        private double confidence;

        /** 匹配的关键词 */
        private List<String> matchedKeywords;
    }

    /**
     * 领域分析结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DomainAnalysisResult {
        /** 原始问题 */
        private String question;

        /** 所有匹配的领域（按置信度降序） */
        private List<DomainMatch> domains;

        /** 主要领域（置信度最高的） */
        private DomainMatch primaryDomain;

        /** 是否涉及多个领域 */
        private boolean isMultiDomain;
    }
}


