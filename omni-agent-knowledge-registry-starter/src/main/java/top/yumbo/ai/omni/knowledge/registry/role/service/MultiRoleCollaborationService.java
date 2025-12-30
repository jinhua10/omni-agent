package top.yumbo.ai.omni.knowledge.registry.role.service;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.knowledge.registry.model.role.KnowledgeRole;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

/**
 * 多角色协作服务 - 复杂问题的多角色协作解答
 * (Multi-Role Collaboration Service - Multi-role collaboration for complex questions)
 *
 * <p>
 * 核心功能：
 * - 问题分解：将复杂问题分解为多个子问题
 * - 角色分配：为每个子问题分配最合适的角色
 * - 并行查询：多个角色并行回答子问题
 * - 答案综合：融合多个角色的答案
 * </p>
 *
 * <p>
 * 协作流程：
 * 1. 分析问题复杂度
 * 2. 如果是简单问题，单角色回答
 * 3. 如果是复杂问题，分解为子问题
 * 4. 为每个子问题匹配角色
 * 5. 并行查询各角色
 * 6. 综合所有答案
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class MultiRoleCollaborationService {

    @Autowired
    private RoleService roleService;

    @Autowired
    private RoleMatcherService roleMatcherService;

    @Autowired
    private DomainAnalyzer domainAnalyzer;

    @Autowired
    private AIService aiService;

    /**
     * 线程池（用于并行查询）
     */
    private final ExecutorService executorService = Executors.newFixedThreadPool(5);

    /**
     * 协作式回答问题
     *
     * @param question 用户问题
     * @param context 上下文（检索到的文档等）
     * @return 协作结果
     */
    public CollaborationResult collaborate(String question, String context) {
        log.info("🤝 启动多角色协作: {}", question);

        long startTime = System.currentTimeMillis();

        try {
            // 1. 分析问题复杂度
            QuestionComplexity complexity = analyzeComplexity(question);
            log.info("问题复杂度: {} (置信度: {:.2f})",
                    complexity.getLevel(), complexity.getConfidence());

            // 2. 根据复杂度选择协作策略
            if (complexity.getLevel() == ComplexityLevel.SIMPLE) {
                // 简单问题：单角色回答
                return handleSimpleQuestion(question, context);
            } else {
                // 复杂问题：多角色协作
                return handleComplexQuestion(question, context, complexity);
            }

        } catch (Exception e) {
            log.error("❌ 多角色协作失败", e);

            // 降级：使用默认角色回答
            return CollaborationResult.builder()
                    .question(question)
                    .collaborationType(CollaborationType.SINGLE_ROLE)
                    .roles(List.of(roleService.getRole("default")))
                    .answer("协作过程出现错误，使用默认回答。")
                    .error(e.getMessage())
                    .processingTimeMs(System.currentTimeMillis() - startTime)
                    .build();
        }
    }

    /**
     * 处理简单问题（单角色）
     */
    private CollaborationResult handleSimpleQuestion(String question, String context) {
        log.info("📝 处理简单问题（单角色）");

        // 找到最佳角色
        KnowledgeRole bestRole = roleMatcherService.findBestRole(question);

        // 构建提示词
        String prompt = buildRolePrompt(bestRole, question, context);

        // 生成答案
        String answer = aiService.chat(prompt);

        return CollaborationResult.builder()
                .question(question)
                .collaborationType(CollaborationType.SINGLE_ROLE)
                .roles(List.of(bestRole))
                .answer(answer)
                .subResults(Collections.emptyList())
                .processingTimeMs(0)
                .build();
    }

    /**
     * 处理复杂问题（多角色协作）
     */
    private CollaborationResult handleComplexQuestion(String question, String context,
                                                     QuestionComplexity complexity) {
        log.info("🤝 处理复杂问题（多角色协作）");

        // 1. 分解问题
        List<SubQuestion> subQuestions = decomposeQuestion(question, complexity);
        log.info("问题分解: {} 个子问题", subQuestions.size());

        // 2. 为每个子问题匹配角色
        Map<SubQuestion, KnowledgeRole> assignments = assignRolesToSubQuestions(subQuestions);

        // 3. 并行查询
        List<SubResult> subResults = queryInParallel(assignments, context);

        // 4. 综合答案
        String synthesizedAnswer = synthesizeAnswers(question, subResults);

        // 收集所有参与的角色
        List<KnowledgeRole> involvedRoles = subResults.stream()
                .map(SubResult::getRole)
                .distinct()
                .collect(Collectors.toList());

        return CollaborationResult.builder()
                .question(question)
                .collaborationType(CollaborationType.MULTI_ROLE)
                .roles(involvedRoles)
                .answer(synthesizedAnswer)
                .subResults(subResults)
                .processingTimeMs(0)
                .build();
    }

    /**
     * 分析问题复杂度
     */
    private QuestionComplexity analyzeComplexity(String question) {
        // 简化实现：基于启发式规则

        // 1. 长度检查
        int length = question.length();

        // 2. 多领域检查
        DomainAnalyzer.DomainAnalysisResult domainResult = domainAnalyzer.analyzeDomain(question);
        boolean isMultiDomain = domainResult.isMultiDomain();

        // 3. 复合句检查
        boolean hasMultipleClauses = question.contains("并且") || question.contains("以及") ||
                                   question.contains("同时") || question.contains("，") ||
                                   question.split("[?？]").length > 1;

        // 4. 综合判断
        ComplexityLevel level;
        double confidence;

        if (isMultiDomain && hasMultipleClauses) {
            level = ComplexityLevel.COMPLEX;
            confidence = 0.9;
        } else if (isMultiDomain || hasMultipleClauses || length > 100) {
            level = ComplexityLevel.MODERATE;
            confidence = 0.7;
        } else {
            level = ComplexityLevel.SIMPLE;
            confidence = 0.8;
        }

        return QuestionComplexity.builder()
                .level(level)
                .confidence(confidence)
                .isMultiDomain(isMultiDomain)
                .hasMultipleClauses(hasMultipleClauses)
                .build();
    }

    /**
     * 分解问题为子问题
     */
    private List<SubQuestion> decomposeQuestion(String question, QuestionComplexity complexity) {
        List<SubQuestion> subQuestions = new ArrayList<>();

        // 简化实现：基于标点符号和连词分割
        String[] parts = question.split("[，,；;]|并且|以及|同时");

        for (int i = 0; i < parts.length; i++) {
            String part = parts[i].trim();
            if (!part.isEmpty() && part.length() > 5) { // 过滤太短的片段
                subQuestions.add(SubQuestion.builder()
                        .id("sub-" + i)
                        .question(part)
                        .originalQuestion(question)
                        .build());
            }
        }

        // 如果分解失败，将整个问题作为单个子问题
        if (subQuestions.isEmpty()) {
            subQuestions.add(SubQuestion.builder()
                    .id("sub-0")
                    .question(question)
                    .originalQuestion(question)
                    .build());
        }

        return subQuestions;
    }

    /**
     * 为子问题分配角色
     */
    private Map<SubQuestion, KnowledgeRole> assignRolesToSubQuestions(List<SubQuestion> subQuestions) {
        Map<SubQuestion, KnowledgeRole> assignments = new HashMap<>();

        for (SubQuestion subQ : subQuestions) {
            KnowledgeRole bestRole = roleMatcherService.findBestRole(subQ.getQuestion());
            assignments.put(subQ, bestRole);
            log.info("子问题分配: [{}] -> 角色 [{}]", subQ.getQuestion(), bestRole.getRoleName());
        }

        return assignments;
    }

    /**
     * 并行查询多个角色
     */
    private List<SubResult> queryInParallel(Map<SubQuestion, KnowledgeRole> assignments, String context) {
        log.info("🚀 并行查询 {} 个角色", assignments.size());

        List<CompletableFuture<SubResult>> futures = new ArrayList<>();

        for (Map.Entry<SubQuestion, KnowledgeRole> entry : assignments.entrySet()) {
            SubQuestion subQ = entry.getKey();
            KnowledgeRole role = entry.getValue();

            CompletableFuture<SubResult> future = CompletableFuture.supplyAsync(() -> {
                try {
                    String prompt = buildRolePrompt(role, subQ.getQuestion(), context);
                    String answer = aiService.chat(prompt);

                    return SubResult.builder()
                            .subQuestion(subQ)
                            .role(role)
                            .answer(answer)
                            .success(true)
                            .build();
                } catch (Exception e) {
                    log.error("❌ 子问题查询失败: {}", subQ.getQuestion(), e);
                    return SubResult.builder()
                            .subQuestion(subQ)
                            .role(role)
                            .answer("查询失败")
                            .success(false)
                            .error(e.getMessage())
                            .build();
                }
            }, executorService);

            futures.add(future);
        }

        // 等待所有查询完成（最多30秒）
        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                    .get(30, TimeUnit.SECONDS);
        } catch (Exception e) {
            log.error("❌ 并行查询超时", e);
        }

        // 收集结果
        return futures.stream()
                .map(f -> {
                    try {
                        return f.get();
                    } catch (Exception e) {
                        return null;
                    }
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    /**
     * 综合多个角色的答案
     */
    private String synthesizeAnswers(String originalQuestion, List<SubResult> subResults) {
        log.info("📝 综合 {} 个角色的答案", subResults.size());

        StringBuilder synthesized = new StringBuilder();
        synthesized.append("【多角色协作回答】\n\n");

        for (int i = 0; i < subResults.size(); i++) {
            SubResult result = subResults.get(i);

            if (result.isSuccess()) {
                synthesized.append(String.format("**%d. %s（由%s回答）**\n\n",
                        i + 1,
                        result.getSubQuestion().getQuestion(),
                        result.getRole().getRoleName()));
                synthesized.append(result.getAnswer()).append("\n\n");
            }
        }

        synthesized.append("---\n\n");
        synthesized.append("以上回答由 ").append(subResults.size()).append(" 位专家协作完成。");

        return synthesized.toString();
    }

    /**
     * 构建角色提示词
     */
    private String buildRolePrompt(KnowledgeRole role, String question, String context) {
        return String.format(
                "你是%s，%s\n\n" +
                "基于以下知识回答问题：\n\n%s\n\n" +
                "问题：%s\n\n" +
                "请以你的专业角色身份回答。",
                role.getRoleName(),
                role.getDescription(),
                context.isEmpty() ? "暂无特定知识" : context,
                question
        );
    }

    /**
     * 问题复杂度
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class QuestionComplexity {
        private ComplexityLevel level;
        private double confidence;
        private boolean isMultiDomain;
        private boolean hasMultipleClauses;
    }

    /**
     * 复杂度级别
     */
    public enum ComplexityLevel {
        SIMPLE,     // 简单问题
        MODERATE,   // 中等复杂度
        COMPLEX     // 复杂问题
    }

    /**
     * 子问题
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SubQuestion {
        private String id;
        private String question;
        private String originalQuestion;
    }

    /**
     * 子结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class SubResult {
        private SubQuestion subQuestion;
        private KnowledgeRole role;
        private String answer;
        private boolean success;
        private String error;
    }

    /**
     * 协作类型
     */
    public enum CollaborationType {
        SINGLE_ROLE,    // 单角色
        MULTI_ROLE      // 多角色
    }

    /**
     * 协作结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CollaborationResult {
        /** 原始问题 */
        private String question;

        /** 协作类型 */
        private CollaborationType collaborationType;

        /** 参与的角色列表 */
        private List<KnowledgeRole> roles;

        /** 最终答案 */
        private String answer;

        /** 子结果列表 */
        private List<SubResult> subResults;

        /** 处理时间（毫秒） */
        private long processingTimeMs;

        /** 错误信息（如果有） */
        private String error;
    }
}


