package top.yumbo.ai.omni.core.hope;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * HOPE 知识管理器 - 统一管理三层知识的查询和学习
 * (HOPE Knowledge Manager - Unified management of three-layer knowledge query and learning)
 *
 * <p>
 * 重构说明 (Refactoring Notes):
 * - 本类是协调器，不直接依赖持久化
 * - 通过注入各层服务间接使用持久化接口
 * - 各层服务负责与持久化接口交互
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0 (Refactored)
 * @version 3.0.0 - 重构为可插拔架构
 */
@Slf4j
@Service
public class HOPEKnowledgeManager {

    private final QuestionClassifier questionClassifier;

    /**
     * 构造函数 - Spring 自动注入
     *
     * @param questionClassifier 问题分类器
     */
    @Autowired
    public HOPEKnowledgeManager(QuestionClassifier questionClassifier) {
        this.questionClassifier = questionClassifier;
        log.info("HOPEKnowledgeManager initialized");
    }

    /**
     * 智能查询 - 按层级依次查询
     * (Smart query - Query layers in order)
     *
     * @param question 用户问题
     * @param sessionId 会话ID
     * @return 查询结果
     */
    public QueryResult smartQuery(String question, String sessionId) {
        log.debug("Smart query: question={}, sessionId={}", question, sessionId);

        // 1. 问题分类
        QuestionClassifier.ClassificationResult classification = questionClassifier.classify(question);

        log.debug("Question classified: type={}, layer={}, confidence={}",
                 classification.getQuestionType(),
                 classification.getSuggestedLayer(),
                 classification.getConfidence());

        // 2. 根据分类结果决定查询策略
        // TODO: 实现完整的三层查询逻辑

        return QueryResult.builder()
            .questionType(classification.getQuestionType())
            .suggestedLayer(classification.getSuggestedLayer())
            .confidence(classification.getConfidence())
            .needsLLM(true) // 默认需要 LLM
            .build();
    }

    /**
     * 查询结果
     */
    @lombok.Data
    @lombok.Builder
    @lombok.NoArgsConstructor
    @lombok.AllArgsConstructor
    public static class QueryResult {
        /** 问题类型 */
        private String questionType;

        /** 建议的知识层 */
        private String suggestedLayer;

        /** 置信度 */
        private double confidence;

        /** 是否需要 LLM */
        private boolean needsLLM;

        /** 答案 */
        private String answer;

        /** 来源层 */
        private String sourceLayer;
    }
}

