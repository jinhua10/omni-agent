package top.yumbo.ai.omni.core.qa.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.core.qa.model.*;
import top.yumbo.ai.omni.core.router.DomainRouter;
import top.yumbo.ai.omni.core.service.knowledge.KnowledgeExtractionService;
import top.yumbo.ai.omni.core.model.KnowledgeDocument;
import top.yumbo.ai.omni.rag.model.Document;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 智能问答服务
 * 整合意图分析、知识检索、缺口检测、响应生成
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class IntelligentQAService {

    @Autowired
    private IntentAnalyzer intentAnalyzer;

    @Autowired
    private ConversationManager conversationManager;

    @Autowired
    private DomainRouter domainRouter;

    @Autowired
    private KnowledgeExtractionService extractionService;

    @Autowired
    private AIService aiService;

    /**
     * 智能问答（主入口）
     */
    public IntelligentQAResponse ask(IntelligentQARequest request) {
        String question = request.getQuestion();
        String conversationId = request.getConversationId();
        String userId = request.getUserId();

        log.info("🤔 智能问答开始: question={}, conversationId={}", question, conversationId);

        try {
            // 1. 获取或创建对话
            Conversation conversation = conversationManager.getOrCreateConversation(conversationId, userId);

            // 2. 添加用户消息
            Message userMessage = Message.builder()
                    .messageId(UUID.randomUUID().toString())
                    .role("user")
                    .content(question)
                    .build();
            conversation.addMessage(userMessage);

            // 3. 意图分析
            log.info("🎯 步骤1: 意图分析");
            IntentAnalysisResult intent = intentAnalyzer.analyzeIntent(question, conversation.getConversationId());

            // 4. 知识检索
            log.info("📚 步骤2: 知识检索");
            KnowledgeGapResult gapResult = retrieveAndEvaluateKnowledge(intent);

            // 5. 生成回答
            log.info("💡 步骤3: 生成回答");
            String answer;
            boolean needsMoreInfo = false;

            if (gapResult.isNeedsUserInput()) {
                // 需要更多信息
                answer = generateRequestForInfo(gapResult.getQuestionsForUser());
                needsMoreInfo = true;
            } else {
                // 知识充足，生成完整回答
                answer = generateFullAnswer(question, intent, gapResult, conversation);
            }

            // 6. 添加助手消息
            Message assistantMessage = Message.builder()
                    .messageId(UUID.randomUUID().toString())
                    .role("assistant")
                    .content(answer)
                    .build();
            conversation.addMessage(assistantMessage);

            // 7. 构建响应
            IntelligentQAResponse response = IntelligentQAResponse.builder()
                    .conversationId(conversation.getConversationId())
                    .question(question)
                    .answer(answer)
                    .intent(intent)
                    .hasKnowledge(gapResult.isHasKnowledge())
                    .knowledgeSufficient(gapResult.isKnowledgeSufficient())
                    .needsMoreInfo(needsMoreInfo)
                    .references(extractReferences(gapResult))
                    .build();

            log.info("✅ 智能问答完成: needsMoreInfo={}, referencesCount={}",
                    needsMoreInfo, response.getReferences().size());

            return response;

        } catch (Exception e) {
            log.error("❌ 智能问答失败", e);

            // 返回错误响应
            return IntelligentQAResponse.builder()
                    .question(question)
                    .answer("抱歉，处理您的问题时出现了错误：" + e.getMessage())
                    .hasKnowledge(false)
                    .knowledgeSufficient(false)
                    .needsMoreInfo(false)
                    .build();
        }
    }

    /**
     * 检索和评估知识
     */
    private KnowledgeGapResult retrieveAndEvaluateKnowledge(IntentAnalysisResult intent) {
        KnowledgeGapResult result = KnowledgeGapResult.builder().build();

        try {
            // 1. 路由到相关域
            var routeResult = domainRouter.route(intent.getIntent());
            List<String> relevantDomains = routeResult.getDomainIds();
            log.info("路由到 {} 个相关域: {}", relevantDomains.size(), relevantDomains);

            // 2. 从相关域检索知识
            Map<String, List<Document>> domainKnowledge = new HashMap<>();
            for (String domainId : relevantDomains) {
                try {
                    List<KnowledgeDocument> docs = extractionService.extractDocuments(
                            domainId,
                            intent.getIntent(),
                            5
                    );

                    if (!docs.isEmpty()) {
                        List<Document> convertedDocs = docs.stream()
                                .map(this::convertToDocument)
                                .collect(Collectors.toList());
                        domainKnowledge.put(domainId, convertedDocs);
                    }
                } catch (Exception e) {
                    log.warn("从域 {} 检索知识失败: {}", domainId, e.getMessage());
                }
            }

            result.setHasKnowledge(!domainKnowledge.isEmpty());
            result.setRetrievedKnowledge(domainKnowledge);

            // 3. 评估知识完整性
            if (!domainKnowledge.isEmpty()) {
                KnowledgeCompleteness completeness = evaluateCompleteness(intent, domainKnowledge);
                result.setCompleteness(completeness);

                // 4. 如果知识不完整，生成问题
                if (completeness.getScore() < 0.7 || intent.hasMissingInfo()) {
                    result.setNeedsUserInput(true);
                    result.setQuestionsForUser(generateQuestions(intent));
                }
            } else {
                // 没有找到知识
                result.setCompleteness(KnowledgeCompleteness.builder()
                        .score(0.0)
                        .reason("知识库中未找到相关内容")
                        .build());
                result.setNeedsUserInput(true);
                result.setQuestionsForUser(generateQuestions(intent));
            }

        } catch (Exception e) {
            log.error("检索和评估知识失败", e);
            result.setHasKnowledge(false);
            result.setNeedsUserInput(false);
        }

        return result;
    }

    /**
     * 评估知识完整性
     */
    private KnowledgeCompleteness evaluateCompleteness(
            IntentAnalysisResult intent,
            Map<String, List<Document>> knowledge) {

        // 简化版评估逻辑
        int totalDocs = knowledge.values().stream()
                .mapToInt(List::size)
                .sum();

        // 基础评分
        double baseScore = Math.min(totalDocs * 0.2, 1.0);

        // 如果有缺失信息，降低评分
        if (intent.hasMissingInfo()) {
            baseScore = Math.min(baseScore, 0.6);
        }

        return KnowledgeCompleteness.builder()
                .score(baseScore)
                .missing(new ArrayList<>(intent.getMissingInfo()))
                .reason(baseScore >= 0.7 ? "知识充足" : "知识不足或需要更多上下文")
                .build();
    }

    /**
     * 生成向用户提问的问题
     */
    private List<String> generateQuestions(IntentAnalysisResult intent) {
        List<String> questions = new ArrayList<>();

        for (String missingInfo : intent.getMissingInfo()) {
            String question = switch (missingInfo.toLowerCase()) {
                case "技术栈", "项目框架", "framework" ->
                    "您使用的是什么技术栈？（如：Spring Boot, Node.js, Django等）";
                case "安全要求", "security" ->
                    "有什么特殊的安全要求吗？";
                case "数据库", "database" ->
                    "您使用的是什么数据库？（如：MySQL, PostgreSQL, MongoDB等）";
                default ->
                    "关于" + missingInfo + "，能否提供更多详细信息？";
            };
            questions.add(question);
        }

        return questions;
    }

    /**
     * 生成请求更多信息的回复
     */
    private String generateRequestForInfo(List<String> questions) {
        if (questions.isEmpty()) {
            return "为了更好地帮助您，我需要了解更多信息。";
        }

        StringBuilder sb = new StringBuilder();
        sb.append("为了更好地帮助您，我需要了解一些额外信息：\n\n");

        for (int i = 0; i < questions.size(); i++) {
            sb.append(String.format("%d. %s\n", i + 1, questions.get(i)));
        }

        sb.append("\n请提供这些信息，我将为您生成更准确的答案。");
        return sb.toString();
    }

    /**
     * 生成完整答案
     */
    private String generateFullAnswer(
            String question,
            IntentAnalysisResult intent,
            KnowledgeGapResult gapResult,
            Conversation conversation) {

        // 1. 整合所有知识
        String consolidatedKnowledge = consolidateKnowledge(gapResult.getRetrievedKnowledge());

        // 2. 构建提示词
        String prompt = buildAnswerPrompt(question, intent, consolidatedKnowledge, conversation);

        // 3. 调用 AI 生成回答
        return aiService.chat(prompt);
    }

    /**
     * 整合知识
     */
    private String consolidateKnowledge(Map<String, List<Document>> domainKnowledge) {
        if (domainKnowledge.isEmpty()) {
            return "（暂无相关知识）";
        }

        StringBuilder sb = new StringBuilder();
        int index = 1;

        for (Map.Entry<String, List<Document>> entry : domainKnowledge.entrySet()) {
            for (Document doc : entry.getValue()) {
                sb.append(String.format("\n【知识%d】\n%s\n", index++, doc.getContent()));
            }
        }

        return sb.toString();
    }

    /**
     * 构建回答提示词
     */
    private String buildAnswerPrompt(
            String question,
            IntentAnalysisResult intent,
            String knowledge,
            Conversation conversation) {

        String conversationHistory = conversationManager.formatConversationHistory(
                conversation.getConversationId());

        return String.format("""
                你是一个专业的技术助手。请基于以下信息回答用户的问题。
                
                ## 用户问题
                %s
                
                ## 意图分析
                - 核心意图：%s
                - 技术栈：%s
                
                ## 知识库内容
                %s
                
                ## 对话历史
                %s
                
                ## 回答要求
                1. 直接回答用户的问题
                2. 提供具体的实现步骤或解决方案
                3. 包含代码示例（如果适用）
                4. 说明关键注意事项
                5. 使用清晰的 Markdown 格式
                6. 如果知识库内容不足，请基于你的专业知识补充
                
                请生成专业、准确、实用的回答。
                """,
                question,
                intent.getIntent(),
                String.join(", ", intent.getTechStack()),
                knowledge,
                conversationHistory
        );
    }

    /**
     * 提取参考文档
     */
    private List<Document> extractReferences(KnowledgeGapResult gapResult) {
        return gapResult.getRetrievedKnowledge().values().stream()
                .flatMap(List::stream)
                .collect(Collectors.toList());
    }

    /**
     * 转换 KnowledgeDocument 到 Document
     */
    private Document convertToDocument(KnowledgeDocument kDoc) {
        return Document.builder()
                .id(kDoc.getId())
                .content(kDoc.getContent())
                .build();
    }
}

