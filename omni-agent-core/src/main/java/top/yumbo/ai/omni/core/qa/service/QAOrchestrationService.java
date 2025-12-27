package top.yumbo.ai.omni.core.qa.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.core.qa.model.IntelligentQARequest;
import top.yumbo.ai.omni.core.qa.model.IntelligentQAResponse;
import top.yumbo.ai.omni.core.role.Role;
import top.yumbo.ai.omni.core.role.RoleService;
import top.yumbo.ai.omni.core.util.ContextBuilder;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;

import java.util.List;
import java.util.Map;
import java.util.HashMap;

/**
 * 问答编排服务
 *
 * <p>负责协调不同知识模式的问答流程，从 Controller 中解耦业务逻辑</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class QAOrchestrationService {

    @Autowired
    private AIService aiService;

    @Autowired
    private RagService ragService;

    @Autowired
    private RoleService roleService;

    @Autowired(required = false)
    private IntelligentQAService intelligentQAService;

    /**
     * 执行问答（统一入口）
     */
    public QAResult executeQA(QARequest request) {
        String knowledgeMode = request.getKnowledgeMode();

        try {
            return switch (knowledgeMode.toLowerCase()) {
                case "intelligent", "none" -> executeIntelligentQA(request);
                case "role" -> executeRoleQA(request);
                case "rag" -> executeRAGQA(request);
                default -> executeRAGQA(request);
            };
        } catch (Exception e) {
            log.error("问答执行失败: mode={}", knowledgeMode, e);
            return QAResult.error(e.getMessage());
        }
    }

    /**
     * 执行智能问答
     */
    private QAResult executeIntelligentQA(QARequest request) {
        if (intelligentQAService != null) {
            try {
                IntelligentQARequest qaRequest = IntelligentQARequest.builder()
                        .question(request.getQuestion())
                        .conversationId(request.getConversationId())
                        .userId(request.getUserId())
                        .build();

                IntelligentQAResponse qaResponse = intelligentQAService.ask(qaRequest);

                return QAResult.builder()
                        .answer(qaResponse.getAnswer())
                        .references(qaResponse.getReferences())
                        .conversationId(qaResponse.getConversationId())
                        .hasKnowledge(qaResponse.getHasKnowledge())
                        .knowledgeSufficient(qaResponse.getKnowledgeSufficient())
                        .needsMoreInfo(qaResponse.getNeedsMoreInfo())
                        .intentAnalysis(buildIntentAnalysisMap(qaResponse))
                        .success(true)
                        .build();
            } catch (Exception e) {
                log.warn("智能问答失败，降级到直接 AI: {}", e.getMessage());
                // 降级到直接 AI
                String answer = aiService.chat(request.getQuestion());
                return QAResult.builder()
                        .answer(answer)
                        .success(true)
                        .build();
            }
        } else {
            // 智能问答服务不可用
            log.info("智能问答服务未启用，使用直接 AI");
            String answer = aiService.chat(request.getQuestion());
            return QAResult.builder()
                    .answer(answer)
                    .success(true)
                    .build();
        }
    }

    /**
     * 执行角色问答
     */
    private QAResult executeRoleQA(QARequest request) {
        String roleName = request.getRoleName();
        if (roleName == null || roleName.isEmpty()) {
            return QAResult.error("roleName is required for role mode");
        }

        Role roleEntity = roleService.getRole(roleName);
        List<Document> roleDocuments = ragService.semanticSearch(request.getQuestion(), 5);

        String roleContext = ContextBuilder.buildRoleContext(
                roleDocuments.stream()
                        .map(top.yumbo.ai.omni.rag.model.SearchResult::fromDocument)
                        .toList()
        );

        String rolePrompt = String.format(
                "你是%s，%s\n\n基于以下知识回答问题：\n\n%s\n\n问题：%s",
                roleEntity.getName(),
                roleEntity.getDescription(),
                roleContext,
                request.getQuestion()
        );

        String answer = aiService.chat(rolePrompt);

        return QAResult.builder()
                .answer(answer)
                .references(roleDocuments)
                .success(true)
                .build();
    }

    /**
     * 执行 RAG 问答
     */
    private QAResult executeRAGQA(QARequest request) {
        List<Document> ragDocuments = ragService.semanticSearch(request.getQuestion(), 5);

        String context = ContextBuilder.buildContext(
                ragDocuments.stream()
                        .map(top.yumbo.ai.omni.rag.model.SearchResult::fromDocument)
                        .toList()
        );

        String prompt = String.format(
                "基于以下知识回答问题：\n\n%s\n\n问题：%s",
                context,
                request.getQuestion()
        );

        String answer = aiService.chat(prompt);

        return QAResult.builder()
                .answer(answer)
                .references(ragDocuments)
                .success(true)
                .build();
    }

    /**
     * 构建提示词（用于流式问答）
     */
    public String buildPrompt(String question, String knowledgeMode, String roleName) {
        if ("none".equals(knowledgeMode)) {
            return question;
        } else if ("role".equals(knowledgeMode) && roleName != null) {
            Role role = roleService.getRole(roleName);
            var documents = ragService.semanticSearch(question, 5);
            var references = documents.stream()
                    .map(top.yumbo.ai.omni.rag.model.SearchResult::fromDocument)
                    .toList();
            String context = ContextBuilder.buildRoleContext(references);
            return String.format(
                    "你是%s，%s\n\n基于以下知识回答问题：\n\n%s\n\n问题：%s",
                    role.getName(), role.getDescription(), context, question
            );
        } else {
            var documents = ragService.semanticSearch(question, 5);
            var references = documents.stream()
                    .map(top.yumbo.ai.omni.rag.model.SearchResult::fromDocument)
                    .toList();
            String context = ContextBuilder.buildContext(references);
            return String.format("基于以下知识回答问题：\n\n%s\n\n问题：%s", context, question);
        }
    }

    /**
     * 构建智能问答的增强提示词（用于流式）
     */
    public String buildEnhancedPrompt(String question, IntelligentQAResponse qaResponse) {
        StringBuilder enhancedPrompt = new StringBuilder();
        enhancedPrompt.append("用户问题：").append(question).append("\n\n");

        if (qaResponse.getIntent() != null && qaResponse.getIntent().getIntent() != null) {
            enhancedPrompt.append("意图分析：").append(qaResponse.getIntent().getIntent()).append("\n\n");
        }

        if (qaResponse.getReferences() != null && !qaResponse.getReferences().isEmpty()) {
            enhancedPrompt.append("知识库相关内容：\n");
            int index = 1;
            for (var doc : qaResponse.getReferences()) {
                enhancedPrompt.append("\n【知识").append(index++).append("】\n");
                enhancedPrompt.append(doc.getContent()).append("\n");
            }
            enhancedPrompt.append("\n基于以上知识，请详细回答用户的问题。");
        } else {
            enhancedPrompt.append("请基于你的知识回答用户的问题。");
        }

        return enhancedPrompt.toString();
    }

    /**
     * 构建意图分析映射
     */
    private Map<String, Object> buildIntentAnalysisMap(IntelligentQAResponse qaResponse) {
        if (qaResponse.getIntent() == null) {
            return null;
        }

        Map<String, Object> intentAnalysis = new HashMap<>();
        intentAnalysis.put("intent", qaResponse.getIntent().getIntent());
        intentAnalysis.put("entities", qaResponse.getIntent().getEntities());
        intentAnalysis.put("techStack", qaResponse.getIntent().getTechStack());
        intentAnalysis.put("missingInfo", qaResponse.getIntent().getMissingInfo());
        intentAnalysis.put("confidence", qaResponse.getIntent().getConfidence());

        return intentAnalysis;
    }

    // ========== 内部类 ==========

    /**
     * 问答请求
     */
    public static class QARequest {
        private String question;
        private String knowledgeMode;
        private String roleName;
        private String conversationId;
        private String userId;

        public static QARequest of(String question, String knowledgeMode, String roleName,
                                   String conversationId, String userId) {
            QARequest req = new QARequest();
            req.question = question;
            req.knowledgeMode = knowledgeMode != null ? knowledgeMode : "rag";
            req.roleName = roleName;
            req.conversationId = conversationId;
            req.userId = userId;
            return req;
        }

        // Getters
        public String getQuestion() { return question; }
        public String getKnowledgeMode() { return knowledgeMode; }
        public String getRoleName() { return roleName; }
        public String getConversationId() { return conversationId; }
        public String getUserId() { return userId; }
    }

    /**
     * 问答结果
     */
    public static class QAResult {
        private boolean success;
        private String answer;
        private List<Document> references;
        private String conversationId;
        private Boolean hasKnowledge;
        private Boolean knowledgeSufficient;
        private Boolean needsMoreInfo;
        private Map<String, Object> intentAnalysis;
        private String error;

        public static QAResult error(String error) {
            QAResult result = new QAResult();
            result.success = false;
            result.error = error;
            return result;
        }

        public static QAResultBuilder builder() {
            return new QAResultBuilder();
        }

        // Getters
        public boolean isSuccess() { return success; }
        public String getAnswer() { return answer; }
        public List<Document> getReferences() { return references; }
        public String getConversationId() { return conversationId; }
        public Boolean getHasKnowledge() { return hasKnowledge; }
        public Boolean getKnowledgeSufficient() { return knowledgeSufficient; }
        public Boolean getNeedsMoreInfo() { return needsMoreInfo; }
        public Map<String, Object> getIntentAnalysis() { return intentAnalysis; }
        public String getError() { return error; }

        public static class QAResultBuilder {
            private final QAResult result = new QAResult();

            public QAResultBuilder success(boolean success) {
                result.success = success;
                return this;
            }

            public QAResultBuilder answer(String answer) {
                result.answer = answer;
                return this;
            }

            public QAResultBuilder references(List<Document> references) {
                result.references = references;
                return this;
            }

            public QAResultBuilder conversationId(String conversationId) {
                result.conversationId = conversationId;
                return this;
            }

            public QAResultBuilder hasKnowledge(Boolean hasKnowledge) {
                result.hasKnowledge = hasKnowledge;
                return this;
            }

            public QAResultBuilder knowledgeSufficient(Boolean knowledgeSufficient) {
                result.knowledgeSufficient = knowledgeSufficient;
                return this;
            }

            public QAResultBuilder needsMoreInfo(Boolean needsMoreInfo) {
                result.needsMoreInfo = needsMoreInfo;
                return this;
            }

            public QAResultBuilder intentAnalysis(Map<String, Object> intentAnalysis) {
                result.intentAnalysis = intentAnalysis;
                return this;
            }

            public QAResult build() {
                return result;
            }
        }
    }
}

