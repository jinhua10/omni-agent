package top.yumbo.ai.omni.integration;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import top.yumbo.ai.omni.core.feedback.FeedbackService;
import top.yumbo.ai.omni.core.hope.HOPEKnowledgeManager;
import top.yumbo.ai.omni.core.hope.QuestionClassifier;
import top.yumbo.ai.omni.core.knowledge.KnowledgeLoader;
import top.yumbo.ai.omni.core.query.QueryService;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.persistence.api.model.QuestionTypeConfig;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.SearchResult;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * 服务集成测试
 * 
 * @author OmniAgent Team
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("Service Integration Tests")
class ServiceIntegrationTest {

    @Mock
    private QuestionClassifierPersistence persistence;

    @Mock
    private RAGService ragService;

    private QuestionClassifier questionClassifier;
    private HOPEKnowledgeManager hopeManager;
    private FeedbackService feedbackService;
    private QueryService queryService;
    private KnowledgeLoader knowledgeLoader;

    @BeforeEach
    void setUp() {
        questionClassifier = new QuestionClassifier(persistence);
        feedbackService = new FeedbackService();
        queryService = new QueryService(ragService);
        knowledgeLoader = new KnowledgeLoader(10);
    }

    @Test
    @DisplayName("应该支持查询-反馈-学习工作流")
    void shouldSupportQueryFeedbackLearningWorkflow() {
        // Given - 配置查询服务
        List<SearchResult> mockResults = createMockResults(3);
        when(ragService.searchByText(anyString(), anyInt())).thenReturn(mockResults);

        // When - 执行查询
        List<SearchResult> results = queryService.search("如何学习Java", 10);

        // Then - 验证查询结果
        assertThat(results).hasSize(3);

        // And - 收集用户反馈
        feedbackService.collectExplicitFeedback("session-001", "user-001", 1.0);

        // And - 验证反馈已记录
        assertThat(feedbackService.getSessionFeedback("session-001")).hasSize(1);
    }

    @Test
    @DisplayName("应该支持问题分类与知识检索集成")
    void shouldSupportQuestionClassificationWithKnowledgeRetrieval() {
        // Given - 配置问题分类器
        QuestionTypeConfig skillType = QuestionTypeConfig.builder()
                .id("SKILL")
                .name("技能类")
                .enabled(true)
                .build();

        when(persistence.getAllQuestionTypes()).thenReturn(List.of(skillType));
        when(persistence.getKeywords("SKILL")).thenReturn(List.of("Java", "编程", "学习"));

        // And - 配置查询服务
        when(ragService.searchByText(anyString(), anyInt()))
                .thenReturn(createMockResults(5));

        // When - 分类问题
        questionClassifier.loadQuestionTypes();
        String questionType = questionClassifier.classify("如何学习Java编程");

        // Then - 验证分类
        assertThat(questionType).isEqualTo("SKILL");

        // And - 基于分类执行查询
        List<SearchResult> results = queryService.search("Java编程教程", 5);
        assertThat(results).hasSize(5);
    }

    @Test
    @DisplayName("应该支持知识加载与缓存")
    void shouldSupportKnowledgeLoadingWithCache() {
        // Given
        String key1 = "concept-001";
        String key2 = "concept-002";
        String value1 = "Java Programming";
        String value2 = "Design Patterns";

        // When - 加载知识
        knowledgeLoader.load(key1, k -> value1);
        knowledgeLoader.load(key2, k -> value2);

        // Then - 验证缓存命中
        String cached1 = knowledgeLoader.get(key1);
        String cached2 = knowledgeLoader.get(key2);

        assertThat(cached1).isEqualTo(value1);
        assertThat(cached2).isEqualTo(value2);
    }

    @Test
    @DisplayName("应该支持多服务协同处理用户请求")
    void shouldSupportMultiServiceCollaboration() {
        // Given - 配置所有服务
        when(persistence.getAllQuestionTypes()).thenReturn(List.of(
                QuestionTypeConfig.builder().id("SKILL").name("技能类").enabled(true).build()
        ));
        when(persistence.getKeywords("SKILL")).thenReturn(List.of("编程"));
        when(ragService.searchByText(anyString(), anyInt())).thenReturn(createMockResults(5));

        // When - 用户提问
        String question = "如何学习编程";

        // Step 1: 问题分类
        questionClassifier.loadQuestionTypes();
        String type = questionClassifier.classify(question);

        // Step 2: 执行查询
        List<SearchResult> results = queryService.search(question, 5);

        // Step 3: 收集反馈
        feedbackService.collectExplicitFeedback("session-123", "user-123", 1.0);

        // Then - 验证整个流程
        assertThat(type).isEqualTo("SKILL");
        assertThat(results).hasSize(5);
        assertThat(feedbackService.getSessionFeedback("session-123")).hasSize(1);
    }

    @Test
    @DisplayName("应该支持反馈驱动的服务改进")
    void shouldSupportFeedbackDrivenImprovement() {
        // Given - 收集多个反馈
        String sessionId = "session-feedback";
        String userId = "user-feedback";

        // When - 收集正面和负面反馈
        feedbackService.collectExplicitFeedback(sessionId, userId, 1.0);
        feedbackService.collectExplicitFeedback(sessionId, userId, -1.0);
        feedbackService.collectExplicitFeedback(sessionId, userId, 1.0);

        // Then - 验证反馈统计
        var feedback = feedbackService.getSessionFeedback(sessionId);
        assertThat(feedback).hasSize(3);

        // And - 验证可以基于反馈进行改进
        long positiveFeedback = feedback.stream().filter(f -> f.getValue() > 0).count();
        long negativeFeedback = feedback.stream().filter(f -> f.getValue() < 0).count();

        assertThat(positiveFeedback).isEqualTo(2);
        assertThat(negativeFeedback).isEqualTo(1);
    }

    @Test
    @DisplayName("应该处理服务异常并保持稳定")
    void shouldHandleServiceExceptionsGracefully() {
        // Given - 查询服务抛出异常
        when(ragService.searchByText(anyString(), anyInt()))
                .thenThrow(new RuntimeException("Service unavailable"));

        // When/Then - 异常应该被传播但不影响其他服务
        assertThatThrownBy(() -> queryService.search("test", 10))
                .isInstanceOf(RuntimeException.class);

        // And - 反馈服务仍然正常工作
        assertThatNoException().isThrownBy(() ->
                feedbackService.collectExplicitFeedback("session", "user", 1.0));
    }

    @Test
    @DisplayName("应该支持知识检索的混合策略")
    void shouldSupportHybridRetrievalStrategy() {
        // Given - 配置混合检索
        float[] embedding = new float[]{0.1f, 0.2f, 0.3f};
        String queryText = "机器学习算法";

        when(ragService.hybridSearch(any())).thenReturn(createMockResults(10));

        // When - 执行混合检索
        List<SearchResult> results = queryService.hybridSearch(queryText, embedding, 10);

        // Then - 验证结果
        assertThat(results).hasSize(10);
        verify(ragService).hybridSearch(any());
    }

    @Test
    @DisplayName("应该支持批量查询处理")
    void shouldSupportBatchQueryProcessing() {
        // Given
        when(ragService.searchByText(anyString(), anyInt())).thenReturn(createMockResults(3));

        // When - 批量查询
        List<String> queries = List.of("query1", "query2", "query3", "query4", "query5");
        List<List<SearchResult>> allResults = new ArrayList<>();

        for (String query : queries) {
            allResults.add(queryService.search(query, 3));
        }

        // Then - 验证所有查询都成功
        assertThat(allResults).hasSize(5);
        assertThat(allResults).allMatch(results -> results.size() == 3);

        // And - 验证查询统计
        var stats = queryService.getStatistics();
        assertThat(stats.get("totalQueries")).isEqualTo(5);
    }

    @Test
    @DisplayName("应该支持知识缓存失效和重载")
    void shouldSupportCacheInvalidationAndReload() {
        // Given
        String key = "cached-knowledge";
        String value1 = "Initial Value";
        String value2 = "Updated Value";

        // When - 初始加载
        knowledgeLoader.load(key, k -> value1);
        String initial = knowledgeLoader.get(key);

        // And - 使缓存失效
        knowledgeLoader.invalidate(key);

        // And - 重新加载
        knowledgeLoader.load(key, k -> value2);
        String updated = knowledgeLoader.get(key);

        // Then
        assertThat(initial).isEqualTo(value1);
        assertThat(updated).isEqualTo(value2);
    }

    @Test
    @DisplayName("应该支持会话级别的反馈跟踪")
    void shouldSupportSessionLevelFeedbackTracking() {
        // Given - 多个会话
        String session1 = "session-001";
        String session2 = "session-002";
        String user = "user-001";

        // When - 为不同会话收集反馈
        feedbackService.collectExplicitFeedback(session1, user, 1.0);
        feedbackService.collectExplicitFeedback(session1, user, 1.0);
        feedbackService.collectExplicitFeedback(session2, user, -1.0);

        // Then - 验证会话隔离
        assertThat(feedbackService.getSessionFeedback(session1)).hasSize(2);
        assertThat(feedbackService.getSessionFeedback(session2)).hasSize(1);

        // And - 验证用户级别反馈
        var userFeedback = feedbackService.getUserFeedback(user);
        assertThat(userFeedback).hasSize(3);
    }

    // Helper methods

    private List<SearchResult> createMockResults(int count) {
        List<SearchResult> results = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            Document doc = Document.builder()
                    .id("doc-" + i)
                    .content("Content " + i)
                    .build();

            results.add(SearchResult.builder()
                    .document(doc)
                    .score(0.9f - i * 0.1f)
                    .build());
        }
        return results;
    }
}
