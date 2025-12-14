package top.yumbo.ai.omni.core.hope;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

/**
 * HOPEKnowledgeManager 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("HOPEKnowledgeManager Tests")
class HOPEKnowledgeManagerTest {

    private QuestionClassifier mockClassifier;
    private HOPEKnowledgeManager hopeManager;

    @BeforeEach
    void setUp() {
        mockClassifier = mock(QuestionClassifier.class);
        hopeManager = new HOPEKnowledgeManager(mockClassifier);
    }

    @Test
    @DisplayName("智能查询 - 事实型问题")
    void testSmartQuery_FactualQuestion() {
        // Given
        String question = "北京的首都是什么？";
        String sessionId = "session-123";

        QuestionClassifier.ClassificationResult classification =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("FACTUAL")
                .suggestedLayer("PERMANENT")
                .confidence(0.95)
                .build();

        when(mockClassifier.classify(question))
            .thenReturn(classification);

        // When
        HOPEKnowledgeManager.QueryResult result = hopeManager.smartQuery(question, sessionId);

        // Then
        assertNotNull(result);
        assertEquals("FACTUAL", result.getQuestionType());
        assertEquals("PERMANENT", result.getSuggestedLayer());
        assertEquals(0.95, result.getConfidence(), 0.001);
        assertTrue(result.isNeedsLLM());
        verify(mockClassifier).classify(question);
    }

    @Test
    @DisplayName("智能查询 - 上下文问题")
    void testSmartQuery_ContextualQuestion() {
        // Given
        String question = "刚才那个呢？";
        String sessionId = "session-456";

        QuestionClassifier.ClassificationResult classification =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("CONTEXTUAL")
                .suggestedLayer("HIGH_FREQUENCY")
                .confidence(0.88)
                .build();

        when(mockClassifier.classify(question))
            .thenReturn(classification);

        // When
        HOPEKnowledgeManager.QueryResult result = hopeManager.smartQuery(question, sessionId);

        // Then
        assertNotNull(result);
        assertEquals("CONTEXTUAL", result.getQuestionType());
        assertEquals("HIGH_FREQUENCY", result.getSuggestedLayer());
        assertEquals(0.88, result.getConfidence(), 0.001);
        assertTrue(result.isNeedsLLM());
        verify(mockClassifier).classify(question);
    }

    @Test
    @DisplayName("智能查询 - 常见问题")
    void testSmartQuery_CommonQuestion() {
        // Given
        String question = "如何重置密码？";
        String sessionId = "session-789";

        QuestionClassifier.ClassificationResult classification =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("COMMON")
                .suggestedLayer("ORDINARY")
                .confidence(0.92)
                .build();

        when(mockClassifier.classify(question))
            .thenReturn(classification);

        // When
        HOPEKnowledgeManager.QueryResult result = hopeManager.smartQuery(question, sessionId);

        // Then
        assertNotNull(result);
        assertEquals("COMMON", result.getQuestionType());
        assertEquals("ORDINARY", result.getSuggestedLayer());
        assertEquals(0.92, result.getConfidence(), 0.001);
        verify(mockClassifier).classify(question);
    }

    @Test
    @DisplayName("智能查询 - 低置信度")
    void testSmartQuery_LowConfidence() {
        // Given
        String question = "嗯...那个...怎么说呢...";
        String sessionId = "session-low";

        QuestionClassifier.ClassificationResult classification =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("UNCLEAR")
                .suggestedLayer("ORDINARY")
                .confidence(0.35)
                .build();

        when(mockClassifier.classify(question))
            .thenReturn(classification);

        // When
        HOPEKnowledgeManager.QueryResult result = hopeManager.smartQuery(question, sessionId);

        // Then
        assertNotNull(result);
        assertEquals("UNCLEAR", result.getQuestionType());
        assertEquals(0.35, result.getConfidence(), 0.001);
        assertTrue(result.isNeedsLLM());
    }

    @Test
    @DisplayName("智能查询 - 空会话ID")
    void testSmartQuery_EmptySessionId() {
        // Given
        String question = "测试问题";
        String sessionId = "";

        QuestionClassifier.ClassificationResult classification =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("GENERAL")
                .suggestedLayer("ORDINARY")
                .confidence(0.75)
                .build();

        when(mockClassifier.classify(question))
            .thenReturn(classification);

        // When
        HOPEKnowledgeManager.QueryResult result = hopeManager.smartQuery(question, sessionId);

        // Then
        assertNotNull(result);
        assertEquals("GENERAL", result.getQuestionType());
        verify(mockClassifier).classify(question);
    }

    @Test
    @DisplayName("智能查询 - null会话ID")
    void testSmartQuery_NullSessionId() {
        // Given
        String question = "测试问题";

        QuestionClassifier.ClassificationResult classification =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("GENERAL")
                .suggestedLayer("ORDINARY")
                .confidence(0.75)
                .build();

        when(mockClassifier.classify(question))
            .thenReturn(classification);

        // When
        HOPEKnowledgeManager.QueryResult result = hopeManager.smartQuery(question, null);

        // Then
        assertNotNull(result);
        verify(mockClassifier).classify(question);
    }

    @Test
    @DisplayName("智能查询 - 长问题")
    void testSmartQuery_LongQuestion() {
        // Given
        StringBuilder longQuestion = new StringBuilder("我想知道关于");
        for (int i = 0; i < 100; i++) {
            longQuestion.append("人工智能、机器学习、深度学习、");
        }
        longQuestion.append("的详细信息");
        String sessionId = "session-long";

        QuestionClassifier.ClassificationResult classification =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("COMPLEX")
                .suggestedLayer("PERMANENT")
                .confidence(0.80)
                .build();

        when(mockClassifier.classify(anyString()))
            .thenReturn(classification);

        // When
        HOPEKnowledgeManager.QueryResult result = hopeManager.smartQuery(
            longQuestion.toString(), sessionId);

        // Then
        assertNotNull(result);
        assertEquals("COMPLEX", result.getQuestionType());
        verify(mockClassifier).classify(anyString());
    }

    @Test
    @DisplayName("智能查询 - 多次查询")
    void testSmartQuery_MultipleQueries() {
        // Given
        String[] questions = {
            "什么是AI？",
            "AI的应用有哪些？",
            "如何学习AI？"
        };
        String sessionId = "session-multi";

        QuestionClassifier.ClassificationResult classification1 =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("DEFINITION")
                .suggestedLayer("PERMANENT")
                .confidence(0.90)
                .build();

        QuestionClassifier.ClassificationResult classification2 =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("APPLICATION")
                .suggestedLayer("ORDINARY")
                .confidence(0.85)
                .build();

        QuestionClassifier.ClassificationResult classification3 =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("HOWTO")
                .suggestedLayer("ORDINARY")
                .confidence(0.88)
                .build();

        when(mockClassifier.classify(questions[0])).thenReturn(classification1);
        when(mockClassifier.classify(questions[1])).thenReturn(classification2);
        when(mockClassifier.classify(questions[2])).thenReturn(classification3);

        // When
        HOPEKnowledgeManager.QueryResult result1 = hopeManager.smartQuery(questions[0], sessionId);
        HOPEKnowledgeManager.QueryResult result2 = hopeManager.smartQuery(questions[1], sessionId);
        HOPEKnowledgeManager.QueryResult result3 = hopeManager.smartQuery(questions[2], sessionId);

        // Then
        assertEquals("DEFINITION", result1.getQuestionType());
        assertEquals("APPLICATION", result2.getQuestionType());
        assertEquals("HOWTO", result3.getQuestionType());
        verify(mockClassifier, times(3)).classify(anyString());
    }

    @Test
    @DisplayName("智能查询 - 特殊字符问题")
    void testSmartQuery_SpecialCharacters() {
        // Given
        String question = "什么是@#$%^&*()特殊字符？";
        String sessionId = "session-special";

        QuestionClassifier.ClassificationResult classification =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("GENERAL")
                .suggestedLayer("ORDINARY")
                .confidence(0.70)
                .build();

        when(mockClassifier.classify(question))
            .thenReturn(classification);

        // When
        HOPEKnowledgeManager.QueryResult result = hopeManager.smartQuery(question, sessionId);

        // Then
        assertNotNull(result);
        assertEquals("GENERAL", result.getQuestionType());
        verify(mockClassifier).classify(question);
    }

    @Test
    @DisplayName("智能查询 - 英文问题")
    void testSmartQuery_EnglishQuestion() {
        // Given
        String question = "What is artificial intelligence?";
        String sessionId = "session-en";

        QuestionClassifier.ClassificationResult classification =
            QuestionClassifier.ClassificationResult.builder()
                .questionType("DEFINITION")
                .suggestedLayer("PERMANENT")
                .confidence(0.92)
                .build();

        when(mockClassifier.classify(question))
            .thenReturn(classification);

        // When
        HOPEKnowledgeManager.QueryResult result = hopeManager.smartQuery(question, sessionId);

        // Then
        assertNotNull(result);
        assertEquals("DEFINITION", result.getQuestionType());
        assertEquals("PERMANENT", result.getSuggestedLayer());
        verify(mockClassifier).classify(question);
    }

    @Test
    @DisplayName("查询结果 - Builder模式")
    void testQueryResult_Builder() {
        // When
        HOPEKnowledgeManager.QueryResult result = HOPEKnowledgeManager.QueryResult.builder()
            .questionType("TEST")
            .suggestedLayer("ORDINARY")
            .confidence(0.85)
            .needsLLM(true)
            .answer("Test answer")
            .sourceLayer("PERMANENT")
            .build();

        // Then
        assertEquals("TEST", result.getQuestionType());
        assertEquals("ORDINARY", result.getSuggestedLayer());
        assertEquals(0.85, result.getConfidence(), 0.001);
        assertTrue(result.isNeedsLLM());
        assertEquals("Test answer", result.getAnswer());
        assertEquals("PERMANENT", result.getSourceLayer());
    }

    @Test
    @DisplayName("查询结果 - 默认构造")
    void testQueryResult_NoArgsConstructor() {
        // When
        HOPEKnowledgeManager.QueryResult result = new HOPEKnowledgeManager.QueryResult();
        result.setQuestionType("TEST");
        result.setSuggestedLayer("ORDINARY");
        result.setConfidence(0.75);
        result.setNeedsLLM(false);

        // Then
        assertEquals("TEST", result.getQuestionType());
        assertEquals("ORDINARY", result.getSuggestedLayer());
        assertEquals(0.75, result.getConfidence(), 0.001);
        assertFalse(result.isNeedsLLM());
    }
}

