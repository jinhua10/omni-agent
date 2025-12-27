package top.yumbo.ai.omni.core.hope;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.omni.persistence.api.model.QuestionTypeConfig;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * QuestionClassifier 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("QuestionClassifier 测试")
class QuestionClassifierTest {

    @Mock
    private QuestionClassifierPersistence persistence;

    private QuestionClassifier classifier;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        classifier = new QuestionClassifier(persistence);
    }

    @Test
    @DisplayName("分类简单的如何问题")
    void testClassifySimpleHowQuestion() {
        // Given
        String question = "如何学习Java?";
        QuestionTypeConfig config = createConfig("HOW");
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.singletonList(config));
        when(persistence.getKeywords("HOW")).thenReturn(Arrays.asList("如何", "怎么", "怎样"));
        when(persistence.getPatterns("HOW")).thenReturn(Collections.emptyList());
        when(persistence.getVersion()).thenReturn("1.0.0");
        classifier.init();

        // When
        QuestionClassifier.ClassificationResult result = classifier.classify(question);

        // Then
        assertNotNull(result);
        assertEquals("HOW", result.getQuestionType());
        assertTrue(result.getConfidence() >= 0.0, "Confidence should be non-negative");
    }

    @Test
    @DisplayName("分类简单的为什么问题")
    void testClassifySimpleWhyQuestion() {
        // Given
        String question = "为什么Java这么流行?";
        QuestionTypeConfig config = createConfig("WHY");
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.singletonList(config));
        when(persistence.getKeywords("WHY")).thenReturn(Arrays.asList("为什么", "为何", "怎么会"));
        when(persistence.getPatterns("WHY")).thenReturn(Collections.emptyList());
        when(persistence.getVersion()).thenReturn("1.0.0");
        classifier.init();

        // When
        QuestionClassifier.ClassificationResult result = classifier.classify(question);

        // Then
        assertNotNull(result);
        assertEquals("WHY", result.getQuestionType());
    }

    @Test
    @DisplayName("分类简单的是什么问题")
    void testClassifySimpleWhatQuestion() {
        // Given
        String question = "什么是Spring Boot?";
        QuestionTypeConfig config = createConfig("WHAT");
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.singletonList(config));
        when(persistence.getKeywords("WHAT")).thenReturn(Arrays.asList("什么是", "啥是", "何为"));
        when(persistence.getPatterns("WHAT")).thenReturn(Collections.emptyList());
        when(persistence.getVersion()).thenReturn("1.0.0");
        classifier.init();

        // When
        QuestionClassifier.ClassificationResult result = classifier.classify(question);

        // Then
        assertNotNull(result);
        assertEquals("WHAT", result.getQuestionType());
    }

    @Test
    @DisplayName("分类未知类型返回default")
    void testClassifyUnknownQuestion() {
        // Given
        String question = "今天天气真好";
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.emptyList());
        when(persistence.getVersion()).thenReturn("1.0.0");
        classifier.init();

        // When
        QuestionClassifier.ClassificationResult result = classifier.classify(question);

        // Then
        assertNotNull(result);
        assertEquals("default", result.getQuestionType());
    }

    @Test
    @DisplayName("处理null问题")
    void testClassifyNullQuestion() {
        // When
        QuestionClassifier.ClassificationResult result = classifier.classify(null);

        // Then
        assertNotNull(result);
        assertEquals("default", result.getQuestionType());
        assertEquals(0.0, result.getConfidence());
    }

    @Test
    @DisplayName("处理空字符串问题")
    void testClassifyEmptyQuestion() {
        // When
        QuestionClassifier.ClassificationResult result = classifier.classify("");

        // Then
        assertNotNull(result);
        assertEquals("default", result.getQuestionType());
        assertEquals(0.0, result.getConfidence());
    }

    @Test
    @DisplayName("重新加载配置")
    void testReloadConfiguration() {
        // Given
        QuestionTypeConfig config = createConfig("NEW_TYPE");
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.singletonList(config));
        when(persistence.getKeywords("NEW_TYPE")).thenReturn(Arrays.asList("新关键词"));
        when(persistence.getPatterns("NEW_TYPE")).thenReturn(Collections.emptyList());
        when(persistence.getVersion()).thenReturn("1.0.1");

        // When
        classifier.reload();

        // Then
        verify(persistence, atLeastOnce()).getAllQuestionTypes();
    }

    @Test
    @DisplayName("获取所有问题类型")
    void testGetAllQuestionTypes() {
        // Given
        QuestionTypeConfig config1 = createConfig("TYPE1");
        QuestionTypeConfig config2 = createConfig("TYPE2");
        when(persistence.getAllQuestionTypes()).thenReturn(Arrays.asList(config1, config2));
        when(persistence.getKeywords(anyString())).thenReturn(Collections.emptyList());
        when(persistence.getPatterns(anyString())).thenReturn(Collections.emptyList());
        when(persistence.getVersion()).thenReturn("1.0.0");
        classifier.init();

        // When
        List<QuestionTypeConfig> types = classifier.getAllQuestionTypes();

        // Then
        assertNotNull(types);
        assertEquals(2, types.size());
    }

    // Helper methods
    private QuestionTypeConfig createConfig(String typeId) {
        return QuestionTypeConfig.builder()
            .id(typeId)
            .name(typeId + "类型")
            .nameEn(typeId + " Type")
            .priority(100)
            .complexity("MEDIUM")
            .suggestedLayer("ORDINARY")
            .enabled(true)
            .description("Test config for " + typeId)
            .createdAt(System.currentTimeMillis())
            .build();
    }
}


