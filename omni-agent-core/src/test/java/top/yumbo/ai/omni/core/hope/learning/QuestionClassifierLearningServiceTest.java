package top.yumbo.ai.omni.core.hope.learning;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.quality.Strictness;
import org.mockito.junit.jupiter.MockitoSettings;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.persistence.api.model.QuestionTypeConfig;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * QuestionClassifierLearningService 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
@DisplayName("QuestionClassifierLearningService Tests")
class QuestionClassifierLearningServiceTest {

    @Mock
    private QuestionClassifierPersistence persistence;

    private QuestionClassifierLearningService learningService;

    @BeforeEach
    void setUp() {
        learningService = new QuestionClassifierLearningService(persistence);
    }

    @Test
    @DisplayName("应该记录分类结果")
    void shouldRecordClassification() {
        // When
        learningService.recordClassification("测试问题", "SKILL", "SKILL", 0.9);

        // Then
        QuestionClassifierLearningService.LearningStatistics stats = learningService.getStatistics();
        assertThat(stats.getCacheSize()).isEqualTo(1);
    }

    @Test
    @DisplayName("应该忽略空问题")
    void shouldIgnoreEmptyQuestion() {
        // When
        learningService.recordClassification(null, "SKILL", "SKILL", 0.9);
        learningService.recordClassification("", "SKILL", "SKILL", 0.9);
        learningService.recordClassification("   ", "SKILL", "SKILL", 0.9);

        // Then
        QuestionClassifierLearningService.LearningStatistics stats = learningService.getStatistics();
        assertThat(stats.getCacheSize()).isEqualTo(0);
    }

    @Test
    @DisplayName("应该从用户反馈学习")
    void shouldLearnFromFeedback() {
        // Given
        QuestionTypeConfig config = QuestionTypeConfig.builder()
                .id("SKILL")
                .name("技能类")
                .build();

        when(persistence.getQuestionType("SKILL")).thenReturn(Optional.of(config));
        when(persistence.getKeywords("SKILL")).thenReturn(List.of("技能", "能力"));

        // When
        learningService.learnFromFeedback("如何学习编程技能？", "SKILL");

        // Then
        verify(persistence).getQuestionType("SKILL");
        verify(persistence).getKeywords("SKILL");
        verify(persistence).saveKeywords(eq("SKILL"), anyList());
    }

    @Test
    @DisplayName("应该忽略无效的反馈")
    void shouldIgnoreInvalidFeedback() {
        // When
        learningService.learnFromFeedback(null, "SKILL");
        learningService.learnFromFeedback("问题", null);

        // Then
        verify(persistence, never()).getQuestionType(anyString());
    }

    @Test
    @DisplayName("应该批量学习")
    void shouldPerformBatchLearning() {
        // Given
        when(persistence.getAllQuestionTypes()).thenReturn(new ArrayList<>());

        // When - 添加记录但不触发自动学习
        for (int i = 0; i < 50; i++) {
            learningService.recordClassification("问题" + i, "SKILL", "SKILL", 0.9);
        }

        // Then
        QuestionClassifierLearningService.LearningStatistics stats = learningService.getStatistics();
        assertThat(stats.getCacheSize()).isEqualTo(50);
    }

    @Test
    @DisplayName("应该在达到阈值时自动学习")
    void shouldAutoLearnWhenThresholdReached() {
        // Given
        when(persistence.getAllQuestionTypes()).thenReturn(new ArrayList<>());

        // When - 添加超过阈值的记录
        for (int i = 0; i < 100; i++) {
            learningService.recordClassification("问题" + i, "SKILL", "SKILL", 0.9);
        }

        // Then - 缓存应该被清空
        QuestionClassifierLearningService.LearningStatistics stats = learningService.getStatistics();
        assertThat(stats.getCacheSize()).isEqualTo(0);
    }

    @Test
    @DisplayName("应该手动触发学习")
    void shouldTriggerLearningManually() {
        // Given
        when(persistence.getAllQuestionTypes()).thenReturn(new ArrayList<>());

        learningService.recordClassification("问题1", "SKILL", "SKILL", 0.9);
        learningService.recordClassification("问题2", "SKILL", "SKILL", 0.8);

        // When
        learningService.triggerLearning();

        // Then
        QuestionClassifierLearningService.LearningStatistics stats = learningService.getStatistics();
        assertThat(stats.getCacheSize()).isEqualTo(0);
    }

    @Test
    @DisplayName("应该获取学习统计")
    void shouldGetStatistics() {
        // Given
        when(persistence.getAllQuestionTypes()).thenReturn(List.of(
                QuestionTypeConfig.builder().id("SKILL").build(),
                QuestionTypeConfig.builder().id("KNOWLEDGE").build()
        ));

        learningService.recordClassification("问题1", "SKILL", "SKILL", 0.9);
        learningService.recordClassification("问题2", "SKILL", "SKILL", 0.8);

        // When
        QuestionClassifierLearningService.LearningStatistics stats = learningService.getStatistics();

        // Then
        assertThat(stats.getCacheSize()).isEqualTo(2);
        assertThat(stats.getCacheThreshold()).isEqualTo(100);
        assertThat(stats.getTotalTypes()).isEqualTo(2);
    }

    @Test
    @DisplayName("应该处理学习异常")
    void shouldHandleLearningException() {
        // Given - performLearning会抓取异常，所以需要mock的是persistence.addKeywords
        List<QuestionTypeConfig> types = new ArrayList<>();
        types.add(QuestionTypeConfig.builder().id("SKILL").name("技能类").build());
        when(persistence.getAllQuestionTypes()).thenReturn(types);
        doThrow(new RuntimeException("Database error")).when(persistence).addKeywords(anyString(), anyList());

        // When - 添加多个相同问题以触发高频关键词学习
        for (int i = 0; i < 5; i++) {
            learningService.recordClassification("如何学习Java", "SKILL", "SKILL", 0.9);
        }

        // Then - 不应该抛出异常
        assertThatNoException().isThrownBy(() -> learningService.triggerLearning());
    }

    @Test
    @DisplayName("应该更新高频关键词")
    void shouldUpdateHighFrequencyKeywords() {
        // Given
        List<QuestionTypeConfig> types = new ArrayList<>();
        types.add(QuestionTypeConfig.builder().id("SKILL").name("技能类").build());
        when(persistence.getAllQuestionTypes()).thenReturn(types);

        // When - 添加多个包含相同关键词的问题
        for (int i = 0; i < 5; i++) {
            learningService.recordClassification("如何学习Java编程？", "SKILL", "SKILL", 0.9);
        }

        learningService.triggerLearning();

        // Then - 应该添加高频关键词
        verify(persistence, atLeastOnce()).addKeywords(eq("SKILL"), anyList());
    }

    @Test
    @DisplayName("应该过滤低频关键词")
    void shouldFilterLowFrequencyKeywords() {
        // Given
        List<QuestionTypeConfig> types = new ArrayList<>();
        types.add(QuestionTypeConfig.builder().id("SKILL").name("技能类").build());
        when(persistence.getAllQuestionTypes()).thenReturn(types);

        // When - 添加不同的问题（关键词频率低）
        learningService.recordClassification("如何学习Java？", "SKILL", "SKILL", 0.9);
        learningService.recordClassification("Python好学吗？", "SKILL", "SKILL", 0.8);

        learningService.triggerLearning();

        // Then - 不应该添加低频关键词（出现少于3次）
        verify(persistence, never()).addKeywords(anyString(), anyList());
    }

    @Test
    @DisplayName("应该处理空缓存的学习")
    void shouldHandleEmptyCacheLearning() {
        // When
        learningService.triggerLearning();

        // Then - 不应该抛出异常
        verify(persistence, never()).getAllQuestionTypes();
    }

    @Test
    @DisplayName("应该记录预测和实际类型不同的情况")
    void shouldRecordMismatchedPredictions() {
        // When
        learningService.recordClassification("问题", "SKILL", "KNOWLEDGE", 0.6);

        // Then
        QuestionClassifierLearningService.LearningStatistics stats = learningService.getStatistics();
        assertThat(stats.getCacheSize()).isEqualTo(1);
    }

    @Test
    @DisplayName("应该处理反馈学习中的异常")
    void shouldHandleFeedbackLearningException() {
        // Given
        when(persistence.getQuestionType(anyString())).thenThrow(new RuntimeException("Error"));

        // When/Then - 不应该抛出异常
        assertThatNoException().isThrownBy(() ->
                learningService.learnFromFeedback("问题", "SKILL")
        );
    }

    @Test
    @DisplayName("应该只学习实际类型存在的记录")
    void shouldOnlyLearnRecordsWithActualType() {
        // Given
        List<QuestionTypeConfig> types = new ArrayList<>();
        types.add(QuestionTypeConfig.builder().id("SKILL").name("技能类").build());
        types.add(QuestionTypeConfig.builder().id("KNOWLEDGE").name("知识类").build());
        when(persistence.getAllQuestionTypes()).thenReturn(types);

        // When
        learningService.recordClassification("问题1", "SKILL", "SKILL", 0.9);
        learningService.recordClassification("问题2", "SKILL", null, 0.9);
        learningService.recordClassification("问题3", "SKILL", "KNOWLEDGE", 0.8);

        // Then
        QuestionClassifierLearningService.LearningStatistics stats = learningService.getStatistics();
        // 应该有 3 条记录，包括 null actualType 的记录
        assertThat(stats.getCacheSize()).isEqualTo(3);
    }
}
