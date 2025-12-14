package top.yumbo.ai.omni.core.hope.layer;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.persistence.api.model.QuestionTypeConfig;

import java.util.*;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

/**
 * 中频层服务测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("中频层服务测试")
class OrdinaryLayerServiceTest {

    @Mock
    private QuestionClassifierPersistence persistence;

    private OrdinaryLayerService ordinaryLayerService;

    @BeforeEach
    void setUp() {
        ordinaryLayerService = new OrdinaryLayerService(persistence);
    }

    @Test
    @DisplayName("应该成功初始化服务")
    void shouldInitializeSuccessfully() {
        // Given
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.emptyList());

        // When
        ordinaryLayerService.init();

        // Then
        assertThat(ordinaryLayerService).isNotNull();
        verify(persistence).getAllQuestionTypes();
    }

    @Test
    @DisplayName("应该加载ORDINARY层知识到缓存")
    void shouldLoadOrdinaryKnowledgeToCache() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        when(persistence.getKeywords(anyString())).thenReturn(Arrays.asList("普通", "知识"));

        // When
        ordinaryLayerService.init();

        // Then
        verify(persistence).getAllQuestionTypes();
        verify(persistence, atLeastOnce()).getKeywords(anyString());
    }

    @Test
    @DisplayName("应该通过关键词查询到知识并标记需要LLM")
    void shouldQueryKnowledgeWithLLMFlag() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        when(persistence.getKeywords("ord-001")).thenReturn(Arrays.asList("spring", "框架"));
        
        ordinaryLayerService.init();

        // When
        OrdinaryLayerService.QueryResult result = 
            ordinaryLayerService.query("如何使用Spring框架?");

        // Then
        assertThat(result).isNotNull();
        assertThat(result.isFound()).isTrue();
        assertThat(result.isNeedsLLM()).isTrue(); // 中频层需要LLM辅助
        assertThat(result.getConfidence()).isEqualTo(0.7);
    }

    @Test
    @DisplayName("应该处理空问题并标记需要LLM")
    void shouldHandleEmptyQuestionWithLLMFlag() {
        // When
        OrdinaryLayerService.QueryResult result = 
            ordinaryLayerService.query("");

        // Then
        assertThat(result).isNotNull();
        assertThat(result.isFound()).isFalse();
        assertThat(result.isNeedsLLM()).isTrue();
    }

    @Test
    @DisplayName("应该处理null问题")
    void shouldHandleNullQuestion() {
        // When
        OrdinaryLayerService.QueryResult result = 
            ordinaryLayerService.query(null);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.isFound()).isFalse();
        assertThat(result.isNeedsLLM()).isTrue();
    }

    @Test
    @DisplayName("应该返回上下文信息")
    void shouldReturnContextInformation() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        when(persistence.getKeywords("ord-001")).thenReturn(Arrays.asList("数据库"));
        
        ordinaryLayerService.init();

        // When
        OrdinaryLayerService.QueryResult result = 
            ordinaryLayerService.query("数据库如何优化?");

        // Then
        assertThat(result.isFound()).isTrue();
        assertThat(result.getContext()).isNotNull();
    }

    @Test
    @DisplayName("应该添加新知识")
    void shouldAddNewKnowledge() {
        // Given
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.emptyList());
        ordinaryLayerService.init();

        QuestionTypeConfig newConfig = createConfig("ord-002", "新知识", "ORDINARY");
        when(persistence.saveQuestionType(newConfig)).thenReturn(true);

        // When
        boolean result = ordinaryLayerService.addKnowledge(newConfig);

        // Then
        assertThat(result).isTrue();
        verify(persistence).saveQuestionType(newConfig);
    }

    @Test
    @DisplayName("应该重新加载知识")
    void shouldReloadKnowledge() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        ordinaryLayerService.init();

        // When
        ordinaryLayerService.reload();

        // Then
        verify(persistence, atLeast(2)).getAllQuestionTypes();
    }

    @Test
    @DisplayName("应该获取所有知识")
    void shouldGetAllKnowledge() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        ordinaryLayerService.init();

        // When
        List<QuestionTypeConfig> result = ordinaryLayerService.getAllKnowledge();

        // Then
        assertThat(result).isNotNull();
        assertThat(result).hasSizeGreaterThanOrEqualTo(1);
    }

    @Test
    @DisplayName("应该过滤非ORDINARY层的知识")
    void shouldFilterNonOrdinaryKnowledge() {
        // Given
        List<QuestionTypeConfig> configs = new ArrayList<>();
        configs.add(createConfig("perm-001", "永久知识", "PERMANENT"));
        configs.add(createConfig("ord-001", "普通知识", "ORDINARY"));
        configs.add(createConfig("high-001", "高频知识", "HIGH_FREQUENCY"));
        
        when(persistence.getAllQuestionTypes()).thenReturn(configs);

        // When
        ordinaryLayerService.init();
        List<QuestionTypeConfig> result = ordinaryLayerService.getAllKnowledge();

        // Then
        assertThat(result).hasSize(1);
        assertThat(result.get(0).getSuggestedLayer()).isEqualTo("ORDINARY");
    }

    @Test
    @DisplayName("应该正确构建关键词索引")
    void shouldBuildKeywordIndex() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        when(persistence.getKeywords("ord-001"))
            .thenReturn(Arrays.asList("微服务", "架构"));

        // When
        ordinaryLayerService.init();
        OrdinaryLayerService.QueryResult result = 
            ordinaryLayerService.query("微服务架构怎么设计?");

        // Then
        assertThat(result.isFound()).isTrue();
    }

    @Test
    @DisplayName("应该返回中等置信度结果")
    void shouldReturnMediumConfidenceResult() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        when(persistence.getKeywords("ord-001")).thenReturn(Arrays.asList("docker"));
        
        ordinaryLayerService.init();

        // When
        OrdinaryLayerService.QueryResult result = 
            ordinaryLayerService.query("docker容器如何管理?");

        // Then
        assertThat(result.isFound()).isTrue();
        assertThat(result.getConfidence()).isEqualTo(0.7); // 中等置信度
    }

    @Test
    @DisplayName("应该处理查询失败")
    void shouldHandleQueryFailure() {
        // Given
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.emptyList());
        ordinaryLayerService.init();

        // When
        OrdinaryLayerService.QueryResult result = 
            ordinaryLayerService.query("不存在的知识");

        // Then
        assertThat(result.isFound()).isFalse();
        assertThat(result.isNeedsLLM()).isTrue();
    }

    // 辅助方法

    private List<QuestionTypeConfig> createTestConfigs() {
        List<QuestionTypeConfig> configs = new ArrayList<>();
        configs.add(createConfig("ord-001", "测试知识", "ORDINARY"));
        return configs;
    }

    private QuestionTypeConfig createConfig(String id, String name, String layer) {
        QuestionTypeConfig config = new QuestionTypeConfig();
        config.setId(id);
        config.setName(name);
        config.setDescription("测试描述");
        config.setSuggestedLayer(layer);
        config.setEnabled(true);
        return config;
    }
}
