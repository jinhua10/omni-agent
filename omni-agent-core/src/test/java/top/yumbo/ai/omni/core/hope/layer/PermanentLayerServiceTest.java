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
 * 低频层服务测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("低频层服务测试")
class PermanentLayerServiceTest {

    @Mock
    private QuestionClassifierPersistence persistence;

    private PermanentLayerService permanentLayerService;

    @BeforeEach
    void setUp() {
        permanentLayerService = new PermanentLayerService(persistence);
    }

    @Test
    @DisplayName("应该成功初始化服务")
    void shouldInitializeSuccessfully() {
        // Given
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.emptyList());

        // When
        permanentLayerService.init();

        // Then
        assertThat(permanentLayerService).isNotNull();
        verify(persistence).getAllQuestionTypes();
    }

    @Test
    @DisplayName("应该加载PERMANENT层知识到缓存")
    void shouldLoadPermanentKnowledgeToCache() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        when(persistence.getKeywords(anyString())).thenReturn(Arrays.asList("测试", "知识"));

        // When
        permanentLayerService.init();

        // Then
        verify(persistence).getAllQuestionTypes();
        verify(persistence, atLeastOnce()).getKeywords(anyString());
    }

    @Test
    @DisplayName("应该通过关键词查询到知识")
    void shouldQueryKnowledgeByKeyword() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        when(persistence.getKeywords("perm-001")).thenReturn(Arrays.asList("java", "编程"));
        
        permanentLayerService.init();

        // When
        PermanentLayerService.QueryResult result = 
            permanentLayerService.query("如何学习Java编程?");

        // Then
        assertThat(result).isNotNull();
        assertThat(result.isFound()).isTrue();
        assertThat(result.getConfidence()).isGreaterThan(0.8);
    }

    @Test
    @DisplayName("应该处理空问题")
    void shouldHandleEmptyQuestion() {
        // When
        PermanentLayerService.QueryResult result = 
            permanentLayerService.query("");

        // Then
        assertThat(result).isNotNull();
        assertThat(result.isFound()).isFalse();
    }

    @Test
    @DisplayName("应该处理null问题")
    void shouldHandleNullQuestion() {
        // When
        PermanentLayerService.QueryResult result = 
            permanentLayerService.query(null);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.isFound()).isFalse();
    }

    @Test
    @DisplayName("应该添加新知识")
    void shouldAddNewKnowledge() {
        // Given
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.emptyList());
        permanentLayerService.init();

        QuestionTypeConfig newConfig = createConfig("perm-002", "新知识", "PERMANENT");
        when(persistence.saveQuestionType(newConfig)).thenReturn(true);

        // When
        boolean result = permanentLayerService.addKnowledge(newConfig);

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
        permanentLayerService.init();

        // When
        permanentLayerService.reload();

        // Then
        verify(persistence, atLeast(2)).getAllQuestionTypes();
    }

    @Test
    @DisplayName("应该获取所有知识")
    void shouldGetAllKnowledge() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        permanentLayerService.init();

        // When
        List<QuestionTypeConfig> result = permanentLayerService.getAllKnowledge();

        // Then
        assertThat(result).isNotNull();
        assertThat(result).hasSizeGreaterThanOrEqualTo(1);
    }

    @Test
    @DisplayName("应该过滤非PERMANENT层的知识")
    void shouldFilterNonPermanentKnowledge() {
        // Given
        List<QuestionTypeConfig> configs = new ArrayList<>();
        configs.add(createConfig("perm-001", "永久知识", "PERMANENT"));
        configs.add(createConfig("ord-001", "普通知识", "ORDINARY"));
        configs.add(createConfig("high-001", "高频知识", "HIGH_FREQUENCY"));
        
        when(persistence.getAllQuestionTypes()).thenReturn(configs);

        // When
        permanentLayerService.init();
        List<QuestionTypeConfig> result = permanentLayerService.getAllKnowledge();

        // Then
        assertThat(result).hasSize(1);
        assertThat(result.get(0).getSuggestedLayer()).isEqualTo("PERMANENT");
    }

    @Test
    @DisplayName("应该正确构建关键词索引")
    void shouldBuildKeywordIndex() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        when(persistence.getKeywords("perm-001"))
            .thenReturn(Arrays.asList("关键词1", "关键词2"));

        // When
        permanentLayerService.init();
        PermanentLayerService.QueryResult result = 
            permanentLayerService.query("这是关键词1的问题");

        // Then
        assertThat(result.isFound()).isTrue();
    }

    @Test
    @DisplayName("应该处理查询失败")
    void shouldHandleQueryFailure() {
        // Given
        when(persistence.getAllQuestionTypes()).thenReturn(Collections.emptyList());
        permanentLayerService.init();

        // When
        PermanentLayerService.QueryResult result = 
            permanentLayerService.query("不存在的知识");

        // Then
        assertThat(result.isFound()).isFalse();
    }

    @Test
    @DisplayName("应该返回高置信度结果")
    void shouldReturnHighConfidenceResult() {
        // Given
        List<QuestionTypeConfig> configs = createTestConfigs();
        when(persistence.getAllQuestionTypes()).thenReturn(configs);
        when(persistence.getKeywords("perm-001")).thenReturn(Arrays.asList("python"));
        
        permanentLayerService.init();

        // When
        PermanentLayerService.QueryResult result = 
            permanentLayerService.query("python是什么?");

        // Then
        assertThat(result.isFound()).isTrue();
        assertThat(result.getConfidence()).isEqualTo(0.9);
    }

    // 辅助方法

    private List<QuestionTypeConfig> createTestConfigs() {
        List<QuestionTypeConfig> configs = new ArrayList<>();
        configs.add(createConfig("perm-001", "测试知识", "PERMANENT"));
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
