package top.yumbo.ai.omni.core.query;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.Query;
import top.yumbo.ai.rag.api.model.SearchResult;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * QueryService 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("QueryService Tests")
class QueryServiceTest {

    @Mock
    private RAGService ragService;

    private QueryService queryService;

    @BeforeEach
    void setUp() {
        queryService = new QueryService(ragService);
    }

    @Test
    @DisplayName("应该执行文本搜索")
    void shouldSearchByText() {
        // Given
        String queryText = "Java编程";
        int limit = 10;
        List<SearchResult> mockResults = createMockResults(5);

        when(ragService.searchByText(queryText, limit)).thenReturn(mockResults);

        // When
        List<SearchResult> results = queryService.search(queryText, limit);

        // Then
        assertThat(results).hasSize(5);
        verify(ragService).searchByText(queryText, limit);
    }

    @Test
    @DisplayName("应该处理空查询结果")
    void shouldHandleEmptyResults() {
        // Given
        when(ragService.searchByText(anyString(), anyInt())).thenReturn(new ArrayList<>());

        // When
        List<SearchResult> results = queryService.search("测试", 10);

        // Then
        assertThat(results).isEmpty();
    }

    @Test
    @DisplayName("应该执行向量搜索")
    void shouldSearchByVector() {
        // Given
        float[] embedding = new float[]{0.1f, 0.2f, 0.3f};
        int limit = 5;
        List<SearchResult> mockResults = createMockResults(3);

        when(ragService.vectorSearch(embedding, limit)).thenReturn(mockResults);

        // When
        List<SearchResult> results = queryService.vectorSearch(embedding, limit);

        // Then
        assertThat(results).hasSize(3);
        verify(ragService).vectorSearch(embedding, limit);
    }

    @Test
    @DisplayName("应该执行混合检索")
    void shouldPerformHybridSearch() {
        // Given
        String queryText = "机器学习";
        float[] embedding = new float[]{0.1f, 0.2f, 0.3f};
        int limit = 10;
        List<SearchResult> mockResults = createMockResults(7);

        when(ragService.hybridSearch(any(Query.class))).thenReturn(mockResults);

        // When
        List<SearchResult> results = queryService.hybridSearch(queryText, embedding, limit);

        // Then
        assertThat(results).hasSize(7);
        verify(ragService).hybridSearch(argThat(query ->
                query.getText().equals(queryText) &&
                query.getEmbedding() == embedding &&
                query.getTopK() == limit
        ));
    }

    @Test
    @DisplayName("应该统计查询次数")
    void shouldTrackQueryStatistics() {
        // Given
        when(ragService.searchByText(anyString(), anyInt())).thenReturn(new ArrayList<>());

        // When
        queryService.search("query1", 10);
        queryService.search("query2", 10);
        queryService.search("query3", 10);

        Map<String, Long> stats = queryService.getStatistics();

        // Then
        assertThat(stats.get("totalQueries")).isEqualTo(3);
    }

    @Test
    @DisplayName("应该重置统计数据")
    void shouldResetStatistics() {
        // Given
        when(ragService.searchByText(anyString(), anyInt())).thenReturn(new ArrayList<>());

        queryService.search("query1", 10);
        queryService.search("query2", 10);

        // When
        queryService.resetStatistics();

        // Then
        Map<String, Long> stats = queryService.getStatistics();
        assertThat(stats.get("totalQueries")).isEqualTo(0);
    }

    @Test
    @DisplayName("应该处理大结果集")
    void shouldHandleLargeResultSet() {
        // Given
        List<SearchResult> mockResults = createMockResults(100);
        when(ragService.searchByText(anyString(), anyInt())).thenReturn(mockResults);

        // When
        List<SearchResult> results = queryService.search("test", 100);

        // Then
        assertThat(results).hasSize(100);
    }

    @Test
    @DisplayName("应该处理搜索异常")
    void shouldHandleSearchException() {
        // Given
        when(ragService.searchByText(anyString(), anyInt()))
                .thenThrow(new RuntimeException("Search failed"));

        // When/Then
        assertThatThrownBy(() -> queryService.search("test", 10))
                .isInstanceOf(RuntimeException.class)
                .hasMessageContaining("Search failed");
    }

    @Test
    @DisplayName("应该支持不同的查询限制")
    void shouldSupportDifferentLimits() {
        // Given
        when(ragService.searchByText(anyString(), anyInt()))
                .thenReturn(createMockResults(1))
                .thenReturn(createMockResults(5))
                .thenReturn(createMockResults(10));

        // When
        List<SearchResult> results1 = queryService.search("test", 1);
        List<SearchResult> results5 = queryService.search("test", 5);
        List<SearchResult> results10 = queryService.search("test", 10);

        // Then
        assertThat(results1).hasSize(1);
        assertThat(results5).hasSize(5);
        assertThat(results10).hasSize(10);
    }

    @Test
    @DisplayName("应该处理向量搜索异常")
    void shouldHandleVectorSearchException() {
        // Given
        float[] embedding = new float[]{0.1f, 0.2f};
        when(ragService.vectorSearch(any(), anyInt()))
                .thenThrow(new RuntimeException("Vector search failed"));

        // When/Then
        assertThatThrownBy(() -> queryService.vectorSearch(embedding, 10))
                .isInstanceOf(RuntimeException.class);
    }

    @Test
    @DisplayName("应该处理混合检索异常")
    void shouldHandleHybridSearchException() {
        // Given
        when(ragService.hybridSearch(any(Query.class)))
                .thenThrow(new RuntimeException("Hybrid search failed"));

        // When/Then
        assertThatThrownBy(() ->
                queryService.hybridSearch("test", new float[]{0.1f}, 10))
                .isInstanceOf(RuntimeException.class);
    }

    @Test
    @DisplayName("应该正确传递查询参数")
    void shouldPassCorrectQueryParameters() {
        // Given
        String queryText = "测试查询";
        int limit = 15;
        when(ragService.searchByText(anyString(), anyInt())).thenReturn(new ArrayList<>());

        // When
        queryService.search(queryText, limit);

        // Then
        verify(ragService).searchByText(eq(queryText), eq(limit));
    }

    @Test
    @DisplayName("应该累计统计多次查询")
    void shouldAccumulateMultipleQueryStatistics() {
        // Given
        when(ragService.searchByText(anyString(), anyInt())).thenReturn(new ArrayList<>());
        when(ragService.vectorSearch(any(), anyInt())).thenReturn(new ArrayList<>());
        when(ragService.hybridSearch(any())).thenReturn(new ArrayList<>());

        // When
        queryService.search("text1", 10);
        queryService.search("text2", 10);
        queryService.vectorSearch(new float[]{0.1f}, 10);
        queryService.hybridSearch("hybrid", new float[]{0.2f}, 10);

        // Then
        // 注意：vectorSearch 和 hybridSearch 不计入 totalQueries
        Map<String, Long> stats = queryService.getStatistics();
        assertThat(stats.get("totalQueries")).isEqualTo(2);
    }

    @Test
    @DisplayName("应该在初始化时统计为零")
    void shouldHaveZeroStatisticsOnInit() {
        // When
        Map<String, Long> stats = queryService.getStatistics();

        // Then
        assertThat(stats.get("totalQueries")).isEqualTo(0);
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
