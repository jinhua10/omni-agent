package top.yumbo.ai.omni.core.optimization;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.OptimizationData;
import top.yumbo.ai.storage.api.model.OptimizationType;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * RAGOptimizationService 单元测试
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@ExtendWith(MockitoExtension.class)
class RAGOptimizationServiceTest {

    @Mock
    private DocumentStorageService storageService;

    @InjectMocks
    private RAGOptimizationService optimizationService;

    private String testDocumentId;
    private Map<String, Object> testData;

    @BeforeEach
    void setUp() {
        testDocumentId = "test-doc-123";
        testData = new HashMap<>();
        testData.put("key1", "value1");
        testData.put("key2", 123);
    }

    // ========== 通用方法测试 ==========

    @Test
    void testSaveOptimizationData_Success() {
        // Given
        String expectedId = testDocumentId + ":ppl";
        when(storageService.saveOptimizationData(eq(testDocumentId), any(OptimizationData.class)))
                .thenReturn(expectedId);

        // When
        String result = optimizationService.saveOptimizationData(
                testDocumentId,
                OptimizationType.PPL.getCode(),
                testData
        );

        // Then
        assertNotNull(result);
        assertEquals(expectedId, result);
        verify(storageService, times(1)).saveOptimizationData(eq(testDocumentId), any(OptimizationData.class));
    }

    @Test
    void testSaveOptimizationData_WithEmptyData_ReturnsNull() {
        // When
        String result = optimizationService.saveOptimizationData(
                testDocumentId,
                OptimizationType.PPL.getCode(),
                new HashMap<>()
        );

        // Then
        assertNull(result);
        verify(storageService, never()).saveOptimizationData(any(), any());
    }

    @Test
    void testSaveOptimizationData_WithMetrics() {
        // Given
        Map<String, Double> metrics = Map.of("precisionGain", 15.5, "recallGain", 12.3);
        String expectedId = testDocumentId + ":hyde";
        when(storageService.saveOptimizationData(eq(testDocumentId), any(OptimizationData.class)))
                .thenReturn(expectedId);

        // When
        String result = optimizationService.saveOptimizationData(
                testDocumentId,
                OptimizationType.HYDE.getCode(),
                testData,
                null,
                metrics
        );

        // Then
        assertNotNull(result);
        assertEquals(expectedId, result);
    }

    @Test
    void testGetOptimizationData_Success() {
        // Given
        OptimizationData mockData = OptimizationData.builder()
                .documentId(testDocumentId)
                .optimizationType(OptimizationType.PPL.getCode())
                .data(testData)
                .build();
        when(storageService.getOptimizationData(testDocumentId, OptimizationType.PPL.getCode()))
                .thenReturn(Optional.of(mockData));

        // When
        Optional<OptimizationData> result = optimizationService.getOptimizationData(
                testDocumentId,
                OptimizationType.PPL.getCode()
        );

        // Then
        assertTrue(result.isPresent());
        assertEquals(testDocumentId, result.get().getDocumentId());
        assertEquals(OptimizationType.PPL.getCode(), result.get().getOptimizationType());
    }

    @Test
    void testGetOptimizationData_NotFound() {
        // Given
        when(storageService.getOptimizationData(testDocumentId, OptimizationType.PPL.getCode()))
                .thenReturn(Optional.empty());

        // When
        Optional<OptimizationData> result = optimizationService.getOptimizationData(
                testDocumentId,
                OptimizationType.PPL.getCode()
        );

        // Then
        assertFalse(result.isPresent());
    }

    @Test
    void testGetAllOptimizationData() {
        // Given
        List<OptimizationData> mockDataList = Arrays.asList(
                OptimizationData.builder()
                        .documentId(testDocumentId)
                        .optimizationType(OptimizationType.PPL.getCode())
                        .build(),
                OptimizationData.builder()
                        .documentId(testDocumentId)
                        .optimizationType(OptimizationType.HYDE.getCode())
                        .build()
        );
        when(storageService.getAllOptimizationData(testDocumentId))
                .thenReturn(mockDataList);

        // When
        List<OptimizationData> result = optimizationService.getAllOptimizationData(testDocumentId);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
    }

    @Test
    void testDeleteOptimizationData() {
        // When
        optimizationService.deleteOptimizationData(testDocumentId, OptimizationType.PPL.getCode());

        // Then
        verify(storageService, times(1)).deleteOptimizationData(
                testDocumentId,
                OptimizationType.PPL.getCode()
        );
    }

    @Test
    void testDeleteAllOptimizationData() {
        // When
        optimizationService.deleteAllOptimizationData(testDocumentId);

        // Then
        verify(storageService, times(1)).deleteAllOptimizationData(testDocumentId);
    }

    // ========== 特定算法便捷方法测试 ==========

    @Test
    void testSavePPLData() {
        // Given
        List<String> points = Arrays.asList("point1", "point2");
        Map<String, Float> scores = Map.of("point1", 0.9f, "point2", 0.8f);
        String modelVersion = "v1.0";
        String expectedId = testDocumentId + ":ppl";
        when(storageService.saveOptimizationData(eq(testDocumentId), any(OptimizationData.class)))
                .thenReturn(expectedId);

        // When
        String result = optimizationService.savePPLData(testDocumentId, points, scores, modelVersion);

        // Then
        assertNotNull(result);
        assertEquals(expectedId, result);
        verify(storageService, times(1)).saveOptimizationData(eq(testDocumentId), any(OptimizationData.class));
    }

    @Test
    void testSaveHyDEData() {
        // Given
        String hypotheticalDoc = "假设性文档内容...";
        float[] embedding = new float[]{0.1f, 0.2f, 0.3f};
        double similarity = 0.85;
        String expectedId = testDocumentId + ":hyde";
        when(storageService.saveOptimizationData(eq(testDocumentId), any(OptimizationData.class)))
                .thenReturn(expectedId);

        // When
        String result = optimizationService.saveHyDEData(testDocumentId, hypotheticalDoc, embedding, similarity);

        // Then
        assertNotNull(result);
        assertEquals(expectedId, result);
    }

    @Test
    void testSaveRerankData() {
        // Given
        List<Integer> rerankedIndices = Arrays.asList(2, 0, 1, 3);
        List<Double> scores = Arrays.asList(0.95, 0.88, 0.76, 0.65);
        String model = "cross-encoder-v1";
        String expectedId = testDocumentId + ":rerank";
        when(storageService.saveOptimizationData(eq(testDocumentId), any(OptimizationData.class)))
                .thenReturn(expectedId);

        // When
        String result = optimizationService.saveRerankData(testDocumentId, rerankedIndices, scores, model);

        // Then
        assertNotNull(result);
        assertEquals(expectedId, result);
    }

    @Test
    void testSaveQueryExpansionData() {
        // Given
        List<String> expandedQueries = Arrays.asList("查询1", "查询2", "查询3");
        Map<String, Double> weights = Map.of("查询1", 1.0, "查询2", 0.8, "查询3", 0.6);
        String expectedId = testDocumentId + ":query_expansion";
        when(storageService.saveOptimizationData(eq(testDocumentId), any(OptimizationData.class)))
                .thenReturn(expectedId);

        // When
        String result = optimizationService.saveQueryExpansionData(testDocumentId, expandedQueries, weights);

        // Then
        assertNotNull(result);
        assertEquals(expectedId, result);
    }

    @Test
    void testSaveMetadataFilterData() {
        // Given
        Map<String, Object> filters = Map.of("type", "technical", "version", "2.0");
        List<String> extractedFilters = Arrays.asList("type=technical", "version=2.0");
        String expectedId = testDocumentId + ":metadata_filter";
        when(storageService.saveOptimizationData(eq(testDocumentId), any(OptimizationData.class)))
                .thenReturn(expectedId);

        // When
        String result = optimizationService.saveMetadataFilterData(testDocumentId, filters, extractedFilters);

        // Then
        assertNotNull(result);
        assertEquals(expectedId, result);
    }

    @Test
    void testSaveContextCompressionData() {
        // Given
        String originalContext = "很长的上下文内容...".repeat(100);
        String compressedContext = "压缩后的关键内容...";
        double compressionRatio = (double) compressedContext.length() / originalContext.length();
        String expectedId = testDocumentId + ":context_compression";
        when(storageService.saveOptimizationData(eq(testDocumentId), any(OptimizationData.class)))
                .thenReturn(expectedId);

        // When
        String result = optimizationService.saveContextCompressionData(
                testDocumentId,
                originalContext,
                compressedContext,
                compressionRatio
        );

        // Then
        assertNotNull(result);
        assertEquals(expectedId, result);
    }

    // ========== 工具方法测试 ==========

    @Test
    void testHasOptimizationData_Exists() {
        // Given
        OptimizationData mockData = OptimizationData.builder().build();
        when(storageService.getOptimizationData(testDocumentId, OptimizationType.PPL.getCode()))
                .thenReturn(Optional.of(mockData));

        // When
        boolean result = optimizationService.hasOptimizationData(testDocumentId, OptimizationType.PPL.getCode());

        // Then
        assertTrue(result);
    }

    @Test
    void testHasOptimizationData_NotExists() {
        // Given
        when(storageService.getOptimizationData(testDocumentId, OptimizationType.PPL.getCode()))
                .thenReturn(Optional.empty());

        // When
        boolean result = optimizationService.hasOptimizationData(testDocumentId, OptimizationType.PPL.getCode());

        // Then
        assertFalse(result);
    }

    @Test
    void testGetOptimizationTypes() {
        // Given
        List<OptimizationData> mockDataList = Arrays.asList(
                OptimizationData.builder().optimizationType("ppl").build(),
                OptimizationData.builder().optimizationType("hyde").build(),
                OptimizationData.builder().optimizationType("ppl").build()  // 重复
        );
        when(storageService.getAllOptimizationData(testDocumentId))
                .thenReturn(mockDataList);

        // When
        List<String> result = optimizationService.getOptimizationTypes(testDocumentId);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());  // 去重后只有2个
        assertTrue(result.contains("ppl"));
        assertTrue(result.contains("hyde"));
    }

    @Test
    void testBatchSaveOptimizationData() {
        // Given
        List<OptimizationData> dataList = Arrays.asList(
                OptimizationData.builder().documentId("doc1").optimizationType("ppl").build(),
                OptimizationData.builder().documentId("doc2").optimizationType("hyde").build()
        );
        when(storageService.saveOptimizationData(anyString(), any(OptimizationData.class)))
                .thenReturn("success");

        // When
        optimizationService.batchSaveOptimizationData(dataList);

        // Then
        verify(storageService, times(2)).saveOptimizationData(anyString(), any(OptimizationData.class));
    }

    // ========== 异常处理测试 ==========

    @Test
    void testSaveOptimizationData_StorageException() {
        // Given
        when(storageService.saveOptimizationData(eq(testDocumentId), any(OptimizationData.class)))
                .thenThrow(new RuntimeException("Storage error"));

        // When
        String result = optimizationService.saveOptimizationData(
                testDocumentId,
                OptimizationType.PPL.getCode(),
                testData
        );

        // Then - 应该优雅降级返回null
        assertNull(result);
    }

    @Test
    void testGetOptimizationData_StorageException() {
        // Given
        when(storageService.getOptimizationData(testDocumentId, OptimizationType.PPL.getCode()))
                .thenThrow(new RuntimeException("Storage error"));

        // When
        Optional<OptimizationData> result = optimizationService.getOptimizationData(
                testDocumentId,
                OptimizationType.PPL.getCode()
        );

        // Then - 应该优雅降级返回empty
        assertFalse(result.isPresent());
    }
}

