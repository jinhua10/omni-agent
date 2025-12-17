package top.yumbo.ai.storage.api.model;

import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * OptimizationData 模型类单元测试
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
class OptimizationDataTest {

    @Test
    void testBuilder() {
        // Given
        String documentId = "doc-123";
        String optimizationType = "ppl";
        Long processedAt = System.currentTimeMillis();

        // When
        OptimizationData data = OptimizationData.builder()
                .documentId(documentId)
                .optimizationType(optimizationType)
                .processedAt(processedAt)
                .build();

        // Then
        assertNotNull(data);
        assertEquals(documentId, data.getDocumentId());
        assertEquals(optimizationType, data.getOptimizationType());
        assertEquals(processedAt, data.getProcessedAt());
    }

    @Test
    void testPutAndGetData() {
        // Given
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .build();

        // When
        data.putData("key1", "value1");
        data.putData("key2", 123);
        data.putData("key3", List.of("item1", "item2"));

        // Then
        assertEquals("value1", data.getData("key1", String.class));
        assertEquals(123, data.getData("key2", Integer.class));

        @SuppressWarnings("unchecked")
        List<String> list = data.getData("key3", List.class);
        assertNotNull(list);
        assertEquals(2, list.size());
    }

    @Test
    void testPutAndGetMetadata() {
        // Given
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .build();

        // When
        data.putMetadata("author", "OmniAgent Team");
        data.putMetadata("version", "v1.0");

        // Then
        Map<String, Object> metadata = data.getMetadata();
        assertNotNull(metadata);
        assertEquals(2, metadata.size());
        assertEquals("OmniAgent Team", metadata.get("author"));
        assertEquals("v1.0", metadata.get("version"));
    }

    @Test
    void testPutAndGetMetric() {
        // Given
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .build();

        // When
        data.putMetric("precisionGain", 15.5);
        data.putMetric("recallGain", 12.3);
        data.putMetric("processingTime", 125.0);

        // Then
        assertEquals(15.5, data.getMetric("precisionGain"));
        assertEquals(12.3, data.getMetric("recallGain"));
        assertEquals(125.0, data.getMetric("processingTime"));
    }

    @Test
    void testGetMetric_NotExists() {
        // Given
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .build();

        // When
        Double result = data.getMetric("nonexistent");

        // Then
        assertNull(result);
    }

    @Test
    void testDataWithMap() {
        // Given
        Map<String, Object> initialData = new HashMap<>();
        initialData.put("probablePoints", List.of("point1", "point2"));
        initialData.put("scores", Map.of("point1", 0.9f, "point2", 0.8f));

        // When
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .data(initialData)
                .build();

        // Then
        assertNotNull(data.getData());
        assertEquals(2, data.getData().size());
        assertTrue(data.getData().containsKey("probablePoints"));
        assertTrue(data.getData().containsKey("scores"));
    }

    @Test
    void testMetricsWithMap() {
        // Given
        Map<String, Double> initialMetrics = Map.of(
                "precisionGain", 15.5,
                "recallGain", 12.3,
                "f1Score", 0.85
        );

        // When
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("hyde")
                .metrics(initialMetrics)
                .build();

        // Then
        assertNotNull(data.getMetrics());
        assertEquals(3, data.getMetrics().size());
        assertEquals(15.5, data.getMetric("precisionGain"));
        assertEquals(12.3, data.getMetric("recallGain"));
        assertEquals(0.85, data.getMetric("f1Score"));
    }

    @Test
    void testCompleteOptimizationData() {
        // Given & When
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("rerank")
                .algorithmVersion("v2.0")
                .processedAt(System.currentTimeMillis())
                .data(Map.of("rerankedIndices", List.of(2, 0, 1)))
                .metadata(Map.of("model", "cross-encoder", "confidence", 0.95))
                .metrics(Map.of("accuracyGain", 18.5, "latency", 50.0))
                .build();

        // Then
        assertNotNull(data);
        assertEquals("doc-123", data.getDocumentId());
        assertEquals("rerank", data.getOptimizationType());
        assertEquals("v2.0", data.getAlgorithmVersion());
        assertNotNull(data.getProcessedAt());
        assertNotNull(data.getData());
        assertNotNull(data.getMetadata());
        assertNotNull(data.getMetrics());

        // 验证数据内容
        assertEquals(1, data.getData().size());
        assertEquals(2, data.getMetadata().size());
        assertEquals(2, data.getMetrics().size());
    }

    @Test
    void testSerialization() {
        // Given
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .data(Map.of("key", "value"))
                .build();

        // Then - 确保实现了Serializable
        assertTrue(data instanceof java.io.Serializable);
    }

    @Test
    void testDataImmutability() {
        // Given
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .build();

        // When - 初始状态data为null
        assertNotNull(data.getData());  // Builder默认创建了空Map

        // When - 添加数据后
        data.putData("key1", "value1");

        // Then - 可以继续添加
        data.putData("key2", "value2");
        assertEquals(2, data.getData().size());
    }

    @Test
    void testNullSafeGetData() {
        // Given
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .data(null)  // 显式设置为null
                .build();

        // When
        String result = data.getData("nonexistent", String.class);

        // Then
        assertNull(result);
    }

    @Test
    void testEqualsAndHashCode() {
        // Given
        OptimizationData data1 = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .build();

        OptimizationData data2 = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .build();

        // Then
        assertEquals(data1, data2);
        assertEquals(data1.hashCode(), data2.hashCode());
    }

    @Test
    void testToString() {
        // Given
        OptimizationData data = OptimizationData.builder()
                .documentId("doc-123")
                .optimizationType("ppl")
                .algorithmVersion("v1.0")
                .build();

        // When
        String result = data.toString();

        // Then
        assertNotNull(result);
        assertTrue(result.contains("doc-123"));
        assertTrue(result.contains("ppl"));
    }
}

