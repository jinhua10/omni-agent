package top.yumbo.ai.p2p.core;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;
import top.yumbo.ai.omni.p2p.api.P2PTransferBridge;

import java.util.*;
import java.util.function.Function;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * P2P传输桥接服务测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("P2P传输桥接服务测试")
class DefaultP2PTransferBridgeTest {

    @Mock
    private P2PDataTransferService sourceService;

    @Mock
    private P2PDataTransferService targetService;

    private DefaultP2PTransferBridge transferBridge;

    @BeforeEach
    void setUp() {
        transferBridge = new DefaultP2PTransferBridge();
    }

    @Test
    @DisplayName("应该成功传输数据")
    void shouldTransferDataSuccessfully() {
        // Given
        List<Map<String, Object>> sourceData = createTestData(10);
        when(sourceService.readFromSource(anyMap())).thenReturn(sourceData);
        when(targetService.writeToTarget(anyList())).thenAnswer(invocation -> {
            List<?> batch = invocation.getArgument(0);
            return batch.size();
        });

        Map<String, Object> query = new HashMap<>();

        // When
        P2PDataTransferService.TransferResult result = transferBridge.transfer(
            sourceService, targetService, query, null, 5
        );

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getTotalRecords()).isEqualTo(10);
        assertThat(result.getSuccessCount()).isEqualTo(10);
        assertThat(result.getFailureCount()).isEqualTo(0);
    }

    @Test
    @DisplayName("应该应用数据转换器")
    void shouldApplyTransformer() {
        // Given
        List<Map<String, Object>> sourceData = createTestData(5);
        when(sourceService.readFromSource(anyMap())).thenReturn(sourceData);
        when(targetService.writeToTarget(anyList())).thenReturn(5);

        Function<Map<String, Object>, Map<String, Object>> transformer = data -> {
            Map<String, Object> transformed = new HashMap<>(data);
            transformed.put("transformed", true);
            return transformed;
        };

        // When
        P2PDataTransferService.TransferResult result = transferBridge.transfer(
            sourceService, targetService, new HashMap<>(), transformer, 10
        );

        // Then
        assertThat(result.getSuccessCount()).isEqualTo(5);
        verify(targetService).writeToTarget(argThat(list -> 
            list.stream().allMatch(map -> map.containsKey("transformed"))
        ));
    }

    @Test
    @DisplayName("应该分批传输数据")
    void shouldTransferDataInBatches() {
        // Given
        List<Map<String, Object>> sourceData = createTestData(25);
        when(sourceService.readFromSource(anyMap())).thenReturn(sourceData);
        when(targetService.writeToTarget(anyList())).thenAnswer(invocation -> {
            List<?> batch = invocation.getArgument(0);
            return batch.size();
        });

        int batchSize = 10;

        // When
        P2PDataTransferService.TransferResult result = transferBridge.transfer(
            sourceService, targetService, new HashMap<>(), null, batchSize
        );

        // Then
        assertThat(result.getTotalRecords()).isEqualTo(25);
        assertThat(result.getSuccessCount()).isEqualTo(25);
        verify(targetService, times(3)).writeToTarget(anyList()); // 25/10 = 3 batches
    }

    @Test
    @DisplayName("应该处理空数据源")
    void shouldHandleEmptySource() {
        // Given
        when(sourceService.readFromSource(anyMap())).thenReturn(Collections.emptyList());

        // When
        P2PDataTransferService.TransferResult result = transferBridge.transfer(
            sourceService, targetService, new HashMap<>(), null, 10
        );

        // Then
        assertThat(result.getTotalRecords()).isEqualTo(0);
        assertThat(result.getSuccessCount()).isEqualTo(0);
        verify(targetService, never()).writeToTarget(anyList());
    }

    @Test
    @DisplayName("应该处理部分写入失败")
    void shouldHandlePartialWriteFailure() {
        // Given
        List<Map<String, Object>> sourceData = createTestData(10);
        when(sourceService.readFromSource(anyMap())).thenReturn(sourceData);
        when(targetService.writeToTarget(anyList()))
            .thenReturn(8); // 只成功写入8条

        // When
        P2PDataTransferService.TransferResult result = transferBridge.transfer(
            sourceService, targetService, new HashMap<>(), null, 10
        );

        // Then
        assertThat(result.getSuccessCount()).isEqualTo(8);
        assertThat(result.getFailureCount()).isEqualTo(2);
    }

    @Test
    @DisplayName("应该处理写入异常")
    void shouldHandleWriteException() {
        // Given
        List<Map<String, Object>> sourceData = createTestData(10);
        when(sourceService.readFromSource(anyMap())).thenReturn(sourceData);
        when(targetService.writeToTarget(anyList()))
            .thenThrow(new RuntimeException("Write failed"));

        // When
        P2PDataTransferService.TransferResult result = transferBridge.transfer(
            sourceService, targetService, new HashMap<>(), null, 10
        );

        // Then
        assertThat(result.getFailureCount()).isEqualTo(10);
        assertThat(result.getSuccessCount()).isEqualTo(0);
    }

    @Test
    @DisplayName("应该测量传输耗时")
    void shouldMeasureTransferDuration() {
        // Given
        List<Map<String, Object>> sourceData = createTestData(5);
        when(sourceService.readFromSource(anyMap())).thenReturn(sourceData);
        when(targetService.writeToTarget(anyList())).thenReturn(5);

        // When
        P2PDataTransferService.TransferResult result = transferBridge.transfer(
            sourceService, targetService, new HashMap<>(), null, 10
        );

        // Then
        assertThat(result.getDurationMs()).isGreaterThanOrEqualTo(0);
    }

    @Test
    @DisplayName("应该执行SOURCE_WINS双向同步")
    void shouldPerformSourceWinsBidirectionalSync() {
        // Given
        List<Map<String, Object>> service1Data = createTestData(5);
        List<Map<String, Object>> service2Data = createTestData(3);
        
        when(sourceService.readFromSource(anyMap())).thenReturn(service1Data);
        when(targetService.readFromSource(anyMap())).thenReturn(service2Data);
        when(targetService.writeToTarget(anyList())).thenReturn(5);

        // When
        P2PTransferBridge.SyncResult result = transferBridge.bidirectionalSync(
            sourceService, targetService, P2PTransferBridge.SyncStrategy.SOURCE_WINS
        );

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getService1ToService2Count()).isGreaterThan(0);
    }

    @Test
    @DisplayName("应该执行TARGET_WINS双向同步")
    void shouldPerformTargetWinsBidirectionalSync() {
        // Given
        List<Map<String, Object>> service1Data = createTestData(3);
        List<Map<String, Object>> service2Data = createTestData(5);
        
        when(sourceService.readFromSource(anyMap())).thenReturn(service1Data);
        when(targetService.readFromSource(anyMap())).thenReturn(service2Data);
        when(sourceService.writeToTarget(anyList())).thenReturn(5);

        // When
        P2PTransferBridge.SyncResult result = transferBridge.bidirectionalSync(
            sourceService, targetService, P2PTransferBridge.SyncStrategy.TARGET_WINS
        );

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getService2ToService1Count()).isGreaterThan(0);
    }

    @Test
    @DisplayName("应该执行LATEST_WINS双向同步")
    void shouldPerformLatestWinsBidirectionalSync() {
        // Given
        List<Map<String, Object>> service1Data = createTestDataWithTimestamps(3, 1000L);
        List<Map<String, Object>> service2Data = createTestDataWithTimestamps(3, 2000L);
        
        when(sourceService.readFromSource(anyMap())).thenReturn(service1Data);
        when(targetService.readFromSource(anyMap())).thenReturn(service2Data);
        when(sourceService.writeToTarget(anyList())).thenReturn(3);
        when(targetService.writeToTarget(anyList())).thenReturn(0);

        // When
        P2PTransferBridge.SyncResult result = transferBridge.bidirectionalSync(
            sourceService, targetService, P2PTransferBridge.SyncStrategy.LATEST_WINS
        );

        // Then
        assertThat(result).isNotNull();
        // service2数据更新,应该同步到service1
        assertThat(result.getService2ToService1Count()).isGreaterThan(0);
    }

    @Test
    @DisplayName("应该执行MERGE双向同步")
    void shouldPerformMergeBidirectionalSync() {
        // Given
        List<Map<String, Object>> service1Data = createTestData(3);
        List<Map<String, Object>> service2Data = createTestData(3);
        
        when(sourceService.readFromSource(anyMap())).thenReturn(service1Data);
        when(targetService.readFromSource(anyMap())).thenReturn(service2Data);
        when(sourceService.writeToTarget(anyList())).thenReturn(3);
        when(targetService.writeToTarget(anyList())).thenReturn(3);

        // When
        P2PTransferBridge.SyncResult result = transferBridge.bidirectionalSync(
            sourceService, targetService, P2PTransferBridge.SyncStrategy.MERGE
        );

        // Then
        assertThat(result).isNotNull();
        // MERGE策略应该双向同步
        assertThat(result.getService1ToService2Count() + result.getService2ToService1Count())
            .isGreaterThan(0);
    }

    @Test
    @DisplayName("应该处理读取异常")
    void shouldHandleReadException() {
        // Given
        when(sourceService.readFromSource(anyMap()))
            .thenThrow(new RuntimeException("Read failed"));

        // When
        P2PDataTransferService.TransferResult result = transferBridge.transfer(
            sourceService, targetService, new HashMap<>(), null, 10
        );

        // Then
        assertThat(result.getTotalRecords()).isEqualTo(0);
        assertThat(result.getSuccessCount()).isEqualTo(0);
    }

    @Test
    @DisplayName("应该处理null转换器")
    void shouldHandleNullTransformer() {
        // Given
        List<Map<String, Object>> sourceData = createTestData(5);
        when(sourceService.readFromSource(anyMap())).thenReturn(sourceData);
        when(targetService.writeToTarget(anyList())).thenReturn(5);

        // When
        P2PDataTransferService.TransferResult result = transferBridge.transfer(
            sourceService, targetService, new HashMap<>(), null, 10
        );

        // Then
        assertThat(result.getSuccessCount()).isEqualTo(5);
        verify(targetService).writeToTarget(argThat(list -> 
            list.stream().noneMatch(map -> map.containsKey("transformed"))
        ));
    }

    // 辅助方法

    private List<Map<String, Object>> createTestData(int count) {
        List<Map<String, Object>> data = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            Map<String, Object> record = new HashMap<>();
            record.put("id", "record-" + i);
            record.put("content", "Content " + i);
            data.add(record);
        }
        return data;
    }

    private List<Map<String, Object>> createTestDataWithTimestamps(int count, long baseTimestamp) {
        List<Map<String, Object>> data = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            Map<String, Object> record = new HashMap<>();
            record.put("id", "record-" + i);
            record.put("content", "Content " + i);
            record.put("timestamp", baseTimestamp + i);
            data.add(record);
        }
        return data;
    }
}

