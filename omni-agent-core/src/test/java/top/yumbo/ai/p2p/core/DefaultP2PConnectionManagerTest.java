package top.yumbo.ai.p2p.core;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import top.yumbo.ai.p2p.api.*;

import java.util.*;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.Mockito.*;

/**
 * P2P连接管理器测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("P2P连接管理器测试")
class DefaultP2PConnectionManagerTest {

    @Mock
    private P2PTransferBridge transferBridge;

    @Mock
    private P2PDataTransferService sourceService;

    @Mock
    private P2PDataTransferService targetService;

    private DefaultP2PConnectionManager connectionManager;

    private P2PConnection.EndpointInfo sourceEndpoint;
    private P2PConnection.EndpointInfo targetEndpoint;

    @BeforeEach
    void setUp() {
        connectionManager = new DefaultP2PConnectionManager(transferBridge);
        
        // 注册服务
        connectionManager.registerService("h2", sourceService);
        connectionManager.registerService("sqlite", targetService);

        // 创建端点信息 - 使用必需的构造参数
        sourceEndpoint = new P2PConnection.EndpointInfo("source-001", "h2");
        targetEndpoint = new P2PConnection.EndpointInfo("target-001", "sqlite");
    }

    @Test
    @DisplayName("应该成功建立P2P连接")
    void shouldEstablishConnection() {
        // Given
        Map<String, Object> config = new HashMap<>();
        config.put("timeout", 30000);

        // When
        P2PConnection connection = connectionManager.establish(
            sourceEndpoint, targetEndpoint, config
        );

        // Then
        assertThat(connection).isNotNull();
        assertThat(connection.getConnectionId()).isNotNull();
        assertThat(connection.getSourceEndpoint()).isEqualTo(sourceEndpoint);
        assertThat(connection.getTargetEndpoint()).isEqualTo(targetEndpoint);
        assertThat(connection.getStatus()).isIn(
            P2PConnection.ConnectionStatus.ESTABLISHED,
            P2PConnection.ConnectionStatus.IDLE
        );
    }

    @Test
    @DisplayName("应该注册数据传输服务")
    void shouldRegisterDataTransferService() {
        // Given
        P2PDataTransferService newService = mock(P2PDataTransferService.class);

        // When
        connectionManager.registerService("mongodb", newService);

        // Then
        // 验证可以使用新注册的服务
        P2PConnection.EndpointInfo mongoEndpoint = 
            new P2PConnection.EndpointInfo("mongo-001", "mongodb");

        P2PConnection connection = connectionManager.establish(
            sourceEndpoint, mongoEndpoint, null
        );

        assertThat(connection).isNotNull();
    }

    @Test
    @DisplayName("应该拒绝未注册的源服务")
    void shouldRejectUnregisteredSourceService() {
        // Given
        P2PConnection.EndpointInfo unregisteredEndpoint = 
            new P2PConnection.EndpointInfo("unknown-001", "unknown");

        // When & Then
        assertThatThrownBy(() -> 
            connectionManager.establish(unregisteredEndpoint, targetEndpoint, null)
        )
        .isInstanceOf(IllegalArgumentException.class)
        .hasMessageContaining("Source service not registered");
    }

    @Test
    @DisplayName("应该拒绝未注册的目标服务")
    void shouldRejectUnregisteredTargetService() {
        // Given
        P2PConnection.EndpointInfo unregisteredEndpoint = 
            new P2PConnection.EndpointInfo("unknown-001", "unknown");

        // When & Then
        assertThatThrownBy(() -> 
            connectionManager.establish(sourceEndpoint, unregisteredEndpoint, null)
        )
        .isInstanceOf(IllegalArgumentException.class)
        .hasMessageContaining("Target service not registered");
    }

    @Test
    @DisplayName("应该使用空配置建立连接")
    void shouldEstablishConnectionWithNullConfig() {
        // When
        P2PConnection connection = connectionManager.establish(
            sourceEndpoint, targetEndpoint, null
        );

        // Then
        assertThat(connection).isNotNull();
        assertThat(connection.getConfiguration()).isNotNull();
    }

    @Test
    @DisplayName("应该获取活动连接列表")
    void shouldGetActiveConnections() {
        // Given
        connectionManager.establish(sourceEndpoint, targetEndpoint, null);
        
        P2PConnection.EndpointInfo anotherTarget = 
            new P2PConnection.EndpointInfo("target-002", "sqlite");
        connectionManager.establish(sourceEndpoint, anotherTarget, null);

        // When
        List<P2PConnection> activeConnections = connectionManager.getActiveConnections();

        // Then
        assertThat(activeConnections).isNotNull();
        assertThat(activeConnections).hasSizeGreaterThanOrEqualTo(2);
    }

    @Test
    @DisplayName("应该通过连接传输数据")
    void shouldTransferThroughConnection() {
        // Given
        P2PConnection connection = connectionManager.establish(
            sourceEndpoint, targetEndpoint, null
        );

        Map<String, Object> query = new HashMap<>();
        query.put("type", "all");

        P2PDataTransferService.TransferResult expectedResult = 
            new P2PDataTransferService.TransferResult(100, 95, 5, 1000L);

        when(transferBridge.transfer(any(), any(), anyMap(), any(), anyInt()))
            .thenReturn(expectedResult);

        // When
        P2PDataTransferService.TransferResult result = 
            connectionManager.transferThroughConnection(
                connection.getConnectionId(), query, 100
            );

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getTotalRecords()).isEqualTo(100);
        assertThat(result.getSuccessCount()).isEqualTo(95);
    }

    @Test
    @DisplayName("应该关闭指定连接")
    void shouldCloseConnection() {
        // Given
        P2PConnection connection = connectionManager.establish(
            sourceEndpoint, targetEndpoint, null
        );
        String connectionId = connection.getConnectionId();

        // When
        boolean result = connectionManager.closeConnection(connectionId);

        // Then
        assertThat(result).isTrue();
        
        // 验证连接已从活动列表移除
        List<P2PConnection> activeConnections = connectionManager.getActiveConnections();
        boolean connectionExists = activeConnections.stream()
            .anyMatch(c -> c.getConnectionId().equals(connectionId));
        
        assertThat(connectionExists).isFalse();
    }

    @Test
    @DisplayName("应该获取连接统计信息")
    void shouldGetConnectionStatistics() {
        // Given
        P2PConnection connection = connectionManager.establish(
            sourceEndpoint, targetEndpoint, null
        );

        // When
        Map<String, Object> stats = connectionManager.getConnectionStatistics(
            connection.getConnectionId()
        );

        // Then
        assertThat(stats).isNotNull();
    }

    @Test
    @DisplayName("应该处理不存在的连接ID")
    void shouldHandleNonExistentConnectionId() {
        // Given
        String nonExistentId = "non-existent-connection";

        // When & Then
        assertThatThrownBy(() -> 
            connectionManager.transferThroughConnection(nonExistentId, new HashMap<>(), 100)
        )
        .isInstanceOf(IllegalArgumentException.class)
        .hasMessageContaining("Connection not found");
    }

    @Test
    @DisplayName("应该支持并发建立多个连接")
    void shouldSupportConcurrentConnections() {
        // Given
        int connectionCount = 5;
        List<P2PConnection> connections = new ArrayList<>();

        // When
        for (int i = 0; i < connectionCount; i++) {
            P2PConnection.EndpointInfo target = 
                new P2PConnection.EndpointInfo("target-" + i, "sqlite");
            
            P2PConnection connection = connectionManager.establish(
                sourceEndpoint, target, null
            );
            connections.add(connection);
        }

        // Then
        assertThat(connections).hasSize(connectionCount);
        assertThat(connectionManager.getActiveConnections())
            .hasSizeGreaterThanOrEqualTo(connectionCount);
    }

    @Test
    @DisplayName("应该处理传输失败情况")
    void shouldHandleTransferFailure() {
        // Given
        P2PConnection connection = connectionManager.establish(
            sourceEndpoint, targetEndpoint, null
        );

        when(transferBridge.transfer(any(), any(), anyMap(), any(), anyInt()))
            .thenThrow(new RuntimeException("Transfer failed"));

        // When & Then
        assertThatThrownBy(() -> 
            connectionManager.transferThroughConnection(
                connection.getConnectionId(), new HashMap<>(), 100
            )
        )
        .isInstanceOf(RuntimeException.class);
    }

    @Test
    @DisplayName("应该检查连接健康状态")
    void shouldCheckConnectionHealth() {
        // Given
        P2PConnection connection = connectionManager.establish(
            sourceEndpoint, targetEndpoint, null
        );

        // When
        boolean isHealthy = connectionManager.isHealthy(connection.getConnectionId());

        // Then
        assertThat(isHealthy).isTrue();
    }

    @Test
    @DisplayName("应该获取指定连接")
    void shouldGetSpecificConnection() {
        // Given
        P2PConnection connection = connectionManager.establish(
            sourceEndpoint, targetEndpoint, null
        );

        // When
        Optional<P2PConnection> retrieved = connectionManager.getConnection(
            connection.getConnectionId()
        );

        // Then
        assertThat(retrieved).isPresent();
        assertThat(retrieved.get().getConnectionId())
            .isEqualTo(connection.getConnectionId());
    }

    @Test
    @DisplayName("应该处理不存在的连接查询")
    void shouldHandleNonExistentConnectionQuery() {
        // When
        Optional<P2PConnection> result = connectionManager.getConnection("non-existent");

        // Then
        assertThat(result).isEmpty();
    }
}
