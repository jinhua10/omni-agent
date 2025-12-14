package top.yumbo.ai.p2p.core;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.p2p.api.P2PConnection;
import top.yumbo.ai.p2p.api.P2PConnectionManager;
import top.yumbo.ai.p2p.api.P2PDataTransferService;
import top.yumbo.ai.p2p.api.P2PTransferBridge;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 默认P2P连接管理器实现
 * (Default P2P Connection Manager Implementation)
 *
 * <p>管理端到端连接的完整生命周期</p>
 * <p>Manages the complete lifecycle of peer-to-peer connections</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class DefaultP2PConnectionManager implements P2PConnectionManager {

    private final Map<String, P2PConnectionImpl> connections = new ConcurrentHashMap<>();
    private final Map<String, P2PDataTransferService> serviceRegistry = new ConcurrentHashMap<>();
    private final P2PTransferBridge transferBridge;

    public DefaultP2PConnectionManager(P2PTransferBridge transferBridge) {
        this.transferBridge = transferBridge;
    }

    /**
     * 注册数据传输服务
     */
    public void registerService(String storageType, P2PDataTransferService service) {
        serviceRegistry.put(storageType.toLowerCase(), service);
        log.info("Registered P2P service for storage type: {}", storageType);
    }

    @Override
    public P2PConnection establish(
            P2PConnection.EndpointInfo sourceEndpoint,
            P2PConnection.EndpointInfo targetEndpoint,
            Map<String, Object> config) {

        String connectionId = UUID.randomUUID().toString();
        
        log.info("Establishing P2P connection {} from {}:{} to {}:{}",
                connectionId,
                sourceEndpoint.getStorageType(),
                sourceEndpoint.getEndpointId(),
                targetEndpoint.getStorageType(),
                targetEndpoint.getEndpointId());

        // 验证服务是否注册
        P2PDataTransferService sourceService = getService(sourceEndpoint.getStorageType());
        P2PDataTransferService targetService = getService(targetEndpoint.getStorageType());

        if (sourceService == null) {
            throw new IllegalArgumentException(
                "Source service not registered: " + sourceEndpoint.getStorageType());
        }

        if (targetService == null) {
            throw new IllegalArgumentException(
                "Target service not registered: " + targetEndpoint.getStorageType());
        }

        // 创建连接
        P2PConnectionImpl connection = new P2PConnectionImpl(
                connectionId,
                sourceEndpoint,
                targetEndpoint,
                sourceService,
                targetService,
                config != null ? config : new HashMap<>()
        );

        connections.put(connectionId, connection);

        log.info("P2P connection {} established successfully", connectionId);

        return connection;
    }

    @Override
    public Optional<P2PConnection> getConnection(String connectionId) {
        return Optional.ofNullable(connections.get(connectionId));
    }

    @Override
    public List<P2PConnection> getActiveConnections() {
        return connections.values().stream()
                .filter(conn -> conn.getStatus() == P2PConnection.ConnectionStatus.ESTABLISHED
                             || conn.getStatus() == P2PConnection.ConnectionStatus.IDLE)
                .collect(Collectors.toList());
    }

    @Override
    public boolean closeConnection(String connectionId) {
        P2PConnectionImpl connection = connections.get(connectionId);
        if (connection == null) {
            return false;
        }

        connection.close();
        connections.remove(connectionId);

        log.info("Closed P2P connection: {}", connectionId);
        return true;
    }

    @Override
    public boolean isHealthy(String connectionId) {
        P2PConnectionImpl connection = connections.get(connectionId);
        return connection != null && connection.isAlive();
    }

    @Override
    public P2PDataTransferService.TransferResult transferThroughConnection(
            String connectionId,
            Map<String, Object> query,
            int batchSize) {

        P2PConnectionImpl connection = connections.get(connectionId);
        if (connection == null) {
            throw new IllegalArgumentException("Connection not found: " + connectionId);
        }

        if (!connection.isAlive()) {
            throw new IllegalStateException("Connection is not alive: " + connectionId);
        }

        log.info("Transferring data through connection: {}", connectionId);
        connection.setStatus(P2PConnection.ConnectionStatus.TRANSFERRING);

        try {
            P2PDataTransferService.TransferResult result = transferBridge.transfer(
                    connection.getSourceService(),
                    connection.getTargetService(),
                    query,
                    null,
                    batchSize
            );

            connection.updateLastActiveTime();
            connection.recordTransfer(result);
            connection.setStatus(P2PConnection.ConnectionStatus.IDLE);

            log.info("Transfer completed through connection {}: {} records", 
                    connectionId, result.getSuccessCount());

            return result;

        } catch (Exception e) {
            log.error("Transfer failed through connection {}", connectionId, e);
            connection.setStatus(P2PConnection.ConnectionStatus.ERROR);
            throw e;
        }
    }

    @Override
    public Map<String, Object> getConnectionStatistics(String connectionId) {
        P2PConnectionImpl connection = connections.get(connectionId);
        if (connection == null) {
            return Collections.emptyMap();
        }

        return connection.getStatistics();
    }

    private P2PDataTransferService getService(String storageType) {
        return serviceRegistry.get(storageType.toLowerCase());
    }

    /**
     * 内部连接实现类
     */
    private static class P2PConnectionImpl implements P2PConnection {
        private final String connectionId;
        private final EndpointInfo sourceEndpoint;
        private final EndpointInfo targetEndpoint;
        private final P2PDataTransferService sourceService;
        private final P2PDataTransferService targetService;
        private final Map<String, Object> configuration;
        private final LocalDateTime establishedTime;
        private LocalDateTime lastActiveTime;
        private ConnectionStatus status;

        // 统计信息
        private int totalTransfers = 0;
        private long totalRecordsTransferred = 0;
        private long totalBytesTransferred = 0;
        private List<TransferRecord> transferHistory = new ArrayList<>();

        public P2PConnectionImpl(
                String connectionId,
                EndpointInfo sourceEndpoint,
                EndpointInfo targetEndpoint,
                P2PDataTransferService sourceService,
                P2PDataTransferService targetService,
                Map<String, Object> configuration) {

            this.connectionId = connectionId;
            this.sourceEndpoint = sourceEndpoint;
            this.targetEndpoint = targetEndpoint;
            this.sourceService = sourceService;
            this.targetService = targetService;
            this.configuration = configuration;
            this.establishedTime = LocalDateTime.now();
            this.lastActiveTime = LocalDateTime.now();
            this.status = ConnectionStatus.ESTABLISHED;
        }

        @Override
        public String getConnectionId() {
            return connectionId;
        }

        @Override
        public EndpointInfo getSourceEndpoint() {
            return sourceEndpoint;
        }

        @Override
        public EndpointInfo getTargetEndpoint() {
            return targetEndpoint;
        }

        @Override
        public ConnectionStatus getStatus() {
            return status;
        }

        public void setStatus(ConnectionStatus status) {
            this.status = status;
        }

        @Override
        public LocalDateTime getEstablishedTime() {
            return establishedTime;
        }

        @Override
        public LocalDateTime getLastActiveTime() {
            return lastActiveTime;
        }

        public void updateLastActiveTime() {
            this.lastActiveTime = LocalDateTime.now();
        }

        @Override
        public Map<String, Object> getConfiguration() {
            return new HashMap<>(configuration);
        }

        @Override
        public boolean isAlive() {
            return status == ConnectionStatus.ESTABLISHED 
                || status == ConnectionStatus.IDLE
                || status == ConnectionStatus.TRANSFERRING;
        }

        @Override
        public void close() {
            this.status = ConnectionStatus.CLOSED;
        }

        public P2PDataTransferService getSourceService() {
            return sourceService;
        }

        public P2PDataTransferService getTargetService() {
            return targetService;
        }

        public void recordTransfer(P2PDataTransferService.TransferResult result) {
            totalTransfers++;
            totalRecordsTransferred += result.getSuccessCount();
            
            TransferRecord record = new TransferRecord(
                    LocalDateTime.now(),
                    result.getTotalRecords(),
                    result.getSuccessCount(),
                    result.getDurationMs()
            );
            
            transferHistory.add(record);
            
            // 保留最近100次记录
            if (transferHistory.size() > 100) {
                transferHistory.remove(0);
            }
        }

        public Map<String, Object> getStatistics() {
            Map<String, Object> stats = new HashMap<>();
            stats.put("connection_id", connectionId);
            stats.put("source_type", sourceEndpoint.getStorageType());
            stats.put("target_type", targetEndpoint.getStorageType());
            stats.put("status", status.toString());
            stats.put("established_time", establishedTime.toString());
            stats.put("last_active_time", lastActiveTime.toString());
            stats.put("total_transfers", totalTransfers);
            stats.put("total_records_transferred", totalRecordsTransferred);
            stats.put("total_bytes_transferred", totalBytesTransferred);
            stats.put("recent_transfers", transferHistory.size());
            
            if (!transferHistory.isEmpty()) {
                long avgDuration = transferHistory.stream()
                        .mapToLong(TransferRecord::getDurationMs)
                        .sum() / transferHistory.size();
                stats.put("avg_transfer_duration_ms", avgDuration);
            }
            
            return stats;
        }

        private static class TransferRecord {
            private final LocalDateTime timestamp;
            private final int totalRecords;
            private final int successCount;
            private final long durationMs;

            public TransferRecord(LocalDateTime timestamp, int totalRecords, 
                                int successCount, long durationMs) {
                this.timestamp = timestamp;
                this.totalRecords = totalRecords;
                this.successCount = successCount;
                this.durationMs = durationMs;
            }

            public long getDurationMs() {
                return durationMs;
            }
        }
    }
}
