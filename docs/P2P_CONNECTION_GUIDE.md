# P2P端到端连接管理指南
# P2P Point-to-Point Connection Management Guide

## 概述 (Overview)

P2P连接管理提供了在不同存储端点之间建立持久连接的能力，支持连接复用、健康监控和统计跟踪。

P2P connection management provides the ability to establish persistent connections between different storage endpoints, supporting connection reuse, health monitoring, and statistics tracking.

## 架构设计 (Architecture)

### 三层架构 (Three-Layer Architecture)

```
┌─────────────────────────────────────────┐
│    Connection Management Layer          │
│  (P2PConnectionManager)                 │
│  - Establish connections                │
│  - Monitor health                       │
│  - Track statistics                     │
└─────────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│    Transfer Orchestration Layer         │
│  (P2PTransferBridge)                    │
│  - Coordinate transfers                 │
│  - Transform data                       │
│  - Batch processing                     │
└─────────────────────────────────────────┘
                 ↓
┌─────────────────────────────────────────┐
│    Storage Service Layer                │
│  (P2PDataTransferService)               │
│  - Read from source                     │
│  - Write to target                      │
│  - Storage-specific operations          │
└─────────────────────────────────────────┘
```

## 核心接口 (Core Interfaces)

### P2PConnection - 连接抽象

表示两个存储端点之间的持久连接。

Represents a persistent connection between two storage endpoints.

```java
public interface P2PConnection {
    
    // 连接标识
    String getConnectionId();
    
    // 源端点信息
    EndpointInfo getSourceEndpoint();
    
    // 目标端点信息
    EndpointInfo getTargetEndpoint();
    
    // 连接状态
    ConnectionStatus getStatus();
    
    // 健康检查
    boolean isAlive();
    
    // 关闭连接
    void close();
    
    // 端点信息
    class EndpointInfo {
        private String endpointId;      // 端点唯一标识
        private String storageType;     // 存储类型: sqlite, redis, mongodb, elasticsearch
        private String host;            // 主机地址
        private Integer port;           // 端口号
        private String database;        // 数据库名称
        private Map<String, Object> metadata;  // 其他元数据
    }
    
    // 连接状态
    enum ConnectionStatus {
        CONNECTING,     // 正在连接
        ESTABLISHED,    // 已建立
        TRANSFERRING,   // 传输中
        IDLE,          // 空闲
        ERROR,         // 错误
        CLOSED         // 已关闭
    }
}
```

### P2PConnectionManager - 连接管理器

管理连接的生命周期。

Manages the lifecycle of connections.

```java
public interface P2PConnectionManager {
    
    // 建立连接
    P2PConnection establish(
        EndpointInfo sourceEndpoint,
        EndpointInfo targetEndpoint,
        Map<String, Object> config
    );
    
    // 获取连接
    Optional<P2PConnection> getConnection(String connectionId);
    
    // 获取所有活跃连接
    List<P2PConnection> getActiveConnections();
    
    // 关闭连接
    boolean closeConnection(String connectionId);
    
    // 通过连接传输数据
    TransferResult transferThroughConnection(
        String connectionId,
        Map<String, Object> query,
        int batchSize
    );
    
    // 获取连接统计
    Map<String, Object> getConnectionStatistics(String connectionId);
    
    // 检查连接健康状态
    boolean isHealthy(String connectionId);
}
```

## 使用示例 (Usage Examples)

### 1. 基本连接建立 (Basic Connection Establishment)

```java
@Autowired
private P2PConnectionManager connectionManager;

// 配置SQLite源端点
P2PConnection.EndpointInfo sourceEndpoint = new P2PConnection.EndpointInfo(
    "sqlite-local",
    "sqlite"
);
sourceEndpoint.setDatabase("./data/knowledge.db");

// 配置Elasticsearch目标端点
P2PConnection.EndpointInfo targetEndpoint = new P2PConnection.EndpointInfo(
    "es-cluster",
    "elasticsearch"
);
targetEndpoint.setHost("localhost");
targetEndpoint.setPort(9200);
targetEndpoint.setDatabase("knowledge_base");

// 连接配置
Map<String, Object> config = Map.of(
    "batch_size", 100,
    "auto_reconnect", true
);

// 建立连接
P2PConnection connection = connectionManager.establish(
    sourceEndpoint,
    targetEndpoint,
    config
);

log.info("Connection established: {}", connection.getConnectionId());
```

### 2. 数据传输 (Data Transfer)

```java
// 定义查询条件
Map<String, Object> query = Map.of(
    "type", "knowledge",
    "limit", 1000
);

// 通过连接传输数据
TransferResult result = connectionManager.transferThroughConnection(
    connection.getConnectionId(),
    query,
    100  // batch size
);

log.info("Transferred {} records in {}ms",
    result.getSuccessCount(),
    result.getDurationMs());
```

### 3. 连接复用 (Connection Reuse)

```java
// 同一连接可用于多次传输
for (int i = 0; i < 10; i++) {
    Map<String, Object> query = Map.of(
        "offset", i * 100,
        "limit", 100
    );
    
    TransferResult result = connectionManager.transferThroughConnection(
        connection.getConnectionId(),
        query,
        100
    );
    
    log.info("Batch {}: {} records", i, result.getSuccessCount());
}
```

### 4. 健康监控 (Health Monitoring)

```java
// 检查连接健康状态
boolean healthy = connectionManager.isHealthy(connection.getConnectionId());

if (!healthy) {
    log.warn("Connection {} is unhealthy", connection.getConnectionId());
    
    // 可以选择重新建立连接
    connectionManager.closeConnection(connection.getConnectionId());
    P2PConnection newConnection = connectionManager.establish(
        sourceEndpoint,
        targetEndpoint,
        config
    );
}
```

### 5. 统计信息 (Statistics)

```java
// 获取连接统计信息
Map<String, Object> stats = connectionManager.getConnectionStatistics(
    connection.getConnectionId()
);

log.info("Connection Statistics:");
log.info("  Total transfers: {}", stats.get("total_transfers"));
log.info("  Total records: {}", stats.get("total_records_transferred"));
log.info("  Success count: {}", stats.get("success_count"));
log.info("  Average duration: {}ms", stats.get("avg_duration_ms"));
log.info("  Last transfer: {}", stats.get("last_transfer_time"));
```

### 6. 多连接管理 (Multiple Connections)

```java
// MongoDB → Redis 连接
P2PConnection.EndpointInfo mongoEndpoint = new P2PConnection.EndpointInfo(
    "mongo-primary",
    "mongodb"
);
mongoEndpoint.setHost("localhost");
mongoEndpoint.setPort(27017);

P2PConnection.EndpointInfo redisEndpoint = new P2PConnection.EndpointInfo(
    "redis-cache",
    "redis"
);
redisEndpoint.setHost("localhost");
redisEndpoint.setPort(6379);

P2PConnection mongoToRedis = connectionManager.establish(
    mongoEndpoint,
    redisEndpoint,
    config
);

// 查看所有活跃连接
List<P2PConnection> activeConnections = connectionManager.getActiveConnections();
log.info("Active connections: {}", activeConnections.size());

for (P2PConnection conn : activeConnections) {
    log.info("  {} → {} ({})",
        conn.getSourceEndpoint().getStorageType(),
        conn.getTargetEndpoint().getStorageType(),
        conn.getStatus());
}
```

### 7. 关闭连接 (Close Connection)

```java
// 完成传输后关闭连接
boolean closed = connectionManager.closeConnection(connection.getConnectionId());

if (closed) {
    log.info("Connection closed successfully");
} else {
    log.warn("Failed to close connection");
}
```

## 服务注册 (Service Registration)

### 自动注册 (Auto Registration)

Spring Boot会自动注册所有可用的P2PDataTransferService实现：

```java
@Configuration
@AutoConfiguration
public class P2PConnectionAutoConfiguration {
    
    @Bean
    public P2PConnectionManager p2pConnectionManager(
            P2PTransferBridge transferBridge,
            List<P2PDataTransferService> services) {
        
        DefaultP2PConnectionManager manager = 
            new DefaultP2PConnectionManager(transferBridge);
        
        // 自动注册所有服务
        services.forEach(service -> {
            String storageType = extractStorageType(service);
            manager.registerService(storageType, service);
        });
        
        return manager;
    }
}
```

### 手动注册 (Manual Registration)

```java
DefaultP2PConnectionManager manager = 
    new DefaultP2PConnectionManager(transferBridge);

// 注册SQLite服务
manager.registerService("sqlite", sqliteService);

// 注册Redis服务
manager.registerService("redis", redisService);

// 注册MongoDB服务
manager.registerService("mongodb", mongoService);

// 注册Elasticsearch服务
manager.registerService("elasticsearch", esService);
```

## 支持的存储类型 (Supported Storage Types)

当前支持以下存储类型：

Currently supports the following storage types:

| 存储类型 | 标识符 | Spring Starter |
|---------|--------|----------------|
| Memory | `memory` | `omni-agent-p2p-starter-memory` |
| SQLite | `sqlite` | `omni-agent-p2p-starter-sqlite` |
| Redis | `redis` | `omni-agent-p2p-starter-redis` |
| MongoDB | `mongodb` | `omni-agent-p2p-starter-mongodb` |
| Elasticsearch | `elasticsearch` | `omni-agent-p2p-starter-elasticsearch` |

## 配置选项 (Configuration Options)

### 连接配置 (Connection Configuration)

```java
Map<String, Object> config = new HashMap<>();

// 批量大小
config.put("batch_size", 100);

// 自动重连
config.put("auto_reconnect", true);

// 最大重试次数
config.put("max_retries", 3);

// 连接超时（毫秒）
config.put("connection_timeout", 5000);

// 传输超时（毫秒）
config.put("transfer_timeout", 30000);

// 自定义元数据
config.put("description", "Knowledge base transfer");
```

## 最佳实践 (Best Practices)

### 1. 连接复用 (Connection Reuse)

✅ **推荐**: 建立一次连接，用于多次传输
```java
P2PConnection connection = connectionManager.establish(...);

for (int i = 0; i < batches; i++) {
    connectionManager.transferThroughConnection(
        connection.getConnectionId(), ...
    );
}

connectionManager.closeConnection(connection.getConnectionId());
```

❌ **不推荐**: 每次传输都建立新连接
```java
for (int i = 0; i < batches; i++) {
    P2PConnection connection = connectionManager.establish(...);
    connectionManager.transferThroughConnection(...);
    connectionManager.closeConnection(...);
}
```

### 2. 健康检查 (Health Check)

```java
// 长时间运行的传输任务中定期检查
if (!connectionManager.isHealthy(connectionId)) {
    // 重新建立连接
    connectionManager.closeConnection(connectionId);
    connection = connectionManager.establish(...);
}
```

### 3. 错误处理 (Error Handling)

```java
try {
    TransferResult result = connectionManager.transferThroughConnection(
        connectionId, query, batchSize
    );
    
    if (result.getFailureCount() > 0) {
        log.warn("Transfer had {} failures", result.getFailureCount());
        // 处理失败记录
    }
} catch (Exception e) {
    log.error("Transfer failed", e);
    // 重试或回滚
}
```

### 4. 资源清理 (Resource Cleanup)

```java
try {
    P2PConnection connection = connectionManager.establish(...);
    
    // 使用连接进行传输
    connectionManager.transferThroughConnection(...);
    
} finally {
    // 确保连接被关闭
    if (connection != null) {
        connectionManager.closeConnection(connection.getConnectionId());
    }
}
```

## 性能优化 (Performance Optimization)

### 1. 批量大小调优

- **小批量** (50-100): 适合频繁更新、低延迟场景
- **中批量** (100-500): 平衡性能和内存使用
- **大批量** (500-1000): 适合大量数据迁移

### 2. 并发传输

```java
// 为不同的数据分区使用不同的连接
List<P2PConnection> connections = new ArrayList<>();

for (int i = 0; i < partitions; i++) {
    P2PConnection conn = connectionManager.establish(...);
    connections.add(conn);
}

// 并行传输
connections.parallelStream().forEach(conn -> {
    connectionManager.transferThroughConnection(
        conn.getConnectionId(), ...
    );
});
```

### 3. 连接池管理

当前版本使用ConcurrentHashMap管理连接。未来版本将支持：
- 连接池大小限制
- 连接TTL（Time To Live）
- 连接预热（Warm-up）

## 故障排查 (Troubleshooting)

### 问题1: 连接建立失败

```
错误: Failed to establish connection: Service not found for storage type 'xxx'
```

**解决方案**: 确保相应的starter已添加到依赖中：

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-p2p-starter-xxx</artifactId>
    <version>1.0.0</version>
</dependency>
```

### 问题2: 传输失败

```
错误: Connection is not in a valid state for transfer
```

**解决方案**: 检查连接状态和健康度：

```java
P2PConnection conn = connectionManager.getConnection(connectionId).orElse(null);
if (conn == null || !conn.isAlive()) {
    // 重新建立连接
}
```

### 问题3: 性能问题

**症状**: 传输速度慢

**解决方案**:
1. 增加批量大小
2. 检查网络延迟
3. 优化查询条件
4. 使用并行传输

## 下一步 (Next Steps)

- 查看 [P2P_DATA_TRANSFER_GUIDE.md](P2P_DATA_TRANSFER_GUIDE.md) 了解数据传输API
- 查看 [EXAMPLES.md](EXAMPLES.md) 获取完整示例
- 参考各个starter的README了解存储特定配置
