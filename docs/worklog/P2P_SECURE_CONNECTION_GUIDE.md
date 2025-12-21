# P2P安全连接指南
# P2P Secure Connection Guide

## 概述 (Overview)

P2P安全连接提供了通过网络发现端点并使用连接码建立安全连接的能力，确保端到端连接的安全性和可靠性。

P2P secure connection provides the ability to discover endpoints over the network and establish secure connections using connection codes, ensuring the security and reliability of peer-to-peer connections.

## 核心概念 (Core Concepts)

### 1. 端点发现 (Endpoint Discovery)

通过网络扫描和注册机制，使端点能够相互发现。

```
┌─────────────────────────────────────────┐
│      Network Endpoint Registry          │
│  (端点注册表)                            │
├─────────────────────────────────────────┤
│  Endpoint 1: SQLite @ 192.168.1.100     │
│  Endpoint 2: ES @ 192.168.1.200         │
│  Endpoint 3: MongoDB @ 192.168.1.201    │
│  ...                                    │
└─────────────────────────────────────────┘
```

### 2. 连接码机制 (Connection Code)

类似于Wi-Fi密码，用于验证连接请求的合法性。

```
连接码特性:
- 8位随机字符（排除易混淆字符）
- 可设置有效期（默认24小时）
- 安全哈希存储
- 一次性使用或可复用
```

### 3. 安全握手 (Secure Handshake)

基于挑战-响应机制的安全连接建立过程。

```
源端点                      目标端点
   |                           |
   |---(1) 发起握手----------->|
   |    (连接码)               |
   |                           |
   |<--(2) 挑战令牌------------|
   |                           |
   |---(3) 响应挑战----------->|
   |    (验证连接码)           |
   |                           |
   |<--(4) 建立连接------------|
   |    (共享密钥)             |
```

## 安全连接流程 (Secure Connection Flow)

### 完整流程图

```
┌────────────────────────────────────────────────────────┐
│  步骤1: 端点注册 (Endpoint Registration)               │
├────────────────────────────────────────────────────────┤
│  1.1 生成连接码                                        │
│      connectionCode = generateConnectionCode()         │
│                                                        │
│  1.2 注册端点到网络                                    │
│      registerEndpoint(endpoint, connectionCode)        │
└────────────────────────────────────────────────────────┘
                         ↓
┌────────────────────────────────────────────────────────┐
│  步骤2: 端点发现 (Endpoint Discovery)                  │
├────────────────────────────────────────────────────────┤
│  2.1 扫描网络                                          │
│      endpoints = scanEndpoints(filter)                 │
│                                                        │
│  2.2 选择目标端点                                      │
│      targetEndpoint = selectEndpoint(endpoints)        │
└────────────────────────────────────────────────────────┘
                         ↓
┌────────────────────────────────────────────────────────┐
│  步骤3: 安全握手 (Secure Handshake)                    │
├────────────────────────────────────────────────────────┤
│  3.1 发起握手                                          │
│      session = initiateHandshake(source, target, code) │
│                                                        │
│  3.2 验证连接码                                        │
│      result = acceptHandshake(sessionId, code)         │
│                                                        │
│  3.3 生成共享密钥                                      │
│      sharedSecret = generateSharedSecret()             │
└────────────────────────────────────────────────────────┘
                         ↓
┌────────────────────────────────────────────────────────┐
│  步骤4: 建立安全连接 (Establish Secure Connection)     │
├────────────────────────────────────────────────────────┤
│  connection = establishWithHandshake(                  │
│      source, target, connectionCode, config            │
│  )                                                     │
└────────────────────────────────────────────────────────┘
                         ↓
┌────────────────────────────────────────────────────────┐
│  步骤5: 安全数据传输 (Secure Data Transfer)            │
├────────────────────────────────────────────────────────┤
│  result = transferThroughConnection(                   │
│      connectionId, query, batchSize                    │
│  )                                                     │
└────────────────────────────────────────────────────────┘
```

## 使用示例 (Usage Examples)

### 场景1: 端点注册

```java
@Autowired
private P2PEndpointDiscovery endpointDiscovery;

// 创建端点信息
P2PConnection.EndpointInfo endpoint = new P2PConnection.EndpointInfo(
    "sqlite-local",
    "sqlite"
);
endpoint.setDatabase("./data/local.db");
endpoint.setHost("192.168.1.100");
endpoint.setPort(8080);

// 生成连接码（30分钟有效期）
String connectionCode = endpointDiscovery.generateConnectionCode(
    endpoint.getEndpointId(),
    30
);

log.info("连接码: {}", connectionCode);
// 输出: 连接码: A3K9X7M2

// 注册端点
EndpointRegistration registration = endpointDiscovery.registerEndpoint(
    endpoint,
    connectionCode
);

log.info("端点已注册，过期时间: {}", registration.getExpiresAt());
```

### 场景2: 扫描和发现端点

```java
// 扫描所有端点
List<DiscoveredEndpoint> allEndpoints = endpointDiscovery.scanEndpoints(null);

log.info("发现 {} 个端点", allEndpoints.size());
for (DiscoveredEndpoint ep : allEndpoints) {
    log.info("- {} ({}) @ {}:{}",
        ep.getEndpointInfo().getEndpointId(),
        ep.getEndpointInfo().getStorageType(),
        ep.getEndpointInfo().getHost(),
        ep.getEndpointInfo().getPort());
}

// 按存储类型过滤
EndpointFilter filter = new EndpointFilter();
filter.setStorageType("elasticsearch");

List<DiscoveredEndpoint> esEndpoints = endpointDiscovery.scanEndpoints(filter);
log.info("找到 {} 个Elasticsearch端点", esEndpoints.size());

// 按网络段过滤
filter.setNetworkSegment("192.168.1.0/24");
List<DiscoveredEndpoint> localEndpoints = endpointDiscovery.scanEndpoints(filter);
```

### 场景3: 安全握手

```java
@Autowired
private P2PSecureHandshake secureHandshake;

// 发起握手
HandshakeSession session = secureHandshake.initiateHandshake(
    sourceEndpoint,
    targetEndpoint,
    connectionCode
);

log.info("握手会话: {}", session.getSessionId());
log.info("状态: {}", session.getStatus());

// 接受握手
HandshakeResult result = secureHandshake.acceptHandshake(
    session.getSessionId(),
    connectionCode
);

if (result.isSuccess()) {
    log.info("✓ 握手成功");
    log.info("连接ID: {}", result.getConnectionId());
    log.info("共享密钥: {}", result.getSharedSecret());
} else {
    log.error("✗ 握手失败: {}", result.getFailureReason());
}
```

### 场景4: 建立安全连接

```java
@Autowired
private P2PConnectionManager connectionManager;

// 配置连接参数
Map<String, Object> config = new HashMap<>();
config.put("batch_size", 100);
config.put("encryption", "AES-256");
config.put("timeout", 30000);

// 通过安全握手建立连接
P2PConnection connection = connectionManager.establishWithHandshake(
    sourceEndpoint,
    targetEndpoint,
    connectionCode,
    config
);

log.info("✓ 安全连接已建立: {}", connection.getConnectionId());

// 检查安全配置
boolean isSecure = (Boolean) connection.getConfiguration()
    .getOrDefault("secure_connection", false);

log.info("安全连接: {}", isSecure ? "是" : "否");
```

### 场景5: 通过安全连接传输数据

```java
// 定义查询条件
Map<String, Object> query = new HashMap<>();
query.put("type", "knowledge");
query.put("from_date", "2024-01-01");
query.put("limit", 1000);

// 通过安全连接传输
TransferResult result = connectionManager.transferThroughConnection(
    connection.getConnectionId(),
    query,
    100
);

log.info("传输结果:");
log.info("- 总记录: {}", result.getTotalRecords());
log.info("- 成功: {}", result.getSuccessCount());
log.info("- 失败: {}", result.getFailureCount());
log.info("- 耗时: {}ms", result.getDurationMs());
```

## API参考 (API Reference)

### P2PEndpointDiscovery

**端点注册**:
```java
EndpointRegistration registerEndpoint(
    EndpointInfo endpoint,
    String connectionCode
)
```

**生成连接码**:
```java
String generateConnectionCode(
    String endpointId,
    int validityMinutes
)
```

**扫描端点**:
```java
List<DiscoveredEndpoint> scanEndpoints(
    EndpointFilter filter
)
```

**验证连接码**:
```java
boolean validateConnectionCode(
    String endpointId,
    String connectionCode
)
```

### P2PSecureHandshake

**发起握手**:
```java
HandshakeSession initiateHandshake(
    EndpointInfo sourceEndpoint,
    EndpointInfo targetEndpoint,
    String connectionCode
)
```

**接受握手**:
```java
HandshakeResult acceptHandshake(
    String sessionId,
    String connectionCode
)
```

**验证握手**:
```java
boolean verifyHandshake(String sessionId)
```

### P2PConnectionManager

**安全连接建立**:
```java
P2PConnection establishWithHandshake(
    EndpointInfo sourceEndpoint,
    EndpointInfo targetEndpoint,
    String connectionCode,
    Map<String, Object> config
)
```

## 安全特性 (Security Features)

### 1. 连接码安全

- ✅ **随机生成**: 使用SecureRandom生成
- ✅ **哈希存储**: 不存储明文连接码
- ✅ **有效期限**: 可设置过期时间
- ✅ **排除混淆字符**: 避免0/O、1/I等

### 2. 握手安全

- ✅ **挑战-响应**: 防止重放攻击
- ✅ **会话超时**: 默认5分钟
- ✅ **双向验证**: 源和目标都需验证
- ✅ **共享密钥**: 握手成功后生成

### 3. 传输安全

- ✅ **加密传输**: 支持AES-256
- ✅ **完整性校验**: 数据完整性验证
- ✅ **连接隔离**: 每个连接独立密钥
- ✅ **审计日志**: 记录所有连接操作

## 配置选项 (Configuration)

### 端点注册配置

```java
Map<String, Object> metadata = new HashMap<>();
metadata.put("description", "SQLite数据库");
metadata.put("version", "3.36.0");
metadata.put("max_connections", 10);

endpoint.setMetadata(metadata);
```

### 连接码配置

```java
// 生成连接码时指定有效期
String code15min = generateConnectionCode(endpointId, 15);   // 15分钟
String code1hour = generateConnectionCode(endpointId, 60);   // 1小时
String code1day = generateConnectionCode(endpointId, 1440);  // 24小时
```

### 安全连接配置

```java
Map<String, Object> config = new HashMap<>();

// 加密配置
config.put("encryption", "AES-256");
config.put("key_exchange", "ECDH");

// 超时配置
config.put("connection_timeout", 5000);   // 连接超时（毫秒）
config.put("handshake_timeout", 30000);   // 握手超时（毫秒）
config.put("transfer_timeout", 300000);   // 传输超时（毫秒）

// 重试配置
config.put("max_retries", 3);
config.put("retry_delay", 1000);

// 批量配置
config.put("batch_size", 100);
config.put("parallel_transfers", 4);
```

## 最佳实践 (Best Practices)

### 1. 连接码管理

✅ **推荐**:
```java
// 使用适当的有效期
String code = generateConnectionCode(endpointId, 30);  // 30分钟

// 连接建立后立即失效
endpointDiscovery.unregisterEndpoint(endpointId);
```

❌ **不推荐**:
```java
// 永久有效的连接码（安全风险）
String code = generateConnectionCode(endpointId, Integer.MAX_VALUE);
```

### 2. 端点发现

✅ **推荐**:
```java
// 使用过滤器缩小扫描范围
EndpointFilter filter = new EndpointFilter();
filter.setStorageType("elasticsearch");
filter.setNetworkSegment("192.168.1.0/24");

List<DiscoveredEndpoint> endpoints = scanEndpoints(filter);
```

❌ **不推荐**:
```java
// 频繁扫描整个网络
while (true) {
    scanEndpoints(null);  // 性能问题
    Thread.sleep(100);
}
```

### 3. 错误处理

```java
try {
    P2PConnection connection = connectionManager.establishWithHandshake(
        source, target, connectionCode, config
    );
    
    // 使用连接
    
} catch (SecurityException e) {
    log.error("安全验证失败: {}", e.getMessage());
    // 通知用户检查连接码
    
} catch (IllegalArgumentException e) {
    log.error("端点配置错误: {}", e.getMessage());
    // 检查端点信息
    
} catch (Exception e) {
    log.error("连接建立失败: {}", e.getMessage());
    // 通用错误处理
}
```

### 4. 资源清理

```java
try {
    P2PConnection connection = connectionManager.establishWithHandshake(...);
    
    // 使用连接进行传输
    transferThroughConnection(...);
    
} finally {
    // 确保关闭连接
    if (connection != null) {
        connectionManager.closeConnection(connection.getConnectionId());
    }
    
    // 注销端点
    endpointDiscovery.unregisterEndpoint(endpointId);
}
```

## 故障排查 (Troubleshooting)

### 问题1: 连接码验证失败

```
错误: Invalid connection code
```

**原因**:
- 连接码输入错误
- 连接码已过期
- 端点未注册

**解决方案**:
```java
// 1. 验证连接码是否有效
boolean valid = endpointDiscovery.validateConnectionCode(endpointId, code);
if (!valid) {
    // 重新生成连接码
    String newCode = endpointDiscovery.generateConnectionCode(endpointId, 30);
}

// 2. 检查端点是否注册
Optional<DiscoveredEndpoint> endpoint = endpointDiscovery.findEndpoint(endpointId);
if (endpoint.isEmpty()) {
    // 重新注册端点
    endpointDiscovery.registerEndpoint(endpointInfo, code);
}
```

### 问题2: 握手超时

```
错误: Session expired
```

**原因**:
- 网络延迟
- 握手超时（默认5分钟）

**解决方案**:
```java
// 增加握手超时时间（修改DefaultP2PSecureHandshake）
private static final int HANDSHAKE_TIMEOUT_MINUTES = 10;  // 改为10分钟

// 或者快速重试
for (int i = 0; i < 3; i++) {
    try {
        HandshakeSession session = initiateHandshake(...);
        HandshakeResult result = acceptHandshake(...);
        if (result.isSuccess()) break;
    } catch (Exception e) {
        log.warn("握手尝试 {} 失败", i + 1);
    }
}
```

### 问题3: 端点发现失败

```
错误: No endpoints found
```

**原因**:
- 端点未注册
- 过滤条件过严
- 端点已过期

**解决方案**:
```java
// 1. 不使用过滤器扫描
List<DiscoveredEndpoint> all = scanEndpoints(null);
log.info("总端点数: {}", all.size());

// 2. 检查端点状态
for (DiscoveredEndpoint ep : all) {
    log.info("端点: {} - 状态: {}", 
        ep.getEndpointInfo().getEndpointId(),
        ep.getStatus());
}

// 3. 清理过期端点并重新注册
endpointDiscovery.unregisterEndpoint(expiredId);
endpointDiscovery.registerEndpoint(endpoint, newCode);
```

## 性能优化 (Performance Optimization)

### 1. 端点缓存

```java
// 缓存发现的端点（避免频繁扫描）
private final Map<String, DiscoveredEndpoint> endpointCache = new ConcurrentHashMap<>();
private LocalDateTime lastScan = LocalDateTime.now();

public List<DiscoveredEndpoint> getCachedEndpoints() {
    // 每5分钟刷新一次
    if (Duration.between(lastScan, LocalDateTime.now()).toMinutes() > 5) {
        List<DiscoveredEndpoint> endpoints = scanEndpoints(null);
        endpoints.forEach(ep -> 
            endpointCache.put(ep.getEndpointInfo().getEndpointId(), ep)
        );
        lastScan = LocalDateTime.now();
    }
    return new ArrayList<>(endpointCache.values());
}
```

### 2. 连接池复用

```java
// 复用已建立的安全连接
private final Map<String, P2PConnection> connectionPool = new ConcurrentHashMap<>();

public P2PConnection getOrCreateConnection(String sourceId, String targetId, String code) {
    String key = sourceId + "->" + targetId;
    
    return connectionPool.computeIfAbsent(key, k -> 
        connectionManager.establishWithHandshake(
            sourceEndpoint, targetEndpoint, code, config
        )
    );
}
```

## 下一步 (Next Steps)

- 查看 [P2P_CONNECTION_GUIDE.md](P2P_CONNECTION_GUIDE.md) 了解基础连接管理
- 查看 [P2P_DATA_TRANSFER_GUIDE.md](P2P_DATA_TRANSFER_GUIDE.md) 了解数据传输API
- 运行 `P2PSecureConnectionExample` 查看完整示例
- 参考 [SECURITY.md](SECURITY.md) 了解安全最佳实践
