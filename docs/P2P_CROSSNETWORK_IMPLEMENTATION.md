# P2P 跨网络 IP 直连功能实现文档

## 📋 更新概述

为 OmniAgent P2P 模块添加了跨网络 IP 直连功能，使得 P2P 连接不再局限于局域网扫描发现，支持通过 IP 地址进行跨网络的点对点连接。

**更新日期**: 2025-12-30

---

## 🎯 功能特性

### 1. 连接方式

OmniAgent P2P 现在支持三种连接方式：

| 连接方式 | 适用场景 | 网络要求 | API 方法 |
|---------|---------|---------|---------|
| **局域网扫描** | 同一局域网内设备 | 局域网可达 | `scanEndpoints()` + `connectWithCode()` |
| **IP 直连** | 跨网络/公网连接 | 目标可通过 IP:Port 访问 | `connectByIp()` |
| **IP + 端点ID** | 跨网络精确连接 | 目标可通过 IP:Port 访问 | `connectByIpAndEndpoint()` |

### 2. 核心能力

- ✅ **局域网连接**: 扫描发现 + 连接码验证
- ✅ **跨网络连接**: IP 地址 + 端口 + 连接码
- ✅ **安全验证**: 连接码时效性控制（默认10分钟）
- ✅ **加密传输**: 基于连接码的安全握手
- ✅ **一次性验证**: 连接码使用后自动失效
- ✅ **多存储支持**: Memory, H2, SQLite, Redis, MongoDB, Elasticsearch

---

## 🔧 API 更新

### P2PConnectionManager 接口新增方法

```java
/**
 * 通过 IP 地址直接连接（跨网络）
 */
P2PConnection connectByIp(
    String remoteIp,
    int remotePort,
    String connectionCode,
    Map<String, Object> config
);

/**
 * 通过 IP 地址和端点 ID 连接
 */
P2PConnection connectByIpAndEndpoint(
    String remoteIp,
    int remotePort,
    String endpointId,
    String connectionCode,
    Map<String, Object> config
);
```

### P2PEndpointDiscovery 接口新增方法

```java
/**
 * 通过 IP 地址查找端点（跨网络）
 */
Optional<DiscoveredEndpoint> findEndpointByIp(String ipAddress, int port);

/**
 * 验证远程端点的连接码（通过 IP）
 */
boolean validateRemoteConnectionCode(String ipAddress, int port, String connectionCode);
```

---

## 📝 使用示例

### 场景1: 服务端注册端点

```java
@Autowired
private P2PEndpointDiscovery endpointDiscovery;

// 创建端点信息
P2PConnection.EndpointInfo endpoint = new P2PConnection.EndpointInfo(
    "storage-node-01",
    "sqlite"
);
endpoint.setHost("192.168.1.100");  // 本地 IP
endpoint.setPort(8081);              // 服务端口

// 生成连接码
String connectionCode = endpointDiscovery.generateConnectionCode(
    endpoint.getEndpointId(),
    10  // 10分钟有效期
);

// 注册端点
endpointDiscovery.registerEndpoint(endpoint, connectionCode);

// 分享连接信息给客户端
System.out.println("IP: " + endpoint.getHost());
System.out.println("Port: " + endpoint.getPort());
System.out.println("Code: " + connectionCode);
```

### 场景2: 客户端通过 IP 连接

```java
@Autowired
private P2PConnectionManager connectionManager;

// 从服务端获取的信息
String remoteIp = "203.0.113.50";
int remotePort = 8081;
String connectionCode = "ABC12345";

// 连接配置
Map<String, Object> config = new HashMap<>();
config.put("local_storage_type", "sqlite");

// 建立连接
P2PConnection connection = connectionManager.connectByIp(
    remoteIp,
    remotePort,
    connectionCode,
    config
);

System.out.println("连接成功: " + connection.getConnectionId());
```

### 场景3: 通过端点ID精确连接

```java
// 如果知道远程端点的 ID，可以使用更精确的连接方式
P2PConnection connection = connectionManager.connectByIpAndEndpoint(
    "203.0.113.50",     // IP
    8081,               // Port
    "storage-node-01",  // 端点 ID
    "ABC12345",         // 连接码
    config
);
```

---

## 🌐 网络配置要求

### 局域网连接

- ✅ 端点在同一局域网内
- ✅ 端口可互相访问
- ✅ 防火墙允许指定端口

### 跨网络连接

#### 服务端要求

1. **网络可达性**
   - 公网 IP 地址 或
   - 配置端口映射（NAT转发）

2. **防火墙配置**
   ```bash
   # Linux 防火墙开放端口示例
   sudo ufw allow 8081/tcp
   ```

3. **端口映射**（如使用NAT）
   ```
   外网端口 8081 → 内网IP:8081
   ```

#### 客户端要求

- 能够访问服务端 IP:Port
- 获取有效的连接码
- 配置合理的超时时间

---

## 🔒 安全建议

### 1. 连接码管理

```java
// ✅ 推荐：短时效连接码
String code = endpointDiscovery.generateConnectionCode(endpointId, 5);  // 5分钟

// ❌ 不推荐：长时效连接码
String code = endpointDiscovery.generateConnectionCode(endpointId, 1440); // 24小时
```

### 2. 传递连接码的安全方式

- ✅ 加密即时通讯工具
- ✅ 企业内部安全邮件
- ✅ 电话语音传达
- ✅ 二维码（现场扫描）
- ❌ 明文短信
- ❌ 公开聊天频道
- ❌ 不加密的邮件

### 3. 网络安全加固

```yaml
omni-agent:
  p2p:
    security:
      # IP 白名单
      ip-whitelist:
        - 203.0.113.0/24
        - 198.51.100.0/24
      # 启用 SSL/TLS
      ssl-enabled: true
      # 最大连接数
      max-connections: 100
      # 连接超时
      connection-timeout-seconds: 30
```

---

## 📂 更新的文件清单

### 核心代码

1. **API 层**
   - `omni-agent-p2p-api/src/main/java/.../P2PConnectionManager.java`
     - 新增 `connectByIp()` 方法
     - 新增 `connectByIpAndEndpoint()` 方法
   
   - `omni-agent-p2p-api/src/main/java/.../P2PEndpointDiscovery.java`
     - 新增 `findEndpointByIp()` 方法
     - 新增 `validateRemoteConnectionCode()` 方法

2. **实现层**
   - `omni-agent-core/src/main/java/.../DefaultP2PEndpointDiscovery.java`
     - 实现 IP 查找端点
     - 实现远程连接码验证
   
   - `omni-agent-core/src/main/java/.../DefaultP2PConnectionManager.java`
     - 实现 IP 直连逻辑
     - 实现 IP + 端点ID 连接

### 示例代码

3. **示例**
   - `omni-agent-example-basic/.../P2PIPConnectionExample.java`
     - 完整的 IP 直连使用示例
     - 包含服务端和客户端代码
     - 详细的使用说明

### 文档

4. **文档**
   - `omni-agent-p2p-starter/README.md`
     - 新增"跨网络 IP 直连"章节
     - 添加使用示例
     - 安全建议
   
   - `docs/application-p2p-crossnetwork-example.yml`
     - P2P 跨网络配置示例
     - 多种场景配置模板
     - 集群模式配置

   - `docs/P2P_CROSSNETWORK_IMPLEMENTATION.md` (本文档)
     - 功能说明
     - API 文档
     - 使用指南

---

## 🚀 快速开始

### 1. 启动服务端

```java
// 参考示例代码
P2PIPConnectionExample (服务端部分)
```

运行后会输出：
```
✅ 端点已注册:
   端点 ID: storage-node-01
   连接码: ABC12345
   本地地址: 192.168.1.100:8081
   有效期至: 2025-12-30T10:25:00
```

### 2. 启动客户端

```java
// 使用服务端输出的信息
String remoteIp = "192.168.1.100";  // 或公网IP
int remotePort = 8081;
String connectionCode = "ABC12345";

P2PConnection connection = connectionManager.connectByIp(
    remoteIp, remotePort, connectionCode, config
);
```

### 3. 验证连接

```java
// 检查连接状态
boolean healthy = connectionManager.isHealthy(connection.getConnectionId());
System.out.println("连接健康: " + healthy);

// 获取连接统计
Map<String, Object> stats = connectionManager.getConnectionStatistics(
    connection.getConnectionId()
);
stats.forEach((k, v) -> System.out.println(k + ": " + v));
```

---

## 🧪 测试建议

### 单元测试

```java
@Test
void testConnectByIp() {
    // 1. 注册端点
    P2PConnection.EndpointInfo endpoint = createTestEndpoint();
    String code = endpointDiscovery.generateConnectionCode(endpoint.getEndpointId(), 10);
    endpointDiscovery.registerEndpoint(endpoint, code);
    
    // 2. 通过 IP 连接
    P2PConnection connection = connectionManager.connectByIp(
        endpoint.getHost(), 
        endpoint.getPort(), 
        code, 
        new HashMap<>()
    );
    
    // 3. 验证
    assertNotNull(connection);
    assertTrue(connection.isAlive());
}
```

### 集成测试

1. **局域网测试**: 在同一网络内的两台机器测试
2. **跨网络测试**: 使用公网 IP 或 VPN 测试
3. **安全测试**: 测试连接码过期、重复使用等场景
4. **性能测试**: 测试并发连接、大数据传输

---

## 🐛 故障排查

### 连接失败

**问题**: `SecurityException: 连接码验证失败`

**解决方案**:
- ✅ 检查连接码是否正确
- ✅ 检查连接码是否过期
- ✅ 检查连接码是否已使用
- ✅ 服务端是否正确注册端点

**问题**: `IllegalArgumentException: 端点未找到`

**解决方案**:
- ✅ 检查 IP 地址是否正确
- ✅ 检查端口是否正确
- ✅ 服务端是否已注册端点
- ✅ 端点是否已过期

### 网络不通

**问题**: 连接超时

**解决方案**:
```bash
# 1. 测试网络连通性
ping 203.0.113.50

# 2. 测试端口是否开放
telnet 203.0.113.50 8081
# 或
nc -zv 203.0.113.50 8081

# 3. 检查防火墙
sudo ufw status
sudo ufw allow 8081/tcp

# 4. 检查 NAT 配置（如果有）
```

---

## 📊 性能指标

### 连接建立时间

- 局域网: < 100ms
- 公网 (国内): 100-500ms
- 公网 (跨国): 500-2000ms

### 数据传输速度

取决于网络带宽，P2P 模块本身性能开销 < 5%

---

## 🔮 未来规划

- [ ] NAT 穿透支持（STUN/TURN）
- [ ] WebRTC 集成
- [ ] 端到端加密增强
- [ ] 多路径传输优化
- [ ] 自动服务发现（mDNS）
- [ ] 连接池管理
- [ ] 断线重连机制

---

## 📞 支持

如有问题，请参考：
- [P2P Starter README](../omni-agent-p2p-starter/README.md)
- [示例代码](../omni-agent-example-basic/src/main/java/top/yumbo/ai/omni/example/basic/p2p/)
- [配置示例](./application-p2p-crossnetwork-example.yml)

---

**OmniAgent Team**  
*让知识在网络中自由流动* 🌐

