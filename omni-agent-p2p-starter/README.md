# OmniAgent P2P Starter

## 📚 概述

`omni-agent-p2p-starter` 是一个统一的 P2P 协作 Starter，整合了多种数据源实现，通过配置即可切换不同的存储后端。

## ✨ 特性

### 支持的存储后端

| 存储类型 | 配置值 | 适用场景 | 依赖 |
|---------|--------|---------|------|
| **Memory** | `memory` | 开发、测试 | 无 |
| **H2** | `h2` | 嵌入式应用 | `h2` |
| **SQLite** | `sqlite` | 单机应用 | `sqlite-jdbc` |
| **Redis** | `redis` | 分布式缓存 | `spring-boot-starter-data-redis` |
| **MongoDB** | `mongodb` | 文档存储 | `spring-boot-starter-data-mongodb` |
| **Elasticsearch** | `elasticsearch` | 搜索场景 | `elasticsearch-java` |

### 核心功能

- ✅ **连接管理**：生成连接码、建立 P2P 连接
- ✅ **知识共享**：点对点知识传递
- ✅ **数据传输**：加密数据传输
- ✅ **统计分析**：共享统计

## 🚀 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-p2p-starter</artifactId>
    <version>1.0.0</version>
</dependency>
```

### 2. 配置（可选）

#### 使用 Memory（默认）

```yaml
omni-agent:
  p2p:
    enabled: true
    storage-type: memory  # 默认值
```

#### 使用 Redis

```yaml
omni-agent:
  p2p:
    enabled: true
    storage-type: redis
    redis:
      key-prefix: "p2p:"
      ttl: 0  # 0 表示永不过期

spring:
  redis:
    host: localhost
    port: 6379
```

#### 使用 MongoDB

```yaml
omni-agent:
  p2p:
    enabled: true
    storage-type: mongodb
    mongodb:
      database: omni-agent
      collection-prefix: "p2p_"

spring:
  data:
    mongodb:
      uri: mongodb://localhost:27017/omni-agent
```

#### 使用 H2

```yaml
omni-agent:
  p2p:
    enabled: true
    storage-type: h2
    h2:
      db-path: ./data/p2p/h2
      db-name: p2p
```

#### 使用 SQLite

```yaml
omni-agent:
  p2p:
    enabled: true
    storage-type: sqlite
    sqlite:
      db-path: ./data/p2p/sqlite/p2p.db
```

#### 使用 Elasticsearch

```yaml
omni-agent:
  p2p:
    enabled: true
    storage-type: elasticsearch
    elasticsearch:
      index-prefix: "p2p-"

spring:
  elasticsearch:
    uris: http://localhost:9200
```

### 3. 使用

```java
@Autowired
private P2PCollaborationService p2pService;

@Autowired
private P2PDataTransferService dataTransferService;

// 生成连接码
ConnectionCode code = p2pService.generateConnectionCode(
    "user123", 
    "Alice", 
    30
);

// 使用连接码连接
PeerConnection connection = p2pService.connectWithCode(
    code.getCode(),
    "user456",
    "Bob"
);

// 共享知识
SharedKnowledge knowledge = p2pService.shareKnowledge(
    connection.getConnectionId(),
    SharedKnowledge.builder()
        .sourceUserId("user123")
        .sourceUserName("Alice")
        .encryptedContent("encrypted knowledge data")
        .knowledgeType(KnowledgeType.DOCUMENT)
        .build()
);

// 接收知识
List<SharedKnowledge> receivedKnowledge = p2pService.receiveKnowledge(
    connection.getConnectionId()
);
```

## 📦 项目结构

```
omni-agent-p2p-starter/
├── src/main/java/
│   └── top/yumbo/ai/omni/p2p/starter/
│       ├── config/
│       │   ├── P2PAutoConfiguration.java    # 统一自动配置
│       │   └── P2PProperties.java           # 配置属性
│       ├── memory/                          # Memory 实现
│       │   ├── MemoryP2PCollaborationService.java
│       │   └── MemoryP2PDataTransferService.java
│       ├── h2/                              # H2 实现
│       ├── sqlite/                          # SQLite 实现
│       ├── redis/                           # Redis 实现
│       ├── mongodb/                         # MongoDB 实现
│       └── elasticsearch/                   # Elasticsearch 实现
└── src/main/resources/
    └── META-INF/
        └── spring.factories                 # Spring Boot 自动配置
```

## 🎯 设计优势

### 1. 统一管理

- **单一依赖**：只需引入一个 starter
- **统一配置**：通过 `storage-type` 切换实现
- **简化维护**：代码集中管理

### 2. 按需加载

- **可选依赖**：各数据源依赖都是 optional
- **条件注册**：根据配置自动选择实现
- **零侵入**：不使用的实现不会被加载

### 3. 易于扩展

- **包隔离**：每种实现独立包
- **接口统一**：都实现相同的 API
- **新增简单**：添加新实现只需新建包

## 🔧 高级配置

### 自动清理

```yaml
omni-agent:
  p2p:
    redis:
      ttl: 86400  # 24 小时后自动清理
```

### 性能优化

```yaml
omni-agent:
  p2p:
    elasticsearch:
      index-prefix: "p2p-"
      # 使用 Elasticsearch 的分片和副本配置
```

### 安全配置

```yaml
omni-agent:
  p2p:
    # 启用加密
    encryption:
      enabled: true
      algorithm: AES-256-GCM
```

## 📊 迁移指南

### 从旧的单独 starter 迁移

**之前（多个 starter）**：
```xml
<!-- 需要根据环境选择不同的 starter -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-p2p-starter-redis</artifactId>
</dependency>
```

**现在（统一 starter）**：
```xml
<!-- 只需一个 starter -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-p2p-starter</artifactId>
</dependency>

<!-- 可选：添加需要的数据源依赖 -->
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-redis</artifactId>
</dependency>
```

**配置变更**：
```yaml
# 之前：通过不同的 starter 隐式指定
# 现在：通过配置显式指定
omni-agent:
  p2p:
    storage-type: redis  # 明确指定使用 Redis
```

## 🐛 故障排查

### 问题 1：找不到实现

**错误**：`No qualifying bean of type 'P2PCollaborationService'`

**解决**：
1. 检查 `storage-type` 配置是否正确
2. 确认对应的数据源依赖已添加
3. 检查数据源配置是否正确

### 问题 2：多个实现冲突

**错误**：`Expected single matching bean but found 2`

**解决**：
- 明确指定 `storage-type`，避免多个实现同时生效

### 问题 3：连接失败

**检查**：
1. 数据源服务是否启动（Redis、MongoDB 等）
2. 连接配置是否正确
3. 网络是否畅通

## 📞 技术支持

如有问题，请联系 OmniAgent 团队或提交 Issue。

---

**版本**: 1.0.0  
**作者**: OmniAgent Team  
**更新日期**: 2025-01-28

