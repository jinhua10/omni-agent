# Phase 1 实施方案改进说明

> 从 JPA 依赖改为通用存储抽象的设计说明

---

## 🎯 问题识别

**原始问题：**
```java
@Entity
@Table(name = "knowledge_domains")  // ❌ 强依赖 JPA
public class KnowledgeDomain {
    @Id
    @Column(nullable = false)
    private String domainId;
    // ...
}
```

**为什么这是个问题？**
- ❌ 违反了 OmniAgent 的**可插拔架构原则**
- ❌ 只能用关系型数据库（MySQL, PostgreSQL）
- ❌ 无法使用 File, MongoDB, Redis, Elasticsearch 等已有存储方案
- ❌ 与现有的 Persistence Starter 模式不一致

---

## ✅ 改进方案

### 1. 纯 POJO 实体

```java
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeDomain implements Serializable {
    // ✅ 无任何存储框架注解
    private String domainId;
    private String domainName;
    // ...
}
```

**优势：**
- ✅ 可以存储到任何后端
- ✅ 序列化友好（JSON, Binary, Protobuf）
- ✅ 测试简单（不需要数据库）

---

### 2. 存储接口抽象

```java
public interface KnowledgeDomainPersistence {
    String save(KnowledgeDomain domain);
    Optional<KnowledgeDomain> findById(String domainId);
    List<KnowledgeDomain> findByType(DomainType type);
    // ...
}
```

**多种实现：**
- `FileKnowledgeDomainPersistence` - JSON 文件
- `MongoKnowledgeDomainPersistence` - MongoDB
- `RedisKnowledgeDomainPersistence` - Redis
- `ElasticsearchKnowledgeDomainPersistence` - ES
- `SQLiteKnowledgeDomainPersistence` - SQLite
- `H2KnowledgeDomainPersistence` - H2

---

### 3. Spring Boot Starter 模式

```
omni-agent-knowledge-domain-starter-file/
├── FileKnowledgeDomainPersistence.java
├── FileKnowledgeDomainAutoConfiguration.java
└── spring.factories
```

**自动配置：**
```java
@Configuration
@ConditionalOnProperty(
    prefix = "omni-agent.knowledge-domain.storage",
    name = "type",
    havingValue = "file"
)
public class FileKnowledgeDomainAutoConfiguration {
    @Bean
    public KnowledgeDomainPersistence knowledgeDomainPersistence() {
        return new FileKnowledgeDomainPersistence();
    }
}
```

---

## 📊 架构对比

### 原方案（JPA）

```
┌─────────────────────────────┐
│   KnowledgeDomain (Entity)  │
│   @Entity @Table            │
└─────────────┬───────────────┘
              │
              ▼
┌─────────────────────────────┐
│   JpaRepository             │
└─────────────┬───────────────┘
              │
              ▼
┌─────────────────────────────┐
│   MySQL / PostgreSQL        │
│   (只能用关系型数据库)      │
└─────────────────────────────┘
```

### 新方案（可插拔）✅

```
┌─────────────────────────────┐
│   KnowledgeDomain (POJO)    │
│   无任何框架注解             │
└─────────────┬───────────────┘
              │
              ▼
┌─────────────────────────────┐
│ KnowledgeDomainPersistence  │
│   (统一接口)                 │
└─────────────┬───────────────┘
              │
      ┌───────┼───────┬───────┬───────┐
      ▼       ▼       ▼       ▼       ▼
   ┌────┐  ┌────┐  ┌────┐  ┌────┐  ┌────┐
   │File│  │Mongo│ │Redis│ │ ES │  │SQLite│
   └────┘  └────┘  └────┘  └────┘  └────┘
```

---

## 🔄 切换示例

### 场景 1: 开发环境（使用 File）

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-knowledge-domain-starter-file</artifactId>
</dependency>
```

```yaml
omni-agent:
  knowledge-domain:
    storage:
      type: file
      registry-path: data/knowledge-network/registry
```

### 场景 2: 生产环境（切换到 MongoDB）

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-knowledge-domain-starter-mongodb</artifactId>
</dependency>
```

```yaml
omni-agent:
  knowledge-domain:
    storage:
      type: mongodb

spring:
  data:
    mongodb:
      uri: mongodb://prod-server:27017/omni-agent
```

**✨ 零代码修改，只改配置！**

---

## 📈 与现有架构的一致性

OmniAgent 已有的可插拔维度：

| 维度 | Starter 数量 | 同样的模式 ✅ |
|-----|------------|-------------|
| Persistence | 6 | Memory, H2, SQLite, Redis, MongoDB, ES |
| Document Storage | 6 | File, MongoDB, Redis, ES, S3, MinIO |
| RAG | 6 | File, H2, SQLite, Redis, MongoDB, ES |
| AI | 2 | Ollama, Online-API |
| P2P | 6 | Memory, H2, SQLite, Redis, MongoDB, ES |
| Voting | 4 | Memory, Redis, MongoDB, ES |
| Behavior | 3 | Memory, Redis, MongoDB |
| **Knowledge Domain** | **6** | **File, MongoDB, Redis, ES, SQLite, H2** |

**完美对齐！** 🎯

---

## 🎁 额外好处

### 1. 测试更简单

```java
@Test
public void testKnowledgeDomain() {
    // 不需要启动数据库
    KnowledgeDomain domain = KnowledgeDomain.builder()
        .domainId("test")
        .domainName("Test Domain")
        .build();
    
    // 直接测试业务逻辑
    assertEquals("Test Domain", domain.getDomainName());
}
```

### 2. 序列化友好

```java
// JSON
String json = objectMapper.writeValueAsString(domain);

// Binary
byte[] bytes = SerializationUtils.serialize(domain);

// Protobuf
ByteString protobuf = domain.toProto();
```

### 3. 分布式友好

```java
// 可以通过 Redis 共享
redisTemplate.opsForValue().set("domain:" + id, domain);

// 可以通过消息队列传输
kafkaTemplate.send("domains", domain);
```

---

## 🚀 实施步骤

### Phase 1: File Starter（默认实现）

**优先级 P0** - 立即开始

1. ✅ 创建 `KnowledgeDomain` POJO
2. ✅ 创建 `KnowledgeDomainPersistence` 接口
3. ✅ 实现 `FileKnowledgeDomainPersistence`
4. ✅ 创建 `omni-agent-knowledge-domain-starter-file` 模块
5. ✅ 实现 Auto-Configuration
6. ✅ 编写测试

**预计时间：** 2天

### Phase 2: MongoDB & Redis Starter

**优先级 P1** - 生产环境需要

1. 实现 `MongoKnowledgeDomainPersistence`
2. 实现 `RedisKnowledgeDomainPersistence`
3. 创建对应的 Starter 模块

**预计时间：** 3-4天

### Phase 3: 其他 Starter（按需）

- Elasticsearch
- SQLite
- H2

---

## 💡 最佳实践

### 1. 配置优先级

```yaml
# 默认配置（开发环境）
omni-agent:
  knowledge-domain:
    storage:
      type: file

---
# 生产环境配置
spring:
  profiles: production

omni-agent:
  knowledge-domain:
    storage:
      type: mongodb
```

### 2. 数据迁移

如果需要在不同存储之间迁移：

```java
@Service
public class DomainMigrationService {
    
    public void migrate(
        KnowledgeDomainPersistence source,
        KnowledgeDomainPersistence target
    ) {
        List<KnowledgeDomain> domains = source.findAll();
        for (KnowledgeDomain domain : domains) {
            target.save(domain);
        }
    }
}
```

### 3. 混合存储

同时使用多个存储（高级场景）：

```java
// 主存储：MongoDB（持久化）
@Primary
@Bean
public KnowledgeDomainPersistence primaryPersistence() {
    return new MongoKnowledgeDomainPersistence();
}

// 缓存：Redis（加速读取）
@Bean
public KnowledgeDomainPersistence cachePersistence() {
    return new RedisKnowledgeDomainPersistence();
}
```

---

## ✅ 总结

**核心改进：**
1. ✅ 移除 JPA 依赖 → 通用 POJO
2. ✅ 统一存储接口 → 多种实现
3. ✅ Starter 模式 → 自动配置
4. ✅ 与现有架构一致 → 7+1 维可插拔

**优势：**
- 🎯 完全兼容 OmniAgent 架构
- 🔌 真正的可插拔存储
- 🚀 零代码切换后端
- 🧪 测试简单
- 📦 易于扩展

**下一步：**
开始实施 Phase 1 - 创建 File Starter！

---

**更新时间：** 2025-12-27  
**作者：** OmniAgent 架构组

