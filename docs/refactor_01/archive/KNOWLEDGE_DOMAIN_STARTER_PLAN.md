# 知识域存储 Starter 模块规划

> 按照 OmniAgent 的可插拔架构风格，为知识域持久化创建多种 Starter

---

## 🎯 设计目标

遵循 OmniAgent 现有的 Starter 模式：
- ✅ **零侵入切换**：只需修改 Maven 依赖
- ✅ **自动配置**：Spring Boot Auto-Configuration
- ✅ **统一接口**：`KnowledgeDomainPersistence` 接口

---

## 📦 Starter 模块列表

### 1. omni-agent-knowledge-domain-starter-file ✅
**默认实现，Phase 1 优先**

**存储方式**：JSON 文件
**依赖**：Jackson
**适用场景**：开发测试、单机部署

**目录结构：**
```
omni-agent-knowledge-domain-starter-file/
├── pom.xml
└── src/main/java/top/yumbo/ai/knowledge/domain/file/
    ├── FileKnowledgeDomainPersistence.java
    ├── FileKnowledgeDomainProperties.java
    └── FileKnowledgeDomainAutoConfiguration.java
```

**配置示例：**
```yaml
omni-agent:
  knowledge-domain:
    storage:
      type: file
      registry-path: data/knowledge-network/registry
```

---

### 2. omni-agent-knowledge-domain-starter-mongodb

**存储方式**：MongoDB Collection
**依赖**：Spring Data MongoDB
**适用场景**：生产环境、分布式部署

**实现要点：**
```java
@Document(collection = "knowledge_domains")
public class KnowledgeDomainDocument {
    @Id
    private String domainId;
    // ...其他字段
}

@Service
public class MongoKnowledgeDomainPersistence implements KnowledgeDomainPersistence {
    private final MongoTemplate mongoTemplate;
    // 实现接口方法
}
```

**配置示例：**
```yaml
omni-agent:
  knowledge-domain:
    storage:
      type: mongodb
spring:
  data:
    mongodb:
      uri: mongodb://localhost:27017/omni-agent
```

---

### 3. omni-agent-knowledge-domain-starter-redis

**存储方式**：Redis Hash + Set
**依赖**：Spring Data Redis
**适用场景**：高性能读写、缓存场景

**数据结构：**
```
# Hash 存储域详情
knowledge:domain:{domainId} -> {KnowledgeDomain JSON}

# Set 存储域列表
knowledge:domains:all -> {domainId1, domainId2, ...}
knowledge:domains:type:{DOCUMENT} -> {domainId1, ...}
```

**配置示例：**
```yaml
omni-agent:
  knowledge-domain:
    storage:
      type: redis
spring:
  redis:
    host: localhost
    port: 6379
```

---

### 4. omni-agent-knowledge-domain-starter-elasticsearch

**存储方式**：ES Index
**依赖**：Spring Data Elasticsearch
**适用场景**：需要全文搜索域元数据

**索引定义：**
```json
{
  "mappings": {
    "properties": {
      "domainId": { "type": "keyword" },
      "domainName": { "type": "text", "analyzer": "ik_smart" },
      "domainType": { "type": "keyword" },
      "description": { "type": "text", "analyzer": "ik_smart" },
      "status": { "type": "keyword" },
      "createdAt": { "type": "date" }
    }
  }
}
```

**配置示例：**
```yaml
omni-agent:
  knowledge-domain:
    storage:
      type: elasticsearch
      index-name: knowledge-domains
spring:
  elasticsearch:
    uris: http://localhost:9200
```

---

### 5. omni-agent-knowledge-domain-starter-sqlite

**存储方式**：SQLite 数据库
**依赖**：SQLite JDBC
**适用场景**：轻量级、嵌入式部署

**表结构：**
```sql
CREATE TABLE knowledge_domains (
    domain_id TEXT PRIMARY KEY,
    domain_name TEXT NOT NULL,
    domain_type TEXT NOT NULL,
    description TEXT,
    storage_path TEXT NOT NULL,
    rag_index_path TEXT NOT NULL,
    config_json TEXT,
    status TEXT NOT NULL,
    linked_entity_id TEXT,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL
);

CREATE INDEX idx_domain_type ON knowledge_domains(domain_type);
CREATE INDEX idx_status ON knowledge_domains(status);
```

---

### 6. omni-agent-knowledge-domain-starter-h2

**存储方式**：H2 内存/文件数据库
**依赖**：H2 Database
**适用场景**：开发测试、快速原型

---

## 🏗️ 统一的 Auto-Configuration 模式

每个 Starter 都遵循相同的配置模式：

```java
@Configuration
@ConditionalOnProperty(
    prefix = "omni-agent.knowledge-domain.storage",
    name = "type",
    havingValue = "file"  // 对应的类型
)
@EnableConfigurationProperties(FileKnowledgeDomainProperties.class)
public class FileKnowledgeDomainAutoConfiguration {
    
    @Bean
    @ConditionalOnMissingBean(KnowledgeDomainPersistence.class)
    public KnowledgeDomainPersistence knowledgeDomainPersistence(
        FileKnowledgeDomainProperties properties
    ) {
        return new FileKnowledgeDomainPersistence(
            properties.getRegistryPath()
        );
    }
}
```

---

## 📝 spring.factories

每个 Starter 都需要添加：

```properties
# src/main/resources/META-INF/spring.factories
org.springframework.boot.autoconfigure.EnableAutoConfiguration=\
  top.yumbo.ai.knowledge.domain.file.FileKnowledgeDomainAutoConfiguration
```

---

## 🔧 Phase 1 实施计划

### 优先级 P0: File Starter（立即实现）

**目标**：作为默认实现，无需额外依赖

**步骤：**
1. ✅ 创建 `omni-agent-knowledge-domain-starter-file` 模块
2. ✅ 实现 `FileKnowledgeDomainPersistence`
3. ✅ 实现 `FileKnowledgeDomainAutoConfiguration`
4. ✅ 添加配置属性类
5. ✅ 编写单元测试

### 优先级 P1: MongoDB Starter（Phase 2）

**理由**：最常用的生产环境存储

### 优先级 P2: Redis Starter（Phase 2）

**理由**：高性能场景

### 优先级 P3: 其他 Starter（Phase 3+）

根据实际需求逐步添加

---

## 🧪 测试计划

### 集成测试

每个 Starter 都需要通过相同的接口测试套件：

```java
@SpringBootTest
public abstract class KnowledgeDomainPersistenceTestBase {
    
    @Autowired
    protected KnowledgeDomainPersistence persistence;
    
    @Test
    public void testSaveAndFind() {
        // 测试保存和查找
    }
    
    @Test
    public void testUpdate() {
        // 测试更新
    }
    
    @Test
    public void testDelete() {
        // 测试删除
    }
    
    @Test
    public void testFindByType() {
        // 测试按类型查找
    }
    
    // 更多测试...
}

// 具体实现
@ActiveProfiles("file")
public class FileKnowledgeDomainPersistenceTest 
    extends KnowledgeDomainPersistenceTestBase {
    // 继承所有测试
}
```

---

## 📊 切换示例

### 从 File 切换到 MongoDB

**1. 修改 pom.xml：**
```xml
<!-- 移除 -->
<!--
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-knowledge-domain-starter-file</artifactId>
</dependency>
-->

<!-- 添加 -->
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-knowledge-domain-starter-mongodb</artifactId>
</dependency>
```

**2. 修改 application.yml：**
```yaml
omni-agent:
  knowledge-domain:
    storage:
      type: mongodb  # 从 file 改为 mongodb

spring:
  data:
    mongodb:
      uri: mongodb://localhost:27017/omni-agent
```

**3. 重启应用 - 完成！**

---

## 🎁 优势

1. **完全兼容现有架构**：遵循 OmniAgent 的 Starter 模式
2. **零代码修改**：切换存储只需改配置
3. **可扩展**：易于添加新的存储后端
4. **测试友好**：每个实现都有统一的测试套件
5. **生产就绪**：支持各种部署场景

---

## 📅 实施时间线

| Starter | Phase | 预计时间 | 优先级 |
|---------|-------|---------|--------|
| File | Phase 1 | 2天 | P0 |
| MongoDB | Phase 2 | 3天 | P1 |
| Redis | Phase 2 | 3天 | P1 |
| Elasticsearch | Phase 3 | 4天 | P2 |
| SQLite | Phase 3 | 2天 | P2 |
| H2 | Phase 3 | 2天 | P2 |

---

**下一步：** 开始实现 `omni-agent-knowledge-domain-starter-file` 模块！

