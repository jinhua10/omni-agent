# 🔧 工作流存储配置示例

## 📋 自动检测模式（推荐）⭐

系统会根据项目中的依赖自动选择最合适的存储方式。

### 配置

```yaml
omni-agent:
  workflow:
    storage-type: auto  # 自动检测（默认）
```

### 检测顺序

1. **MongoDB** - 如果检测到 `spring-data-mongodb`
2. **Elasticsearch** - 如果检测到 `elasticsearch-java`
3. **SQLite** - 如果检测到 `sqlite-jdbc`
4. **File** - 默认回退（无需额外依赖）

---

## 💾 SQLite 存储（单机部署）

### 适用场景
- 单机部署
- 中小规模应用
- 快速开发和测试

### Maven 依赖

```xml
<dependency>
    <groupId>org.xerial</groupId>
    <artifactId>sqlite-jdbc</artifactId>
</dependency>
```

### 配置

```yaml
omni-agent:
  workflow:
    storage-type: sqlite
    
    sqlite:
      # 数据库文件路径
      db-path: ./data/workflows/workflows.db
      
      # 连接池配置（可选）
      pool:
        max-size: 10
        min-idle: 2
```

### 特点
- ✅ 轻量级，无需独立服务
- ✅ 零配置，开箱即用
- ✅ 性能优秀（单机场景）
- ⚠️ 不支持分布式

---

## 🍃 MongoDB 存储（分布式部署）

### 适用场景
- 分布式部署
- 大规模应用
- 高并发场景

### Maven 依赖

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-mongodb</artifactId>
</dependency>
```

### 配置

```yaml
omni-agent:
  workflow:
    storage-type: mongodb
    
    mongodb:
      # 连接 URI
      uri: mongodb://localhost:27017
      # 或集群模式
      # uri: mongodb://user:password@host1:27017,host2:27017,host3:27017/omniagent?replicaSet=rs0
      
      # 数据库名称
      database: omniagent
      
      # 集合名称
      collection: workflows
      
      # 连接配置（可选）
      connection:
        connect-timeout: 10000
        socket-timeout: 5000
        max-pool-size: 100
        min-pool-size: 10
```

### 特点
- ✅ 分布式，高可用
- ✅ 横向扩展
- ✅ 文档模型，灵活
- ⚠️ 需要独立服务

---

## 🔍 Elasticsearch 存储（搜索优化）

### 适用场景
- 需要强大的全文搜索
- 复杂查询场景
- 大数据量分析

### Maven 依赖

```xml
<dependency>
    <groupId>co.elastic.clients</groupId>
    <artifactId>elasticsearch-java</artifactId>
</dependency>
```

### 配置

```yaml
omni-agent:
  workflow:
    storage-type: elasticsearch
    
    elasticsearch:
      # 节点地址（支持多个）
      uris: 
        - http://localhost:9200
        - http://localhost:9201
      # 或单个
      # uris: http://localhost:9200
      
      # 索引名称
      index: market-workflows
      
      # 索引配置（可选）
      settings:
        number-of-shards: 3
        number-of-replicas: 1
        refresh-interval: 5s
      
      # 认证（可选）
      auth:
        username: elastic
        password: changeme
```

### 特点
- ✅ 强大的全文搜索
- ✅ 实时分析
- ✅ 高性能查询
- ⚠️ 资源消耗较高

---

## 📄 File 存储（开发测试）

### 适用场景
- 本地开发
- 快速测试
- 小规模应用

### Maven 依赖

无需额外依赖（Jackson YAML 已包含）

### 配置

```yaml
omni-agent:
  workflow:
    storage-type: file
    
    file:
      # 工作流定义目录
      definitions-dir: ./data/workflows/definitions
      
      # 版本归档目录
      versions-dir: ./data/workflows/versions
```

### 特点
- ✅ 零依赖
- ✅ 易于查看和编辑
- ✅ 版本控制友好
- ⚠️ 不支持高并发
- ⚠️ 查询性能有限

---

## 🎛️ 完整配置示例

### 开发环境

```yaml
omni-agent:
  workflow:
    storage-type: auto  # 自动检测
    
    market:
      enabled: true
      page-size: 10  # 小一点，方便测试
```

### 生产环境（单机）

```yaml
omni-agent:
  workflow:
    storage-type: sqlite
    
    sqlite:
      db-path: /data/omniagent/workflows.db
      pool:
        max-size: 20
        min-idle: 5
    
    market:
      enabled: true
      page-size: 20
      max-file-size: 10485760  # 10MB
      cache:
        enabled: true
        ttl: 600  # 10分钟
```

### 生产环境（分布式）

```yaml
omni-agent:
  workflow:
    storage-type: mongodb
    
    mongodb:
      uri: mongodb://workflow-user:password@mongo1:27017,mongo2:27017,mongo3:27017/omniagent?replicaSet=rs0
      database: omniagent
      collection: workflows
      connection:
        connect-timeout: 10000
        socket-timeout: 5000
        max-pool-size: 100
    
    market:
      enabled: true
      page-size: 20
      cache:
        enabled: true
        ttl: 300
```

### 生产环境（搜索优化）

```yaml
omni-agent:
  workflow:
    storage-type: elasticsearch
    
    elasticsearch:
      uris: 
        - https://es1.example.com:9200
        - https://es2.example.com:9200
      index: market-workflows
      auth:
        username: ${ES_USERNAME}
        password: ${ES_PASSWORD}
      settings:
        number-of-shards: 5
        number-of-replicas: 2
    
    market:
      enabled: true
      page-size: 50  # ES 查询快，可以大一点
```

---

## 🔄 存储迁移

### 从 File 迁移到 SQLite

```bash
# 1. 添加 SQLite 依赖到 pom.xml
# 2. 修改配置
omni-agent:
  workflow:
    storage-type: sqlite

# 3. 重启应用，自动创建数据库表
# 4. 使用迁移工具导入 YAML 文件（TODO）
```

### 从 SQLite 迁移到 MongoDB

```bash
# 1. 安装 MongoDB
# 2. 添加 MongoDB 依赖到 pom.xml
# 3. 修改配置
omni-agent:
  workflow:
    storage-type: mongodb
    mongodb:
      uri: mongodb://localhost:27017
      database: omniagent

# 4. 使用迁移工具导入数据（TODO）
```

---

## 📊 存储对比

| 特性 | File | SQLite | MongoDB | Elasticsearch |
|------|------|--------|---------|---------------|
| **部署复杂度** | ⭐ 最简单 | ⭐⭐ 简单 | ⭐⭐⭐ 中等 | ⭐⭐⭐⭐ 复杂 |
| **性能（单机）** | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ |
| **性能（分布式）** | ❌ 不支持 | ❌ 不支持 | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **搜索能力** | ⭐ | ⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **并发支持** | ⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **可视化管理** | ⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **资源消耗** | ⭐ 最小 | ⭐⭐ 小 | ⭐⭐⭐ 中等 | ⭐⭐⭐⭐ 大 |

---

## 🎯 选择建议

### 场景 1：个人开发/学习
**推荐**：File 或 SQLite (auto)
```yaml
omni-agent:
  workflow:
    storage-type: auto
```

### 场景 2：小团队/中小项目
**推荐**：SQLite
```yaml
omni-agent:
  workflow:
    storage-type: sqlite
```

### 场景 3：企业级/大规模部署
**推荐**：MongoDB
```yaml
omni-agent:
  workflow:
    storage-type: mongodb
```

### 场景 4：需要强大搜索
**推荐**：Elasticsearch
```yaml
omni-agent:
  workflow:
    storage-type: elasticsearch
```

---

## 🔧 故障排查

### SQLite 相关

**问题**：数据库文件被锁定
```
Solution: 检查是否有其他进程在使用数据库文件
```

**问题**：数据库文件不存在
```
Solution: 确保目录存在，应用会自动创建数据库文件
mkdir -p ./data/workflows
```

### MongoDB 相关

**问题**：连接超时
```yaml
# 增加超时时间
mongodb:
  connection:
    connect-timeout: 30000
```

**问题**：认证失败
```
Solution: 检查用户名、密码和权限
```

### Elasticsearch 相关

**问题**：索引创建失败
```
Solution: 检查用户是否有创建索引的权限
```

**问题**：连接被拒绝
```
Solution: 检查 ES 节点是否启动，防火墙是否开放
```

---

## 📝 环境变量支持

可以使用环境变量覆盖配置：

```yaml
omni-agent:
  workflow:
    storage-type: ${WORKFLOW_STORAGE_TYPE:auto}
    
    sqlite:
      db-path: ${WORKFLOW_DB_PATH:./data/workflows/workflows.db}
    
    mongodb:
      uri: ${MONGODB_URI:mongodb://localhost:27017}
      database: ${MONGODB_DATABASE:omniagent}
    
    elasticsearch:
      uris: ${ES_URIS:http://localhost:9200}
      auth:
        username: ${ES_USERNAME:}
        password: ${ES_PASSWORD:}
```

---

## 🎉 总结

- **开发测试**：使用 `auto` 或 `file`
- **单机部署**：使用 `sqlite`
- **分布式**：使用 `mongodb`
- **搜索优化**：使用 `elasticsearch`

**推荐配置**：使用 `storage-type: auto`，让系统自动选择！🚀

