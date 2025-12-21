# 🔧 持久化配置切换指南

**文档版本**: v1.0  
**更新时间**: 2025-12-19

---

## 📋 概述

OmniAgent 支持**四维可插拔架构**，每个维度都支持多种后端实现。您只需修改 `application.yml` 中的 `type` 字段即可切换。

---

## 🎯 四个可插拔维度

### 1. 持久化 (Persistence)
存储应用数据（用户、配置、历史等）

### 2. 文档存储 (Document Storage)
存储原始文档、分块、图片等

### 3. RAG (检索增强生成)
向量索引和检索

### 4. AI (人工智能)
LLM 推理服务

---

## 📦 1. 持久化配置 (Persistence)

### 支持的后端

| 后端 | 适用场景 | 优点 | 缺点 |
|------|---------|------|------|
| **file** | 快速原型 | 零依赖、最简单 | 性能低、不支持复杂查询 |
| **sqlite** | 单机部署 | 轻量、零配置 | 不支持分布式 |
| **h2** | 开发测试 | 快速、内存模式 | 功能有限 |
| **memory** | 快速测试 | 最快速度 | 重启丢失 |
| **redis** | 生产环境 | 高性能、分布式 | 需要额外服务 |
| **mongodb** | 大规模 | 水平扩展 | 配置复杂 |
| **elasticsearch** | 全文搜索 | 强大搜索 | 资源消耗大 |

### 配置示例

#### File（最简单、零依赖）

```yaml
omni-agent:
  persistence:
    type: file
    file:
      base-path: ./data/persistence
```

**何时使用**:
- ✅ 快速原型开发
- ✅ 学习和测试
- ✅ 数据量 < 1000 条记录
- ✅ 完全零依赖需求

**特点**:
- ✅ **零依赖**: 无需任何数据库
- ✅ **最简单**: 数据存储为 JSON 文件
- ✅ **易调试**: 可以直接查看文件内容
- ⚠️ **性能低**: 不适合大量数据
- ⚠️ **功能弱**: 不支持复杂查询

**数据存储结构**:
```
./data/persistence/
├── questions.json          # 问题数据
├── classifiers.json        # 分类器数据
└── metadata.json           # 元数据
```

**启动命令**:
```bash
# 无需额外服务，直接启动
mvn spring-boot:run
```

---

#### SQLite（推荐：单机部署）✅

```yaml
omni-agent:
  persistence:
    type: sqlite
    sqlite:
      db-path: ./data/omni-agent.db
      auto-create-tables: true
      show-sql: false
      connection-timeout: 30000
```

**何时使用**:
- ✅ 单机部署
- ✅ 数据量 < 10GB
- ✅ 无需分布式
- ✅ 零配置需求

**启动命令**:
```bash
# 无需额外服务，直接启动
mvn spring-boot:run
```

---

#### H2（开发测试）

```yaml
omni-agent:
  persistence:
    type: h2
    h2:
      db-path: ./data/omni-agent-h2
      mode: file              # file 或 memory
      auto-create-tables: true
      show-sql: false
```

**内存模式**（最快）:
```yaml
omni-agent:
  persistence:
    type: h2
    h2:
      mode: memory
      db-name: testdb
      auto-create-tables: true
```

**何时使用**:
- ✅ 本地开发
- ✅ 单元测试
- ✅ 快速原型

---

#### Memory（快速测试）

```yaml
omni-agent:
  persistence:
    type: memory
    memory:
      initial-capacity: 1000
```

**何时使用**:
- ✅ 性能测试
- ✅ 临时验证
- ⚠️ **数据会丢失**

---

#### Redis（生产环境推荐）⭐

```yaml
omni-agent:
  persistence:
    type: redis
    redis:
      host: localhost
      port: 6379
      password: your-password
      database: 0
      timeout: 3000
      pool:
        max-active: 8
        max-idle: 8
        min-idle: 0
        max-wait: -1
```

**何时使用**:
- ✅ 生产环境
- ✅ 高并发
- ✅ 分布式部署
- ✅ 需要缓存加速

**启动 Redis**:
```bash
# Docker 方式
docker run -d --name redis -p 6379:6379 redis:7

# 或使用密码
docker run -d --name redis \
  -p 6379:6379 \
  redis:7 redis-server --requirepass your-password
```

---

#### MongoDB（大规模）

```yaml
omni-agent:
  persistence:
    type: mongodb
    mongodb:
      uri: mongodb://localhost:27017
      database: omni-agent
```

**集群配置**:
```yaml
omni-agent:
  persistence:
    type: mongodb
    mongodb:
      uri: mongodb://user:password@host1:27017,host2:27017,host3:27017/omni-agent?replicaSet=rs0
      database: omni-agent
```

**何时使用**:
- ✅ 数据量 > 100GB
- ✅ 需要水平扩展
- ✅ 复杂查询
- ✅ 文档型数据

**启动 MongoDB**:
```bash
docker run -d --name mongodb \
  -p 27017:27017 \
  -e MONGO_INITDB_ROOT_USERNAME=admin \
  -e MONGO_INITDB_ROOT_PASSWORD=password \
  mongo:7
```

---

#### Elasticsearch（全文搜索）

```yaml
omni-agent:
  persistence:
    type: elasticsearch
    elasticsearch:
      hosts:
        - localhost:9200
      username: elastic
      password: changeme
      connection-timeout: 5000
      socket-timeout: 60000
      index-prefix: omni-agent
```

**何时使用**:
- ✅ 需要全文搜索
- ✅ 复杂聚合查询
- ✅ 大规模日志分析
- ✅ 实时分析

**启动 Elasticsearch**:
```bash
docker run -d --name elasticsearch \
  -p 9200:9200 \
  -e "discovery.type=single-node" \
  -e "xpack.security.enabled=false" \
  elasticsearch:8.11.0
```

---

## 📦 2. 文档存储配置 (Document Storage)

### 支持的后端

| 后端 | 适用场景 | 优点 | 缺点 |
|------|---------|------|------|
| **file** | 单机部署 | 简单、直接 | 不支持分布式 |
| **minio** | 生产环境 | 对象存储、可扩展 | 需要额外服务 |
| **s3** | 云部署 | AWS生态 | 有成本 |
| **redis** | 缓存式 | 高性能 | 内存限制 |
| **mongodb** | 大规模 | GridFS支持 | 配置复杂 |

### 配置示例

#### File（单机推荐）✅

```yaml
omni-agent:
  document-storage:
    type: file
    file:
      base-path: ./data/documents
      chunk-path: ./data/chunks
      image-path: ./data/images
      ppl-path: ./data/ppl
      max-file-size: 104857600  # 100MB
```

---

#### MinIO（生产推荐）⭐

```yaml
omni-agent:
  document-storage:
    type: minio
    minio:
      endpoint: http://localhost:9000
      access-key: minioadmin
      secret-key: minioadmin
      bucket: omni-agent
      auto-create-bucket: true
```

**启动 MinIO**:
```bash
docker run -d --name minio \
  -p 9000:9000 \
  -p 9001:9001 \
  -e "MINIO_ROOT_USER=minioadmin" \
  -e "MINIO_ROOT_PASSWORD=minioadmin" \
  minio/minio server /data --console-address ":9001"
```

访问 MinIO 控制台: http://localhost:9001

---

#### AWS S3（云部署）

```yaml
omni-agent:
  document-storage:
    type: s3
    s3:
      region: us-east-1
      access-key: AKIAIOSFODNN7EXAMPLE
      secret-key: wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
      bucket: omni-agent
```

---

## 📦 3. RAG 配置

### 支持的后端

| 后端 | 适用场景 | 优点 | 缺点 |
|------|---------|------|------|
| **file** | 单机部署 | Lucene高性能 | 不支持分布式 |
| **h2/sqlite** | 开发测试 | 轻量 | 向量搜索简单 |
| **redis** | 生产环境 | RediSearch强大 | 需要插件 |
| **mongodb** | 大规模 | Atlas Vector Search | 需要云服务 |
| **elasticsearch** | 企业级 | 最强搜索 | 资源消耗大 |

### 配置示例

#### File（Lucene）✅

```yaml
omni-agent:
  rag:
    type: file
    file:
      index-path: ./data/rag-index
      analyzer: smartcn         # smartcn(中文) 或 standard(英文)
      max-results: 100
      similarity-threshold: 0.7
```

---

#### Redis（高性能）⭐

```yaml
omni-agent:
  rag:
    type: redis
    redis:
      host: localhost
      port: 6379
      database: 2
      key-prefix: rag:
      vector-dimension: 768
```

**需要 RediSearch 模块**:
```bash
docker run -d --name redis-stack \
  -p 6379:6379 \
  redis/redis-stack:latest
```

---

#### Elasticsearch（企业级）

```yaml
omni-agent:
  rag:
    type: elasticsearch
    elasticsearch:
      hosts:
        - localhost:9200
      index-name: omni-agent-rag
      vector-dimension: 768
      similarity: cosine
      shard-count: 1
      replica-count: 0
```

---

## 📦 4. AI 配置

### 支持的后端

| 后端 | 适用场景 | 优点 | 缺点 |
|------|---------|------|------|
| **ollama** | 本地开发 | 隐私、免费 | 性能受限 |
| **online-api** | 生产环境 | 强大、稳定 | 有成本 |

### 配置示例

#### Ollama（本地）

```yaml
omni-agent:
  ai:
    type: ollama
    ollama:
      base-url: http://localhost:11434
      model: qwen2.5:latest
      temperature: 0.7
      max-tokens: 2000
      timeout: 30000
```

**启动 Ollama**:
```bash
# 安装 Ollama
curl -fsSL https://ollama.com/install.sh | sh

# 下载模型
ollama pull qwen2.5:latest

# 或 GPU 加速版本
ollama pull qwen2.5:7b
```

---

#### Online API（生产推荐）⭐

**千问（推荐）**:
```yaml
omni-agent:
  ai:
    type: online-api
    online:
      provider: qianwen
      base-url: https://dashscope.aliyuncs.com/compatible-mode/v1
      api-key: ${QW_API_KEY}
      default-model: qwen-plus
      temperature: 0.7
      max-tokens: 2048
```

**DeepSeek（高性价比）**:
```yaml
omni-agent:
  ai:
    type: online-api
    online:
      provider: deepseek
      endpoint: https://api.deepseek.com/v1/chat/completions
      api-key: ${AI_API_KEY}
      default-model: deepseek-chat
```

**OpenAI**:
```yaml
omni-agent:
  ai:
    type: online-api
    online:
      provider: openai
      base-url: https://api.openai.com/v1
      api-key: ${OPENAI_API_KEY}
      default-model: gpt-4-turbo
```

---

## 🔄 切换配置的步骤

### 步骤 1: 修改 application.yml

```yaml
omni-agent:
  persistence:
    type: redis  # 从 sqlite 改为 redis
```

### 步骤 2: 启动依赖服务

```bash
# 启动 Redis
docker run -d --name redis -p 6379:6379 redis:7
```

### 步骤 3: 重启应用

```bash
mvn spring-boot:run
```

### 步骤 4: 验证

查看日志，应该看到：
```
[INFO] Persistence backend: redis
[INFO] Connected to Redis: localhost:6379
```

---

## 📊 组合推荐

### 快速原型（最小配置）

```yaml
omni-agent:
  persistence:
    type: file
  document-storage:
    type: file
  rag:
    type: file
  ai:
    type: ollama
```

**优点**: 
- ✅ 完全零依赖
- ✅ 30秒内启动
- ✅ 适合学习和演示

**缺点**:
- ⚠️ 性能较低
- ⚠️ 不适合生产环境

---

### 开发环境（推荐）

```yaml
omni-agent:
  persistence:
    type: sqlite
  document-storage:
    type: file
  rag:
    type: file
  ai:
    type: ollama
```

**优点**: 零配置、快速启动

---

### 生产环境（小规模）

```yaml
omni-agent:
  persistence:
    type: sqlite
  document-storage:
    type: file
  rag:
    type: file
  ai:
    type: ollama
```

**优点**: 零配置、快速启动

---

### 生产环境（小规模）

```yaml
omni-agent:
  persistence:
    type: sqlite
  document-storage:
    type: minio
  rag:
    type: file
  ai:
    type: online-api
```

**优点**: 平衡性能和成本

---

### 生产环境（大规模）⭐

```yaml
omni-agent:
  persistence:
    type: redis
  document-storage:
    type: minio
  rag:
    type: elasticsearch
  ai:
    type: online-api
```

**优点**: 高性能、可扩展

---

## 🛠️ Docker Compose 一键启动

创建 `docker-compose.yml`:

```yaml
version: '3.8'

services:
  redis:
    image: redis:7
    ports:
      - "6379:6379"
    volumes:
      - redis-data:/data

  minio:
    image: minio/minio
    ports:
      - "9000:9000"
      - "9001:9001"
    environment:
      MINIO_ROOT_USER: minioadmin
      MINIO_ROOT_PASSWORD: minioadmin
    command: server /data --console-address ":9001"
    volumes:
      - minio-data:/data

  elasticsearch:
    image: elasticsearch:8.11.0
    ports:
      - "9200:9200"
    environment:
      - discovery.type=single-node
      - xpack.security.enabled=false
    volumes:
      - es-data:/usr/share/elasticsearch/data

volumes:
  redis-data:
  minio-data:
  es-data:
```

**启动所有服务**:
```bash
docker-compose up -d
```

---

## ✅ 验证清单

- [ ] 修改 `application.yml` 的 `type` 字段
- [ ] 启动依赖服务（如 Redis、MinIO）
- [ ] 配置连接参数（host、port、密码等）
- [ ] 重启应用
- [ ] 查看启动日志确认连接成功
- [ ] 测试基本功能

---

## 🎉 总结

OmniAgent 的四维可插拔架构让您可以：

1. ✅ **灵活选择**: 根据场景选择最合适的后端
2. ✅ **平滑切换**: 只需修改配置即可切换
3. ✅ **零侵入**: 业务代码无需修改
4. ✅ **逐步升级**: 从单机到分布式平滑过渡

**从开发到生产，一套代码，多种部署方式！** 🚀

