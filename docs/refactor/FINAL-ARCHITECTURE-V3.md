# 🎯 OmniAgent 四维可插拔架构最终方案

> **版本**: v3.0 Final  
> **日期**: 2025-12-14  
> **核心**: 四个维度完全可插拔

---

## 🌟 架构全景图

```
                        用户应用
                           │
           ┌───────────────┼───────────────┐
           │               │               │
    ┌──────▼──────┐ ┌─────▼─────┐ ┌──────▼──────┐
    │ Persistence │ │  Document  │ │    RAG      │
    │   (持久化)  │ │  Storage   │ │  (检索)     │
    │             │ │ (文档存储) │ │             │
    └──────┬──────┘ └─────┬─────┘ └──────┬──────┘
           │               │               │
           │               │               │
    ┌──────▼───────────────▼───────────────▼──────┐
    │         omni-agent-core (核心业务)          │
    │                                              │
    │  HOPE • Chunking • Image • Role • Evolution │
    └──────────────────────────────────────────────┘
```

---

## 🎯 四大可插拔维度

### 1️⃣ 持久化层 (Persistence)
**用途**: 存储结构化业务数据  
**数据类型**: HOPE 知识、问题分类、配置、用户数据等

| Starter | 适用场景 | 特点 |
|---------|----------|------|
| memory | 开发/测试 | 快速、不持久 |
| h2 | 测试/单机 | 嵌入式、SQL |
| sqlite | 单机部署 | 轻量级、文件 |
| redis | 高性能 | 内存、分布式 |
| mongodb | 文档数据 | NoSQL、灵活 |
| elasticsearch | 生产/搜索 | 分布式、高可用 |

**配置示例**:
```yaml
omni-agent:
  persistence:
    type: elasticsearch
    elasticsearch:
      host: localhost:9200
```

---

### 2️⃣ 文档存储层 (Document Storage) ⭐ 关键
**用途**: 存储文档分块、图像、大文件  
**数据类型**: 文档 Chunk、提取的图像、PPL 分析数据

| Starter | 适用场景 | 特点 |
|---------|----------|------|
| file | 开发/小型 | 本地文件、简单 |
| mongodb | 中大型 | GridFS、适合文档 |
| s3 | 云部署 | AWS S3、高可靠 |
| minio | 私有云 | 兼容 S3、自托管 |
| redis | 缓存层 | 快速访问 |
| elasticsearch | 混合 | 存储+搜索 |

**配置示例**:
```yaml
omni-agent:
  document-storage:
    type: mongodb
    mongodb:
      uri: mongodb://localhost:27017
      database: omni-storage
```

**核心接口**:
```java
public interface DocumentStorageService {
    // 分块
    String saveChunk(String documentId, Chunk chunk);
    List<Chunk> getChunksByDocument(String documentId);
    
    // 图像
    String saveImage(String documentId, Image image);
    Optional<Image> getImage(String imageId);
    
    // PPL 数据
    String savePPLData(String documentId, PPLData data);
    Optional<PPLData> getPPLData(String documentId);
}
```

---

### 3️⃣ RAG 检索层 (RAG)
**用途**: 文档索引和向量检索  
**数据类型**: 倒排索引、向量嵌入、全文索引

| Starter | 适用场景 | 特点 |
|---------|----------|------|
| file | 开发/本地 | Lucene、无依赖 |
| h2 | 测试 | 嵌入式、SQL |
| sqlite | 单机 | 轻量级 |
| redis | 高性能 | RediSearch、向量 |
| mongodb | 中型 | Atlas Search |
| elasticsearch | 生产 | 企业级、分布式 |

**配置示例**:
```yaml
omni-agent:
  rag:
    type: elasticsearch
    elasticsearch:
      host: localhost:9200
      index-name: documents
```

---

### 4️⃣ AI 引擎层 (AI)
**用途**: LLM 推理和 Embedding 生成  
**数据类型**: AI 请求、嵌入向量

| Starter | 适用场景 | 特点 |
|---------|----------|------|
| local-ollama | 开发/私有 | 本地部署、离线 |
| remote-ollama | 团队共享 | 远程 Ollama |
| online-api | 生产 | OpenAI/Claude 等 |

---

## 📦 完整模块列表

### API 模块 (4个)
```
1. omni-agent-persistence-api         (持久化接口)
2. omni-agent-document-storage-api    (文档存储接口) ⭐
3. omni-agent-rag-api                 (RAG接口)
4. omni-agent-ai-api                  (AI接口)
```

### Starter 模块 (21个)
```
Persistence (6个):
  - memory, h2, sqlite, redis, mongodb, elasticsearch

Document Storage (6个): ⭐
  - file, mongodb, s3, minio, redis, elasticsearch

RAG (6个):
  - file, h2, sqlite, redis, mongodb, elasticsearch

AI (3个):
  - local-ollama, remote-ollama, online-api
```

### 核心模块 (1个)
```
omni-agent-core (核心业务逻辑)
```

**总计**: 32 个模块

---

## 🎬 使用场景演示

### 场景 1: 个人开发者（本地开发）
```xml
<dependencies>
    <dependency>
        <artifactId>omni-agent-persistence-starter-memory</artifactId>
    </dependency>
    <dependency>
        <artifactId>omni-agent-document-storage-starter-file</artifactId>
    </dependency>
    <dependency>
        <artifactId>omni-agent-rag-starter-file</artifactId>
    </dependency>
    <dependency>
        <artifactId>omni-agent-ai-starter-local-ollama</artifactId>
    </dependency>
</dependencies>
```

**特点**: 
- ✅ 无需外部服务
- ✅ 快速启动
- ✅ 数据本地

---

### 场景 2: 小型团队（混合部署）
```xml
<dependencies>
    <dependency>
        <artifactId>omni-agent-persistence-starter-h2</artifactId>
    </dependency>
    <dependency>
        <artifactId>omni-agent-document-storage-starter-mongodb</artifactId>
    </dependency>
    <dependency>
        <artifactId>omni-agent-rag-starter-elasticsearch</artifactId>
    </dependency>
    <dependency>
        <artifactId>omni-agent-ai-starter-remote-ollama</artifactId>
    </dependency>
</dependencies>
```

**特点**:
- ✅ 结构化数据用 H2
- ✅ 文档/图像用 MongoDB
- ✅ 检索用 ES
- ✅ AI 共享使用

---

### 场景 3: 企业生产（云原生）
```xml
<dependencies>
    <dependency>
        <artifactId>omni-agent-persistence-starter-elasticsearch</artifactId>
    </dependency>
    <dependency>
        <artifactId>omni-agent-document-storage-starter-s3</artifactId>
    </dependency>
    <dependency>
        <artifactId>omni-agent-rag-starter-elasticsearch</artifactId>
    </dependency>
    <dependency>
        <artifactId>omni-agent-ai-starter-online-api</artifactId>
    </dependency>
</dependencies>
```

**配置**:
```yaml
omni-agent:
  persistence:
    type: elasticsearch
    elasticsearch:
      host: es-cluster.p2p.com:9200
      
  document-storage:
    type: s3
    s3:
      bucket-name: company-documents
      region: us-east-1
      
  rag:
    type: elasticsearch
    elasticsearch:
      host: es-cluster.p2p.com:9200
      
  ai:
    type: online-api
    online-api:
      provider: openai
      api-key: ${OPENAI_API_KEY}
```

**特点**:
- ✅ 高可用
- ✅ 可扩展
- ✅ 云托管
- ✅ 企业级

---

## 🔄 数据流向

### 文档上传流程
```
1. 用户上传 PDF
   │
   ├─> DocumentUtils (解析)
   │
   ├─> DocumentChunker (分块)
   │     └─> DocumentStorageService.saveChunk()
   │           ├─ File → 本地磁盘
   │           ├─ MongoDB → GridFS
   │           └─ S3 → 对象存储
   │
   ├─> ImageExtractor (提取图像)
   │     └─> DocumentStorageService.saveImage()
   │
   ├─> PPLService (PPL 分析)
   │     └─> DocumentStorageService.savePPLData()
   │
   └─> RAGService (索引)
         └─> 向量化 + 全文索引
```

### 查询检索流程
```
1. 用户查询
   │
   ├─> RAGService.search() (检索相关文档)
   │     └─ 返回 documentId + chunkIds
   │
   ├─> DocumentStorageService.getChunks() (获取内容)
   │     ├─ File → 读取本地文件
   │     ├─ MongoDB → 查询 GridFS
   │     └─ S3 → 下载对象
   │
   ├─> HOPEKnowledgeManager (知识增强)
   │     └─ PersistenceService (查询知识)
   │
   └─> AIService (生成回答)
```

---

## 💡 关键设计要点

### 1. 职责分离
```
Persistence      → 结构化数据（配置、知识库、元数据）
Document Storage → 非结构化数据（文本块、图像、大文件）
RAG              → 检索索引（向量、全文）
AI               → 智能推理（LLM、Embedding）
```

### 2. 独立可替换
每个维度可以**独立选择**，不影响其他维度：
- 可以 Persistence 用 ES，Document Storage 用 S3
- 可以 RAG 用 ES，Document Storage 用 MongoDB
- 任意组合都能工作

### 3. 编译时决定
通过 `pom.xml` 依赖决定实现，不是运行时切换：
```xml
<!-- 选择哪个 Starter，就用哪个实现 -->
<dependency>
    <artifactId>omni-agent-document-storage-starter-mongodb</artifactId>
</dependency>
```

### 4. Spring Boot 自动配置
```java
// 用户代码只依赖接口
@Autowired
private DocumentStorageService storageService;

// Spring Boot 根据 Starter 自动注入实现
// 可能是 FileDocumentStorage
// 可能是 MongoDocumentStorage
// 可能是 S3DocumentStorage
```

---

## 📊 对比各种方案

| 方案 | Persistence | Document Storage | RAG | AI | 总模块数 |
|------|-------------|------------------|-----|----|----|
| **最小** | Memory | File | File | Local | 4 Starters |
| **推荐** | H2/MongoDB | MongoDB | ES | Remote | 4 Starters |
| **企业** | ES | S3 | ES | Online | 4 Starters |

**灵活性**: 4^6 × 4^6 × 4^6 × 3 = 超过 7 万种组合！

---

## 🎯 实施优先级

### Phase 1: 核心 API (Week 1)
1. persistence-api ✅ (已有)
2. **document-storage-api** ⭐ 新增
3. rag-api
4. ai-api

### Phase 2: 默认 Starter (Week 2-3)
1. persistence-starter-memory
2. **document-storage-starter-file** ⭐ 优先
3. rag-starter-file
4. ai-starter-local-ollama

### Phase 3: 生产 Starter (Week 4-5)
1. persistence-starter-elasticsearch
2. **document-storage-starter-mongodb** ⭐
3. **document-storage-starter-s3** ⭐
4. rag-starter-elasticsearch

---

## ✅ 完成标准

- [ ] 4 个 API 模块定义清晰
- [ ] Core 完全解耦，只依赖接口
- [ ] 至少 2 个 Document Storage Starter 可用
- [ ] 任意组合都能正常工作
- [ ] 切换无需修改代码
- [ ] 文档完整

---

**架构版本**: v3.0 Final  
**完成日期**: 2025-12-14  
**状态**: ✅ 四维可插拔架构设计完成

---

> 🎯 **核心价值**: 四个维度，独立可插拔！  
> 🔧 **关键发现**: 文档存储不能硬编码！  
> 🚀 **最终目标**: 打造真正灵活的全场景 Agent 框架！

