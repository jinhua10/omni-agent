# OmniAgent 模块架构

> **版本：** 1.0.0  
> **更新时间：** 2026-01-01  
> **状态：** ✅ 生产就绪

---

## 📋 模块总览

OmniAgent 采用**模块化、可插拔**的设计，共 25 个功能模块，分为 6 大层次。

```
┌─────────────────────────────────────────────────────────────┐
│                    应用层 (Application)                      │
├─────────────────────────────────────────────────────────────┤
│ • omni-agent-example-basic      示例应用                     │
│ • omni-agent-example-production 生产环境示例                 │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                     Web层 (Web Layer)                        │
├─────────────────────────────────────────────────────────────┤
│ • omni-agent-web               RESTful API 服务              │
│ • omni-agent-workflow          工作流引擎                    │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│              核心智能层 (Intelligence Layer)                 │
├─────────────────────────────────────────────────────────────┤
│ • omni-agent-hope-api          HOPE 接口定义                 │
│ • omni-agent-hope-starter      HOPE 实现（问题分类、自学习）  │
│ • omni-agent-orchestrator      编排协调                      │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│               知识层 (Knowledge Layer)                       │
├─────────────────────────────────────────────────────────────┤
│ • omni-agent-knowledge-registry-api      知识注册表接口      │
│ • omni-agent-knowledge-registry-starter  知识网络实现        │
│ • omni-agent-rag-api                     RAG 接口定义        │
│ • omni-agent-rag-starter-adapter         RAG 适配器          │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                基础服务层 (Service Layer)                    │
├─────────────────────────────────────────────────────────────┤
│ • omni-agent-ai-api                AI 服务接口               │
│ • omni-agent-ai-starter            AI 服务实现               │
│ • omni-agent-chunking-api          分块接口                  │
│ • omni-agent-chunking-starter      分块实现（6种策略）        │
│ • omni-agent-document-processor-api 文档处理接口             │
│ • omni-agent-document-processor-starter 文档处理实现         │
│ • omni-agent-p2p-api               P2P 接口                  │
│ • omni-agent-p2p-starter           P2P 实现                  │
│ • omni-agent-marketplace           算法市场                  │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                 存储层 (Storage Layer)                       │
├─────────────────────────────────────────────────────────────┤
│ • omni-agent-document-storage-api        存储接口            │
│ • omni-agent-document-storage-starter    存储实现            │
│   支持: File/SQLite/H2/MongoDB/Redis/Elasticsearch          │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                  核心层 (Core Layer)                         │
├─────────────────────────────────────────────────────────────┤
│ • omni-agent-core    核心框架                                │
│ • omni-agent-common  通用工具类                              │
└─────────────────────────────────────────────────────────────┘
```

---

## 🎯 核心模块详解

### 1. HOPE 自学习系统

**模块：** `omni-agent-hope-api` + `omni-agent-hope-starter`

**核心功能：**
- ✅ 问题分类器（QuestionClassifier）
- ✅ 三层知识管理（Permanent/Ordinary/HighFrequency）
- ✅ 智能学习模块（自动优化层级）
- ✅ 统计和监控

**关键类：**
```java
// API 接口
top.yumbo.ai.omni.hope.api.QuestionClassifier
top.yumbo.ai.omni.hope.api.HopePersistence

// 实现类
top.yumbo.ai.omni.hope.starter.impl.HOPEKnowledgeManager
top.yumbo.ai.omni.hope.starter.impl.QuestionClassifier
```

**配置项：**
```yaml
omni-agent:
  hope:
    enabled: true
    persistence: knowledge-registry  # 或 in-memory
```

---

### 2. 知识注册表（Knowledge Registry）

**模块：** `omni-agent-knowledge-registry-api` + `omni-agent-knowledge-registry-starter`

**核心功能：**
- ✅ 域管理（Domain Management）
- ✅ 知识网络构建
- ✅ 知识关联和引用
- ✅ 用户偏好学习
- ✅ 跨域查询

**关键类：**
```java
// 域管理
top.yumbo.ai.omni.knowledge.registry.network.impl.KnowledgeDomainService
top.yumbo.ai.omni.knowledge.registry.router.DomainRouter

// 知识提取
top.yumbo.ai.omni.knowledge.registry.network.impl.DefaultKnowledgeExtractionService

// 用户偏好
top.yumbo.ai.omni.knowledge.registry.statistics.preference.UserPreferenceLearner
```

**配置项：**
```yaml
omni-agent:
  knowledge-registry:
    enabled: true
    cache-size: 1000
    cross-domain-query:
      enabled: true
      thread-pool-size: 10
```

---

### 3. RAG 适配器

**模块：** `omni-agent-rag-api` + `omni-agent-rag-starter-adapter`

**支持的 RAG 实现：**
- ✅ **File/Lucene**（默认）- 本地文件 + Lucene 索引
- ⚠️ **Elasticsearch** - 分布式搜索引擎
- ⚠️ **MongoDB** - 文档数据库
- ⚠️ **Redis** - 内存缓存

**关键类：**
```java
// RAG 服务接口
top.yumbo.ai.omni.rag.api.RAGService

// Lucene 实现
top.yumbo.ai.omni.rag.adapter.impl.file.LuceneRAGService

// RAG 注册表
top.yumbo.ai.omni.rag.adapter.impl.RagServiceRegistry
```

**配置示例：**
```yaml
omni-agent:
  rag:
    instances:
      # 默认 Lucene 实例
      - id: default
        type: file
        index-path: ./data/rag-index/file
        
      # Elasticsearch 实例（可选）
      - id: es-rag
        type: elasticsearch
        hosts: localhost:9200
```

---

### 4. 文档处理器

**模块：** `omni-agent-document-processor-api` + `omni-agent-document-processor-starter`

**支持的文档格式：**
- ✅ **Word** (.doc, .docx)
- ✅ **Excel** (.xls, .xlsx)
- ✅ **PPT** (.ppt, .pptx)
- ✅ **PDF** (.pdf)
- ✅ **纯文本** (.txt, .md, etc.)
- ✅ **Vision LLM** - 图片文字提取

**处理器列表：**
```java
top.yumbo.ai.omni.document.processor.starter.processor.WordDocumentProcessor
top.yumbo.ai.omni.document.processor.starter.processor.ExcelDocumentProcessor
top.yumbo.ai.omni.document.processor.starter.processor.PPTDocumentProcessor
top.yumbo.ai.omni.document.processor.starter.processor.PDFDocumentProcessor
top.yumbo.ai.omni.document.processor.starter.processor.PlainTextDocumentProcessor
top.yumbo.ai.omni.document.processor.starter.processor.VisionLLMDocumentProcessor
```

**配置示例：**
```yaml
omni-agent:
  document-processor:
    vision-llm:
      enabled: true
      model: qwen-vl-plus
      api-key: ${DASHSCOPE_API_KEY}
```

---

### 5. 智能分块（Chunking）

**模块：** `omni-agent-chunking-api` + `omni-agent-chunking-starter`

**分块策略：**

| 策略 | 说明 | 推荐场景 |
|------|------|----------|
| **PPL** ⭐ | 基于困惑度的语义边界 | 通用场景，最智能 |
| **SEMANTIC** | 基于向量相似度 | 长文档，需要语义聚合 |
| **PARAGRAPH** | 基于自然段落 | 格式化文档 |
| **SENTENCE** | 基于句子边界 | 短文本、对话 |
| **FIXED_LENGTH** | 固定长度切分 | 简单场景 |
| **CUSTOM** | 自定义策略 | 特殊需求 |

**关键类：**
```java
// 策略接口
top.yumbo.ai.omni.chunking.api.ChunkingStrategy

// PPL 策略（推荐）
top.yumbo.ai.omni.chunking.starter.strategy.PPLChunkingStrategy

// 语义策略
top.yumbo.ai.omni.chunking.starter.strategy.SemanticStrategy
```

**使用示例：**
```java
@Service
public class DocumentService {
    
    private final ChunkingService chunkingService;
    
    public void processDocument(String content) {
        // 使用 PPL 策略
        ChunkingConfig config = ChunkingConfig.builder()
            .strategy("PPL")
            .maxChunkSize(1000)
            .overlapSize(100)
            .build();
            
        List<Chunk> chunks = chunkingService.chunk(content, config);
    }
}
```

---

### 6. AI 服务

**模块：** `omni-agent-ai-api` + `omni-agent-ai-starter`

**支持的 AI 提供商：**

| 提供商 | 类型 | 特点 |
|--------|------|------|
| **Ollama** | 本地推理 | 免费、隐私、离线可用 |
| **DeepSeek** | 在线API | 高性价比、效果好 |
| **OpenAI** | 在线API | 效果最好、成本高 |
| **通义千问** | 在线API | 国内访问快 |
| **ONNX** | 本地推理 | 边缘设备、嵌入式 |

**关键类：**
```java
// AI 服务接口
top.yumbo.ai.omni.ai.api.AIService

// Ollama 实现
top.yumbo.ai.omni.ai.starter.impl.OllamaAIService

// 在线 API 实现
top.yumbo.ai.omni.ai.starter.impl.OnlineAPIAIService

// ONNX 实现
top.yumbo.ai.omni.ai.starter.impl.OnnxAIService
```

**配置示例：**
```yaml
omni-agent:
  ai:
    # Ollama 配置
    provider: ollama
    model: qwen2.5:7b
    base-url: http://localhost:11434
    
    # 或者在线 API
    # provider: deepseek
    # model: deepseek-chat
    # api-key: ${DEEPSEEK_API_KEY}
```

---

### 7. 文档存储

**模块：** `omni-agent-document-storage-api` + `omni-agent-document-storage-starter`

**支持的存储引擎：**

| 存储类型 | 适用场景 | 特点 |
|---------|---------|------|
| **File** | 开发测试 | 简单、无依赖 |
| **SQLite** | 单机部署 | 嵌入式数据库 |
| **H2** | 内存缓存 | 快速、临时存储 |
| **MongoDB** | 生产环境 | 分布式、高性能 |
| **Redis** | 缓存层 | 超高速、内存存储 |
| **Elasticsearch** | 大规模搜索 | 全文检索、分布式 |

**关键类：**
```java
// 存储服务接口
top.yumbo.ai.omni.storage.api.DocumentStorageService

// File 实现
top.yumbo.ai.omni.storage.impl.file.FileDocumentStorage

// MongoDB 实现
top.yumbo.ai.omni.storage.impl.mongodb.MongoDocumentStorage
```

**多实例配置：**
```yaml
omni-agent:
  storage:
    instances:
      # 主存储（File）
      - id: primary
        type: file
        base-path: ./data/storage
        
      # 备份存储（MongoDB）
      - id: backup
        type: mongodb
        database: omni-agent
        collection: documents
```

---

### 8. P2P 知识共享

**模块：** `omni-agent-p2p-api` + `omni-agent-p2p-starter`

**核心功能：**
- ✅ 端到端连接
- ✅ 知识传输
- ✅ 协作学习
- ✅ 连接码机制

**存储支持：**
- Memory（内存）
- SQLite（单机）
- H2（临时）
- Redis（分布式）
- MongoDB（持久化）

**关键类：**
```java
// P2P 连接管理
top.yumbo.ai.omni.p2p.api.P2PConnectionManager

// 数据传输
top.yumbo.ai.omni.p2p.api.P2PDataTransferService

// 协作服务
top.yumbo.ai.omni.p2p.api.P2PCollaborationService
```

**配置示例：**
```yaml
omni-agent:
  p2p:
    enabled: true
    storage-type: sqlite
    connection-code: "my-unique-code-123"
    auto-sync: true
```

---

### 9. 工作流引擎

**模块：** `omni-agent-workflow`

**核心功能：**
- ✅ 工作流定义和执行
- ✅ 文件监听和自动索引
- ✅ 文档处理流程
- ✅ 工作流市场

**关键类：**
```java
// 工作流注册表
top.yumbo.ai.omni.workflow.WorkflowRegistry

// 文件监听服务
top.yumbo.ai.omni.workflow.service.FileWatcherService

// 文档处理服务
top.yumbo.ai.omni.workflow.service.DocumentProcessingService
```

**配置示例：**
```yaml
omni-agent:
  workflow:
    storage-type: sqlite
    sqlite-db-path: ./data/workflows/workflows.db
    file-watcher:
      enabled: true
      auto-index: true
      watch-path: ./data/documents
```

---

### 10. 算法市场

**模块：** `omni-agent-marketplace`

**核心功能：**
- ✅ 算法组件注册
- ✅ 查询扩展
- ✅ 重排序算法
- ✅ 自定义算法

**内置算法：**
- **query_expansion** - 查询扩展
- **semantic_chunking** - 语义分块
- **rerank** - 重排序

**关键类：**
```java
// 算法市场服务
top.yumbo.ai.omni.marketplace.AlgorithmMarketService

// 查询增强服务
top.yumbo.ai.omni.marketplace.EnhancedQueryService
```

---

## 🔧 模块依赖关系

```
omni-agent-example-basic
  ├─→ omni-agent-web
  │    ├─→ omni-agent-workflow
  │    ├─→ omni-agent-hope-starter
  │    └─→ omni-agent-knowledge-registry-starter
  │
  ├─→ omni-agent-hope-starter
  │    ├─→ omni-agent-hope-api
  │    └─→ omni-agent-knowledge-registry-api
  │
  ├─→ omni-agent-knowledge-registry-starter
  │    ├─→ omni-agent-knowledge-registry-api
  │    ├─→ omni-agent-rag-api
  │    └─→ omni-agent-storage-api
  │
  ├─→ omni-agent-rag-starter-adapter
  │    ├─→ omni-agent-rag-api
  │    └─→ omni-agent-ai-api
  │
  ├─→ omni-agent-document-processor-starter
  │    ├─→ omni-agent-document-processor-api
  │    ├─→ omni-agent-chunking-api
  │    └─→ omni-agent-ai-api
  │
  └─→ omni-agent-core
       ├─→ omni-agent-common
       └─→ Spring Boot 3.4.1
```

---

## 📊 模块统计

| 分类 | 模块数 | 说明 |
|------|--------|------|
| **API 接口** | 8 | 定义核心接口 |
| **Starter 实现** | 12 | 具体实现 |
| **核心模块** | 2 | core + common |
| **Web 模块** | 2 | web + workflow |
| **示例应用** | 2 | basic + production |
| **总计** | **26** | 全部模块 |

---

## 🎯 模块选择指南

### 最小化配置（开发测试）

```xml
<dependencies>
    <!-- 核心 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-core</artifactId>
    </dependency>
    
    <!-- AI 服务 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-ai-starter</artifactId>
    </dependency>
    
    <!-- HOPE 系统 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-hope-starter</artifactId>
    </dependency>
</dependencies>
```

### 推荐配置（生产环境）

```xml
<dependencies>
    <!-- Web 服务 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-web</artifactId>
    </dependency>
    
    <!-- HOPE + 知识网络 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-hope-starter</artifactId>
    </dependency>
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-knowledge-registry-starter</artifactId>
    </dependency>
    
    <!-- 文档处理 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-document-processor-starter</artifactId>
    </dependency>
    
    <!-- RAG -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-rag-starter-adapter</artifactId>
    </dependency>
</dependencies>
```

---

## 📚 相关文档

- 📖 [完整架构](ARCHITECTURE.md)
- 🧠 [HOPE 系统](HOPE_SYSTEM.md)
- 🕸️ [知识网络](KNOWLEDGE_NETWORK.md)
- 🚀 [快速开始](QUICKSTART.md)

---

**文档维护者：** OmniAgent Team  
**最后更新：** 2026-01-01

