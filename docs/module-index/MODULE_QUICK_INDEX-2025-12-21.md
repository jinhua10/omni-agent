# 📚 OmniAgent 模块快速索引 (2025-12-21)

> **版本**: 1.0.0  
> **更新日期**: 2025年12月21日  
> **架构模式**: Spring Boot Starter 可插拔架构

---

## 🎯 项目概述

**OmniAgent** 是一个四维可插拔AI智能体框架，基于 Spring Boot Starter 模式实现完全可插拔的架构设计。系统包含 **40+ Java模块** 和 **1个前端UI模块**，通过API层、实现层、核心层和应用层四层架构实现高度解耦。

### 核心特性
- ✅ **四维可插拔架构**: API层、Starter实现层、核心业务层、应用示例层
- ✅ **双轨问答系统**: 左轨(RAG+LLM) + 右轨(HOPE知识演化)
- ✅ **智能文档处理**: 支持 Office、PDF、图片、媒体文件等多种格式
- ✅ **算法市场**: 用户可上传和分享自定义RAG优化算法
- ✅ **P2P协作网络**: 多智能体协作与知识共享
- ✅ **角色数据库**: 专业角色对问题的定制化回复

---

## 📊 模块总览

### 模块统计
| 层级 | 模块数量 | 说明 |
|------|---------|------|
| **API 层** | 7 | 定义所有接口规范 |
| **Starter 实现层** | 30 | 可插拔实现（多种存储、AI服务等） |
| **核心业务层** | 6 | 核心逻辑、算法、工作流 |
| **应用示例层** | 2 | 基础示例、生产示例 |
| **前端UI** | 1 | React + Vite |
| **总计** | 46 | 模块总数 |

---

## 🏗️ 架构层级详解

### 第一层：API 接口层 (7个模块)

定义系统所有接口规范，确保核心业务层只依赖接口，不依赖具体实现。

| 模块名称 | 职责 | 关键接口 |
|---------|------|---------|
| **omni-agent-persistence-api** | 持久化接口 | 问题分类、知识管理等结构化数据持久化 |
| **omni-agent-document-storage-api** | 文档存储接口 | 文档分块、图像、大文件等非结构化数据存储 |
| **omni-agent-rag-api** | RAG检索接口 | 向量检索、全文检索、混合检索 |
| **omni-agent-ai-api** | AI服务接口 | LLM调用、Embedding生成、Vision LLM |
| **omni-agent-p2p-api** | P2P协作接口 | 智能体注册、知识共享、协作通信 |
| **omni-agent-voting-api** | 投票系统接口 | 多智能体投票决策 |
| **omni-agent-behavior-api** | 行为分析接口 | 用户行为分析、反馈学习 |

---

### 第二层：Starter 实现层 (30个模块)

提供可插拔的实现，用户可根据需求选择不同的存储、AI服务等。

#### 2.1 持久化实现 (7个模块)
存储结构化数据（问题分类、知识图谱、角色信息等）

| 模块名称 | 存储方式 | 适用场景 |
|---------|---------|---------|
| **omni-agent-persistence-starter-memory** | 内存 | 开发测试、快速原型 |
| **omni-agent-persistence-starter-file** | 文件系统 | 单机部署、简单场景 |
| **omni-agent-persistence-starter-h2** | H2数据库 | 嵌入式数据库、演示 |
| **omni-agent-persistence-starter-sqlite** | SQLite | 轻量级应用 |
| **omni-agent-persistence-starter-redis** | Redis | 高性能缓存、分布式 |
| **omni-agent-persistence-starter-mongodb** | MongoDB | 文档型数据、灵活Schema |
| **omni-agent-persistence-starter-elasticsearch** | Elasticsearch | 全文检索、大规模数据 |

#### 2.2 文档存储实现 (6个模块)
存储非结构化数据（文档分块、图像、PPL数据等）

| 模块名称 | 存储方式 | 适用场景 |
|---------|---------|---------|
| **omni-agent-document-storage-starter-file** | 文件系统 | 单机部署、开发测试 |
| **omni-agent-document-storage-starter-mongodb** | MongoDB GridFS | 大文件存储、分布式 |
| **omni-agent-document-storage-starter-redis** | Redis | 小文件、高性能 |
| **omni-agent-document-storage-starter-elasticsearch** | Elasticsearch | 全文检索、大规模 |
| **omni-agent-document-storage-starter-s3** | AWS S3 | 云存储、对象存储 |
| **omni-agent-document-storage-starter-minio** | MinIO | 自建对象存储 |

#### 2.3 RAG检索实现 (6个模块)
实现向量检索、全文检索、混合检索

| 模块名称 | 检索方式 | 适用场景 |
|---------|---------|---------|
| **omni-agent-rag-starter-file** | 文件索引 | 单机、Lucene全文检索 |
| **omni-agent-rag-starter-h2** | H2全文检索 | 嵌入式、演示 |
| **omni-agent-rag-starter-sqlite** | SQLite FTS | 轻量级全文检索 |
| **omni-agent-rag-starter-redis** | Redis向量检索 | 高性能、分布式 |
| **omni-agent-rag-starter-mongodb** | MongoDB检索 | 向量+元数据混合检索 |
| **omni-agent-rag-starter-elasticsearch** | ES向量检索 | 大规模、生产环境 |

#### 2.4 AI服务实现 (2个模块)

| 模块名称 | AI服务类型 | 支持模型 |
|---------|-----------|---------|
| **omni-agent-ai-starter-ollama** | 本地Ollama | Llama3、Qwen、Gemma等 |
| **omni-agent-ai-starter-online-api** | 在线API | OpenAI、Claude、通义千问等 |

#### 2.5 P2P协作实现 (6个模块)
智能体之间的协作通信与知识共享

| 模块名称 | 存储方式 | 适用场景 |
|---------|---------|---------|
| **omni-agent-p2p-starter-memory** | 内存 | 开发测试 |
| **omni-agent-p2p-starter-h2** | H2数据库 | 嵌入式 |
| **omni-agent-p2p-starter-sqlite** | SQLite | 轻量级 |
| **omni-agent-p2p-starter-redis** | Redis | 分布式、高性能 |
| **omni-agent-p2p-starter-mongodb** | MongoDB | 大规模协作 |
| **omni-agent-p2p-starter-elasticsearch** | Elasticsearch | 智能体搜索 |

#### 2.6 投票系统实现 (4个模块)
多智能体投票决策

| 模块名称 | 存储方式 | 适用场景 |
|---------|---------|---------|
| **omni-agent-voting-starter-memory** | 内存 | 快速投票 |
| **omni-agent-voting-starter-redis** | Redis | 分布式投票 |
| **omni-agent-voting-starter-mongodb** | MongoDB | 历史记录 |
| **omni-agent-voting-starter-elasticsearch** | Elasticsearch | 投票分析 |

#### 2.7 行为分析实现 (3个模块)
用户行为分析与学习

| 模块名称 | 存储方式 | 适用场景 |
|---------|---------|---------|
| **omni-agent-behavior-starter-memory** | 内存 | 实时分析 |
| **omni-agent-behavior-starter-redis** | Redis | 高性能、缓存 |
| **omni-agent-behavior-starter-mongodb** | MongoDB | 行为历史 |

---

### 第三层：核心业务层 (6个模块)

实现系统核心业务逻辑，只依赖API接口，不依赖具体实现。

| 模块名称 | 职责 | 核心功能 |
|---------|------|---------|
| **omni-agent-common** | 通用工具 | HTTP客户端、工具类、配置 |
| **omni-agent-core** | 核心业务 | 文档处理、分块、查询、HOPE知识演化 |
| **omni-agent-ppl-onnx** | PPL算法服务 | 基于ONNX的困惑度计算、智能分块 |
| **omni-agent-marketplace** | 算法市场 | 查询扩展、重排序、自定义算法 |
| **omni-agent-workflow** | 工作流引擎 | YAML定义工作流、任务编排 |
| **omni-agent-web** | Web服务 | REST API、文件上传、SSE流式输出 |

#### 核心模块详解

##### **omni-agent-core** - 核心业务模块
```
omni-agent-core/
├── chunking/              # 分块策略
│   ├── ChunkingStrategyManager.java    # 策略管理器
│   └── strategy/
│       ├── FixedSizeChunkingStrategy.java       # 固定大小分块
│       ├── SemanticChunkingStrategy.java        # 语义分块
│       ├── PPLChunkingStrategy.java             # PPL增强分块
│       └── ParagraphChunkingStrategy.java       # 段落分块
├── document/              # 文档处理
│   ├── DocumentProcessorManager.java   # 处理器管理
│   └── processor/
│       ├── VisionLLMDocumentProcessor.java      # Office、PDF文档
│       ├── PlainTextDocumentProcessor.java      # 纯文本
│       └── MediaFileProcessor.java              # 媒体文件(未来)
├── query/                 # 查询服务
│   └── QueryService.java               # 查询入口
├── concept/               # 最小概念
├── knowledge/             # 知识管理
├── evolution/             # 知识演化
├── hope/                  # HOPE算法
├── role/                  # 角色管理
└── feedback/              # 反馈学习
```

##### **omni-agent-marketplace** - 算法市场
- **查询扩展 (Query Expansion)**: 生成多个查询变体，提高召回率
- **结果重排序 (Rerank)**: 优化检索结果顺序
- **多查询融合 (Multi-Query Fusion)**: 融合多个查询的结果
- **自定义算法上传**: 用户可上传自己的优化算法

##### **omni-agent-web** - Web服务
- REST API提供第三方集成
- 文件上传与监控
- SSE流式输出（双轨问答）
- 文档管理（类FTP文件浏览）

---

### 第四层：应用示例层 (2个模块)

| 模块名称 | 说明 | 配置 |
|---------|------|------|
| **omni-agent-example-basic** | 基础示例 | 本地Ollama + 文件存储 |
| **omni-agent-example-production** | 生产示例 | 在线API + Redis/MongoDB |

---

### 前端UI模块 (1个模块)

**omni-agent/UI** - React + Vite前端

```
UI/
├── src/
│   ├── api/                    # API 接口封装
│   ├── components/             # React 组件
│   │   ├── qa/                 # 双轨问答模块
│   │   ├── document/           # 文档管理（类FTP浏览）
│   │   ├── statistics/         # 统计模块
│   │   ├── feedback/           # 反馈系统
│   │   ├── role/               # 角色管理
│   │   ├── collaboration/      # 协作网络
│   │   └── ai-service/         # AI服务市场
│   ├── lang/                   # 国际化(中英文)
│   └── utils/                  # 工具函数
└── vite.config.js
```

---

## 🔄 RAG文档处理流程详解

### RAG处理完整流程

```
┌─────────────────────────────────────────────────────────────┐
│                    文档来源 (2种方式)                        │
├─────────────────────────────────────────────────────────────┤
│ 方式1: 直接放入 data/documents/ (FileWatcherService监听)   │
│ 方式2: Web上传 → data/documents/ (FileWatcherService监听)  │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│              阶段1: 文档监听与触发 RAG                       │
├─────────────────────────────────────────────────────────────┤
│ FileWatcherService                                          │
│  - 监听 data/documents/ 目录                                │
│  - 检测新文件 → 触发 RAG 处理                               │
│  - 处理成功 → 移动到 data/storage/documents/{原文件名}     │
│  - 处理失败 → 记录日志，保留在 documents/ 等待重试          │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│           阶段2: 文档格式识别与文本化                        │
├─────────────────────────────────────────────────────────────┤
│ DocumentProcessorManager.findProcessor(扩展名)              │
│  ↓                                                           │
│ 根据文件类型选择处理器:                                      │
│  - .pptx/.ppt/.docx/.doc/.xlsx/.xls/.pdf                    │
│    → VisionLLMDocumentProcessor                             │
│    → 提取页面/幻灯片 → 转为图片                             │
│    → Vision LLM理解 → 生成文本                              │
│                                                              │
│  - .txt/.md/.java/.py/.json/.xml                            │
│    → PlainTextDocumentProcessor                             │
│    → 直接读取文本                                            │
│                                                              │
│  - .mp4/.mp3/.wav (未来)                                    │
│    → MediaFileProcessor                                     │
│    → 媒体文件处理                                            │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│         阶段3: 智能分块 (ChunkingStrategyManager)           │
├─────────────────────────────────────────────────────────────┤
│ 1. 推断文档类型 (inferDocumentType)                         │
│    - TECHNICAL (技术文档)                                    │
│    - API (API文档)                                          │
│    - CODE (代码)                                            │
│    - FAQ (问答)                                             │
│    - MARKDOWN                                               │
│    - LONG_ARTICLE (长文章)                                  │
│                                                              │
│ 2. 自动选择分块策略 (selectBestStrategy)                    │
│    - 技术文档 → SemanticChunkingStrategy                    │
│    - API文档 → PPLChunkingStrategy                          │
│    - 代码 → SemanticChunkingStrategy                        │
│    - FAQ → Sentence Boundary                                │
│    - Markdown → Paragraph                                   │
│    - 长文章 → PPLChunkingStrategy                           │
│                                                              │
│ 3. 执行分块                                                  │
│    - FixedSizeChunkingStrategy: 固定大小(800字符)           │
│    - SemanticChunkingStrategy: 语义相似度分块               │
│    - PPLChunkingStrategy: PPL困惑度分块                     │
│    - ParagraphChunkingStrategy: 段落分块                    │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│         阶段4: 分块存储 (DocumentStorageService)            │
├─────────────────────────────────────────────────────────────┤
│ 存储结构 (文件存储为例):                                     │
│                                                              │
│ data/storage/                                               │
│ ├── documents/                                              │
│ │   └── {原文档名}.pptx              # 原始文档              │
│ │                                                            │
│ ├── chunks/                                                 │
│ │   └── {原文档名}/                  # 分块目录              │
│ │       ├── chunk_000                # 第1个分块            │
│ │       ├── chunk_001                                       │
│ │       └── chunk_002                                       │
│ │                                                            │
│ ├── images/                                                 │
│ │   └── {原文档名}/                  # 图片目录              │
│ │       ├── page_01_img_001.png                             │
│ │       └── page_02_img_001.png                             │
│ │                                                            │
│ ├── ppl/                                                    │
│ │   └── {原文档名}/                  # PPL数据               │
│ │       └── perplexities.json                               │
│ │                                                            │
│ └── optimization/                                           │
│     └── {原文档名}/                  # 优化数据              │
│         └── embeddings.bin                                  │
│                                                              │
│ 注: 其他存储方式(MongoDB/Redis/S3等)也保持相同的逻辑层级   │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│       阶段5: 向量索引 (RAGService.indexDocument)            │
├─────────────────────────────────────────────────────────────┤
│ 1. 为每个 Chunk 生成 Embedding (向量)                       │
│    - 调用 AIService.generateEmbedding()                     │
│    - 支持 BGE-M3 / OpenAI Embedding 等                      │
│                                                              │
│ 2. 存储到向量数据库                                          │
│    - File: Lucene索引                                       │
│    - Redis: RedisSearch向量检索                             │
│    - MongoDB: 向量+元数据混合检索                            │
│    - Elasticsearch: 向量检索+全文检索                        │
│                                                              │
│ 3. 构建元数据                                                │
│    - documentId: 文档ID                                     │
│    - chunkId: 分块ID                                        │
│    - fileName: 原文件名                                      │
│    - chunkIndex: 分块序号                                    │
│    - content: 分块内容                                       │
│    - vector: 向量                                            │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│              阶段6: 文档状态管理 (UI展示)                    │
├─────────────────────────────────────────────────────────────┤
│ 文档状态:                                                    │
│  - PENDING: 待处理                                           │
│  - INDEXING: 索引中                                         │
│  - DONE: 完成                                               │
│  - FAILED: 失败                                             │
│                                                              │
│ UI功能:                                                      │
│  - 类FTP文件浏览器视图                                       │
│  - 文件状态筛选                                              │
│  - 手动重建索引                                              │
│  - 拖拽文件到AI分析面板                                      │
│  - 批量操作(删除、重建)                                      │
└─────────────────────────────────────────────────────────────┘
```

### RAG检索流程

```
用户问题
   ↓
EnhancedQueryService (算法市场)
   ├─ 1. 查询扩展 (Query Expansion)
   │    - 原始问题: "Spring Boot如何配置?"
   │    - 扩展查询1: "Spring Boot配置文件"
   │    - 扩展查询2: "application.yml配置"
   │    - 扩展查询3: "Spring Boot参数设置"
   │
   ├─ 2. 多查询检索 (Multi-Query)
   │    - 并行检索所有查询
   │    - RAGService.searchByText()
   │
   ├─ 3. 结果融合 (Fusion)
   │    - 去重 (基于文档ID)
   │    - 分数融合
   │
   └─ 4. 重排序 (Rerank)
        - 基于相关性重新排序
        - 返回Top-K结果
```

---

## 🎨 双轨问答系统架构

```
用户问题
   ↓
┌────────────────────────────────────────────────────┐
│              前端问答界面 (QA Component)            │
└────────────────────────────────────────────────────┘
   ↓
┌─────────────────────┬──────────────────────────────┐
│    左轨: RAG + LLM   │   右轨: HOPE知识演化          │
│                     │                              │
│ 1. RAG检索          │ 1. 最小概念提取               │
│    ↓                │    ↓                         │
│ 2. 检索相关文档      │ 2. HOPE知识图谱              │
│    ↓                │    ↓                         │
│ 3. 拼接 Prompt      │ 3. 知识演化推理               │
│    ↓                │    ↓                         │
│ 4. LLM生成回答      │ 4. 综合生成回答               │
│    ↓                │    ↓                         │
│ 5. SSE流式输出      │ 5. SSE流式输出                │
└─────────────────────┴──────────────────────────────┘
   ↓                          ↓
用户反馈 → 系统学习 → 知识演化
```

### 左轨: RAG + LLM
- 使用 **EnhancedQueryService** 检索相关文档
- 拼接检索结果到 Prompt
- 调用 LLM (Ollama/在线API) 生成回答
- 适用于通用问题、文档问答

### 右轨: HOPE知识演化
- 提取**最小概念** (Minimal Concept)
- 查询 **HOPE知识图谱**
- 基于知识演化推理生成回答
- 随着用户反馈不断学习，越来越智能
- 适用于专业知识、角色定制化回复

---

## 🔮 未来计划与扩展

### Phase 1: 查询扩展实现 ✅ (已完成)
- [x] ~~从 `old/ai-reviewer-base-file-rag` 复用查询扩展代码~~ (基础实现已完成)
- [x] ~~集成到 `omni-agent-marketplace`~~ (EnhancedQueryService)
- [x] ~~实现多策略查询扩展~~ (同义词、领域词、RRF融合)
- [x] ~~实现结果重排序~~ (Rerank组件)
- [ ] 🔄 集成 LLM 驱动的查询扩展 (改进中)
- [ ] 🔄 从 old 复用高级查询处理器 (缓存、分页) (改进中)

> 📊 详细报告: [QUERY_EXPANSION_STATUS_REPORT-2025-12-21.md](../worklog/QUERY_EXPANSION_STATUS_REPORT-2025-12-21.md)

### Phase 2: 角色数据库
- [ ] 不同角色对同一问题的不同回复
- [ ] 通用角色自动调度专业角色
- [ ] 角色知识库构建

### Phase 3: 媒体文件支持
- [ ] 视频文件处理 (.mp4, .avi)
- [ ] 音频文件处理 (.mp3, .wav)
- [ ] 语音识别 + 字幕提取
- [ ] 大文件分块处理

### Phase 4: UI可视化RAG调优
- [ ] 可视化分块策略选择
- [ ] 实时查看RAG过程
- [ ] 调整检索参数
- [ ] 算法效果对比

### Phase 5: 在线LLM直接文件分析
- [ ] 支持直接传文件给在线LLM
- [ ] Claude/GPT-4V 直接分析文档
- [ ] 减少中间转换步骤

---

## 📦 依赖关系图

```
┌─────────────────────────────────────────────────────────────┐
│                       应用示例层                             │
│  omni-agent-example-basic    omni-agent-example-production  │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                       核心业务层                             │
│  omni-agent-core      omni-agent-web                        │
│  omni-agent-marketplace   omni-agent-workflow               │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                    Starter 实现层 (可插拔)                   │
│  Persistence-Starters (7个)                                 │
│  Document-Storage-Starters (6个)                            │
│  RAG-Starters (6个)                                         │
│  AI-Starters (2个)                                          │
│  P2P-Starters (6个)                                         │
│  Voting-Starters (4个)                                      │
│  Behavior-Starters (3个)                                    │
└─────────────────────────────────────────────────────────────┘
                            ↓
┌─────────────────────────────────────────────────────────────┐
│                       API 接口层                             │
│  persistence-api    document-storage-api    rag-api         │
│  ai-api    p2p-api    voting-api    behavior-api            │
└─────────────────────────────────────────────────────────────┘
```

---

## 🛠️ 快速开始

### 1. 克隆项目
```bash
git clone https://github.com/jinhua10/omni-agent.git
cd omni-agent
```

### 2. 编译项目
```bash
mvn clean install -DskipTests
```

### 3. 运行基础示例
```bash
cd omni-agent-example-basic
mvn spring-boot:run
```

### 4. 启动前端UI
```bash
cd UI
npm install
npm run dev
```

### 5. 访问
- **前端**: http://localhost:5173
- **后端API**: http://localhost:8080
- **Swagger文档**: http://localhost:8080/swagger-ui.html

---

## 📖 相关文档

### 核心文档
- [README.md](../../README.md) - 项目总览
- [VISION_LLM_OPTIMIZATION.md](../../VISION_LLM_OPTIMIZATION.md) - Vision LLM优化
- [MODULE_QUICK_INDEX.md](./MODULE_QUICK_INDEX.md) - 旧版模块索引

### 技术文档
- [RAG_ALGORITHM_DECISION_TREE.md](../RAG_ALGORITHM_DECISION_TREE.md) - RAG算法决策树
- [DOCUMENT_UPLOAD_PROCESSING_FIX.md](../DOCUMENT_UPLOAD_PROCESSING_FIX.md) - 文档上传修复
- [UNIFIED_DOCUMENT_PROCESSING.md](../UNIFIED_DOCUMENT_PROCESSING.md) - 统一文档处理

### API文档
- [api/](../api/) - API接口文档目录

---

## 🤝 贡献指南

欢迎贡献代码、提交Issue、完善文档！

### 开发规范
1. 遵循 Spring Boot Starter 模式
2. API层只定义接口，不包含实现
3. 核心业务层只依赖API，不依赖具体实现
4. Starter层提供可插拔实现
5. 充分的日志记录和错误处理

### 提交规范
```
feat: 新功能
fix: 修复
docs: 文档
refactor: 重构
test: 测试
chore: 构建/工具
```

---

## 📄 许可证

Apache License 2.0

---

## 👥 开发团队

- **Jinhua Yu** - 项目负责人
- Email: 1015770492@qq.com
- GitHub: https://github.com/jinhua10/omni-agent

---

**最后更新**: 2025年12月21日  
**版本**: v1.0.0  
**Java版本**: 21  
**Spring Boot版本**: 3.2.11

