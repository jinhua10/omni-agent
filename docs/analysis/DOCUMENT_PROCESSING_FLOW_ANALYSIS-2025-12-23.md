# 📄 文档处理流程 - 三个核心类的协作关系

> **分析时间**: 2025-12-23  
> **核心类**: DocumentManagementController、FileWatcherService、SystemRAGConfigController

---

## 🎯 核心类职责

### 1. DocumentManagementController
**角色**: 📤 **文档上传入口**

**职责**:
- 接收用户上传的文件
- 保存文件到**中转站**（`./data/documents`）
- 触发异步RAG处理
- 提供文档列表、下载、删除等管理功能

**关键方法**:
```java
@PostMapping("/upload")
public UploadResponse uploadDocument(MultipartFile file)
```

---

### 2. FileWatcherService
**角色**: 👀 **文件监听与注册服务**

**职责**:
- 监听 `./data/documents` 目录的文件变化
- **定期扫描**未注册的文件（每30秒）
- 将新文件**注册**到 `SystemRAGConfigService`（状态：PENDING）
- **不自动处理**，等待用户在UI中配置和触发

**关键方法**:
```java
private void scanAndProcessUnindexedFiles()  // 扫描未注册文件
```

**新逻辑**:
- ❌ 不再自动RAG处理
- ✅ 只注册文件，设置状态为 PENDING
- ✅ 由用户决定何时处理

---

### 3. SystemRAGConfigController
**角色**: ⚙️ **RAG配置与处理控制器**

**职责**:
- 管理系统级RAG配置（自动提取、自动RAG等）
- 管理文档级RAG配置（提取模型、分块策略等）
- 提供文本提取、分块、重建等操作接口
- 管理RAG策略模板

**关键方法**:
```java
@PostMapping("/document/{documentId}/extract")  // 触发文本提取
@PostMapping("/document/{documentId}/chunk")    // 触发分块
@PostMapping("/document/{documentId}/rebuild")  // 重建文档
```

---

## 🔄 完整的文档处理流程

```
┌─────────────────────────────────────────────────────────────┐
│                     文档处理完整流程                           │
└─────────────────────────────────────────────────────────────┘

第1阶段: 用户上传 (DocumentManagementController)
├─ 用户通过前端上传文件
├─ POST /api/documents/upload
├─ 文件保存到中转站: ./data/documents/文件.pptx
├─ 生成 documentId = 文件名
├─ 触发 DocumentProcessingService.processDocument()
└─ 返回"索引中"状态（indexing: true）

                    ↓

第2阶段: 文件扫描与注册 (FileWatcherService)
├─ 定期扫描任务（每30秒）: scanAndProcessUnindexedFiles()
├─ 发现新文件: ./data/documents/文件.pptx
├─ 检查是否已注册: ragConfigService.hasDocumentConfig()
├─ 如果未注册:
│   ├─ 创建文档配置: DocumentRAGConfig
│   │   ├─ documentId = 文件名
│   │   ├─ status = "PENDING"
│   │   ├─ textExtractionModel = 系统默认
│   │   └─ chunkingStrategy = 系统默认
│   └─ 注册到配置服务: ragConfigService.setDocumentConfig()
└─ 日志: "📝 发现新文档，已注册: 文件.pptx (PENDING)"

                    ↓

第3阶段: 用户配置（前端UI + SystemRAGConfigController）
├─ 前端从 GET /api/system/rag-config/documents-status 获取待处理文档
├─ 用户看到文档列表，状态为 PENDING
├─ 用户可以选择:
│   ├─ 方式1: 使用默认配置直接处理
│   ├─ 方式2: 自定义文本提取模型
│   ├─ 方式3: 自定义分块策略
│   └─ 方式4: 应用已有模板
└─ 配置保存到: PUT /api/system/rag-config/document/{documentId}

                    ↓

第4阶段: 文本提取 (SystemRAGConfigController)
├─ 用户触发: POST /api/system/rag-config/document/{documentId}/extract
├─ 读取中转站文件: ./data/documents/文件.pptx
├─ 调用文本提取服务:
│   ├─ standard: 标准文本提取
│   ├─ vision-llm: Vision LLM提取（PPT、图表）
│   └─ ocr: OCR识别
├─ 流式返回进度（SSE）:
│   ├─ 10%: 正在读取文档...
│   ├─ 50%: 正在提取文本...
│   └─ 100%: 文本提取完成
├─ 保存提取结果到配置:
│   ├─ config.extractedText = 提取的文本
│   └─ config.status = "EXTRACTED"
└─ 前端状态更新: PENDING → EXTRACTING → EXTRACTED

                    ↓

第5阶段: 智能分块 (SystemRAGConfigController)
├─ 用户触发: POST /api/system/rag-config/document/{documentId}/chunk
├─ 读取提取的文本: config.extractedText
├─ 调用分块服务:
│   ├─ fixed-size: 固定大小分块
│   ├─ semantic: 语义分块
│   ├─ ppl: PPL分块
│   └─ hybrid: 混合策略
├─ 流式返回进度（SSE）:
│   ├─ 20%: 正在分析文档结构...
│   ├─ 60%: 正在智能分块...
│   └─ 100%: 分块完成，共X个分块
├─ 保存分块结果:
│   ├─ storageService.saveChunks(documentId, chunks)
│   └─ config.status = "CHUNKED"
└─ 前端状态更新: EXTRACTED → CHUNKING → CHUNKED

                    ↓

第6阶段: 向量化与索引 (SystemRAGConfigController)
├─ 自动或手动触发: POST /api/system/rag-config/document/{documentId}/index
├─ 读取分块: storageService.getChunksByDocument(documentId)
├─ 向量化处理:
│   ├─ 为每个分块生成向量
│   └─ 保存向量到向量数据库
├─ 建立RAG索引:
│   └─ ragService.indexDocument(document)
├─ 更新状态:
│   └─ config.status = "COMPLETED"
└─ 前端状态更新: CHUNKED → INDEXING → COMPLETED

                    ↓

第7阶段: 归档到存储服务 (未完全实现 ⚠️)
├─ RAG处理完成后
├─ 保存到虚拟路径系统:
│   └─ storageService.saveDocument(documentId, filename, content)
│   └─ 虚拟路径: documents/文件.pptx
├─ 删除中转站文件:
│   └─ Files.delete(./data/documents/文件.pptx)
└─ 完成整个流程 ✅

                    ↓

第8阶段: 用户查询与检索
├─ 用户通过前端查询
├─ POST /api/qa/ask
├─ 调用 RAG 检索:
│   ├─ ragService.search(query)
│   └─ 返回相关分块
├─ 生成回答:
│   └─ LLM根据检索结果生成答案
└─ 返回给用户 ✅
```

---

## 📊 三个类的交互时序图

```
用户          Controller        FileWatcher       RAGConfig        Processing
 │                │                  │                 │                │
 │  上传文件      │                  │                 │                │
 ├──────────────>│                  │                 │                │
 │                │  保存到中转站     │                 │                │
 │                ├─────────────────>│                 │                │
 │                │                  │                 │  触发异步处理   │
 │                │                  │                 │───────────────>│
 │                │                  │                 │                │
 │  返回索引中    │                  │                 │                │
 │<───────────────┤                  │                 │                │
 │                │                  │                 │                │
 │                │     定期扫描      │                 │                │
 │                │  (每30秒)         │                 │                │
 │                │                  │  发现新文件      │                │
 │                │                  │  注册文档配置    │                │
 │                │                  ├────────────────>│                │
 │                │                  │  (PENDING)      │                │
 │                │                  │                 │                │
 │  查看待处理    │                  │                 │                │
 │  文档列表      │                  │                 │                │
 ├──────────────>│                  │                 │                │
 │                │                  │  获取文档状态    │                │
 │                ├────────────────────────────────────>│                │
 │  显示PENDING   │                  │                 │                │
 │<───────────────┤                  │                 │                │
 │                │                  │                 │                │
 │  配置并触发    │                  │                 │                │
 │  文本提取      │                  │                 │                │
 ├──────────────>│                  │                 │                │
 │                │                  │  更新配置        │                │
 │                ├────────────────────────────────────>│                │
 │                │                  │                 │  执行提取       │
 │                │                  │                 ├───────────────>│
 │                │                  │                 │                │
 │  SSE进度推送   │                  │                 │  返回进度       │
 │<────────────────────────────────────────────────────┤                │
 │  (10%...50%...100%)               │                 │                │
 │                │                  │                 │                │
 │  触发分块      │                  │                 │                │
 ├──────────────>│                  │                 │                │
 │                │                  │                 │  执行分块       │
 │                ├────────────────────────────────────>│                │
 │                │                  │                 │                │
 │  SSE进度推送   │                  │                 │                │
 │<────────────────────────────────────────────────────┤                │
 │                │                  │                 │                │
 │  完成！        │                  │                 │                │
 │<───────────────┤                  │                 │                │
```

---

## 🔑 关键设计要点

### 1. 中转站模式 ⭐
```
用户上传 → 中转站(./data/documents) → RAG处理 → 存储服务(documents/)
```

**优势**:
- ✅ 用户可以介入RAG过程
- ✅ 支持自定义配置
- ✅ 支持重新处理
- ✅ 失败后可重试

### 2. 状态驱动的流程 ⭐
```
PENDING → EXTRACTING → EXTRACTED → CHUNKING → CHUNKED → INDEXING → COMPLETED
```

**每个状态对应用户可以执行的操作**:
- `PENDING`: 配置提取模型
- `EXTRACTED`: 配置分块策略
- `CHUNKED`: 触发索引
- `COMPLETED`: 可以查询

### 3. 配置分离 ⭐
```
系统配置（全局）
├─ autoTextExtraction (是否自动提取)
├─ autoRAG (是否自动RAG)
├─ defaultTextExtractionModel (默认提取模型)
└─ defaultChunkingStrategy (默认分块策略)

文档配置（单个文档）
├─ documentId
├─ status (PENDING/EXTRACTING/EXTRACTED...)
├─ textExtractionModel
├─ chunkingStrategy
├─ chunkingParams
└─ extractedText
```

### 4. 异步处理与进度推送 ⭐
```java
// 异步处理
CompletableFuture.runAsync(() -> {
    // 长时间运行的任务
})

// SSE进度推送
SseEmitter emitter = new SseEmitter();
emitter.send(SseEmitter.event()
    .data("{\"percent\":50,\"message\":\"处理中...\"}"));
```

---

## ⚠️ 当前存在的问题

### 问题1: 归档未完成
**现状**: RAG处理完成后，文件仍在中转站
**原因**: `DocumentProcessingService` 未调用存储服务保存
**解决**: 在 `performFullRAG()` 完成后添加：
```java
storageService.saveDocument(documentId, filename, content);
Files.delete(Paths.get(watchDirectory).resolve(filename));
```

### 问题2: FileWatcherService 职责混乱
**现状**: 既监听文件，又注册文档
**建议**: 拆分为两个服务：
- `FileWatcherService`: 只负责监听文件变化
- `DocumentRegistrationService`: 负责注册文档到配置服务

### 问题3: 配置与处理耦合
**现状**: `SystemRAGConfigController` 既管理配置又执行处理
**建议**: 拆分为：
- `SystemRAGConfigController`: 只管理配置
- `DocumentProcessingController`: 专门处理文档（提取、分块、索引）

---

## 🎯 推荐的改进方案

### 方案1: 简化流程
```
上传 → 自动注册 → 自动处理（使用默认配置） → 完成
```
**适合**: 快速原型、简单应用

### 方案2: 完全手动
```
上传 → 注册(PENDING) → 用户配置 → 手动触发提取 → 手动触发分块 → 手动触发索引 → 完成
```
**适合**: 专业用户、精细控制

### 方案3: 智能混合（推荐）⭐
```
上传 → 自动注册 → 
    ├─ 系统配置=自动 → 自动处理 → 完成
    └─ 系统配置=手动 → PENDING → 用户介入 → 完成
```
**适合**: 大多数场景，兼顾易用性和灵活性

---

## 📋 总结

### DocumentManagementController
- 📤 **上传入口**
- 保存到中转站
- 触发异步处理

### FileWatcherService
- 👀 **监听与注册**
- 定期扫描新文件
- 注册到配置服务（PENDING状态）

### SystemRAGConfigController
- ⚙️ **配置与控制**
- 管理系统/文档配置
- 提供处理操作接口（提取、分块、索引）

### 协作关系
```
DocumentManagementController → 中转站
                                  ↓
                        FileWatcherService → 扫描注册
                                  ↓
                        SystemRAGConfigController → 用户配置处理
                                  ↓
                        DocumentProcessingService → 执行RAG
                                  ↓
                        存储服务 → 最终归档
```

---

**分析时间**: 2025-12-23  
**状态**: ✅ 流程梳理完成  
**下一步**: 完善归档逻辑，优化职责分离

