# 📄 文档上传、下载、删除流程说明（虚拟路径系统）

> **更新时间**: 2025-12-22 19:38  
> **状态**: ✅ 路径处理已修复  

---

## 🎯 核心问题与解决方案

### 问题1: 路径重复添加前缀 ❌

**原因**: 前端传递的路径已包含 `documents/`，后端又添加了一次

**示例错误**:
```
前端: documents/文件.pptx
后端处理: documents/ + documents/文件.pptx
结果: documents/documents/文件.pptx ❌
```

**解决方案** ✅:
```java
// 智能判断路径是否已包含documents前缀
if (path.startsWith(VIRTUAL_ROOT + "/") || path.equals(VIRTUAL_ROOT)) {
    virtualPath = path;  // 已经包含documents前缀
} else {
    virtualPath = VIRTUAL_ROOT + "/" + path;  // 添加documents前缀
}
```

---

## 📂 当前文档流转流程

### 阶段1: 上传阶段（中转站模式）⭐

```
用户上传文件
    ↓
保存到中转站: ./data/documents/文件.pptx
    ↓
触发异步RAG处理
    ↓
返回"索引中"状态
```

**代码位置**: `DocumentManagementController.uploadDocument()`

```java
// ⭐ 步骤1：先保存到监听目录作为中转站
Path watchDir = Paths.get(watchDirectory);  // ./data/documents
Path targetFile = watchDir.resolve(filename);
file.transferTo(targetFile);

// ⭐ 步骤2：触发异步RAG处理
documentProcessingService.processDocument(documentId, filename, file.getBytes());
```

### 阶段2: RAG处理阶段（异步）⭐

```
文本提取
    ↓
智能分块
    ↓
向量化
    ↓
建立索引
    ↓
⚠️ 移动到存储服务（TODO）
```

**当前状态**: 
- ✅ 文本提取（模拟）
- ✅ 智能分块（模拟）
- ✅ 向量化（模拟）
- ✅ 索引（模拟）
- ⚠️ **未实现**：RAG完成后移动文件到存储服务

**代码位置**: `DocumentProcessingService.performFullRAG()`

### 阶段3: 最终存储阶段（需要实现）⚠️

```
RAG处理完成
    ↓
调用存储服务保存
    ↓
文件存入虚拟路径: documents/文件.pptx
    ↓
删除中转站文件: ./data/documents/文件.pptx
```

**需要添加的代码**:
```java
// 在 DocumentProcessingService.performFullRAG() 完成后
storageService.saveDocument(documentId, filename, content);
// 删除中转站文件
Files.deleteIfExists(Paths.get(watchDirectory).resolve(filename));
```

---

## 🔄 文件下载流程

### 当前实现

通过 `DocumentBrowseController` 下载：

```java
GET /api/documents/browse/download?path=文件.pptx

// 自动判断是否包含documents前缀
String virtualPath = 正规化路径(path);
byte[] data = storageService.readFile(virtualPath);
return 文件流;
```

### 路径处理示例

| 前端传递 | 后端处理后 | 存储服务查询 |
|---------|-----------|------------|
| `文件.pptx` | `documents/文件.pptx` | ✅ |
| `documents/文件.pptx` | `documents/文件.pptx` | ✅ |
| `子目录/文件.pptx` | `documents/子目录/文件.pptx` | ✅ |
| `documents/子目录/文件.pptx` | `documents/子目录/文件.pptx` | ✅ |

---

## 🗑️ 文件删除流程

### 通过浏览器删除（虚拟路径）

```java
DELETE /api/documents/browse/delete?path=documents/文件.pptx

// 自动判断路径
String virtualPath = 正规化路径(path);
boolean success = storageService.deleteFile(virtualPath);
```

### 通过文档管理删除（完整删除）

```java
DELETE /api/documents/{documentId}

// 删除所有相关数据
storageService.deleteDocument(documentId);      // 原始文档
storageService.deleteChunksByDocument(documentId);  // 分块
storageService.deleteImagesByDocument(documentId);   // 图片
ragService.deleteDocument(documentId);              // RAG索引
```

---

## ⚠️ 当前存在的问题

### 问题1: 文件只在中转站，未存入存储服务

**现象**:
- 文件上传成功
- RAG处理完成
- 但文件仍在 `./data/documents`
- 存储服务中没有文件

**原因**: `DocumentProcessingService` 未调用 `storageService.saveDocument()`

**解决方案**: 在RAG完成后添加保存逻辑

### 问题2: 删除操作返回400错误

**原因**: ✅ 已修复
- 路径重复添加前缀导致找不到文件

**解决方案**: ✅ 已实现智能路径判断

### 问题3: 中转站文件未清理

**现象**:
- RAG处理后，`./data/documents` 中的文件未删除
- 磁盘空间浪费

**解决方案**: RAG完成后删除中转站文件

---

## 🔧 需要实现的功能

### 1. 完善 DocumentProcessingService ⭐⭐⭐

```java
private void performFullRAG(...) {
    // ...existing code...
    
    // ⭐ 新增：RAG完成后保存到存储服务
    log.info("💾 保存文档到存储服务...");
    storageService.saveDocument(documentId, documentName, content);
    
    // ⭐ 新增：删除中转站文件
    Path watchFile = Paths.get(watchDirectory).resolve(documentName);
    if (Files.exists(watchFile)) {
        Files.delete(watchFile);
        log.info("🗑️ 已删除中转站文件: {}", watchFile);
    }
    
    // 完成
    pushProgress(documentId, "COMPLETED", 100, "处理完成！", ...);
}
```

### 2. 实现真实的RAG处理 ⭐⭐

当前是模拟实现，需要调用实际服务：

```java
// 文本提取
DocumentProcessor processor = documentProcessorManager.getProcessor(filename);
ProcessingResult result = processor.process(content, filename);

// 分块
List<Chunk> chunks = chunkingStrategyManager.chunk(
    documentId, 
    result.getText(), 
    docConfig.getChunkingStrategy()
);

// 保存分块
storageService.saveChunks(documentId, chunks);

// 索引
ragService.indexDocument(document);
```

### 3. 添加文件管理API ⭐

```java
// 从中转站移动到存储服务
POST /api/documents/move-to-storage
{
    "filename": "文件.pptx"
}

// 清理中转站
DELETE /api/documents/cleanup-staging
```

---

## 📊 完整的文件生命周期

```
┌─────────────┐
│  用户上传    │
└──────┬──────┘
       ↓
┌──────────────────────────┐
│ 中转站（./data/documents）│
│  - 文件.pptx              │
└──────┬───────────────────┘
       ↓
┌──────────────────────────┐
│   RAG处理（异步）         │
│  1. 文本提取              │
│  2. 智能分块              │
│  3. 向量化                │
│  4. 建立索引              │
└──────┬───────────────────┘
       ↓
┌──────────────────────────┐
│ 存储服务（虚拟路径系统）   │
│ documents/文件.pptx       │
│  - File/MongoDB/S3...     │
└──────┬───────────────────┘
       ↓
┌──────────────────────────┐
│  删除中转站文件           │
│  ./data/documents 清空    │
└──────────────────────────┘
```

---

## ✅ 已修复的问题

### 1. 路径处理逻辑 ✅

**修复内容**:
- `DocumentBrowseController` 所有方法
- 智能判断路径是否已包含前缀
- 避免重复添加 `documents/`

**修复文件**:
- `DocumentBrowseController.java`
  - `listFiles()`
  - `downloadFile()`
  - `deleteFileOrFolder()`
  - `createFolder()`

### 2. 上传流程保留中转站 ✅

**修复内容**:
- 恢复中转站模式
- 文件先保存到 `./data/documents`
- 触发异步RAG处理

**修复文件**:
- `DocumentManagementController.uploadDocument()`

---

## 🎯 总结

### 当前状态

- ✅ 路径处理逻辑已修复
- ✅ 上传使用中转站模式
- ✅ 下载、删除支持虚拟路径
- ⚠️ RAG处理后未存入存储服务
- ⚠️ 中转站文件未清理

### 下一步工作

1. **优先级1**: 实现RAG完成后保存到存储服务
2. **优先级2**: 实现中转站文件清理
3. **优先级3**: 替换模拟实现为真实RAG处理

---

**更新时间**: 2025-12-22 19:38  
**状态**: ✅ 路径修复完成，存储流程待完善  
**编译**: ✅ BUILD SUCCESS

