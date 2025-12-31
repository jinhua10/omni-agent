# omni-agent-document-storage-api

## 📋 模块概述

文档存储服务API模块，提供文档、图像、分块等非结构化数据的存储管理接口。

### 职责说明

本模块负责**业务数据和内容**的存储管理，包括：
- ✅ 原始文档文件（PDF, PPT, Word等）
- ✅ 提取的文本内容（可能很大）
- ✅ 文档分块和图像
- ✅ RAG优化分析数据
- ✅ 大数据量（MB-GB级别），简单CRUD操作

### 核心接口

- **DocumentStorageService** - 文档存储服务核心接口

### 依赖关系

```
omni-agent-document-storage-api
├── depends on: omni-agent-common
└── depends on: omni-agent-chunking-api (Chunk模型)
```

## 🚀 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-document-storage-api</artifactId>
    <version>${omni-agent.version}</version>
</dependency>
```

### 2. 基本使用示例

#### 保存和获取文档

```java
@Autowired
private DocumentStorageService storageService;

// 保存文档
String documentId = "doc123";
byte[] fileData = Files.readAllBytes(Path.of("document.pdf"));
String storageId = storageService.saveDocument(documentId, "document.pdf", fileData);

// 获取文档
Optional<byte[]> data = storageService.getDocument(documentId);
if (data.isPresent()) {
    Files.write(Path.of("output.pdf"), data.get());
}

// 删除文档
storageService.deleteDocument(documentId);
```

#### 流式读取大文件 ⭐ NEW

```java
// 流式读取（避免内存溢出）
try (InputStream inputStream = storageService.getDocumentStream(documentId)) {
    // 处理流
    inputStream.transferTo(outputStream);
}

// 流式保存
try (InputStream fileStream = new FileInputStream("large-document.pdf")) {
    String id = storageService.saveDocumentStream(documentId, "large-document.pdf", fileStream);
}

// 复制到输出流
try (OutputStream output = new FileOutputStream("output.pdf")) {
    storageService.copyDocumentToStream(documentId, output);
}
```

#### 批量操作

```java
// 批量保存（非事务性）
List<Map<String, Object>> documents = Arrays.asList(
    Map.of("documentId", "doc1", "filename", "file1.pdf", "fileData", data1),
    Map.of("documentId", "doc2", "filename", "file2.pdf", "fileData", data2)
);
BatchOperationResult result = storageService.saveDocuments(documents);
System.out.println("成功: " + result.getSuccessCount() + ", 失败: " + result.getFailureCount());

// 批量保存（事务性） ⭐ NEW
try {
    BatchOperationResult txResult = storageService.saveDocumentsTransactional(documents);
    // 全部成功
} catch (BatchOperationException e) {
    // 有失败，已回滚
    System.out.println("操作失败: " + e.getMessage());
}

// 批量删除（事务性）
try {
    BatchOperationResult deleteResult = storageService.deleteDocumentsTransactional(
        Arrays.asList("doc1", "doc2", "doc3")
    );
} catch (BatchOperationException e) {
    // 删除失败，已恢复
}
```

#### 保存和获取提取的文本

```java
// 保存提取的文本
String text = "这是从PDF中提取的文本内容...";
storageService.saveExtractedText(documentId, text);

// 获取提取的文本
Optional<String> extractedText = storageService.getExtractedText(documentId);

// 流式处理大文本 ⭐ NEW
try (InputStream textStream = storageService.getExtractedTextStream(documentId)) {
    BufferedReader reader = new BufferedReader(
        new InputStreamReader(textStream, StandardCharsets.UTF_8)
    );
    String line;
    while ((line = reader.readLine()) != null) {
        // 逐行处理
    }
}
```

#### 分块存储

```java
// 保存单个分块
Chunk chunk = Chunk.builder()
    .id("chunk1")
    .documentId(documentId)
    .content("这是第一个分块的内容")
    .sequence(1)
    .metadata(Map.of("page", 1))
    .build();
String chunkId = storageService.saveChunk(documentId, chunk);

// 批量保存分块
List<Chunk> chunks = Arrays.asList(chunk1, chunk2, chunk3);
List<String> chunkIds = storageService.saveChunks(documentId, chunks);

// 获取文档所有分块
List<Chunk> allChunks = storageService.getChunksByDocument(documentId);

// 删除分块
storageService.deleteChunk(chunkId);
storageService.deleteChunksByDocument(documentId);
```

#### 图像存储

```java
// 保存图像
Image image = Image.builder()
    .id("img1")
    .documentId(documentId)
    .imageData(imageBytes)
    .format("PNG")
    .page(1)
    .build();
String imageId = storageService.saveImage(documentId, image);

// 批量保存图像
List<Image> images = Arrays.asList(image1, image2, image3);
List<String> imageIds = storageService.saveImages(documentId, images);

// 获取文档所有图像
List<Image> allImages = storageService.getImagesByDocument(documentId);

// 图像去重（通过哈希）
Optional<String> existingImageId = storageService.findImageByHash(imageHash);
if (existingImageId.isPresent()) {
    // 图像已存在，使用已有的
} else {
    // 保存新图像
}
```

#### 元数据管理

```java
// 保存元数据
DocumentMetadata metadata = DocumentMetadata.builder()
    .documentId(documentId)
    .filename("report.pdf")
    .fileSize(1024000L)
    .mimeType("application/pdf")
    .uploadTime(LocalDateTime.now())
    .build();
storageService.saveMetadata(metadata);

// 获取元数据
Optional<DocumentMetadata> meta = storageService.getMetadata(documentId);

// 分页查询 ⭐ NEW
PageRequest pageRequest = PageRequest.of(0, 20);
PageResult<DocumentMetadata> page = storageService.getAllMetadata(pageRequest);

// 搜索元数据
PageResult<DocumentMetadata> searchResult = storageService.searchMetadata("report", pageRequest);
```

#### RAG优化数据存储

```java
// 保存优化数据
OptimizationData pplData = OptimizationData.builder()
    .documentId(documentId)
    .optimizationType(OptimizationType.PPL)
    .data(Map.of("perplexity", 2.5))
    .timestamp(LocalDateTime.now())
    .build();
storageService.saveOptimizationData(documentId, pplData);

// 获取特定类型的优化数据
Optional<OptimizationData> data = storageService.getOptimizationData(documentId, "ppl");

// 获取所有优化数据
List<OptimizationData> allOptData = storageService.getAllOptimizationData(documentId);
```

#### 文档管理

```java
// 分页列出文档
PageRequest pageRequest = PageRequest.of(0, 20);
PageResult<DocumentMetadata> documents = storageService.listDocuments(pageRequest);

// 搜索文档
List<DocumentMetadata> searchResults = storageService.searchDocuments("关键词");

// 获取文档总数
long count = storageService.getDocumentCount();

// 检查文档是否存在
boolean exists = storageService.documentExists(documentId);

// 批量检查存在性
Map<String, List<String>> result = storageService.checkDocumentsExist(
    Arrays.asList("doc1", "doc2", "doc3")
);
List<String> existing = result.get("existing");
List<String> missing = result.get("missing");

// 获取文档大小
long size = storageService.getDocumentSize(documentId);

// 清理文档相关数据
storageService.cleanupDocument(documentId); // 删除分块、图像、优化数据等

// 批量清理
BatchOperationResult cleanupResult = storageService.cleanupDocuments(
    Arrays.asList("doc1", "doc2")
);
```

#### 统计和健康检查

```java
// 获取存储统计
StorageStatistics stats = storageService.getStatistics();
System.out.println("文档总数: " + stats.getTotalDocuments());
System.out.println("总大小: " + stats.getTotalSize() + " bytes");

// 健康检查
boolean healthy = storageService.isHealthy();
```

### 3. 异常处理 ⭐ NEW

```java
try {
    byte[] data = storageService.getDocument(documentId)
        .orElseThrow(() -> new DocumentNotFoundException(documentId));
} catch (DocumentNotFoundException e) {
    System.err.println("文档不存在: " + e.getDocumentId());
} catch (StorageIOException e) {
    System.err.println("IO错误: " + e.getMessage());
} catch (StorageQuotaExceededException e) {
    System.err.println("存储空间不足: " + e.getRequestedSize() + " > " + e.getAvailableSize());
} catch (StorageException e) {
    System.err.println("存储错误 [" + e.getErrorCode() + "]: " + e.getMessage());
}

// 批量操作异常处理
try {
    BatchOperationResult result = storageService.saveDocumentsTransactional(documents);
} catch (BatchOperationException e) {
    System.err.println("批量操作失败: " + e.getMessage());
    System.err.println("失败的ID: " + e.getFailureIds());
    e.getErrorMessages().forEach((id, msg) -> 
        System.err.println("  " + id + ": " + msg)
    );
}
```

## 📦 接口说明

### DocumentStorageService

文档存储服务核心接口，提供以下功能模块：

#### 1. 原始文档存储
- `saveDocument()` - 保存文档
- `saveDocuments()` - 批量保存（非事务）
- `saveDocumentsTransactional()` - 批量保存（事务性） ⭐ NEW
- `getDocument()` - 获取文档
- `getDocumentStream()` - 流式读取 ⭐ NEW
- `saveDocumentStream()` - 流式保存 ⭐ NEW
- `copyDocumentToStream()` - 复制到输出流 ⭐ NEW
- `deleteDocument()` - 删除文档
- `deleteDocuments()` - 批量删除（非事务）
- `deleteDocumentsTransactional()` - 批量删除（事务性） ⭐ NEW

#### 2. 提取文本存储
- `saveExtractedText()` - 保存提取的文本
- `getExtractedText()` - 获取提取的文本
- `getExtractedTextStream()` - 流式读取文本 ⭐ NEW
- `saveExtractedTextStream()` - 流式保存文本 ⭐ NEW
- `deleteExtractedText()` - 删除提取的文本

#### 3. 分块存储
- `saveChunk()` - 保存分块
- `saveChunks()` - 批量保存分块
- `getChunk()` - 获取分块
- `getChunksByDocument()` - 获取文档所有分块
- `deleteChunk()` - 删除分块
- `deleteChunksByDocument()` - 删除文档所有分块

#### 4. 图像存储
- `saveImage()` - 保存图像
- `saveImages()` - 批量保存图像
- `getImage()` - 获取图像
- `getImagesByDocument()` - 获取文档所有图像
- `deleteImage()` - 删除图像
- `deleteImagesByDocument()` - 删除文档所有图像
- `findImageByHash()` - 通过哈希查找图像（去重）

#### 5. 元数据管理
- `saveMetadata()` - 保存元数据
- `getMetadata()` - 获取元数据
- `getAllMetadata()` - 分页查询所有元数据 ⭐ NEW
- `searchMetadata()` - 搜索元数据 ⭐ NEW
- `deleteMetadata()` - 删除元数据
- `deleteMetadataBatch()` - 批量删除元数据

#### 6. RAG优化数据存储
- `saveOptimizationData()` - 保存优化数据
- `getOptimizationData()` - 获取优化数据
- `getAllOptimizationData()` - 获取所有优化数据
- `deleteOptimizationData()` - 删除优化数据
- `deleteAllOptimizationData()` - 删除所有优化数据

#### 7. 文档管理
- `listDocuments()` - 分页列出文档
- `searchDocuments()` - 搜索文档
- `getDocumentCount()` - 获取文档总数
- `documentExists()` - 检查文档是否存在
- `checkDocumentsExist()` - 批量检查存在性 ⭐ NEW
- `getDocumentSize()` - 获取文档大小
- `cleanupDocument()` - 清理文档相关数据
- `cleanupDocuments()` - 批量清理 ⭐ NEW

#### 8. 统计和健康检查
- `getStatistics()` - 获取统计信息
- `isHealthy()` - 健康检查

#### 9. 文件系统浏览
- `listFiles()` - 列出文件和文件夹
- `readFile()` - 读取文件
- `deleteFile()` - 删除文件
- `createDirectory()` - 创建目录
- `getStorageStats()` - 获取存储统计

## 🎯 数据模型

### DocumentMetadata
文档元数据，包含文档的基本信息。

### Image
图像模型，包含图像数据和元信息。

### Chunk
分块模型（来自chunking-api），包含分块内容和序号。

### OptimizationData
RAG优化数据模型，支持多种优化类型（PPL, HyDE, Rerank等）。

### BatchOperationResult
批量操作结果，包含成功/失败统计和详细信息。

### PageRequest & PageResult
分页请求和结果模型。

### StorageStatistics
存储统计信息。

## 🔒 异常体系 ⭐ NEW

### StorageException
存储服务异常基类，所有存储相关异常的父类。

**属性:**
- `errorCode` - 错误代码
- `documentId` - 文档ID（如适用）

### DocumentNotFoundException
文档未找到异常，继承自 `StorageException`。

**错误代码:** `DOCUMENT_NOT_FOUND`

### StorageIOException
存储IO异常，用于封装IO错误。

**错误代码:** `STORAGE_IO_ERROR`

### BatchOperationException
批量操作异常，包含失败详情。

**错误代码:** `BATCH_OPERATION_ERROR`

**属性:**
- `successIds` - 成功的ID列表
- `failureIds` - 失败的ID列表
- `errorMessages` - 错误消息映射

### StorageQuotaExceededException
存储空间不足异常。

**错误代码:** `STORAGE_QUOTA_EXCEEDED`

**属性:**
- `requestedSize` - 请求的大小
- `availableSize` - 可用的大小

## 🏗️ 与 Persistence 层的区别

| 特性 | Storage (本接口) | Persistence |
|------|-----------------|-------------|
| 数据类型 | 非结构化内容 | 结构化配置 |
| 数据量 | 大（MB-GB） | 小（KB） |
| 用途 | 业务数据 | 系统配置 |
| 类比 | 图书馆"书架" | 图书馆"目录" |
| 操作 | 简单CRUD | 复杂查询 |
| 后端 | File, MongoDB, S3, MinIO | Database, Config Files |

## 💡 最佳实践

### 1. 使用流式API处理大文件

```java
// ❌ 不推荐：会导致内存溢出
byte[] largeFile = storageService.getDocument(documentId).orElseThrow();

// ✅ 推荐：使用流式读取
try (InputStream stream = storageService.getDocumentStream(documentId)) {
    // 处理流
}
```

### 2. 使用事务性批量操作

```java
// ❌ 非事务性：部分成功部分失败
BatchOperationResult result = storageService.saveDocuments(documents);

// ✅ 事务性：要么全部成功，要么全部回滚
try {
    BatchOperationResult result = storageService.saveDocumentsTransactional(documents);
} catch (BatchOperationException e) {
    // 处理失败
}
```

### 3. 使用分页查询避免OOM

```java
// ❌ 不推荐：一次性加载所有数据
List<DocumentMetadata> all = storageService.getAllMetadata();

// ✅ 推荐：使用分页
PageRequest pageRequest = PageRequest.of(0, 100);
PageResult<DocumentMetadata> page = storageService.getAllMetadata(pageRequest);
```

### 4. 正确处理异常

```java
try {
    String id = storageService.saveDocument(documentId, filename, data);
} catch (StorageQuotaExceededException e) {
    // 存储空间不足，提示用户清理
} catch (StorageIOException e) {
    // IO错误，记录日志并重试
} catch (StorageException e) {
    // 其他存储错误
    log.error("Storage error [{}]: {}", e.getErrorCode(), e.getMessage());
}
```

### 5. 及时清理文档数据

```java
// 删除文档时清理所有相关数据
storageService.cleanupDocument(documentId);

// 而不是手动逐个删除
storageService.deleteDocument(documentId);
storageService.deleteChunksByDocument(documentId);
storageService.deleteImagesByDocument(documentId);
// ...
```

## 🔧 实现建议

实现 `DocumentStorageService` 接口时的建议：

### 1. 流式方法实现
优先实现流式版本的方法（`*Stream()`），因为默认实现会将数据全部加载到内存。

### 2. 事务性批量操作
对于支持事务的后端（如数据库），应该覆盖 `*Transactional()` 方法以提供真正的事务支持。

### 3. 异步支持
考虑提供异步版本的方法，返回 `CompletableFuture` 或使用 `@Async` 注解。

### 4. 缓存策略
对于频繁访问的小文件元数据，考虑使用缓存。

### 5. 监控和日志
记录所有操作的耗时、成功率等指标，便于性能优化。

## 📝 版本历史

### v1.0.0
- 初始版本
- 基本的CRUD操作
- 批量操作支持
- PPL数据存储（已废弃）

### v1.1.0 ⭐ NEW
- ✅ 添加流式读写API
- ✅ 添加事务性批量操作
- ✅ 完善异常体系
- ✅ 优化数据存储
- ✅ 增强元数据管理
- ✅ 添加分页查询

## 🤝 贡献

欢迎提交Issue和Pull Request！

## 📄 许可证

Apache License 2.0

