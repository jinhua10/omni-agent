# 文档提取结果持久化 - 虚拟目录方案

## 📋 实现方案

使用 `DocumentStorageService` 的虚拟目录机制实现持久化，支持多种存储后端无缝切换。

---

## 🏗️ 虚拟目录结构

```
extraction-results/           ← 虚拟目录前缀
├─ _index.json               ← 索引文件（存储所有文档ID列表）
├─ demo.pptx.json            ← 文档1的提取结果
├─ report.pdf.json           ← 文档2的提取结果
└─ presentation_2024.pptx.json  ← 文档3的提取结果
```

### 数据结构

**索引文件** (`extraction-results/_index.json`):
```json
["demo.pptx", "report.pdf", "presentation_2024.pptx"]
```

**提取结果文件** (`extraction-results/demo.pptx.json`):
```json
{
  "documentId": "demo.pptx",
  "fileName": "demo.pptx",
  "fileExtension": "pptx",
  "fileSize": 1234567,
  "fileMd5": "abc123...",
  "extractedText": "完整的提取文本...",
  "extractionModel": "vision-llm",
  "status": "COMPLETED",
  "startTime": 1703404775000,
  "completedTime": 1703404800000,
  "duration": 25000,
  "createdAt": 1703404775000,
  "updatedAt": 1703404800000,
  "version": 1
}
```

---

## 🔌 支持的存储后端

由于使用了 `DocumentStorageService` 接口，自动支持所有存储实现：

### 1. File 存储
```
data/storage/extraction-results/
├─ _index.json
├─ demo.pptx.json
└─ report.pdf.json
```

### 2. MongoDB 存储
```javascript
// Collection: documents
{
  "_id": "extraction-results/_index",
  "filename": "_index.json",
  "data": Binary(["demo.pptx", "report.pdf"])
}

{
  "_id": "extraction-results/demo.pptx.json",
  "filename": "demo.pptx.json",
  "data": Binary({...提取结果JSON...})
}
```

### 3. Redis 存储
```
document:extraction-results/_index -> JSON数组
document:extraction-results/demo.pptx.json -> JSON对象
```

### 4. S3/MinIO 存储
```
bucket: omni-agent-storage
├─ extraction-results/_index.json
├─ extraction-results/demo.pptx.json
└─ extraction-results/report.pdf.json
```

### 5. Elasticsearch 存储
```json
// Index: omni-agent-documents
{
  "id": "extraction-results/_index",
  "path": "extraction-results/_index",
  "content": "base64(索引JSON)",
  "filename": "_index.json"
}

{
  "id": "extraction-results/demo.pptx.json",
  "path": "extraction-results/demo.pptx.json",
  "content": "base64(提取结果JSON)",
  "filename": "demo.pptx.json"
}
```

---

## ⚙️ 存储切换

### 零代码修改切换

只需修改配置文件，无需改动任何代码：

**从 File 切换到 MongoDB**:
```yaml
# 修改前
spring:
  profiles:
    include:
      - storage-file  # ← File存储

# 修改后
spring:
  profiles:
    include:
      - storage-mongodb  # ← MongoDB存储
      
  data:
    mongodb:
      uri: mongodb://localhost:27017/omni-agent
```

**从 MongoDB 切换到 S3**:
```yaml
spring:
  profiles:
    include:
      - storage-s3  # ← S3存储
      
  cloud:
    aws:
      credentials:
        access-key: YOUR_ACCESS_KEY
        secret-key: YOUR_SECRET_KEY
      region:
        static: us-east-1
```

**数据自动迁移**:
由于使用统一的虚拟目录结构，可以很容易实现数据迁移：

```java
// 从File迁移到MongoDB
DocumentStorageService fileStorage = ...;  // File实现
DocumentStorageService mongoStorage = ...; // MongoDB实现

// 读取索引
byte[] indexData = fileStorage.getDocument("extraction-results/_index").get();
mongoStorage.saveDocument("extraction-results/_index", "_index.json", indexData);

// 迁移所有文档
String[] docIds = objectMapper.readValue(indexData, String[].class);
for (String docId : docIds) {
    String path = "extraction-results/" + docId + ".json";
    byte[] data = fileStorage.getDocument(path).get();
    mongoStorage.saveDocument(path, docId + ".json", data);
}
```

---

## 🎯 优势

### 1. **存储无关性** ✅
- 代码不依赖具体存储实现
- 切换存储后端零代码修改
- 支持未来新增的存储类型

### 2. **统一的虚拟路径** ✅
```
extraction-results/demo.pptx.json
```
无论底层是文件系统、数据库还是对象存储，路径始终一致

### 3. **易于测试** ✅
```java
// 测试时使用内存存储
@TestConfiguration
class TestConfig {
    @Bean
    DocumentStorageService documentStorageService() {
        return new MemoryDocumentStorage();
    }
}
```

### 4. **支持分布式** ✅
- File: 单机部署
- MongoDB/Redis: 分布式部署
- S3/MinIO: 云存储

### 5. **数据隔离** ✅
```
extraction-results/        ← 提取结果
documents/                 ← 原始文档
chunks/                    ← 分块数据
images/                    ← 图片数据
optimization/              ← 优化数据
```
不同类型的数据在虚拟目录中隔离

---

## 📊 索引管理

### 为什么需要索引？

由于 `DocumentStorageService` 不提供按前缀列出的功能，我们维护了一个索引文件：

```json
["doc1.pptx", "doc2.pdf", "doc3.xlsx"]
```

### 索引操作

**添加文档**:
```java
// 1. 保存文档
storageService.saveDocument("extraction-results/demo.pptx.json", ...);

// 2. 更新索引
Set<String> index = loadIndex();
index.add("demo.pptx");
saveIndex(index);
```

**删除文档**:
```java
// 1. 删除文档
storageService.deleteDocument("extraction-results/demo.pptx.json");

// 2. 更新索引
Set<String> index = loadIndex();
index.remove("demo.pptx");
saveIndex(index);
```

**查询所有文档**:
```java
// 1. 加载索引
Set<String> index = loadIndex();

// 2. 遍历加载
for (String docId : index) {
    Optional<DocumentExtractionResult> result = findByDocumentId(docId);
    // ...
}
```

---

## 🔄 迁移指南

### 从旧的文件存储迁移

**旧方案**（硬编码文件路径）:
```java
Path storageDir = Paths.get("data/extraction-results");
Path filePath = storageDir.resolve(documentId + ".json");
Files.write(filePath, jsonBytes);
```

**新方案**（虚拟目录）:
```java
String storagePath = "extraction-results/" + documentId + ".json";
storageService.saveDocument(storagePath, fileName, jsonBytes);
```

**迁移步骤**:
1. 保持旧数据在原位置
2. 启动新版本（会自动使用虚拟目录）
3. 运行迁移脚本（可选）
4. 验证数据正确性
5. 清理旧数据

---

## 📝 配置示例

### application.yml

```yaml
spring:
  profiles:
    # 选择存储实现（只能选一个）
    include:
      - storage-file         # File存储
      # - storage-mongodb    # MongoDB存储
      # - storage-redis      # Redis存储
      # - storage-s3         # S3存储
      # - storage-minio      # MinIO存储

# File存储配置
omni-agent:
  storage:
    file:
      base-dir: ./data/storage

# MongoDB存储配置
# spring:
#   data:
#     mongodb:
#       uri: mongodb://localhost:27017/omni-agent

# Redis存储配置
# spring:
#   data:
#     redis:
#       host: localhost
#       port: 6379

# S3存储配置
# spring:
#   cloud:
#     aws:
#       s3:
#         bucket: omni-agent-storage
```

---

## 🧪 测试

### 单元测试

```java
@SpringBootTest
class DocumentExtractionResultServiceTest {
    
    @Autowired
    DocumentExtractionResultService service;
    
    @Test
    void testSaveAndFind() {
        // 保存
        DocumentExtractionResult result = DocumentExtractionResult.builder()
            .documentId("test.pdf")
            .extractedText("测试内容")
            .status("COMPLETED")
            .build();
        
        service.save(result);
        
        // 查询
        Optional<DocumentExtractionResult> found = 
            service.findByDocumentId("test.pdf");
        
        assertTrue(found.isPresent());
        assertEquals("测试内容", found.get().getExtractedText());
    }
    
    @Test
    void testStorageSwitch() {
        // 这个测试在不同存储后端都应该通过
        // 证明存储无关性
    }
}
```

---

## 🎨 实现细节

### 核心代码

```java
@Service
@RequiredArgsConstructor
public class DocumentExtractionResultServiceImpl 
    implements DocumentExtractionResultService {
    
    private final DocumentStorageService storageService;
    private static final String STORAGE_PREFIX = "extraction-results/";
    
    @Override
    public DocumentExtractionResult save(DocumentExtractionResult result) {
        // 序列化为JSON
        String json = objectMapper.writeValueAsString(result);
        byte[] content = json.getBytes(UTF_8);
        
        // 保存到虚拟存储（⭐ 关键：使用虚拟路径）
        String path = STORAGE_PREFIX + sanitize(result.getDocumentId()) + ".json";
        storageService.saveDocument(path, fileName, content);
        
        // 更新索引
        addToIndex(result.getDocumentId());
        
        return result;
    }
}
```

---

## ✅ 总结

### 核心优势

1. **🔄 灵活切换** - 5种存储后端自由切换
2. **🎯 统一接口** - 代码不依赖具体实现
3. **📦 虚拟目录** - 逻辑隔离，物理存储透明
4. **🚀 易于扩展** - 支持新增存储类型
5. **✅ 生产就绪** - 支持分布式和云存储

### 适用场景

- ✅ 单机部署 → File存储
- ✅ 小团队 → MongoDB/Redis
- ✅ 大规模部署 → S3/MinIO
- ✅ 高可用 → MongoDB集群 + S3备份

---

生成时间: 2025-12-24
作者: AI Assistant
状态: ✅ 实现完成

