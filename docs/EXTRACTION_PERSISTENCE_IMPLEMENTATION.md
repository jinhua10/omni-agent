# 文档提取结果持久化 - 实现说明

## 📋 架构层次

```
应用层 (Application)
  ↓ 使用
DocumentExtractionResultService (接口)
  ↓ 实现
DocumentExtractionResultServiceImpl (实现类)
  ↓ 依赖
DocumentStorageService (接口)
  ↓ 实现 (Starter提供)
FileDocumentStorage / MongoDocumentStorage / RedisDocumentStorage / ...
```

---

## 🔌 各层职责

### 1. 接口层 - DocumentExtractionResultService
**位置**: `omni-agent-core/.../DocumentExtractionResultService.java`

**职责**: 定义文档提取结果管理的API
```java
public interface DocumentExtractionResultService {
    DocumentExtractionResult save(DocumentExtractionResult result);
    Optional<DocumentExtractionResult> findByDocumentId(String documentId);
    void delete(String documentId);
    List<DocumentExtractionResult> findAll();
    // ...
}
```

---

### 2. 实现层 - DocumentExtractionResultServiceImpl
**位置**: `omni-agent-core/.../DocumentExtractionResultServiceImpl.java`

**职责**: 
- 实现业务逻辑（版本管理、时间戳、索引维护）
- 使用 `DocumentStorageService` 存储数据
- 与具体存储解耦

**关键代码**:
```java
@Service
@RequiredArgsConstructor
public class DocumentExtractionResultServiceImpl 
    implements DocumentExtractionResultService {
    
    // ⭐ 依赖注入 DocumentStorageService
    private final DocumentStorageService storageService;
    
    @Override
    public DocumentExtractionResult save(DocumentExtractionResult result) {
        // 业务逻辑：设置时间戳、版本号等
        result.setUpdatedAt(System.currentTimeMillis());
        result.setVersion(result.getVersion() + 1);
        
        // 序列化
        String json = objectMapper.writeValueAsString(result);
        
        // ⭐ 使用 DocumentStorageService 保存
        // 具体存储到哪里由 Starter 决定
        storageService.saveDocument(
            "extraction-results/" + documentId + ".json",
            fileName,
            json.getBytes()
        );
    }
}
```

---

### 3. 存储服务接口 - DocumentStorageService
**位置**: `omni-agent-document-storage-api/.../DocumentStorageService.java`

**职责**: 定义存储操作的统一接口
```java
public interface DocumentStorageService {
    String saveDocument(String documentId, String filename, byte[] fileData);
    Optional<byte[]> getDocument(String documentId);
    void deleteDocument(String documentId);
    // ...
}
```

---

### 4. 存储实现层 - Starter 提供

#### 4.1 File Starter
**位置**: `omni-agent-document-storage-starter-file`

**实现类**: `FileDocumentStorage`
```java
@Service
public class FileDocumentStorage implements DocumentStorageService {
    
    @Value("${omni-agent.storage.file.base-dir:./data/storage}")
    private String baseDir;
    
    @Override
    public String saveDocument(String documentId, String filename, byte[] data) {
        Path filePath = Paths.get(baseDir, documentId);
        Files.write(filePath, data);
        return documentId;
    }
    
    @Override
    public Optional<byte[]> getDocument(String documentId) {
        Path filePath = Paths.get(baseDir, documentId);
        if (Files.exists(filePath)) {
            return Optional.of(Files.readAllBytes(filePath));
        }
        return Optional.empty();
    }
}
```

**存储位置**:
```
data/storage/
└─ extraction-results/
   ├─ _index.json
   ├─ demo.pptx.json
   └─ report.pdf.json
```

---

#### 4.2 MongoDB Starter
**位置**: `omni-agent-document-storage-starter-mongodb`

**实现类**: `MongoDocumentStorage`
```java
@Service
public class MongoDocumentStorage implements DocumentStorageService {
    
    private final MongoTemplate mongoTemplate;
    
    @Override
    public String saveDocument(String documentId, String filename, byte[] data) {
        DocumentEntity entity = new DocumentEntity();
        entity.setId(documentId);
        entity.setFilename(filename);
        entity.setData(data);
        entity.setCreatedAt(new Date());
        
        mongoTemplate.save(entity, "documents");
        return documentId;
    }
    
    @Override
    public Optional<byte[]> getDocument(String documentId) {
        DocumentEntity entity = mongoTemplate.findById(documentId, 
            DocumentEntity.class, "documents");
        return Optional.ofNullable(entity).map(DocumentEntity::getData);
    }
}
```

**存储位置**:
```javascript
// MongoDB Collection: documents
{
  "_id": "extraction-results/_index",
  "filename": "_index.json",
  "data": Binary(["demo.pptx", "report.pdf"]),
  "createdAt": ISODate("2024-12-24T...")
}

{
  "_id": "extraction-results/demo.pptx.json",
  "filename": "demo.pptx.json",
  "data": Binary({documentId: "demo.pptx", ...}),
  "createdAt": ISODate("2024-12-24T...")
}
```

---

#### 4.3 Redis Starter
**位置**: `omni-agent-document-storage-starter-redis`

**实现类**: `RedisDocumentStorage`
```java
@Service
public class RedisDocumentStorage implements DocumentStorageService {
    
    private final RedisTemplate<String, byte[]> redisTemplate;
    
    @Override
    public String saveDocument(String documentId, String filename, byte[] data) {
        String key = "document:" + documentId;
        redisTemplate.opsForValue().set(key, data);
        return documentId;
    }
    
    @Override
    public Optional<byte[]> getDocument(String documentId) {
        String key = "document:" + documentId;
        byte[] data = redisTemplate.opsForValue().get(key);
        return Optional.ofNullable(data);
    }
}
```

**存储位置**:
```
Redis Keys:
document:extraction-results/_index -> Binary JSON Array
document:extraction-results/demo.pptx.json -> Binary JSON Object
```

---

#### 4.4 S3 Starter
**位置**: `omni-agent-document-storage-starter-s3`

**实现类**: `S3DocumentStorage`
```java
@Service
public class S3DocumentStorage implements DocumentStorageService {
    
    private final AmazonS3 s3Client;
    
    @Value("${omni-agent.storage.s3.bucket}")
    private String bucket;
    
    @Override
    public String saveDocument(String documentId, String filename, byte[] data) {
        ObjectMetadata metadata = new ObjectMetadata();
        metadata.setContentLength(data.length);
        
        s3Client.putObject(bucket, documentId, 
            new ByteArrayInputStream(data), metadata);
        return documentId;
    }
    
    @Override
    public Optional<byte[]> getDocument(String documentId) {
        try {
            S3Object object = s3Client.getObject(bucket, documentId);
            byte[] data = IOUtils.toByteArray(object.getObjectContent());
            return Optional.of(data);
        } catch (AmazonS3Exception e) {
            return Optional.empty();
        }
    }
}
```

**存储位置**:
```
S3 Bucket: omni-agent-storage
├─ extraction-results/_index.json
├─ extraction-results/demo.pptx.json
└─ extraction-results/report.pdf.json
```

---

## 🔧 Spring Boot 自动配置

### Starter 如何被激活

#### 1. 引入依赖
```xml
<!-- pom.xml -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-document-storage-starter-file</artifactId>
    <version>${project.version}</version>
</dependency>
```

#### 2. 自动配置类
每个 Starter 都有自动配置类：

**File Starter**:
```java
// META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports
top.yumbo.ai.storage.file.FileDocumentStorageAutoConfiguration

// FileDocumentStorageAutoConfiguration.java
@Configuration
@ConditionalOnMissingBean(DocumentStorageService.class)
public class FileDocumentStorageAutoConfiguration {
    
    @Bean
    public DocumentStorageService documentStorageService(
            @Value("${omni-agent.storage.file.base-dir:./data/storage}") 
            String baseDir) {
        return new FileDocumentStorage(baseDir);
    }
}
```

#### 3. Spring Boot 自动装配
```
应用启动
  ↓
扫描 META-INF/spring/*.imports
  ↓
发现 FileDocumentStorageAutoConfiguration
  ↓
检查条件 @ConditionalOnMissingBean
  ↓
创建 FileDocumentStorage Bean
  ↓
注入到 DocumentExtractionResultServiceImpl
```

---

## 🎯 依赖注入流程

### 完整的依赖链

```
DocumentProcessingController
  ↓ @Autowired
DocumentExtractionResultService
  ↓ 实际注入
DocumentExtractionResultServiceImpl
  ↓ @Autowired (构造器注入)
DocumentStorageService
  ↓ 实际注入 (由 Starter 提供)
FileDocumentStorage (或 MongoDocumentStorage, RedisDocumentStorage...)
```

### Spring 容器中的 Bean

```
ApplicationContext:
  ├─ documentExtractionResultService -> DocumentExtractionResultServiceImpl
  ├─ documentStorageService -> FileDocumentStorage (由 File Starter 提供)
  └─ documentProcessingController -> DocumentProcessingController
```

---

## 📦 example-basic 项目的配置

### pom.xml 依赖
```xml
<!-- Core模块（包含 DocumentExtractionResultServiceImpl） -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-core</artifactId>
</dependency>

<!-- DocumentStorage Starter（提供 DocumentStorageService 实现） -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-document-storage-starter-file</artifactId>
</dependency>
```

### application.yml 配置
```yaml
omni-agent:
  storage:
    file:
      base-dir: ./data/storage  # File存储的基础目录
```

### 启动时的Bean注册
```
1. FileDocumentStorageAutoConfiguration 注册 FileDocumentStorage
2. DocumentExtractionResultServiceImpl 注入 FileDocumentStorage
3. DocumentProcessingController 注入 DocumentExtractionResultServiceImpl
```

---

## 🔄 切换存储实现

### 从 File 切换到 MongoDB

**步骤1**: 修改依赖
```xml
<!-- 移除 File Starter -->
<!--
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-document-storage-starter-file</artifactId>
</dependency>
-->

<!-- 添加 MongoDB Starter -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-document-storage-starter-mongodb</artifactId>
</dependency>
```

**步骤2**: 修改配置
```yaml
# 移除 File 配置
# omni-agent:
#   storage:
#     file:
#       base-dir: ./data/storage

# 添加 MongoDB 配置
spring:
  data:
    mongodb:
      uri: mongodb://localhost:27017/omni-agent
```

**步骤3**: 重启应用
```bash
# DocumentStorageService 的实现自动切换为 MongoDocumentStorage
# DocumentExtractionResultServiceImpl 的代码无需任何修改！
```

---

## ✅ 总结

### 已实现的内容

1. **✅ 接口定义** - `DocumentExtractionResultService`
2. **✅ 业务实现** - `DocumentExtractionResultServiceImpl`
3. **✅ 存储抽象** - `DocumentStorageService`
4. **✅ 存储实现** - 6个Starter（File/MongoDB/Redis/S3/MinIO/Elasticsearch）

### 自动工作的原因

1. **Spring Boot 自动配置** - Starter 自动注册 Bean
2. **依赖注入** - Spring 自动装配依赖
3. **接口解耦** - 业务逻辑不依赖具体实现

### 用户需要做的

1. **引入依赖** - 添加想要的 Starter 到 pom.xml
2. **配置参数** - 在 application.yml 中配置连接信息
3. **启动应用** - Spring Boot 自动完成其余工作

---

## 📝 验证

### 检查Bean是否注册

```bash
# 启动应用后，访问actuator端点（如果启用）
curl http://localhost:3000/actuator/beans | jq '.contexts.application.beans | 
  with_entries(select(.key | contains("document")))'
```

**预期输出**:
```json
{
  "documentStorageService": {
    "type": "top.yumbo.ai.storage.file.FileDocumentStorage"
  },
  "documentExtractionResultService": {
    "type": "top.yumbo.ai.omni.core.document.service.impl.DocumentExtractionResultServiceImpl"
  }
}
```

### 验证存储

```bash
# 提取一个文档
curl -X POST http://localhost:3000/api/documents/processing/test.pdf/extract \
  -H "Content-Type: application/json" \
  -d '{"model": "vision-llm"}'

# 检查文件系统（File Starter）
ls -la data/storage/extraction-results/
# 应该看到：
# _index.json
# test.pdf.json
```

---

生成时间: 2025-12-24
状态: ✅ 完整实现
依赖: 已有的 DocumentStorage Starters

