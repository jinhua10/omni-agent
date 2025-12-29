# ✅ 文档存储零依赖架构设计说明

## 🎯 设计目标

实现一个**零依赖、可扩展**的文档存储架构：
1. **File 存储作为兜底** - 无需任何外部依赖
2. **可选依赖支持** - MongoDB、Redis、S3、MinIO、Elasticsearch
3. **多实例支持** - 可以同时使用多种存储方式
4. **自动降级** - 依赖不存在时自动使用 File 存储

## 📋 当前实现

### 1. DocumentStorageAutoConfiguration.java ✅

```java
@Bean
public Map<String, DocumentStorageService> documentStorageServices(
        DocumentStorageProperties properties,
        ObjectProvider<Object> mongoTemplate,    // ✅ 使用 Object，避免 ClassNotFoundException
        ObjectProvider<Object> redisTemplate,    // ✅
        ObjectProvider<Object> s3Client,         // ✅
        ObjectProvider<Object> minioClient,      // ✅
        ObjectProvider<Object> elasticsearchClient) { // ✅
    
    // 创建每个实例
    for (StorageInstanceConfig config : instances) {
        try {
            DocumentStorageService service = new DocumentStorageInstanceBuilder(config)
                    .withMongoTemplate(mongoTemplate.getIfAvailable())  // 如果不存在，返回 null
                    .withRedisTemplate(redisTemplate.getIfAvailable())
                    .withS3Client(s3Client.getIfAvailable())
                    .withMinioClient(minioClient.getIfAvailable())
                    .withElasticsearchClient(elasticsearchClient.getIfAvailable())
                    .build();
            
            services.put(instanceId, service);
        } catch (Exception e) {
            // 创建失败，降级为 File 存储
            log.error("❌ 实例创建失败，使用 File 存储降级", e);
            services.put(instanceId, new FileDocumentStorage("data/documents"));
        }
    }
}
```

**关键点**：
- ✅ 使用 `ObjectProvider<Object>` 而不是具体类型
- ✅ `getIfAvailable()` 返回 null 而不是抛异常
- ✅ 异常处理降级为 File 存储

### 2. DocumentStorageInstanceBuilder.java ✅

```java
public class DocumentStorageInstanceBuilder {
    private Object mongoTemplate;    // ✅ 使用 Object
    private Object redisTemplate;    // ✅
    private Object s3Client;         // ✅
    private Object minioClient;      // ✅
    private Object elasticsearchClient; // ✅
    
    public DocumentStorageService build() {
        String type = config.getType().toLowerCase();
        
        return switch (type) {
            case "file" -> buildFileStorage();           // ✅ 无需任何依赖
            case "mongodb" -> buildMongoDBStorage();     // 需要 MongoTemplate
            case "redis" -> buildRedisStorage();         // 需要 RedisTemplate
            case "s3" -> buildS3Storage();               // 需要 S3Client
            case "minio" -> buildMinIOStorage();         // 需要 MinioClient
            case "elasticsearch" -> buildElasticsearchStorage(); // 需要 ElasticsearchClient
            default -> buildFileStorage();               // ✅ 降级为 File
        };
    }
    
    private DocumentStorageService buildMongoDBStorage() {
        if (mongoTemplate == null) {
            throw new IllegalStateException("MongoTemplate 未配置");
        }
        // 运行时类型转换
        MongoTemplate template = (MongoTemplate) mongoTemplate;
        return new MongoDBDocumentStorage(template, bucketName);
    }
}
```

**关键点**：
- ✅ 使用 `Object` 类型存储依赖
- ✅ 在运行时进行类型检查和转换
- ✅ 依赖不存在时抛出清晰的异常

## 🔍 工作流程

### 场景 1: 零依赖启动（只有 File）

```yaml
# application.yml
omni-agent:
  document-storage:
    # 不配置或留空
```

**启动流程**：
```
1. Spring 启动
2. DocumentStorageAutoConfiguration 初始化 (HIGHEST_PRECEDENCE)
3. 调用 documentStorageServices()
   ├── mongoTemplate.getIfAvailable() → null  ✅
   ├── redisTemplate.getIfAvailable() → null  ✅
   ├── s3Client.getIfAvailable() → null       ✅
   └── 所有可选依赖都是 null                  ✅
4. instances 为空，创建默认 File 实例
5. DocumentStorageInstanceBuilder(config)
   └── buildFileStorage() → FileDocumentStorage ✅
6. 成功创建 File 存储
7. 应用正常启动 ✅
```

### 场景 2: 有 MongoDB 依赖

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-mongodb</artifactId>
</dependency>
```

```yaml
omni-agent:
  document-storage:
    instances:
      - id: mongo-storage
        type: mongodb
        mongodb:
          database: my-docs
```

**启动流程**：
```
1. Spring 启动
2. MongoDB Auto-Configuration 创建 MongoTemplate Bean
3. DocumentStorageAutoConfiguration 初始化
4. 调用 documentStorageServices()
   ├── mongoTemplate.getIfAvailable() → MongoTemplate 实例 ✅
   ├── redisTemplate.getIfAvailable() → null
   └── 其他为 null
5. DocumentStorageInstanceBuilder(config)
   ├── type = "mongodb"
   ├── withMongoTemplate(mongoTemplate 实例)
   └── buildMongoDBStorage()
       ├── mongoTemplate != null ✅
       ├── 类型转换: (MongoTemplate) mongoTemplate
       └── 创建 MongoDBDocumentStorage ✅
6. 成功创建 MongoDB 存储
7. 应用正常启动 ✅
```

### 场景 3: 多实例混合

```yaml
omni-agent:
  document-storage:
    instances:
      - id: local
        type: file
        primary: true
      - id: cache
        type: redis
      - id: backup
        type: mongodb
```

**启动流程**：
```
1. Spring 启动
2. RedisTemplate、MongoTemplate Bean 创建（如果有依赖）
3. DocumentStorageAutoConfiguration 初始化
4. 创建 3 个实例：
   ├── local (File) → 成功 ✅
   ├── cache (Redis) → 如果有 RedisTemplate 则成功，否则降级 ✅
   └── backup (MongoDB) → 如果有 MongoTemplate 则成功，否则降级 ✅
5. 创建主存储服务（primary）
6. 创建注册表（管理多实例）
7. 应用正常启动 ✅
```

## ✅ 验证清单

### 编译验证
```bash
mvn clean compile
```
- ✅ 无编译错误
- ✅ 只有正常的警告（Spring Bean 方法）

### 零依赖测试
```bash
# 1. 移除所有可选依赖
# 只保留 omni-agent-document-storage-starter

# 2. 启动应用
mvn spring-boot:run

# 3. 预期结果
✅ 应用正常启动
✅ 创建默认 File 存储
✅ 日志: "📋 未配置实例，创建默认 File 实例"
✅ 日志: "✅ 创建 File 存储实例: data/documents"
```

### MongoDB 依赖测试
```bash
# 1. 添加 MongoDB 依赖
# 2. 配置 MongoDB 实例
# 3. 启动应用

# 4. 预期结果
✅ 应用正常启动
✅ 创建 MongoDB 存储
✅ 日志: "✅ 创建 MongoDB 存储实例，bucket: documents"
```

## 🔧 常见问题排查

### 问题 1: ClassNotFoundException

**症状**：
```
Caused by: java.lang.ClassNotFoundException: org.springframework.data.mongodb.core.MongoTemplate
```

**原因**：方法签名中使用了具体类型

**检查**：
```java
// ❌ 错误
ObjectProvider<MongoTemplate> mongoTemplate

// ✅ 正确
ObjectProvider<Object> mongoTemplate
```

### 问题 2: 实例创建失败

**症状**：
```
❌ 实例创建失败: id=mongodb-1, 使用 File 存储降级
```

**原因**：
- 配置了 MongoDB 类型，但没有 MongoDB 依赖
- MongoTemplate 为 null

**解决**：
1. 添加 MongoDB 依赖
2. 或者改用 File 存储
3. 或者依赖降级逻辑会自动处理

### 问题 3: 主存储服务找不到

**症状**：
```
Field documentStorage in KnowledgeNetworkBuilder required a bean of type 'DocumentStorageService'
```

**原因**：Bean 创建顺序问题

**解决**：
```java
@AutoConfigureOrder(Ordered.HIGHEST_PRECEDENCE)  // ✅ 已添加
```

## 📊 架构优势

### 1. 零依赖启动 ✅
- 无需任何外部服务
- File 存储开箱即用
- 适合开发和测试

### 2. 渐进式增强 ✅
- 从 File 开始
- 逐步添加 MongoDB、Redis 等
- 每个依赖都是可选的

### 3. 自动降级 ✅
- 依赖不存在 → File 存储
- 创建失败 → File 存储
- 保证应用不会因存储问题而无法启动

### 4. 多实例支持 ✅
- 同时使用多种存储
- File + MongoDB + Redis
- 适合复杂场景

## 🎉 总结

当前实现完全满足需求：

✅ **零依赖** - File 存储作为兜底，无需任何外部依赖  
✅ **可选依赖** - MongoDB、Redis、S3 等都是可选的  
✅ **多实例** - 支持同时使用多种存储方式  
✅ **自动降级** - 依赖不存在或创建失败时自动降级  
✅ **类型安全** - 使用 Object 避免 ClassNotFoundException  
✅ **优先级正确** - HIGHEST_PRECEDENCE 确保先于其他服务初始化  

**下一步**：
1. 启动应用验证
2. 测试零依赖场景
3. 测试多实例场景

---

**文档创建时间**: 2025-12-29  
**状态**: ✅ 架构设计完成，等待验证

