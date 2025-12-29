# RAG 自动配置类更新报告

## 📋 问题

采用统一的数组配置后，`RagAdapterAutoConfiguration` 存在可选依赖类加载问题：

```java
// ❌ 错误：直接使用具体类型会导致类加载失败
@Autowired(required = false)
private MongoTemplate mongoTemplate;  // 如果没有 MongoDB 依赖，启动失败

@Autowired(required = false)
private RedisTemplate<String, Object> redisTemplate;  // 如果没有 Redis 依赖，启动失败
```

## ✅ 解决方案

### 1. 使用 Object 类型 + Setter 注入

**RagAdapterAutoConfiguration.java**:
```java
// ✅ 正确：使用 Object 类型，避免类加载问题
private Object mongoTemplate;
private Object redisTemplate;
private Object elasticsearchClient;

@Autowired(required = false)
public void setMongoTemplate(Object mongoTemplate) {
    this.mongoTemplate = mongoTemplate;
}

@Autowired(required = false)
public void setRedisTemplate(Object redisTemplate) {
    this.redisTemplate = redisTemplate;
}

@Autowired(required = false)
public void setElasticsearchClient(Object elasticsearchClient) {
    this.elasticsearchClient = elasticsearchClient;
}
```

### 2. 更新 RagInstanceBuilder

**字段声明**:
```java
private Object mongoTemplate;  // 使用 Object
private Object redisTemplate;  // 使用 Object
private Object elasticsearchClient;  // 使用 Object
```

**方法签名**:
```java
public RagInstanceBuilder withMongoTemplate(Object mongoTemplate) {
    this.mongoTemplate = mongoTemplate;
    return this;
}

public RagInstanceBuilder withRedisTemplate(Object redisTemplate) {
    this.redisTemplate = redisTemplate;
    return this;
}

public RagInstanceBuilder withElasticsearchClient(Object elasticsearchClient) {
    this.elasticsearchClient = elasticsearchClient;
    return this;
}
```

### 3. 使用时进行类型转换

**MongoDB**:
```java
private RagService buildMongoDBRAG(String instanceId) {
    // 类型转换
    org.springframework.data.mongodb.core.MongoTemplate template = 
            (org.springframework.data.mongodb.core.MongoTemplate) mongoTemplate;
    
    MongoDBRAGService service = new MongoDBRAGService(template, props, instanceId);
    // ...
}
```

**Redis**:
```java
private RagService buildRedisRAG(String instanceId) {
    // 类型转换
    @SuppressWarnings("unchecked")
    org.springframework.data.redis.core.RedisTemplate<String, Object> template = 
            (org.springframework.data.redis.core.RedisTemplate<String, Object>) redisTemplate;
    
    RedisRAGService service = new RedisRAGService(template, props, instanceId);
    // ...
}
```

**Elasticsearch**:
```java
private RagService buildElasticsearchRAG(String instanceId) {
    // 类型转换
    co.elastic.clients.elasticsearch.ElasticsearchClient client = 
            (co.elastic.clients.elasticsearch.ElasticsearchClient) elasticsearchClient;
    
    ElasticsearchRAGService service = new ElasticsearchRAGService(client, props, instanceId);
    // ...
}
```

## 🎯 优势

### 之前（错误）

```java
@Autowired(required = false)
private MongoTemplate mongoTemplate;  // ❌ 类加载失败
```

**问题**：
- Spring 在启动时必须加载 `MongoTemplate` 类
- 如果没有添加 MongoDB 依赖，抛出 `ClassNotFoundException`
- 即使使用 `required = false` 也无法避免类加载

### 之后（正确）

```java
private Object mongoTemplate;

@Autowired(required = false)
public void setMongoTemplate(Object mongoTemplate) {
    this.mongoTemplate = mongoTemplate;
}
```

**优势**：
- ✅ `Object` 类型总是可用，不会类加载失败
- ✅ `required = false` 确保依赖不存在时不报错
- ✅ 使用时再进行类型转换，延迟类加载
- ✅ 完全兼容可选依赖

## 📊 修改文件

| 文件 | 修改内容 |
|------|---------|
| `RagAdapterAutoConfiguration.java` | 使用 Object + Setter 注入可选依赖 |
| `RagInstanceBuilder.java` | 字段和方法改为 Object 类型，使用时转换 |

## 🔧 配置示例

现在可以自由选择后端，无需添加所有依赖：

### 只使用 File（无需额外依赖）

```yaml
omni-agent:
  rag:
    instances:
      - type: file
        file:
          index-path: ./data/rag
```

**启动成功** ✅ - 不需要 MongoDB/Redis/Elasticsearch 依赖

### 使用 MongoDB

```yaml
omni-agent:
  rag:
    instances:
      - type: mongodb
        mongodb:
          collection-name: rag_docs
```

**需要添加**:
```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-mongodb</artifactId>
</dependency>
```

### 使用 Redis

```yaml
omni-agent:
  rag:
    instances:
      - type: redis
        redis:
          key-prefix: "rag:"
```

**需要添加**:
```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-redis</artifactId>
</dependency>
```

### 使用 Elasticsearch

```yaml
omni-agent:
  rag:
    instances:
      - type: elasticsearch
        elasticsearch:
          index-prefix: "omni-rag-"
```

**需要添加**:
```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-elasticsearch</artifactId>
</dependency>
```

## ✅ 验证结果

- ✅ 编译成功
- ✅ 无编译错误
- ⚠️ 仅有警告（未使用的方法，Spring 自动调用）
- ✅ 支持可选依赖
- ✅ 不添加依赖也能启动

---
**更新日期**: 2025-12-29  
**问题**: 可选依赖类加载失败  
**解决**: Object 类型 + Setter 注入 + 延迟类型转换  
**状态**: ✅ 完成

