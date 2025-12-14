# Redis RAG Starter

Redis RAG 实现提供基于 Redis 的高性能内存检索增强生成服务。

## 特性

- ⚡ **高性能内存存储** - 利用Redis内存数据库实现毫秒级检索
- 🔍 **快速向量搜索** - 基于余弦相似度的向量检索
- 📝 **文本关键词搜索** - 支持基于关键词的全文检索
- 🔄 **混合检索** - 结合文本和向量搜索的混合检索模式
- ⏱️ **TTL支持** - 可配置文档自动过期时间
- 🎯 **过滤器支持** - 支持按类型、来源、作者等字段过滤

## 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-rag-starter-redis</artifactId>
    <version>1.0.0-SNAPSHOT</version>
</dependency>
```

### 2. 配置 Redis

在 `application.yml` 中配置:

```yaml
spring:
  data:
    redis:
      host: localhost
      port: 6379
      password: # 可选
      database: 0
      timeout: 5000ms
      lettuce:
        pool:
          max-active: 8
          max-idle: 8
          min-idle: 0
          max-wait: -1ms

omni-agent:
  rag:
    redis:
      key-prefix: rag:            # Redis Key前缀
      enable-text-index: true     # 启用文本索引
      document-ttl: 0             # 文档TTL（秒），0表示永不过期
      connection-timeout: 2000    # 连接超时（毫秒）
      read-timeout: 5000          # 读取超时（毫秒）
```

### 3. 使用服务

```java
@Autowired
private RAGService ragService;

// 索引文档
Document document = Document.builder()
    .id("doc1")
    .title("Redis 介绍")
    .content("Redis是一个开源的内存数据结构存储系统...")
    .embedding(new float[]{0.1f, 0.2f, 0.3f})
    .build();

String docId = ragService.indexDocument(document);

// 文本搜索
List<SearchResult> textResults = ragService.searchByText("Redis介绍", 10);

// 向量搜索
float[] queryEmbedding = new float[]{0.1f, 0.2f, 0.3f};
List<SearchResult> vectorResults = ragService.vectorSearch(queryEmbedding, 10);

// 混合检索
Query query = Query.builder()
    .text("Redis介绍")
    .embedding(queryEmbedding)
    .mode(SearchMode.HYBRID)
    .textWeight(0.3f)
    .vectorWeight(0.7f)
    .topK(10)
    .build();

List<SearchResult> hybridResults = ragService.search(query);
```

## 配置说明

| 配置项 | 说明 | 默认值 |
|-------|------|--------|
| `omni-agent.rag.redis.key-prefix` | Redis Key前缀 | `rag:` |
| `omni-agent.rag.redis.enable-text-index` | 是否启用文本索引 | `true` |
| `omni-agent.rag.redis.document-ttl` | 文档TTL（秒），0表示永不过期 | `0` |
| `omni-agent.rag.redis.connection-timeout` | Redis连接超时（毫秒） | `2000` |
| `omni-agent.rag.redis.read-timeout` | Redis读取超时（毫秒） | `5000` |

## Redis数据结构

### Key设计

- **文档存储**: `rag:doc:{documentId}` - 存储完整文档对象（Hash）
- **索引集合**: `rag:index:all` - 存储所有文档ID（Set）
- **文本索引**: `rag:text:{keyword}` - 存储包含关键词的文档ID（Set）
- **统计信息**: `rag:stats` - 存储索引统计（Hash）

### 数据持久化

Redis支持以下持久化方式:

1. **RDB快照** - 定期保存数据快照
2. **AOF日志** - 记录每个写操作
3. **混合持久化** - 结合RDB和AOF的优点

建议在 Redis 配置中启用持久化:

```conf
# redis.conf
save 900 1
save 300 10
save 60 10000
appendonly yes
appendfsync everysec
```

## 性能优化

### 1. 连接池配置

```yaml
spring:
  data:
    redis:
      lettuce:
        pool:
          max-active: 20       # 最大连接数
          max-idle: 10         # 最大空闲连接
          min-idle: 5          # 最小空闲连接
          max-wait: 2000ms     # 最大等待时间
```

### 2. 批量操作

```java
// 批量索引文档
List<String> docIds = ragService.indexDocuments(documents);
```

### 3. Pipeline 优化

对于大量读写操作，考虑使用 Redis Pipeline:

```java
redisTemplate.executePipelined(new SessionCallback<Object>() {
    @Override
    public Object execute(RedisOperations operations) {
        // 批量操作
        return null;
    }
});
```

## 向量搜索原理

Redis RAG Starter 使用余弦相似度计算向量相似性:

```
相似度 = (向量A · 向量B) / (||向量A|| * ||向量B||)
```

搜索流程:
1. 遍历所有文档
2. 计算查询向量与文档向量的余弦相似度
3. 按相似度排序
4. 返回 TopK 结果

对于大规模数据，建议:
- 使用 Redis 集群分片存储
- 考虑使用 RediSearch 模块进行向量索引
- 或迁移到专用向量数据库（如 Milvus、Weaviate）

## 文本搜索原理

1. **关键词提取** - 分词并过滤停用词
2. **倒排索引** - 使用 Redis Set 存储关键词→文档ID映射
3. **评分计算** - 统计关键词命中次数并归一化
4. **结果排序** - 按得分降序返回

## 混合检索策略

混合检索结合文本和向量搜索的优势:

```
最终得分 = 文本得分 × 文本权重 + 向量得分 × 向量权重
```

推荐权重配置:
- **偏重语义**: `textWeight=0.3, vectorWeight=0.7`
- **平衡模式**: `textWeight=0.5, vectorWeight=0.5`
- **偏重关键词**: `textWeight=0.7, vectorWeight=0.3`

## 注意事项

1. **内存限制** - Redis是内存数据库，注意数据量和内存使用
2. **持久化** - 确保配置合适的持久化策略防止数据丢失
3. **集群模式** - 大规模应用建议使用Redis集群
4. **监控** - 监控Redis内存使用、命中率、连接数等指标

## 适用场景

- ✅ 小到中等规模数据集（< 100万文档）
- ✅ 需要快速响应的实时检索
- ✅ 临时/缓存性质的向量搜索
- ✅ 与现有Redis基础设施集成
- ❌ 超大规模向量数据（建议使用专用向量数据库）
- ❌ 需要复杂的向量索引算法（HNSW、IVF等）

## 与其他RAG实现对比

| 特性 | Redis | MongoDB | SQLite | Lucene |
|-----|-------|---------|--------|--------|
| 存储类型 | 内存 | 磁盘 | 磁盘 | 磁盘 |
| 查询速度 | 极快 | 快 | 中 | 快 |
| 数据规模 | 中 | 大 | 中 | 大 |
| 向量搜索 | 简单 | 中等 | 简单 | 无 |
| 文本搜索 | 中等 | 好 | 优秀(FTS5) | 优秀 |
| 运维复杂度 | 中 | 中 | 低 | 低 |

## 升级方案

当数据规模增长时，可以考虑:

1. **Redis Enterprise** - 使用商业版获得更好的性能和功能
2. **RediSearch** - 安装RediSearch模块支持更高级的搜索功能
3. **迁移到专用向量数据库** - 如 Milvus、Weaviate、Qdrant

## 许可证

Apache License 2.0
