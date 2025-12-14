# Elasticsearch RAG Starter

Elasticsearch RAG 实现提供基于 Elasticsearch 的生产级分布式检索增强生成服务。

## 特性

- 🚀 **分布式全文搜索** - Elasticsearch BM25 算法，业界领先
- 🎯 **高性能向量搜索** - kNN + HNSW 索引，毫秒级响应
- 🔄 **混合检索** - 文本和向量搜索无缝结合
- 📊 **水平扩展** - 支持集群模式，处理海量数据
- 🛡️ **高可用性** - 分片 + 副本机制，保障服务稳定
- 🔍 **多字段搜索** - 支持 title、content、summary、tags 多字段
- ⚡ **批量操作** - 高效的批量索引和搜索

## 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-rag-starter-elasticsearch</artifactId>
    <version>1.0.0-SNAPSHOT</version>
</dependency>
```

### 2. 配置 Elasticsearch

在 `application.yml` 中配置:

```yaml
spring:
  elasticsearch:
    uris: http://localhost:9200
    username: elastic      # 可选
    password: password     # 可选
    connection-timeout: 5s
    socket-timeout: 60s

omni-agent:
  rag:
    elasticsearch:
      index-name: omni-agent-rag     # 索引名称
      number-of-shards: 3            # 分片数量
      number-of-replicas: 1          # 副本数量
      vector-dimension: 768          # 向量维度
      refresh-after-write: false     # 写入后是否立即刷新
      connection-timeout: 5000       # 连接超时（毫秒）
      socket-timeout: 60000          # Socket超时（毫秒）
```

### 3. 使用服务

```java
@Autowired
private RAGService ragService;

// 索引文档
Document document = Document.builder()
    .id("doc1")
    .title("Elasticsearch 介绍")
    .content("Elasticsearch 是一个分布式、RESTful 搜索和分析引擎...")
    .embedding(new float[]{0.1f, 0.2f, 0.3f})
    .build();

String docId = ragService.indexDocument(document);

// 文本搜索（BM25）
List<SearchResult> textResults = ragService.searchByText("Elasticsearch介绍", 10);

// 向量搜索（kNN）
float[] queryEmbedding = new float[]{0.1f, 0.2f, 0.3f};
List<SearchResult> vectorResults = ragService.vectorSearch(queryEmbedding, 10);

// 混合检索（文本 + 向量）
Query query = Query.builder()
    .text("Elasticsearch介绍")
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
| `omni-agent.rag.elasticsearch.index-name` | 索引名称 | `omni-agent-rag` |
| `omni-agent.rag.elasticsearch.number-of-shards` | 分片数量 | `3` |
| `omni-agent.rag.elasticsearch.number-of-replicas` | 副本数量 | `1` |
| `omni-agent.rag.elasticsearch.vector-dimension` | 向量维度 | `768` |
| `omni-agent.rag.elasticsearch.refresh-after-write` | 写入后立即刷新 | `false` |
| `omni-agent.rag.elasticsearch.connection-timeout` | 连接超时（毫秒） | `5000` |
| `omni-agent.rag.elasticsearch.socket-timeout` | Socket超时（毫秒） | `60000` |

## Elasticsearch 索引结构

### Mapping 定义

```json
{
  "mappings": {
    "properties": {
      "id": { "type": "keyword" },
      "title": { "type": "text", "analyzer": "standard" },
      "content": { "type": "text", "analyzer": "standard" },
      "summary": { "type": "text", "analyzer": "standard" },
      "tags": { "type": "keyword" },
      "type": { "type": "keyword" },
      "source": { "type": "keyword" },
      "author": { "type": "keyword" },
      "embedding": {
        "type": "dense_vector",
        "dims": 768,
        "index": true,
        "similarity": "cosine"
      },
      "createdAt": { "type": "date" },
      "updatedAt": { "type": "date" }
    }
  }
}
```

### 索引设置

```json
{
  "settings": {
    "number_of_shards": 3,
    "number_of_replicas": 1,
    "refresh_interval": "1s"
  }
}
```

## 性能优化

### 1. 分片配置

```yaml
omni-agent:
  rag:
    elasticsearch:
      number-of-shards: 5        # 根据集群节点数调整
      number-of-replicas: 2      # 根据可用性需求调整
```

**分片数量建议**:
- 小数据集（< 1GB）: 1-3 个分片
- 中等数据集（1-50GB）: 3-5 个分片
- 大数据集（> 50GB）: 5-10 个分片

**副本数量建议**:
- 开发环境: 0-1 个副本
- 生产环境: 1-2 个副本
- 高可用: 2+ 个副本

### 2. 批量索引

```java
// 批量索引文档（推荐）
List<Document> documents = Arrays.asList(doc1, doc2, doc3);
List<String> docIds = ragService.indexDocuments(documents);
```

### 3. 刷新策略

```yaml
omni-agent:
  rag:
    elasticsearch:
      refresh-after-write: false  # 生产环境建议false
```

- `true`: 写入后立即可见（实时性好，性能差）
- `false`: 1秒后可见（性能好，实时性略差）

### 4. 向量搜索优化

使用 HNSW 索引加速向量搜索:

```json
{
  "embedding": {
    "type": "dense_vector",
    "dims": 768,
    "index": true,
    "similarity": "cosine",
    "index_options": {
      "type": "hnsw",
      "m": 16,
      "ef_construction": 100
    }
  }
}
```

## 文本搜索原理

### BM25 算法

Elasticsearch 使用 BM25（Best Matching 25）算法进行文本相关性评分:

```
score(D,Q) = Σ IDF(qi) · (f(qi,D) · (k1 + 1)) / (f(qi,D) + k1 · (1 - b + b · |D| / avgdl))
```

- **IDF**: 逆文档频率（Inverse Document Frequency）
- **f(qi,D)**: 词频（Term Frequency）
- **k1, b**: 调优参数
- **|D|**: 文档长度
- **avgdl**: 平均文档长度

### 多字段搜索

```java
// title权重3倍，summary和tags权重2倍
.multiMatch(m -> m
    .query(text)
    .fields("title^3", "content", "summary^2", "tags^2")
    .type(TextQueryType.BestFields)
)
```

## 向量搜索原理

### kNN + HNSW 索引

1. **HNSW（Hierarchical Navigable Small World）**
   - 多层图结构
   - 快速近似最近邻搜索
   - 毫秒级查询响应

2. **余弦相似度**
   ```
   similarity = cos(θ) = (A · B) / (||A|| * ||B||)
   ```

3. **搜索流程**
   - 输入查询向量
   - HNSW 图导航
   - 返回 TopK 最相似文档

## 混合检索策略

### 加权组合

```
最终得分 = 文本得分 × 文本权重 + 向量得分 × 向量权重
```

### 推荐权重配置

| 场景 | 文本权重 | 向量权重 | 说明 |
|-----|---------|---------|------|
| 关键词搜索 | 0.7 | 0.3 | 精确匹配为主 |
| 语义搜索 | 0.3 | 0.7 | 语义理解为主 |
| 平衡模式 | 0.5 | 0.5 | 均衡考虑 |

## 集群部署

### 1. 单节点部署

```yaml
spring:
  elasticsearch:
    uris: http://localhost:9200
```

### 2. 集群部署

```yaml
spring:
  elasticsearch:
    uris:
      - http://es-node1:9200
      - http://es-node2:9200
      - http://es-node3:9200
```

### 3. 高可用配置

```yaml
omni-agent:
  rag:
    elasticsearch:
      number-of-shards: 5
      number-of-replicas: 2
```

## 监控与运维

### 1. 健康检查

```java
boolean healthy = ragService.isHealthy();
```

### 2. 统计信息

```java
IndexStatistics stats = ragService.getStatistics();
System.out.println("总文档数: " + stats.getTotalDocuments());
System.out.println("索引大小: " + stats.getIndexSize());
```

### 3. 索引重建

```java
ragService.rebuildIndex();
```

## 注意事项

1. **内存需求** - Elasticsearch 需要充足内存（建议 JVM heap ≥ 4GB）
2. **磁盘空间** - 预留足够磁盘空间（数据 + 副本）
3. **网络延迟** - 跨数据中心部署需考虑网络延迟
4. **索引优化** - 定期执行 forcemerge 优化索引
5. **备份策略** - 使用快照（snapshot）定期备份数据

## 适用场景

- ✅ 大规模文档检索（百万级 - 亿级文档）
- ✅ 生产环境高可用需求
- ✅ 复杂的全文搜索需求
- ✅ 实时索引和搜索
- ✅ 分布式系统架构
- ✅ 混合检索（文本 + 向量）
- ❌ 极小数据集（建议用 SQLite 或文件）
- ❌ 嵌入式应用（建议用 H2 或 SQLite）

## 与其他 RAG 实现对比

| 特性 | Elasticsearch | MongoDB | Redis | SQLite | Lucene |
|-----|--------------|---------|-------|--------|--------|
| 数据规模 | 亿级+ | 千万级 | 百万级 | 百万级 | 千万级 |
| 分布式 | ✅ 原生支持 | ✅ 支持 | ✅ 支持 | ❌ | ❌ |
| 全文搜索 | 优秀(BM25) | 好 | 中等 | 优秀(FTS5) | 优秀 |
| 向量搜索 | 优秀(kNN+HNSW) | 中等 | 简单 | 简单 | 无 |
| 运维复杂度 | 中等 | 中等 | 中等 | 低 | 低 |
| 成本 | 高 | 中 | 中 | 低 | 低 |

## 升级方案

### 性能提升

1. **增加分片数量** - 提升索引和搜索并发度
2. **启用 HNSW** - 加速向量搜索
3. **调整副本数量** - 提升查询吞吐量

### 扩容方案

1. **垂直扩容** - 增加节点内存和CPU
2. **水平扩容** - 增加集群节点数量
3. **热温冷架构** - 分层存储优化成本

## 许可证

Apache License 2.0
