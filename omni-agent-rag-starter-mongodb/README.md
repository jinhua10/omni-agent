# OmniAgent RAG Starter - MongoDB

基于MongoDB的RAG(检索增强生成)实现,适用于生产环境的文档级应用。

## 特性

- ✅ **MongoDB文档存储** - 灵活的文档模型,支持动态字段
- ✅ **全文搜索** - 基于MongoDB文本索引的强大全文搜索
- ✅ **向量搜索** - 余弦相似度向量搜索
- ✅ **混合检索** - 文本+向量混合检索,可调整权重
- ✅ **高可用** - 支持MongoDB副本集和分片集群
- ✅ **连接池** - 自动配置连接池管理
- ✅ **Spring Boot自动配置** - 零配置开箱即用

## 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-rag-starter-mongodb</artifactId>
    <version>1.0.0</version>
</dependency>
```

### 2. 配置

**application.yml:**

```yaml
omni-agent:
  rag:
    type: mongodb
    mongodb:
      uri: mongodb://localhost:27017
      database: omni_rag
      collection-name: rag_documents
      enable-text-search: true
```

### 3. 使用示例

```java
@Autowired
private RAGService ragService;

// 索引文档
Document doc = new Document();
doc.setTitle("MongoDB RAG示例");
doc.setContent("这是MongoDB实现的RAG系统...");
String docId = ragService.indexDocument(doc);

// 文本搜索
List<SearchResult> results = ragService.searchByText("AI技术", 10);

// 混合搜索
List<SearchResult> hybridResults = ragService.hybridSearch(
    "AI技术", embedding, 0.3f, 0.7f, 10
);
```

## 配置说明

| 属性 | 默认值 | 说明 |
|-----|--------|------|
| `omni-agent.rag.type` | `file` | 设置为`mongodb` |
| `omni-agent.rag.mongodb.uri` | `mongodb://localhost:27017` | MongoDB连接URI |
| `omni-agent.rag.mongodb.database` | `omni_rag` | 数据库名称 |
| `omni-agent.rag.mongodb.collection-name` | `rag_documents` | 集合名称 |
| `omni-agent.rag.mongodb.enable-text-search` | `true` | 启用文本索引 |
| `omni-agent.rag.mongodb.max-pool-size` | `100` | 最大连接池 |

## 功能对比

| 功能 | SQLite | MongoDB | Elasticsearch |
|-----|--------|---------|---------------|
| 全文搜索 | ✅ FTS5 | ✅ 文本索引 | ✅ 最强 |
| 向量搜索 | ✅ | ✅ | ✅ 原生 |
| 扩展性 | ❌ 单机 | ✅ **集群** | ✅ |
| 高可用 | ❌ | ✅ **副本集** | ✅ |
| 灵活性 | ⚠️ 固定 | ✅ **动态** | ✅ |
| 适用场景 | 轻量级 | **生产推荐** | 大规模 |

## 适用场景

### ✅ 推荐使用

- 生产环境(需要高可用)
- 文档结构多变
- 需要水平扩展
- 已有MongoDB基础设施

### ❌ 不推荐

- 开发测试(File/SQLite更简单)
- 纯向量搜索(ES更优)
- 资源受限环境

## 版本要求

- Java 21+
- Spring Boot 3.2.11+
- MongoDB 4.0+ (建议5.0+)

## 版本历史

**1.0.0** (2025-12-15) - 初始版本
