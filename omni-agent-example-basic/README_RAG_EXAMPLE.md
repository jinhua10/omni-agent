# RAGExample 使用指南

## 📚 简介

`RAGExample` 是一个完整的 RAG 检索示例类，演示了系统支持的所有检索方式：

- ✅ **全文检索**（Text Search） - 基于 BM25/Lucene
- ✅ **向量检索**（Vector Search） - 基于余弦相似度
- ✅ **语义检索**（Semantic Search） - 自动文本向量化
- ✅ **混合检索**（Hybrid Search） - 文本 + 向量结合

---

## 🎯 示例列表

### 基础示例

| 编号 | 示例名称 | 说明 | 是否需要 EmbeddingService |
|------|---------|------|------------------------|
| 1 | `indexDocuments()` | 索引文档（支持向量和非向量） | ❌ 可选 |
| 2 | `textSearchExample()` | 纯文本检索 | ❌ 否 |
| 3 | `multiFieldSearchExample()` | 多字段检索 | ❌ 否 |
| 4 | `searchWithFiltersExample()` | 带过滤条件的检索 | ❌ 否 |
| 5 | `technicalTermSearchExample()` | 专业术语精确匹配 | ❌ 否 |
| 6 | `codeSearchExample()` | 代码搜索 | ❌ 否 |

### 高级示例（需要 EmbeddingService）

| 编号 | 示例名称 | 说明 | 是否需要 EmbeddingService |
|------|---------|------|------------------------|
| 7 | `vectorSearchExample()` | 向量检索 | ✅ 是 |
| 8 | `semanticSearchExample()` | 语义检索（自动向量化） | ✅ 是 |
| 9 | `hybridSearchExample()` | 混合检索（文本+向量） | ✅ 是 |

### 性能测试

| 编号 | 示例名称 | 说明 | 是否需要 EmbeddingService |
|------|---------|------|------------------------|
| 10 | `performanceComparisonTest()` | 性能对比测试 | ❌ 可选 |
| 11 | `batchIndexingTest()` | 批量索引测试 | ❌ 否 |

---

## 🚀 快速开始

### 方式 1：运行所有示例

```java
@Autowired
private RAGExample ragExample;

public void demo() {
    // 运行所有示例（包括文本和向量检索）
    ragExample.runAllExamples();
}
```

**输出示例：**
```
========================================
   RAG 检索示例集 - 开始运行
========================================
=== 示例 1：索引文档 ===
✅ 已索引 3 个文档（包含向量，维度: 768）
...
✅ 所有示例运行完成！
💡 支持的检索方式：
   ✓ 全文检索（Text Search）
   ✓ 向量检索（Vector Search）
   ✓ 语义检索（Semantic Search）
   ✓ 混合检索（Hybrid Search）
```

---

### 方式 2：仅运行文本检索示例（无需 EmbeddingService）

```java
@Autowired
private RAGExample ragExample;

public void demo() {
    // 只运行文本检索相关示例
    ragExample.runTextSearchExamples();
}
```

**适用场景**：
- 没有配置 EmbeddingService
- 只需要全文检索功能
- 快速验证系统功能

---

### 方式 3：仅运行向量检索示例（需要 EmbeddingService）

```java
@Autowired
private RAGExample ragExample;

public void demo() {
    // 只运行向量检索相关示例
    ragExample.runVectorSearchExamples();
}
```

**前提条件**：
- 必须配置 EmbeddingService
- 文档已包含向量

---

### 方式 4：运行单个示例

```java
@Autowired
private RAGExample ragExample;

public void demo() {
    // 先索引文档
    ragExample.indexDocuments();
    
    // 然后运行指定的示例
    ragExample.textSearchExample();           // 文本检索
    ragExample.vectorSearchExample();         // 向量检索
    ragExample.semanticSearchExample();       // 语义检索
    ragExample.hybridSearchExample();         // 混合检索
}
```

---

## ⚙️ 配置要求

### 基本配置（仅文本检索）

```yaml
omni-agent:
  rag:
    type: file  # 或 h2, sqlite, mongodb, redis, elasticsearch
    file:
      index-path: ./data/lucene-index
```

**无需额外配置！** 文本检索开箱即用。

---

### 高级配置（支持向量检索）

#### 选项 A：使用 SQLite（推荐入门）

```yaml
omni-agent:
  rag:
    type: sqlite
    sqlite:
      db-path: ./data/rag.db
      enable-fts5: true
      enable-vector-search: true  # ⭐ 启用向量检索
```

#### 选项 B：使用 Elasticsearch（企业级）

```yaml
omni-agent:
  rag:
    type: elasticsearch
    elasticsearch:
      hosts: localhost:9200
      enable-text-search: true
      enable-vector-search: true  # ⭐ 启用向量检索
```

#### 配置 EmbeddingService（可选）

如果需要使用语义检索和混合检索，需要配置 EmbeddingService。

**示例：使用在线 API**

```yaml
embedding:
  provider: online-api
  api-url: https://api.openai.com/v1/embeddings
  api-key: ${OPENAI_API_KEY}
  model: text-embedding-ada-002
```

**或者：实现自己的 EmbeddingService**

参考 `old` 目录中的 `LocalEmbeddingEngine` 实现本地 ONNX Runtime 向量化。

---

## 📖 详细示例说明

### 示例 1：索引文档

```java
ragExample.indexDocuments();
```

**功能**：
- 索引 3 个示例文档
- 如果配置了 EmbeddingService，自动为文档生成向量
- 文档包含：标题、内容、标签、元数据

**输出**：
```
=== 示例 1：索引文档 ===
检测到 EmbeddingService，为文档生成向量...
✅ 已索引 3 个文档（包含向量，维度: 768）
```

---

### 示例 2：纯文本检索

```java
ragExample.textSearchExample();
```

**功能**：
- 演示两种文本检索方式
- 方式 1：`searchByText()` 直接调用
- 方式 2：使用 `Query` 对象（可设置高亮等选项）

**输出**：
```
=== 示例 2：纯文本检索 ===
搜索: 'ONNX Runtime'
  - [评分: 2.5] ONNX Runtime 入门指南
  - [评分: 1.2] LocalEmbeddingEngine 使用示例
```

---

### 示例 7：向量检索

```java
ragExample.vectorSearchExample();
```

**功能**：
- 使用 EmbeddingService 生成查询向量
- 执行向量相似度搜索
- 基于余弦相似度排序

**输出**：
```
=== 示例 7：向量检索 ===
查询: 'ONNX Runtime 推理引擎' (向量维度: 768)
向量检索结果:
  - [相似度: 0.89] ONNX Runtime 入门指南
  - [相似度: 0.76] LocalEmbeddingEngine 使用示例
```

---

### 示例 8：语义检索

```java
ragExample.semanticSearchExample();
```

**功能**：
- 自动将查询文本转换为向量
- 一行代码完成语义搜索
- 内部调用 `embeddingService.embed()` + `vectorSearch()`

**输出**：
```
=== 示例 8：语义检索 ===
查询: '机器学习模型'
语义检索结果:
  - [相似度: 0.85] 如何选择 Embedding 模型？
  - [相似度: 0.72] ONNX Runtime 入门指南
```

---

### 示例 9：混合检索

```java
ragExample.hybridSearchExample();
```

**功能**：
- 结合文本匹配和向量相似度
- 可自定义权重（如文本 30%，向量 70%）
- 综合评分排序

**输出**：
```
=== 示例 9：混合检索 ===
查询: 'ONNX Runtime' (混合模式: 文本 30% + 向量 70%)
混合检索结果:
  - [综合分数: 2.15] ONNX Runtime 入门指南
    文本分数: 2.5, 向量分数: 0.89
  - [综合分数: 0.89] 如何选择 Embedding 模型？
    文本分数: 0.3, 向量分数: 0.85
```

---

### 示例 10：性能对比测试

```java
ragExample.performanceComparisonTest();
```

**功能**：
- 对比全文检索和语义检索的性能
- 测量查询时间、结果数量、平均分数

**输出**：
```
=== 示例 10：性能对比测试 ===
1. 全文检索:
  - 查询时间: 5 ms
  - 结果数量: 10
  - 平均分数: 1.85

2. 语义检索:
  - 查询时间: 78 ms
  - 结果数量: 10
  - 平均分数: 0.76

性能对比:
  - 语义检索耗时是全文检索的 15.6 倍
```

---

## 🎨 使用场景

### 场景 1：关键词精确查询 → 使用文本检索

```java
// 搜索专业术语
ragExample.technicalTermSearchExample();

// 输出：精确匹配 "LocalEmbeddingEngine"、"OrtSession" 等
```

**适用于**：
- 搜索产品型号、编号
- 搜索类名、函数名
- 搜索专业术语

---

### 场景 2：语义理解查询 → 使用向量检索

```java
// 搜索 "机器学习模型" 能匹配到 "Embedding 模型"
ragExample.semanticSearchExample();
```

**适用于**：
- 用户问法多样化
- 需要同义词匹配
- 跨语言查询

---

### 场景 3：综合查询 → 使用混合检索

```java
// 既要关键词匹配，又要语义理解
ragExample.hybridSearchExample();
```

**适用于**：
- 企业知识库
- 智能客服
- 复杂查询场景

---

## 📊 输出格式说明

### SearchResult 结构

```java
SearchResult {
    document: Document,      // 文档对象
    score: float,           // 综合分数（0-1 或更高）
    textScore: float,       // 文本匹配分数（可选）
    vectorScore: float,     // 向量相似度（可选）
    rank: int,              // 排名
    reason: String          // 匹配原因（如"向量相似度"）
}
```

### Document 结构

```java
Document {
    id: String,             // 文档ID
    title: String,          // 标题
    content: String,        // 内容
    tags: List<String>,     // 标签
    metadata: Map,          // 元数据
    embedding: float[],     // 向量（可选）
    source: String,         // 来源
    createdAt: Long         // 创建时间戳
}
```

---

## 🛠️ 常见问题

### Q1: 运行示例时提示 "未配置 EmbeddingService"？

**A**: 这是正常的。如果只需要文本检索，不影响功能。如需向量检索，请配置 EmbeddingService。

```java
// 仅运行文本检索示例（不需要 EmbeddingService）
ragExample.runTextSearchExamples();
```

---

### Q2: 如何验证 EmbeddingService 是否配置成功？

**A**: 查看日志输出：

```
检测到 EmbeddingService，为文档生成向量...
✅ 已索引 3 个文档（包含向量，维度: 768）
```

或者运行：

```java
ragExample.runVectorSearchExamples();
```

---

### Q3: 向量检索和文本检索哪个更好？

**A**: 各有优劣，建议参考：

| 场景 | 推荐方案 |
|------|---------|
| 关键词明确 | 文本检索 ✅ |
| 语义理解 | 向量检索 ✅ |
| 综合查询 | 混合检索 ✅ |

详见：[RAG_COMPARISON_GUIDE.md](../../../docs/RAG_COMPARISON_GUIDE.md)

---

### Q4: 如何自定义示例？

**A**: 参考现有示例方法，创建自己的查询：

```java
@Autowired
private RAGService ragService;

public void myCustomSearch() {
    // 自定义查询
    Query query = Query.builder()
        .text("你的查询文本")
        .mode(Query.SearchMode.TEXT)
        .topK(10)
        .filters(Map.of("category", "tutorial"))
        .build();
    
    List<SearchResult> results = ragService.search(query);
    
    // 处理结果
    results.forEach(result -> {
        System.out.println(result.getDocument().getTitle());
    });
}
```

---

## 📚 相关文档

- [RAG_WITHOUT_EMBEDDING.md](../../../docs/RAG_WITHOUT_EMBEDDING.md) - 不使用向量模型的实现指南
- [RAG_COMPARISON_GUIDE.md](../../../docs/RAG_COMPARISON_GUIDE.md) - 向量检索 vs 全文检索对比
- [README_RAG_DOCS.md](../../../docs/README_RAG_DOCS.md) - RAG 文档索引

---

## ✅ 总结

`RAGExample` 提供了完整的 RAG 检索示例：

- ✅ **灵活配置**：支持有/无 EmbeddingService
- ✅ **全面演示**：覆盖所有检索方式
- ✅ **开箱即用**：无需额外代码即可运行
- ✅ **易于扩展**：可基于示例开发自己的功能

**快速开始：**

```java
@Autowired
private RAGExample ragExample;

// 运行所有示例
ragExample.runAllExamples();
```

**祝您使用愉快！** 🎉

