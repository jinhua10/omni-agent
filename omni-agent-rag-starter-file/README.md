# OmniAgent RAG Starter - File (Lucene)

基于 Apache Lucene 的本地文件 RAG 实现。

## ✨ 特性

- ✅ **全文搜索** - 基于 Lucene 的高性能全文检索
- ✅ **多字段搜索** - 支持标题、内容、摘要、标签等多字段搜索
- ✅ **本地存储** - 无需外部依赖，数据存储在本地文件系统
- ✅ **即插即用** - Spring Boot 自动配置，开箱即用
- ✅ **轻量级** - 适合开发、测试和小规模部署
- ✅ **持久化** - 索引持久化到磁盘，重启不丢失

## 📦 依赖

### Maven

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-rag-starter-file</artifactId>
    <version>1.0.0</version>
</dependency>
```

### Gradle

```groovy
implementation 'top.yumbo.ai.omni:omni-agent-rag-starter-file:1.0.0'
```

## ⚙️ 配置

### application.yml

```yaml
omni-agent:
  rag:
    type: file  # 使用 File RAG（默认值）
    file:
      index-path: ./data/lucene-index  # 索引存储路径
      ram-buffer-size-mb: 256.0        # RAM 缓冲区大小（MB）
      rebuild-on-startup: false        # 是否启动时重建索引
      max-results: 100                 # 最大搜索结果数
      default-top-k: 10                # 默认返回结果数
      highlight-enabled: true          # 启用高亮
      highlight-prefix: "<em>"         # 高亮前缀
      highlight-suffix: "</em>"        # 高亮后缀
      min-score: 0.0                   # 最小相似度阈值
```

### application.properties

```properties
# RAG 类型
omni-agent.rag.type=file

# Lucene 索引路径
omni-agent.rag.file.index-path=./data/lucene-index

# RAM 缓冲区大小（MB）
omni-agent.rag.file.ram-buffer-size-mb=256.0

# 是否在启动时重建索引
omni-agent.rag.file.rebuild-on-startup=false

# 最大搜索结果数
omni-agent.rag.file.max-results=100

# 默认返回结果数
omni-agent.rag.file.default-top-k=10

# 启用高亮
omni-agent.rag.file.highlight-enabled=true

# 高亮标签
omni-agent.rag.file.highlight-prefix=<em>
omni-agent.rag.file.highlight-suffix=</em>

# 最小相似度阈值
omni-agent.rag.file.min-score=0.0
```

## 🚀 使用示例

### 1. 基本使用

```java
@Service
public class MyService {
    
    @Autowired
    private RAGService ragService;
    
    public void example() {
        // 索引文档
        Document doc = Document.builder()
            .id("doc1")
            .title("Spring Boot 教程")
            .content("Spring Boot 是一个快速开发框架...")
            .tags(List.of("Java", "Spring", "教程"))
            .build();
        
        String docId = ragService.indexDocument(doc);
        
        // 搜索文档
        List<SearchResult> results = ragService.searchByText("Spring Boot", 5);
        
        for (SearchResult result : results) {
            System.out.println("标题: " + result.getDocument().getTitle());
            System.out.println("得分: " + result.getScore());
        }
    }
}
```

### 2. 批量索引

```java
List<Document> documents = List.of(
    Document.builder()
        .title("文档1")
        .content("内容1")
        .build(),
    Document.builder()
        .title("文档2")
        .content("内容2")
        .build()
);

List<String> ids = ragService.indexDocuments(documents);
System.out.println("索引了 " + ids.size() + " 个文档");
```

### 3. 高级查询

```java
Query query = Query.builder()
    .text("机器学习")
    .topK(10)
    .mode(Query.SearchMode.TEXT)
    .minScore(0.5f)
    .highlight(true)
    .build();

List<SearchResult> results = ragService.search(query);
```

### 4. 获取统计信息

```java
IndexStatistics stats = ragService.getStatistics();
System.out.println("文档总数: " + stats.getTotalDocuments());
System.out.println("索引大小: " + stats.getIndexSize());
System.out.println("健康状态: " + stats.isHealthy());
```

## 🎯 适用场景

### ✅ 推荐使用

- 开发和测试环境
- 单机部署
- 小规模数据（< 100万文档）
- 无外部依赖限制
- 快速原型开发

### ⚠️ 不推荐使用

- 大规模生产环境（> 100万文档）
- 需要向量搜索（推荐使用 Elasticsearch/MongoDB）
- 需要分布式部署
- 需要高可用（推荐使用 Redis/Elasticsearch）

## 🔧 技术细节

### Lucene 版本

- Apache Lucene 9.10.0

### 索引结构

```
./data/lucene-index/
├── segments_1
├── _0.cfe
├── _0.cfs
├── _0.si
└── write.lock
```

### 字段说明

| 字段 | 类型 | 是否分词 | 是否存储 | 说明 |
|------|------|----------|----------|------|
| id | StringField | ❌ | ✅ | 文档唯一标识 |
| title | TextField | ✅ | ✅ | 文档标题 |
| content | TextField | ✅ | ✅ | 文档内容 |
| summary | TextField | ✅ | ✅ | 文档摘要 |
| tags | TextField | ✅ | ✅ | 标签（逗号分隔） |
| source | StringField | ❌ | ✅ | 来源 |
| type | StringField | ❌ | ✅ | 类型 |
| author | StringField | ❌ | ✅ | 作者 |

### 性能参数

- **RAM 缓冲区**: 256MB（可配置）
- **搜索模式**: 多字段搜索（title, content, summary, tags）
- **分析器**: StandardAnalyzer（支持中英文）

## ⚠️ 限制

### 当前不支持

- ❌ **原生向量搜索** - Lucene 9.x 虽然支持向量搜索，但需要额外配置
- ❌ **语义搜索** - 需要集成 AI Embedding 服务
- ❌ **分布式** - 单机实例

### 解决方案

如需以上功能，请使用：
- 向量搜索 → `omni-agent-rag-starter-elasticsearch`
- 语义搜索 → 集成 `omni-agent-ai-api`
- 分布式 → `omni-agent-rag-starter-redis` 或 `elasticsearch`

## 🔄 切换到其他 RAG

### 切换到 H2

```xml
<dependency>
    <artifactId>omni-agent-rag-starter-h2</artifactId>
</dependency>
```

```yaml
omni-agent:
  rag:
    type: h2
```

### 切换到 Elasticsearch

```xml
<dependency>
    <artifactId>omni-agent-rag-starter-elasticsearch</artifactId>
</dependency>
```

```yaml
omni-agent:
  rag:
    type: elasticsearch
```

## 📊 性能参考

| 操作 | 性能 |
|------|------|
| 索引速度 | ~1000 docs/s |
| 搜索速度 | < 100ms |
| 内存占用 | ~256MB |
| 磁盘占用 | ~原始数据的 0.5-1倍 |

## 🤝 贡献

欢迎提交 Issue 和 Pull Request！

## 📄 许可证

Apache License 2.0
