# OmniAgent RAG Starter - SQLite

基于 SQLite 数据库的轻量级 RAG 实现。

## ✨ 特性

- ✅ **轻量级** - 单文件数据库，无需外部服务
- ✅ **全文搜索** - 基于 SQLite FTS5 的高性能全文检索
- ✅ **向量搜索** - 支持向量相似度搜索（余弦相似度）
- ✅ **混合检索** - 文本搜索 + 向量搜索组合
- ✅ **事务支持** - ACID 特性，数据安全可靠
- ✅ **连接池** - HikariCP 连接池，线程安全
- ✅ **即插即用** - Spring Boot 自动配置，开箱即用
- ✅ **零配置启动** - 自动创建数据库表和索引

## 📦 依赖

### Maven

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-rag-starter-sqlite</artifactId>
    <version>1.0.0</version>
</dependency>
```

### Gradle

```groovy
implementation 'top.yumbo.ai.omni:omni-agent-rag-starter-sqlite:1.0.0'
```

## ⚙️ 配置

### application.yml

```yaml
omni-agent:
  rag:
    type: sqlite  # 使用 SQLite RAG
    sqlite:
      database-path: ./data/rag.db    # 数据库文件路径
      init-database: true              # 启动时初始化数据库
      enable-fts: true                 # 启用 FTS5 全文搜索
      max-pool-size: 10                # 最大连接数
      min-idle: 2                      # 最小空闲连接
      connection-timeout: 30000        # 连接超时（毫秒）
      rebuild-on-startup: false        # 启动时是否重建索引
      max-results: 100                 # 最大搜索结果数
      default-top-k: 10                # 默认返回结果数
      min-score: 0.0                   # 最小相似度阈值
```

### application.properties

```properties
# RAG 类型
omni-agent.rag.type=sqlite

# 数据库路径
omni-agent.rag.sqlite.database-path=./data/rag.db

# 初始化设置
omni-agent.rag.sqlite.init-database=true
omni-agent.rag.sqlite.enable-fts=true

# 连接池配置
omni-agent.rag.sqlite.max-pool-size=10
omni-agent.rag.sqlite.min-idle=2
omni-agent.rag.sqlite.connection-timeout=30000

# 搜索配置
omni-agent.rag.sqlite.max-results=100
omni-agent.rag.sqlite.default-top-k=10
omni-agent.rag.sqlite.min-score=0.0
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
        
        // 全文搜索
        List<SearchResult> results = ragService.searchByText("Spring Boot", 5);
        
        for (SearchResult result : results) {
            System.out.println("标题: " + result.getDocument().getTitle());
            System.out.println("得分: " + result.getScore());
        }
    }
}
```

### 2. 向量搜索

```java
// 创建带向量的文档
Document doc = Document.builder()
    .title("机器学习入门")
    .content("机器学习是人工智能的重要分支...")
    .embedding(new float[]{0.1f, 0.2f, 0.3f, ...})  // 向量
    .build();

ragService.indexDocument(doc);

// 向量搜索
float[] queryVector = new float[]{0.1f, 0.2f, 0.3f, ...};
List<SearchResult> results = ragService.vectorSearch(queryVector, 10);
```

### 3. 混合检索

```java
Query query = Query.builder()
    .text("机器学习")
    .embedding(queryVector)
    .topK(10)
    .mode(Query.SearchMode.HYBRID)
    .textWeight(0.6f)      // 文本权重
    .vectorWeight(0.4f)    // 向量权重
    .build();

List<SearchResult> results = ragService.hybridSearch(query);
```

### 4. 批量索引

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

### 5. 获取统计信息

```java
IndexStatistics stats = ragService.getStatistics();
System.out.println("文档总数: " + stats.getTotalDocuments());
System.out.println("数据库大小: " + stats.getIndexSize() + " bytes");
System.out.println("健康状态: " + stats.isHealthy());
```

## 🎯 适用场景

### ✅ 推荐使用

- 单机部署
- 中小规模数据（< 1000万文档）
- 需要持久化存储
- 嵌入式应用
- 离线应用
- 需要事务支持
- 资源受限环境

### ⚠️ 不推荐使用

- 大规模生产环境（> 1000万文档）
- 需要分布式部署
- 高并发写入场景（> 1000 TPS）
- 需要集群支持

## 🔧 技术细节

### SQLite 版本

- SQLite JDBC 3.45.0.0
- 支持 FTS5 全文搜索扩展

### 数据库表结构

```sql
-- 主表
CREATE TABLE rag_documents (
    id TEXT PRIMARY KEY,
    title TEXT,
    content TEXT NOT NULL,
    summary TEXT,
    source TEXT,
    type TEXT,
    author TEXT,
    tags TEXT,           -- JSON array
    metadata TEXT,       -- JSON object
    embedding TEXT,      -- JSON array (float[])
    created_at INTEGER,
    updated_at INTEGER,
    indexed_at INTEGER
);

-- FTS5 全文搜索表
CREATE VIRTUAL TABLE rag_documents_fts 
USING fts5(id, title, content, summary, tags);
```

### 索引

```sql
CREATE INDEX idx_rag_doc_type ON rag_documents(type);
CREATE INDEX idx_rag_doc_source ON rag_documents(source);
CREATE INDEX idx_rag_doc_author ON rag_documents(author);
CREATE INDEX idx_rag_doc_created ON rag_documents(created_at);
```

### 性能优化

- **WAL 模式** - 提升并发读写性能
- **连接池** - HikariCP 管理数据库连接
- **FTS5** - SQLite 原生全文搜索扩展
- **内存缓存** - cache_size=10000
- **异步写入** - synchronous=NORMAL

## 📊 性能参考

| 操作 | 性能 |
|------|------|
| 索引速度 | ~500 docs/s |
| 文本搜索 | < 50ms (FTS5) |
| 向量搜索 | < 200ms (1万文档) |
| 混合检索 | < 300ms |
| 数据库大小 | ~原始数据的 1-1.5倍 |
| 内存占用 | ~100MB |

## 🔄 切换到其他 RAG

### 切换到 File (Lucene)

```xml
<dependency>
    <artifactId>omni-agent-rag-starter-file</artifactId>
</dependency>
```

```yaml
omni-agent:
  rag:
    type: file
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

## ⚠️ 注意事项

### SQLite 限制

1. **并发写入** - SQLite 同一时刻只支持一个写入事务
2. **网络访问** - 不支持网络访问，仅本地文件
3. **最大数据库大小** - 理论上限 281TB，实际建议 < 1GB

### 最佳实践

1. **批量操作** - 使用 `indexDocuments()` 批量索引
2. **事务控制** - 大量写入时考虑事务批处理
3. **定期备份** - 使用 SQLite 的 BACKUP API
4. **索引优化** - 定期执行 `VACUUM` 优化数据库

## 🛠️ 故障排除

### 数据库锁定

```yaml
omni-agent:
  rag:
    sqlite:
      connection-timeout: 60000  # 增加超时时间
      max-pool-size: 5           # 减少连接数
```

### FTS5 不可用

如果 SQLite 编译时未包含 FTS5：

```yaml
omni-agent:
  rag:
    sqlite:
      enable-fts: false  # 禁用 FTS5，使用 LIKE 查询
```

## 🤝 贡献

欢迎提交 Issue 和 Pull Request！

## 📄 许可证

Apache License 2.0
