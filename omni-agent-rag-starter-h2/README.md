# H2 RAG Starter

H2 RAG 实现提供基于 H2 数据库的嵌入式检索增强生成服务。

## 特性

- 🚀 **零配置启动** - 嵌入式数据库，无需外部依赖
- 📝 **全文搜索** - H2 内置 Full-Text Search 引擎
- 🎯 **向量搜索** - 基于余弦相似度的向量检索
- 🔄 **混合检索** - 结合文本和向量搜索
- 💾 **数据持久化** - 支持文件和内存模式
- 🔧 **HikariCP连接池** - 高性能数据库连接管理
- 🎨 **Web控制台** - 可选的H2 Console可视化管理

## 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-rag-starter-h2</artifactId>
    <version>1.0.0-SNAPSHOT</version>
</dependency>
```

### 2. 配置 H2

在 `application.yml` 中配置:

```yaml
omni-agent:
  rag:
    h2:
      url: jdbc:h2:./data/omni-agent-rag;AUTO_SERVER=TRUE
      username: sa
      password: ""
      max-pool-size: 10
      min-pool-size: 2
      connection-timeout: 30000
      idle-timeout: 600000
      max-lifetime: 1800000

# 可选：启用H2 Console
spring:
  h2:
    console:
      enabled: true
      path: /h2-console
```

### 3. 使用服务

```java
@Autowired
private RAGService ragService;

// 索引文档
Document document = Document.builder()
    .id("doc1")
    .title("H2 数据库介绍")
    .content("H2是一个Java编写的嵌入式数据库...")
    .embedding(new float[]{0.1f, 0.2f, 0.3f})
    .build();

String docId = ragService.indexDocument(document);

// 文本搜索（Full-Text）
List<SearchResult> textResults = ragService.searchByText("H2数据库", 10);

// 向量搜索
float[] queryEmbedding = new float[]{0.1f, 0.2f, 0.3f};
List<SearchResult> vectorResults = ragService.vectorSearch(queryEmbedding, 10);

// 混合检索
Query query = Query.builder()
    .text("H2数据库")
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
| `omni-agent.rag.h2.url` | H2数据库URL | `jdbc:h2:./data/omni-agent-rag;AUTO_SERVER=TRUE` |
| `omni-agent.rag.h2.username` | 数据库用户名 | `sa` |
| `omni-agent.rag.h2.password` | 数据库密码 | `""` |
| `omni-agent.rag.h2.max-pool-size` | 最大连接池大小 | `10` |
| `omni-agent.rag.h2.min-pool-size` | 最小空闲连接数 | `2` |
| `omni-agent.rag.h2.connection-timeout` | 连接超时（毫秒） | `30000` |
| `omni-agent.rag.h2.idle-timeout` | 空闲超时（毫秒） | `600000` |
| `omni-agent.rag.h2.max-lifetime` | 连接最大生命周期（毫秒） | `1800000` |

## H2 数据库模式

### 文件模式（持久化）

```yaml
omni-agent:
  rag:
    h2:
      url: jdbc:h2:./data/omni-agent-rag  # 文件模式
```

**特点**:
- ✅ 数据持久化到磁盘
- ✅ 重启后数据不丢失
- ✅ 适合生产环境

### 内存模式（临时）

```yaml
omni-agent:
  rag:
    h2:
      url: jdbc:h2:mem:omni-agent-rag  # 内存模式
```

**特点**:
- ⚡ 性能更快
- ⚠️ 重启后数据丢失
- ✅ 适合测试和开发

### 服务器模式（多进程）

```yaml
omni-agent:
  rag:
    h2:
      url: jdbc:h2:./data/omni-agent-rag;AUTO_SERVER=TRUE  # 服务器模式
```

**特点**:
- ✅ 支持多进程访问
- ✅ 自动启动TCP服务器
- ✅ 适合分布式环境

## 数据库表结构

### rag_documents 表

```sql
CREATE TABLE rag_documents (
    id VARCHAR(255) PRIMARY KEY,
    title VARCHAR(1000),
    content CLOB,
    summary CLOB,
    tags VARCHAR(2000),
    type VARCHAR(100),
    source VARCHAR(500),
    author VARCHAR(255),
    embedding CLOB,
    metadata CLOB,
    created_at TIMESTAMP,
    updated_at TIMESTAMP
);

-- 索引
CREATE INDEX idx_type ON rag_documents(type);
CREATE INDEX idx_source ON rag_documents(source);
CREATE INDEX idx_author ON rag_documents(author);

-- 全文索引
CALL FT_CREATE_INDEX('PUBLIC', 'RAG_DOCUMENTS', 'TITLE,CONTENT,SUMMARY');
```

## 全文搜索原理

### H2 Full-Text Search

H2 使用 Apache Lucene 引擎实现全文搜索：

1. **索引创建**
   ```sql
   CALL FT_INIT();
   CALL FT_CREATE_INDEX('PUBLIC', 'RAG_DOCUMENTS', 'TITLE,CONTENT,SUMMARY');
   ```

2. **搜索查询**
   ```sql
   SELECT d.*, FT.SCORE
   FROM rag_documents d, FT_SEARCH_DATA('关键词', 0, 0) FT
   WHERE d.id = FT.KEYS[0]
   ORDER BY FT.SCORE DESC;
   ```

3. **评分机制**
   - TF-IDF 算法
   - 词频统计
   - 字段权重

## 向量搜索原理

### 余弦相似度计算

```
similarity = (A · B) / (||A|| * ||B||)
```

**搜索流程**:
1. 从数据库加载所有向量
2. 计算查询向量与每个文档向量的余弦相似度
3. 按相似度降序排序
4. 返回 TopK 结果

**性能优化**:
- 向量以字符串形式存储（逗号分隔）
- 查询时动态计算相似度
- 适合中小规模数据集（< 10万文档）

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

## 性能优化

### 1. 连接池配置

```yaml
omni-agent:
  rag:
    h2:
      max-pool-size: 20      # 增加最大连接数
      min-pool-size: 5       # 增加最小连接数
```

### 2. 批量索引

```java
// 批量索引文档
List<Document> documents = Arrays.asList(doc1, doc2, doc3);
List<String> docIds = ragService.indexDocuments(documents);
```

### 3. 索引优化

```sql
-- 定期优化全文索引
CALL FT_DROP_INDEX('PUBLIC', 'RAG_DOCUMENTS');
CALL FT_CREATE_INDEX('PUBLIC', 'RAG_DOCUMENTS', 'TITLE,CONTENT,SUMMARY');
```

### 4. 查询缓存

启用 H2 查询缓存:

```yaml
omni-agent:
  rag:
    h2:
      url: jdbc:h2:./data/omni-agent-rag;CACHE_SIZE=65536
```

## H2 Console 使用

### 启用控制台

```yaml
spring:
  h2:
    console:
      enabled: true
      path: /h2-console
```

### 访问控制台

1. 启动应用
2. 访问 `http://localhost:8080/h2-console`
3. 输入连接信息：
   - JDBC URL: `jdbc:h2:./data/omni-agent-rag`
   - User Name: `sa`
   - Password: (留空)

### 常用查询

```sql
-- 查看所有文档
SELECT * FROM rag_documents;

-- 查看文档数量
SELECT COUNT(*) FROM rag_documents;

-- 全文搜索测试
SELECT d.*, FT.SCORE
FROM rag_documents d, FT_SEARCH_DATA('测试', 0, 0) FT
WHERE d.id = FT.KEYS[0]
ORDER BY FT.SCORE DESC;

-- 清空数据
DELETE FROM rag_documents;
```

## 备份与恢复

### 导出数据

```sql
-- 导出到SQL脚本
SCRIPT TO 'backup.sql';

-- 导出到CSV
CALL CSVWRITE('documents.csv', 'SELECT * FROM rag_documents');
```

### 导入数据

```sql
-- 从SQL脚本导入
RUNSCRIPT FROM 'backup.sql';

-- 从CSV导入
CREATE TABLE rag_documents AS SELECT * FROM CSVREAD('documents.csv');
```

### Java代码备份

```java
// 创建备份
String backupPath = "./backup/rag-" + System.currentTimeMillis();
try (Connection conn = dataSource.getConnection();
     Statement stmt = conn.createStatement()) {
    stmt.execute("BACKUP TO '" + backupPath + "'");
}
```

## 注意事项

1. **数据规模** - 适合中小规模数据集（< 100万文档）
2. **向量维度** - 大向量会增加存储和计算开销
3. **文件锁定** - 文件模式下同一时刻只能一个进程访问（除非启用AUTO_SERVER）
4. **内存使用** - 向量搜索需要加载所有向量到内存
5. **全文索引** - 需要定期重建以保持最佳性能

## 适用场景

- ✅ 小到中等规模数据集（< 100万文档）
- ✅ 嵌入式应用（无需外部数据库）
- ✅ 快速原型开发
- ✅ 测试和演示环境
- ✅ 单机应用
- ❌ 超大规模数据（建议用 Elasticsearch）
- ❌ 高并发写入（建议用专业数据库）
- ❌ 分布式架构（建议用 Elasticsearch 或 MongoDB）

## 与其他 RAG 实现对比

| 特性 | H2 | SQLite | MongoDB | Redis | Elasticsearch |
|-----|-------|--------|---------|-------|--------------|
| 部署复杂度 | 低 | 低 | 中 | 中 | 中 |
| 数据规模 | 中 | 中 | 大 | 中 | 大 |
| 全文搜索 | 好 | 优秀 | 好 | 中等 | 优秀 |
| 向量搜索 | 简单 | 简单 | 中等 | 简单 | 优秀 |
| 并发性能 | 中 | 中 | 高 | 高 | 高 |
| 内存使用 | 中 | 低 | 中 | 高 | 高 |

## 升级方案

### 数据迁移

当数据规模增长时，可以迁移到其他实现：

1. **导出 H2 数据**
   ```java
   List<Document> allDocs = new ArrayList<>();
   // 逐批导出文档
   ```

2. **导入到目标系统**
   ```java
   // 切换到 Elasticsearch RAG Starter
   elasticsearchRAGService.indexDocuments(allDocs);
   ```

## 许可证

Apache License 2.0
