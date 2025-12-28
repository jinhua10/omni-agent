# OmniAgent 基础示例

这是 OmniAgent 四维可插拔架构的基础示例项目，演示如何使用不同的 Starter 组合。

## 🎯 示例配置

当前示例使用以下 Starter 组合：

| 维度 | 实现 | 用途 |
|------|------|------|
| **Persistence** | Memory | 内存持久化（开发/测试） |
| **Document Storage** | File | 文件存储 |
| **RAG** | File/Lucene | 本地检索 |
| **AI** | Ollama | 本地AI推理 |

## 🚀 快速开始

### 1. 启动应用

```bash
# 进入项目根目录
cd omni-agent-p2p-basic

# 运行应用
mvn spring-boot:run
```

### 2. 访问接口

应用启动后，访问 http://localhost:8080

#### 健康检查
```bash
curl http://localhost:8080/api/demo/health
```

#### 完整流程演示
```bash
curl -X POST http://localhost:8080/api/demo/full-workflow \
  -H "Content-Type: application/json" \
  -d '{
    "documentId": "doc001",
    "title": "OmniAgent 使用指南",
    "content": "OmniAgent 是一个四维可插拔的 AI 智能体框架，支持灵活切换不同的实现。",
    "summary": "可插拔 AI 框架",
    "searchQuery": "可插拔"
  }'
```

## 📝 API 端点

### Persistence（持久化）

- `POST /api/demo/question-type` - 添加问题类型配置
- `GET /api/demo/question-type/{type}` - 获取问题类型配置
- `GET /api/demo/question-type/keyword/{keyword}` - 根据关键词查询
- `GET /api/demo/question-types` - 获取所有问题类型

### Document Storage（文档存储）

- `POST /api/demo/chunks/{documentId}` - 保存文档分块
- `GET /api/demo/chunks/{documentId}` - 获取文档分块
- `GET /api/demo/storage/statistics` - 获取存储统计
- `GET /api/demo/storage/health` - 检查存储健康状态

### RAG（检索增强生成）

- `POST /api/demo/rag/index` - 索引文档
- `GET /api/demo/rag/search?query={query}&topK={topK}` - 文本搜索
- `POST /api/demo/rag/vector-search?topK={topK}` - 向量搜索
- `POST /api/demo/rag/hybrid-search?query={query}&topK={topK}&textWeight={weight}` - 混合检索
- `GET /api/demo/rag/statistics` - 获取索引统计
- `GET /api/demo/rag/health` - 检查 RAG 健康状态
- `POST /api/demo/rag/rebuild` - 重建索引

### 综合示例

- `POST /api/demo/full-workflow` - 完整流程演示（存储 → 索引 → 检索）

## 🔄 切换实现

### 切换到生产级配置

修改 `pom.xml`：

```xml
<!-- 持久化：改用 Elasticsearch -->
<dependency>
    <artifactId>omni-agent-persistence-starter-elasticsearch</artifactId>
</dependency>

<!-- 文档存储：改用 MongoDB -->
<dependency>
    <artifactId>omni-agent-document-storage-starter-mongodb</artifactId>
</dependency>

<!-- RAG：改用 Elasticsearch -->
<dependency>
    <artifactId>omni-agent-rag-starter-elasticsearch</artifactId>
</dependency>

<!-- AI：改用在线API -->
<dependency>
    <artifactId>omni-agent-ai-starter-online-api</artifactId>
</dependency>
```

修改 `application.yml`：

```yaml
omni-agent:
  persistence:
    type: elasticsearch
    elasticsearch:
      host: localhost:9200
  
  document-storage:
    type: mongodb
    mongodb:
      uri: mongodb://localhost:27017
      database: omni-storage
  
  rag:
    type: elasticsearch
    elasticsearch:
      host: localhost:9200
      index: omni-rag
  
  ai:
    type: online-api
    online-api:
      provider: openai
      api-key: your-api-key
      model: gpt-4
```

### 切换到轻量级配置（适合嵌入式/边缘设备）

```xml
<!-- 持久化：改用 SQLite -->
<dependency>
    <artifactId>omni-agent-persistence-starter-sqlite</artifactId>
</dependency>

<!-- 文档存储：改用 File -->
<dependency>
    <artifactId>omni-agent-document-storage-starter-file</artifactId>
</dependency>

<!-- RAG：改用 SQLite -->
<dependency>
    <artifactId>omni-agent-rag-starter-sqlite</artifactId>
</dependency>

<!-- AI：改用本地 Ollama -->
<dependency>
    <artifactId>omni-agent-ai-starter-ollama</artifactId>
</dependency>
```

```yaml
omni-agent:
  persistence:
    type: sqlite
    sqlite:
      database-path: ./data/omni.db
  
  document-storage:
    type: file
    file:
      base-path: ./data/documents
  
  rag:
    type: sqlite
    sqlite:
      database-path: ./data/rag.db
  
  ai:
    type: ollama
    ollama:
      base-url: http://localhost:11434
      model: qwen2.5:latest
```

## 🎓 学习要点

1. **依赖注入**：通过 Spring 自动注入服务接口
   ```java
   @Autowired
   private QuestionClassifierPersistence persistence;
   
   @Autowired
   private DocumentStorageService storageService;
   
   @Autowired
   private RAGService ragService;
   ```

2. **零代码切换**：只需修改 `pom.xml` 和 `application.yml`，业务代码无需改动

3. **统一接口**：所有实现都遵循相同的接口规范

4. **灵活组合**：四个维度可以独立选择不同的实现

## 📚 更多示例

- [生产级示例](../omni-agent-example-production/README.md) - 高可用、分布式部署
- [性能测试示例](../omni-agent-example-benchmark/README.md) - 性能对比测试
- [微服务示例](../omni-agent-example-microservice/README.md) - 微服务架构

## 🔗 相关文档

- [架构设计](../../docs/refactor/ARCHITECTURE-REDESIGN.md)
- [实施路线图](../../docs/refactor/IMPLEMENTATION-ROADMAP.md)
- [API 文档](../../docs/api/README.md)
- [Starter 开发指南](../../docs/starter/README.md)

## 📄 许可证

Apache License 2.0
