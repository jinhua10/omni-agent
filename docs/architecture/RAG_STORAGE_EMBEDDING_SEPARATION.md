# RAG 架构说明 - 存储与嵌入分离

## 📋 问题分析

### 发现的问题

你的观察完全正确！当前实现存在**严重的架构问题**：

1. **Elasticsearch、MongoDB、H2、Redis、SQLite 等存储服务都不支持文本向量化**
2. 它们的 `embed()` 和 `batchEmbed()` 方法只返回 `null` 或空列表
3. **语义搜索完全无法工作**，准确率为 0

### 问题根源

```java
// ElasticsearchRAGService.java
@Override
public Vector embed(String text) {
    log.warn("ElasticsearchRAGService 不提供嵌入功能");  // ❌ 无法向量化
    return null;
}

@Override
public List<Document> semanticSearch(String query, int maxResults) {
    log.debug("语义搜索: query={}, maxResults={}", query, maxResults);
    return searchByTextInternal(query, maxResults);  // ❌ 只是文本搜索，不是语义搜索！
}
```

**这意味着**：
- ❌ 语义搜索降级为普通文本搜索
- ❌ 向量搜索无法使用
- ❌ 文档没有向量，存储的向量都是空的

## ✅ 解决方案：装饰器模式

### 架构设计

```
┌─────────────────────────────────────┐
│  EmbeddingRagServiceDecorator       │
│                                     │
│  ┌──────────────┐  ┌─────────────┐ │
│  │ 存储服务      │  │ 嵌入服务     │ │
│  │ (ES/MongoDB) │  │ (ONNX/...)  │ │
│  └──────────────┘  └─────────────┘ │
└─────────────────────────────────────┘
```

### 职责分离

| 组件 | 职责 | 示例 |
|------|------|------|
| **存储服务** | 存储和检索向量 | Elasticsearch, MongoDB, H2 |
| **嵌入服务** | 文本向量化 | ONNX, Ollama, Online API |
| **装饰器** | 组合两者，自动向量化 | EmbeddingRagServiceDecorator |

## 🔧 实现细节

### 1. EmbeddingRagServiceDecorator

**核心功能**：

```java
public class EmbeddingRagServiceDecorator implements RagService {
    private final RagService storageService;    // 存储后端
    private final RagService embeddingService;  // 嵌入服务
    
    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        // 1. 使用嵌入服务将查询向量化
        Vector queryVector = embeddingService.embed(query);
        
        // 2. 使用存储服务进行向量搜索
        return storageService.vectorSearch(queryVector, maxResults);
    }
    
    @Override
    public void batchIndex(List<Document> documents) {
        // 自动向量化文档
        embedDocumentsIfNeeded(documents);
        
        // 存储到后端
        storageService.batchIndex(documents);
    }
}
```

### 2. RagInstanceBuilder 自动组装

```java
public RagService build() {
    // 1. 创建存储服务
    RagService storageService = switch (type) {
        case "elasticsearch" -> new ElasticsearchRAGService(...);
        case "mongodb" -> new MongoDBRAGService(...);
        // ...
    };
    
    // 2. 如果配置了嵌入模型，包装嵌入功能
    if (config.getEmbedding() != null) {
        RagService embeddingService = buildEmbeddingService();
        return new EmbeddingRagServiceDecorator(
            storageService, 
            embeddingService, 
            instanceId
        );
    }
    
    return storageService;  // 无嵌入功能（仅存储）
}
```

## 📝 配置示例

### 单实例 - Elasticsearch + ONNX

```yaml
omni-agent:
  rag:
    instances:
      - id: main
        type: elasticsearch  # 存储后端
        primary: true
        embedding:
          provider: onnx  # 嵌入服务
          model: bge-base-zh
          dimension: 768
          onnx:
            model-path: models/bge-base-zh/model.onnx
```

**工作流程**：
1. 用户索引文档 → 自动用 ONNX 向量化 → 存储到 Elasticsearch
2. 用户语义搜索 → ONNX 向量化查询 → Elasticsearch 向量搜索 → 返回结果

### 多实例 - 不同组合

```yaml
omni-agent:
  rag:
    instances:
      # MongoDB + Ollama
      - id: chinese-docs
        type: mongodb
        embedding:
          provider: ollama
          model: nomic-embed-text
      
      # Elasticsearch + OpenAI
      - id: english-docs
        type: elasticsearch
        embedding:
          provider: online
          model: text-embedding-ada-002
          online:
            api-key: ${OPENAI_API_KEY}
      
      # H2 + ONNX
      - id: local-cache
        type: h2
        embedding:
          provider: onnx
          model: bge-small-zh
```

## 🎯 优势

### 1. 关注点分离

| 之前（❌错误） | 之后（✅正确） |
|-------------|-------------|
| 每个存储都要实现嵌入 | 存储只管存储 |
| 重复代码 | 嵌入逻辑统一 |
| 难以切换模型 | 配置即可切换 |

### 2. 灵活组合

- ✅ 任意存储 + 任意嵌入模型
- ✅ 不同实例使用不同组合
- ✅ 运行时切换

### 3. 清晰的架构

```
用户 → Decorator → Embedding Service (向量化)
                ↓
                → Storage Service (存储/检索)
```

## 🔮 下一步工作

### 需要实现的嵌入服务

1. **OnnxEmbeddingService** - ONNX 本地推理
   ```java
   public class OnnxEmbeddingService implements RagService {
       @Override
       public Vector embed(String text) {
           // ONNX 推理
       }
   }
   ```

2. **OllamaEmbeddingService** - Ollama API 调用
   ```java
   public class OllamaEmbeddingService implements RagService {
       @Override
       public Vector embed(String text) {
           // 调用 Ollama API
       }
   }
   ```

3. **OnlineEmbeddingService** - 云端 API（OpenAI、Azure等）
   ```java
   public class OnlineEmbeddingService implements RagService {
       @Override
       public Vector embed(String text) {
           // 调用云端 API
       }
   }
   ```

### 配置示例更新

更新所有配置示例，明确说明：
- 存储服务负责什么
- 嵌入服务负责什么
- 如何组合使用

## 📊 对比

### 之前（错误架构）

```
ElasticsearchRAGService
  ├─ embed() → ❌ return null
  └─ semanticSearch() → ❌ 纯文本搜索，不是语义搜索
```

**结果**：语义搜索完全失效

### 之后（正确架构）

```
EmbeddingRagServiceDecorator
  ├─ embeddingService.embed() → ✅ ONNX 向量化
  └─ storageService.vectorSearch() → ✅ ES 向量检索
```

**结果**：真正的语义搜索！

## ⚠️ 重要说明

### 当前状态

- ✅ 架构设计完成
- ✅ 装饰器实现完成
- ✅ 自动组装逻辑完成
- ⚠️ 嵌入服务实现待完成（ONNX/Ollama/Online）

### 临时方案

如果没有配置 `embedding`，系统会：
1. 返回纯存储服务
2. 语义搜索不可用
3. 日志会有警告

建议：**必须配置嵌入服务才能使用语义搜索**

---
**日期**: 2025-12-28  
**问题**: 存储服务不支持向量化，语义搜索失效  
**解决**: 装饰器模式分离存储和嵌入  
**状态**: 架构完成，待实现具体嵌入服务

