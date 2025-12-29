# RAG 嵌入服务实现总结

## ✅ 已完成

### 1. 核心架构 - 装饰器模式

```
EmbeddingRagServiceDecorator
├── 存储服务 (Elasticsearch/MongoDB/H2...)
└── 嵌入服务 (ONNX/Ollama/Online)
```

**职责分离**：
- 存储服务：负责向量存储和检索
- 嵌入服务：负责文本向量化
- 装饰器：组合两者，自动向量化

### 2. 实现的嵌入服务

| 服务 | 文件 | 说明 |
|------|------|------|
| ONNX | `OnnxEmbeddingServiceFactory` | 本地 ONNX 模型推理 |
| Ollama | `OllamaEmbeddingServiceFactory` | Ollama API 调用 |
| Online API | `OnlineEmbeddingServiceFactory` | OpenAI/Azure API |
| 适配器 | `EmbeddingServiceAdapter` | 将 EmbeddingService 适配为 RagService |

### 3. 工作流程

**索引文档**：
```
用户 → Decorator.batchIndex(docs)
      → EmbeddingService.embed(text)  // 向量化
      → StorageService.batchIndex(docs)  // 存储
```

**语义搜索**：
```
用户 → Decorator.semanticSearch(query)
      → EmbeddingService.embed(query)  // 查询向量化
      → StorageService.vectorSearch(vector)  // 向量检索
      → 返回结果
```

## 📝 配置示例

### ONNX 嵌入服务

```yaml
omni-agent:
  rag:
    instances:
      - id: main
        type: elasticsearch
        embedding:
          provider: onnx
          model: bge-base-zh
          dimension: 768
          onnx:
            model-path: models/bge-base-zh/model.onnx
            vocab-path: models/bge-base-zh/vocab.txt
            max-length: 512
```

### Ollama 嵌入服务

```yaml
omni-agent:
  rag:
    instances:
      - id: main
        type: mongodb
        embedding:
          provider: ollama
          model: nomic-embed-text
          dimension: 768
          ollama:
            base-url: http://localhost:11434
            timeout: 30000
```

### Online API 嵌入服务

```yaml
omni-agent:
  rag:
    instances:
      - id: main
        type: h2
        embedding:
          provider: online
          dimension: 1536
          online:
            endpoint: https://api.openai.com/v1
            api-key: ${OPENAI_API_KEY}
            model: text-embedding-ada-002
```

## 🔧 关键实现细节

### EmbeddingServiceAdapter

```java
public class EmbeddingServiceAdapter implements RagService {
    private final EmbeddingService embeddingService;
    
    @Override
    public Vector embed(String text) {
        float[] embedding = embeddingService.embed(text);
        return Vector.of(embedding);  // 使用 Vector.of() 工厂方法
    }
    
    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        // 逐个向量化（EmbeddingService 接口没有批量方法）
        List<Vector> vectors = new ArrayList<>();
        for (String text : texts) {
            vectors.add(embed(text));
        }
        return vectors;
    }
}
```

### EmbeddingRagServiceDecorator

```java
public class EmbeddingRagServiceDecorator implements RagService {
    private final RagService storageService;
    private final RagService embeddingService;
    
    @Override
    public void batchIndex(List<Document> documents) {
        // 1. 自动向量化
        embedDocumentsIfNeeded(documents);
        
        // 2. 存储
        storageService.batchIndex(documents);
    }
    
    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        // 1. 向量化查询
        Vector queryVector = embeddingService.embed(query);
        
        // 2. 向量搜索
        return storageService.vectorSearch(queryVector, maxResults);
    }
}
```

### RagInstanceBuilder

```java
public RagService build() {
    // 1. 创建存储服务
    RagService storageService = buildStorageService();
    
    // 2. 如果配置了嵌入模型，包装嵌入功能
    if (config.getEmbedding() != null) {
        RagService embeddingService = buildEmbeddingService();
        return new EmbeddingRagServiceDecorator(
            storageService, 
            embeddingService, 
            instanceId
        );
    }
    
    return storageService;
}
```

## 🎯 解决的问题

### 问题

❌ Elasticsearch、MongoDB、H2 等存储服务不支持向量化  
❌ `embed()` 方法返回 null  
❌ 语义搜索降级为文本搜索  
❌ 准确性为 0

### 解决方案

✅ 装饰器模式分离存储和嵌入  
✅ 每个实例独立配置嵌入模型  
✅ 自动向量化文档和查询  
✅ 真正的语义搜索

## 📊 对比

| 维度 | 之前（错误） | 之后（正确） |
|------|------------|------------|
| 向量化 | ❌ 返回 null | ✅ ONNX/Ollama/Online |
| 语义搜索 | ❌ 文本搜索 | ✅ 向量搜索 |
| 架构 | ❌ 混乱 | ✅ 清晰分离 |
| 配置 | ❌ 无法配置 | ✅ 灵活配置 |
| 准确性 | ❌ 0% | ✅ 高准确性 |

## 📚 相关文档

- [RAG 存储与嵌入分离架构](./RAG_STORAGE_EMBEDDING_SEPARATION.md)
- [RAG 多实例配置示例](../RAG_MULTI_INSTANCE_EMBEDDING_EXAMPLES.md)

---
**日期**: 2025-12-28  
**状态**: ✅ 已完成  
**版本**: 2.0.0

