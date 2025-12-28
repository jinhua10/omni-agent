# RAG 多实例配置完整示例

## 基础配置

### 示例 1: 单实例（最简配置）

```yaml
omni-agent:
  rag:
    instances:
      - type: file
```

### 示例 2: 单实例（自定义嵌入模型）

```yaml
omni-agent:
  rag:
    instances:
      - id: main
        type: file
        primary: true
        file:
          index-path: ./data/rag
        embedding:
          provider: onnx
          model: bge-base-zh
          dimension: 768
          onnx:
            model-path: models/bge-base-zh/model.onnx
            vocab-path: models/bge-base-zh/vocab.txt
```

## 多实例配置

### 示例 3: 主从架构（不同嵌入模型）

```yaml
omni-agent:
  rag:
    instances:
      # 主实例 - ONNX 本地模型（中文）
      - id: primary
        name: "主索引-中文"
        type: file
        primary: true
        file:
          index-path: ./data/rag-primary
        embedding:
          provider: onnx
          dimension: 768
          model: bge-base-zh
          onnx:
            model-path: models/bge-base-zh/model.onnx
            vocab-path: models/bge-base-zh/vocab.txt
      
      # 备份实例 - OpenAI API（英文）
      - id: backup
        name: "备份存储-英文"
        type: h2
        h2:
          database-path: ./data/rag-backup
        embedding:
          provider: online
          dimension: 1536
          online:
            endpoint: https://api.openai.com/v1
            api-key: ${OPENAI_API_KEY}
            model: text-embedding-ada-002
```

### 示例 4: 三层缓存（混合模型）

```yaml
omni-agent:
  rag:
    instances:
      # L1 热缓存 - Ollama（快速）
      - id: hot-cache
        name: "热数据缓存"
        type: redis
        primary: true
        redis:
          key-prefix: "rag:hot:"
          document-ttl: 1800  # 30分钟
        embedding:
          provider: ollama
          dimension: 768
          ollama:
            base-url: http://localhost:11434
            model: nomic-embed-text
      
      # L2 温存储 - ONNX（高性能）
      - id: warm-storage
        name: "温数据存储"
        type: file
        file:
          index-path: ./data/rag-warm
        embedding:
          provider: onnx
          dimension: 768
          model: bge-base-zh
          onnx:
            model-path: models/bge-base-zh/model.onnx
      
      # L3 冷归档 - ONNX 小模型（节省资源）
      - id: cold-archive
        name: "冷数据归档"
        type: h2
        h2:
          database-path: ./data/rag-cold
        embedding:
          provider: onnx
          dimension: 384  # 小维度模型
          model: bge-small-zh
          onnx:
            model-path: models/bge-small-zh/model.onnx
```

### 示例 5: 多语言支持（每种语言独立模型）

```yaml
omni-agent:
  rag:
    instances:
      # 中文文档
      - id: chinese
        name: "中文文档库"
        type: file
        primary: true
        file:
          index-path: ./data/rag-zh
        embedding:
          provider: onnx
          dimension: 768
          model: bge-base-zh
          onnx:
            model-path: models/bge-base-zh/model.onnx
            vocab-path: models/bge-base-zh/vocab.txt
      
      # 英文文档
      - id: english
        name: "英文文档库"
        type: file
        file:
          index-path: ./data/rag-en
        embedding:
          provider: online
          dimension: 1536
          online:
            endpoint: https://api.openai.com/v1
            api-key: ${OPENAI_API_KEY}
            model: text-embedding-ada-002
      
      # 多语言混合
      - id: multilingual
        name: "多语言文档库"
        type: h2
        h2:
          database-path: ./data/rag-multi
        embedding:
          provider: ollama
          dimension: 768
          ollama:
            base-url: http://localhost:11434
            model: multilingual-e5-base
```

## 代码使用示例

### 单实例使用

```java
@Service
public class DocumentService {
    
    @Autowired
    private RagService ragService;  // 自动注入主实例
    
    public void indexDocument(String text) {
        Document doc = new Document();
        doc.setContent(text);
        ragService.indexDocument(doc);  // 自动使用配置的嵌入模型
    }
    
    public List<Document> search(String query) {
        return ragService.semanticSearch(query, 10);  // 自动向量化查询
    }
}
```

### 多实例使用

```java
@Service
public class MultiLanguageSearchService {
    
    @Autowired
    private RagServiceRegistry registry;
    
    public List<Document> searchChinese(String query) {
        RagService chineseRag = registry.getServiceOrThrow("chinese");
        return chineseRag.semanticSearch(query, 10);  // 使用中文模型
    }
    
    public List<Document> searchEnglish(String query) {
        RagService englishRag = registry.getServiceOrThrow("english");
        return englishRag.semanticSearch(query, 10);  // 使用英文模型
    }
    
    public Map<String, List<Document>> searchAll(String query) {
        Map<String, List<Document>> results = new HashMap<>();
        
        registry.getAllServices().forEach((id, service) -> {
            List<Document> docs = service.semanticSearch(query, 5);
            results.put(id, docs);
        });
        
        return results;
    }
}
```

### 缓存策略示例

```java
@Service
public class CachedSearchService {
    
    @Autowired
    private RagServiceRegistry registry;
    
    public List<Document> search(String query) {
        // 1. 先查热缓存
        Optional<RagService> hotCache = registry.getService("hot-cache");
        if (hotCache.isPresent()) {
            List<Document> results = hotCache.get().semanticSearch(query, 10);
            if (!results.isEmpty()) {
                return results;  // 命中热缓存
            }
        }
        
        // 2. 查温存储
        Optional<RagService> warmStorage = registry.getService("warm-storage");
        if (warmStorage.isPresent()) {
            List<Document> results = warmStorage.get().semanticSearch(query, 10);
            if (!results.isEmpty()) {
                // 回填热缓存
                hotCache.ifPresent(cache -> {
                    results.forEach(cache::indexDocument);
                });
                return results;
            }
        }
        
        // 3. 查冷归档
        RagService coldArchive = registry.getServiceOrThrow("cold-archive");
        return coldArchive.semanticSearch(query, 10);
    }
}
```

## 配置说明

### 嵌入模型配置项

#### ONNX 配置
```yaml
embedding:
  provider: onnx
  model: bge-base-zh  # 模型名称
  dimension: 768       # 向量维度
  onnx:
    model-path: models/bge-base-zh/model.onnx
    vocab-path: models/bge-base-zh/vocab.txt
    max-length: 512    # 最大序列长度
    pooling: true      # 是否使用池化
```

#### Online API 配置
```yaml
embedding:
  provider: online
  dimension: 1536
  online:
    endpoint: https://api.openai.com/v1
    api-key: ${OPENAI_API_KEY}
    model: text-embedding-ada-002
    timeout: 30000  # 超时时间（毫秒）
```

#### Ollama 配置
```yaml
embedding:
  provider: ollama
  dimension: 768
  ollama:
    base-url: http://localhost:11434
    model: nomic-embed-text
    timeout: 30000
```

### 向量维度优先级

1. 实例级别 `vector-dimension`（最高优先级）
2. 嵌入模型配置 `embedding.dimension`
3. 全局配置 `omni-agent.rag.vector-dimension`（默认 768）

示例：
```yaml
omni-agent:
  rag:
    vector-dimension: 768  # 全局默认
    instances:
      - id: instance1
        vector-dimension: 1024  # 实例级别（优先级最高）
        
      - id: instance2
        embedding:
          dimension: 1536  # 嵌入模型级别（次优先级）
          
      - id: instance3
        # 使用全局默认 768
```

## 性能优化建议

### 1. 根据文档语言选择模型

- **中文文档**: `bge-base-zh` 或 `bge-large-zh` (ONNX)
- **英文文档**: `text-embedding-ada-002` (Online) 或 `bge-base-en` (ONNX)
- **多语言**: `multilingual-e5-base` (Ollama) 或 `text-embedding-3-large` (Online)

### 2. 根据规模选择后端

- **小规模 (<10万文档)**: File/Lucene
- **中规模 (10万-100万)**: H2 或 SQLite
- **大规模 (>100万)**: MongoDB 或 Elasticsearch

### 3. 成本优化

- **免费方案**: 全部使用 ONNX 本地模型
- **混合方案**: 热数据用 ONNX，冷数据用 Online API
- **云端方案**: 全部使用 Online API

### 4. 维度选择

| 模型维度 | 性能 | 准确度 | 存储需求 |
|---------|------|--------|---------|
| 384 | 最快 | 低 | 最小 |
| 768 | 快 | 中 | 中等 |
| 1024 | 中 | 高 | 较大 |
| 1536 | 慢 | 最高 | 最大 |

---
**版本**: 2.0.0  
**日期**: 2025-12-28  
**特性**: 支持每个实例独立配置嵌入模型

