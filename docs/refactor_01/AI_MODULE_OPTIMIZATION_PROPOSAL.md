# AI 模块架构分析与优化建议

> 日期：2025-12-27  
> 议题：Ollama 和 Online API 是否应该实现 Embedding 功能

---

## 📊 现状分析

### 当前三个 AI Starter 模块的职责

| 模块 | 当前职责 | 实现接口 | 核心能力 |
|------|---------|---------|---------|
| **omni-agent-ai-starter-onnx** | 文本向量化 | `EmbeddingService` | 本地 ONNX 模型推理 |
| **omni-agent-ai-starter-ollama** | LLM 问答 | `AIService` | 调用 Ollama API |
| **omni-agent-ai-starter-online-api** | LLM 问答 | `AIService` | 调用在线大模型 API |

### 问题分析

**你的观察非常正确！** 当前存在以下问题：

1. **职责划分不清晰**
   - ONNX 只做 Embedding
   - Ollama/Online 只做问答
   - 但实际上 Ollama 和 Online API **都支持 Embedding**！

2. **功能割裂**
   - 想用 Ollama 做问答，还得单独配置 ONNX 做 Embedding
   - 无法利用大模型的语义理解能力进行向量化

3. **能力浪费**
   - 在线大模型（如 GPT-4、Claude）的语义理解能力远超本地小模型
   - Ollama 本身就支持 Embedding API
   - 没有利用这些能力

---

## 🎯 优化建议

### 方案：让 Ollama 和 Online API 同时实现两个接口

```java
// Ollama 应该实现两个接口
public class OllamaAIService implements AIService, EmbeddingService {
    
    // AIService 接口方法
    @Override
    public AIResponse generate(AIRequest request) { ... }
    
    @Override
    public String chat(String userMessage) { ... }
    
    // EmbeddingService 接口方法（新增）
    @Override
    public float[] embed(String text) {
        // 调用 Ollama 的 /api/embeddings 接口
        return ollamaEmbedding(text);
    }
    
    @Override
    public List<float[]> embedBatch(List<String> texts) {
        // 批量调用 Ollama Embedding
        return texts.stream()
            .map(this::embed)
            .toList();
    }
    
    @Override
    public int getDimension() {
        // 根据模型返回维度
        // 例如：nomic-embed-text 是 768 维
        return 768;
    }
    
    @Override
    public String getEmbeddingModel() {
        return properties.getEmbeddingModel(); // 例如 "nomic-embed-text"
    }
}
```

---

## 💡 技术实现

### 1. Ollama Embedding API

Ollama 已经提供了 Embedding API：

```bash
# Ollama Embedding API
curl http://localhost:11434/api/embeddings -d '{
  "model": "nomic-embed-text",
  "prompt": "这是一个测试文本"
}'

# 返回
{
  "embedding": [0.123, 0.456, ..., 0.789]  // 768维向量
}
```

**支持的 Embedding 模型：**
- `nomic-embed-text` - 768维（推荐）
- `mxbai-embed-large` - 1024维
- `all-minilm` - 384维

### 2. OpenAI Embedding API

OpenAI 也提供了强大的 Embedding API：

```bash
# OpenAI Embedding API
curl https://api.openai.com/v1/embeddings \
  -H "Authorization: Bearer $OPENAI_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "input": "这是一个测试文本",
    "model": "text-embedding-3-small"
  }'
```

**支持的 Embedding 模型：**
- `text-embedding-3-small` - 1536维（推荐，性价比高）
- `text-embedding-3-large` - 3072维（最强）
- `text-embedding-ada-002` - 1536维（经典）

---

## 🚀 实施方案

### Phase 1：扩展 OllamaAIService（立即执行）

```java
package top.yumbo.ai.omni.ai.ollama;

import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.ai.api.EmbeddingService;

/**
 * Ollama AI 服务实现（同时支持问答和 Embedding）
 */
@Slf4j
public class OllamaAIService implements AIService, EmbeddingService {
    
    private final RestTemplate restTemplate;
    private final OllamaProperties properties;
    
    // ========== AIService 方法 ==========
    
    @Override
    public AIResponse generate(AIRequest request) {
        // 现有实现
    }
    
    @Override
    public String chat(String userMessage) {
        // 现有实现
    }
    
    // ========== EmbeddingService 方法（新增）==========
    
    @Override
    public float[] embed(String text) {
        String url = properties.getBaseUrl() + "/api/embeddings";
        
        Map<String, Object> requestBody = new HashMap<>();
        requestBody.put("model", properties.getEmbeddingModel()); // 例如 "nomic-embed-text"
        requestBody.put("prompt", text);
        
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        HttpEntity<Map<String, Object>> entity = new HttpEntity<>(requestBody, headers);
        
        ResponseEntity<Map> response = restTemplate.postForEntity(url, entity, Map.class);
        
        if (response.getStatusCode() == HttpStatus.OK && response.getBody() != null) {
            List<Double> embedding = (List<Double>) response.getBody().get("embedding");
            return embedding.stream()
                .mapToDouble(Double::doubleValue)
                .toArray();
        }
        
        throw new RuntimeException("Ollama embedding failed");
    }
    
    @Override
    public List<float[]> embedBatch(List<String> texts) {
        return texts.stream()
            .map(this::embed)
            .toList();
    }
    
    @Override
    public int getDimension() {
        // 根据模型返回维度
        String model = properties.getEmbeddingModel();
        return switch (model) {
            case "nomic-embed-text" -> 768;
            case "mxbai-embed-large" -> 1024;
            case "all-minilm" -> 384;
            default -> 768;
        };
    }
    
    @Override
    public String getEmbeddingModel() {
        return properties.getEmbeddingModel();
    }
}
```

### Phase 2：扩展 OnlineAPIAIService

```java
package top.yumbo.ai.omni.ai.online;

import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.ai.api.EmbeddingService;

/**
 * 在线 API AI 服务实现（同时支持问答和 Embedding）
 */
@Slf4j
public class OnlineAPIAIService implements AIService, EmbeddingService {
    
    // ========== EmbeddingService 方法（新增）==========
    
    @Override
    public float[] embed(String text) {
        String provider = properties.getProvider(); // "openai", "azure", etc.
        
        if ("openai".equals(provider)) {
            return embedWithOpenAI(text);
        } else if ("azure".equals(provider)) {
            return embedWithAzure(text);
        }
        
        throw new UnsupportedOperationException("Provider " + provider + " does not support embedding");
    }
    
    private float[] embedWithOpenAI(String text) {
        String url = properties.getBaseUrl() + "/v1/embeddings";
        
        Map<String, Object> requestBody = new HashMap<>();
        requestBody.put("model", properties.getEmbeddingModel()); // "text-embedding-3-small"
        requestBody.put("input", text);
        
        // ... HTTP 调用
        
        return embedding;
    }
    
    @Override
    public int getDimension() {
        String model = properties.getEmbeddingModel();
        return switch (model) {
            case "text-embedding-3-small", "text-embedding-ada-002" -> 1536;
            case "text-embedding-3-large" -> 3072;
            default -> 1536;
        };
    }
}
```

### Phase 3：配置扩展

```java
// OllamaProperties.java
@Data
@ConfigurationProperties(prefix = "ai.ollama")
public class OllamaProperties {
    
    // 现有配置
    private String baseUrl = "http://localhost:11434";
    private String defaultModel = "qwen2.5:7b";
    
    // 新增 Embedding 配置
    private String embeddingModel = "nomic-embed-text"; // ⭐ 新增
    private boolean enableEmbedding = true; // ⭐ 新增
}
```

```yaml
# application.yml
ai:
  ollama:
    base-url: http://localhost:11434
    default-model: qwen2.5:7b
    embedding-model: nomic-embed-text  # ⭐ 新增
    enable-embedding: true              # ⭐ 新增
```

---

## 🎯 优势分析

### 1. 统一的 AI 服务

```java
@Service
public class MyService {
    
    @Autowired
    private AIService aiService; // 可能是 Ollama
    
    @Autowired
    private EmbeddingService embeddingService; // 也是同一个 Ollama 实例！
    
    public void demo() {
        // 使用同一个服务做问答和 Embedding
        String answer = aiService.chat("什么是 Spring Boot？");
        float[] embedding = embeddingService.embed("Spring Boot 教程");
    }
}
```

### 2. 更强的语义理解

| 模型 | 语义理解能力 | 向量质量 | 成本 |
|------|------------|---------|------|
| ONNX (bge-base-zh) | ⭐⭐⭐ | 中等 | 免费（本地） |
| Ollama (nomic-embed-text) | ⭐⭐⭐⭐ | 较好 | 免费（本地） |
| OpenAI (text-embedding-3-small) | ⭐⭐⭐⭐⭐ | 最佳 | 付费 |

### 3. 灵活配置

```yaml
# 场景1：本地开发，使用 Ollama（问答 + Embedding）
ai:
  ollama:
    enabled: true
    embedding-model: nomic-embed-text

# 场景2：生产环境，使用 OpenAI（问答 + Embedding）
ai:
  online:
    enabled: true
    provider: openai
    embedding-model: text-embedding-3-small

# 场景3：混合模式（Ollama 问答 + OpenAI Embedding）
ai:
  ollama:
    enabled: true
  online:
    enabled: true
    embedding-only: true  # 只用于 Embedding
```

---

## 📊 对比表

### 修改前 vs 修改后

| 维度 | 修改前 | 修改后 |
|------|--------|--------|
| **ONNX** | 只做 Embedding | 只做 Embedding（不变） |
| **Ollama** | 只做问答 | 问答 + Embedding ⭐ |
| **Online API** | 只做问答 | 问答 + Embedding ⭐ |
| **配置复杂度** | 需要配置两个服务 | 配置一个服务即可 |
| **语义理解** | 依赖本地小模型 | 可用大模型 ⭐ |
| **灵活性** | 低 | 高 ⭐ |

---

## 🔧 实施步骤

### Step 1：修改 OllamaAIService（今天���

1. ✅ 实现 `EmbeddingService` 接口
2. ✅ 添加 `embed()` 方法
3. ✅ 添加 `embeddingModel` 配置
4. ✅ 更新文档

### Step 2：修改 OnlineAPIAIService（明天）

1. ✅ 实现 `EmbeddingService` 接口
2. ✅ 支持 OpenAI Embedding API
3. ✅ 支持 Azure Embedding API
4. ✅ 更新配置

### Step 3：更新 FileRagService（后天）

```java
@Configuration
public class FileRagAutoConfiguration {
    
    @Bean
    public RagService fileRagService(
            FileRagProperties properties,
            @Autowired(required = false) EmbeddingService embeddingService) {
        
        // embeddingService 现在可能是：
        // 1. OnnxEmbeddingService
        // 2. OllamaAIService（实现了 EmbeddingService）⭐
        // 3. OnlineAPIAIService（实现了 EmbeddingService）⭐
        
        return new FileRagService(
                properties.getDefaultDomainId(),
                properties.getIndexPath(),
                embeddingService
        );
    }
}
```

---

## ✅ 总结

### 你的观点完全正确！

1. **Ollama 和 Online API 确实应该支持 Embedding**
   - 它们的底层 API 都支持
   - 大模型的语义理解能力更强
   - 可以统一服务，简化配置

2. **当前的划分确实有问题**
   - 职责划分不清晰
   - 功能割裂
   - 能力浪费

3. **建议的改进**
   - 让 Ollama 和 Online 同时实现 `AIService` 和 `EmbeddingService`
   - 用户可以选择使用哪个服务做 Embedding
   - 更灵活、更强大

### 下一步行动

**建议立即实施！** 这个改进非常有价值。

---

**创建时间：** 2025-12-27  
**状态：** 📝 分析完成，建议实施  
**优先级：** 🔴 高（架构优化）


