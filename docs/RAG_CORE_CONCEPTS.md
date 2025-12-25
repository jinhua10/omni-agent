# RAG 系统核心概念说明

## 📚 概念解释

### 1. EmbeddingService（向量生成服务）

**作用**: 将文本转换为向量（Embedding）

- **输入**: 文本字符串 `"这是一段文本"`
- **输出**: 向量数组 `[0.123, 0.456, 0.789, ...]`（例如 768 维）
- **用途**: 生成向量，用于后续的向量检索

**接口定义**:
```java
public interface EmbeddingService {
    float[] embed(String text);
    List<float[]> embedBatch(List<String> texts);
    int getDimension();
    String getEmbeddingModel();
}
```

---

### 2. RAGService（检索增强生成服务）

**作用**: 存储向量并进行相似度检索

**功能**:
- 索引文档（保存向量到向量数据库）
- 向量检索（根据查询向量找到最相似的文档）
- 文本检索（基于 BM25/Lucene 的全文搜索）
- 混合检索（结合文本和向量）

**接口定义**:
```java
public interface RAGService {
    // 文档索引
    String indexDocument(Document document);
    
    // 文本搜索
    List<SearchResult> searchByText(String text, int topK);
    
    // 向量搜索
    List<SearchResult> vectorSearch(float[] embedding, int topK);
    
    // 混合检索
    List<SearchResult> hybridSearch(Query query);
    
    // 语义搜索（自动向量化）
    List<SearchResult> semanticSearch(String text, int topK);
}
```

---

### 3. PPL 困惑度模型（ONNX）

**作用**: 计算文本的困惑度，用于智能分块

- **输入**: 文本序列
- **输出**: 困惑度分数（Perplexity）
- **用途**: **不是**用于向量检索，而是用于文本分块

---

## 🔍 重要区别

### PPL 模型 vs Embedding 模型

| 特性 | PPL 困惑度模型 | Embedding 模型 |
|------|---------------|---------------|
| **目的** | 判断文本连贯性 | 生成语义向量 |
| **输出** | 困惑度分数（标量） | 向量（768维数组） |
| **用途** | 文本分块 | 向量检索 |
| **例子** | "这段文字困惑度=3.5" | "[0.12, 0.45, ...]" |

### 能否用 PPL 做向量检索？❌

**答案：不能直接使用**

**原因**：
- PPL 输出的是困惑度分数（一个数字），不是语义向量
- 向量检索需要高维向量（如 768 维），才能表达丰富的语义信息
- PPL 关注的是文本流畅性，不是语义相似度

---

## 🔧 重构前的实现（old 目录）

在 `old/ai-reviewer-base-file-rag` 项目中，使用了 **ONNX Runtime** 实现了两个功能：

### 1. LocalEmbeddingEngine（文本向量化）

**文件**: `top/yumbo/ai/rag/impl/embedding/LocalEmbeddingEngine.java`

**技术栈**:
- ONNX Runtime (ai.onnxruntime)
- 支持的模型：bge-base-zh-v1.5、bge-m3、text2vec-base-chinese

**核心功能**:
```java
public class LocalEmbeddingEngine implements AutoCloseable {
    private OrtEnvironment env;
    private OrtSession session;
    
    // 将文本转换为向量
    public float[] embed(String text) {
        // 1. 分词
        long[] inputIds = tokenize(text);
        long[] attentionMask = createAttentionMask(inputIds);
        long[] tokenTypeIds = createTokenTypeIds(inputIds);
        
        // 2. 构建 ONNX 输入张量
        OnnxTensor inputIdsTensor = OnnxTensor.createTensor(env, new long[][]{inputIds});
        OnnxTensor attentionMaskTensor = OnnxTensor.createTensor(env, new long[][]{attentionMask});
        OnnxTensor tokenTypeIdsTensor = OnnxTensor.createTensor(env, new long[][]{tokenTypeIds});
        
        Map<String, OnnxTensor> inputs = new HashMap<>();
        inputs.put("input_ids", inputIdsTensor);
        inputs.put("attention_mask", attentionMaskTensor);
        inputs.put("token_type_ids", tokenTypeIdsTensor);
        
        // 3. 模型推理
        OrtSession.Result result = session.run(inputs);
        
        // 4. 提取向量（[CLS] token 的嵌入）
        float[][][] output3d = (float[][][]) result.get(0).getValue();
        float[] vector = output3d[0][0]; // batch=0, token=0 ([CLS])
        
        // 5. L2 归一化（用于余弦相似度）
        return l2Normalize(vector);
    }
    
    // 批量向量化
    public List<float[]> embedBatch(List<String> texts) {
        return texts.stream()
            .map(this::embed)
            .collect(Collectors.toList());
    }
}
```

**特点**:
- **输出维度**: 768 维（bge-base-zh-v1.5）或 1024 维（bge-m3）
- **本地推理**: 无需网络请求
- **适用场景**: 向量检索

---

### 2. PPLOnnxService（困惑度计算）

**文件**: `top/yumbo/ai/rag/ppl/onnx/PPLOnnxService.java`

**技术栈**:
- ONNX Runtime
- HuggingFace Tokenizers (ai.djl.huggingface.tokenizers)

**核心功能**:
```java
@Service
public class PPLOnnxService implements PPLService {
    private OrtEnvironment env;
    private OrtSession session;
    private HuggingFaceTokenizer tokenizer;
    
    @Override
    public double calculatePerplexity(String text) throws PPLException {
        if (text == null || text.trim().isEmpty()) {
            return Double.MAX_VALUE;
        }
        
        // 1. 使用 HuggingFace Tokenizer 分词
        Encoding encoding = tokenizer.encode(text);
        long[] inputIds = encoding.getIds();
        
        // 2. 通过 ONNX Runtime 运行语言模型
        // 3. 计算困惑度分数
        
        return perplexity;
    }
}
```

**特点**:
- **输出**: 困惑度分数（Double 类型）
- **用途**: 智能文本分块，**不是**向量检索
- **支持**: KV Cache 优化（加速推理）

---

## 📦 Maven 依赖配置

重构前项目的依赖配置（`old/ai-reviewer-base-file-rag/pom.xml`）：

```xml
<!-- ONNX Runtime - 本地模型推理（支持 Sentence-BERT） -->
<!-- 升级到 1.19.2 以支持 IR version 10 的模型（bge-m3, bge-base-zh 等新模型）-->
<dependency>
    <groupId>com.microsoft.onnxruntime</groupId>
    <artifactId>onnxruntime</artifactId>
    <version>1.19.2</version>
</dependency>

<!-- Hugging Face Tokenizers - 用于文本分词（PPL 服务需要） -->
<dependency>
    <groupId>ai.djl.huggingface</groupId>
    <artifactId>tokenizers</artifactId>
    <version>0.34.0</version>
</dependency>
```

---

## 🔍 核心实现代码示例

### LocalEmbeddingEngine 向量化过程

```java
public float[] embed(String text) {
    // 1. 分词
    long[] inputIds = tokenize(text);
    long[] attentionMask = createAttentionMask(inputIds);
    long[] tokenTypeIds = createTokenTypeIds(inputIds);
    
    // 2. 构建 ONNX 输入张量
    OnnxTensor inputIdsTensor = OnnxTensor.createTensor(env, new long[][]{inputIds});
    OnnxTensor attentionMaskTensor = OnnxTensor.createTensor(env, new long[][]{attentionMask});
    OnnxTensor tokenTypeIdsTensor = OnnxTensor.createTensor(env, new long[][]{tokenTypeIds});
    
    Map<String, OnnxTensor> inputs = new HashMap<>();
    inputs.put("input_ids", inputIdsTensor);
    inputs.put("attention_mask", attentionMaskTensor);
    inputs.put("token_type_ids", tokenTypeIdsTensor);
    
    // 3. 模型推理
    OrtSession.Result result = session.run(inputs);
    
    // 4. 提取向量（[CLS] token 的嵌入）
    float[][][] output3d = (float[][][]) result.get(0).getValue();
    float[] vector = output3d[0][0]; // batch=0, token=0 ([CLS])
    
    // 5. L2 归一化（用于余弦相似度）
    return l2Normalize(vector);
}
```

### 关键技术点

- 使用 ONNX Runtime Java API (`ai.onnxruntime.*`)
- 支持多种中文 Embedding 模型（bge-base-zh-v1.5、bge-m3 等）
- 简化版 tokenizer（生产环境建议用 HuggingFace Tokenizers）
- L2 归一化保证向量可用于余弦相似度计算
- 本地推理，无需外部 API 调用

---

## 💡 当前系统架构

重构后的系统采用了分层架构，将向量化和RAG检索分离：

### 架构图

```
┌─────────────────────────────────────────┐
│         Application Layer               │
│  (Controller, Service, Examples)        │
└─────────────────┬───────────────────────┘
                  │
    ┌─────────────┴─────────────┐
    │                           │
┌───▼────────────┐    ┌─────────▼────────┐
│ EmbeddingService│    │   RAGService     │
│   (可选)        │    │   (必需)         │
└────────────────┘    └──────────────────┘
    │                          │
    │                          │
    ├──────────────────────────┤
    │                          │
┌───▼──────────────────────────▼───────┐
│         Storage Backends             │
│  (Lucene, H2, SQLite, Redis,        │
│   MongoDB, Elasticsearch)            │
└──────────────────────────────────────┘
```

### 层次说明

1. **EmbeddingService**: 负责文本向量化（**可选**）
   - 如果配置了，支持向量检索、语义检索、混合检索
   - 如果未配置，仍可使用全文检索

2. **RAGService**: 负责向量存储和检索（**必需**）
   - 文本检索（基于 BM25/Lucene）
   - 向量检索（基于余弦相似度）
   - 混合检索（文本 + 向量）

3. **PPLService**: 负责文本分块（**独立模块**）
   - 用于智能文本分块
   - 不参与检索过程

---

## 🎯 是否可以不用向量模型？

### ✅ 完全可以！

当前系统**已经支持不使用向量模型的 RAG 实现**，主要通过以下方案：

### 1. 全文检索（推荐） ⭐⭐⭐⭐⭐

**支持的后端**：
- **File (Lucene)** - Apache Lucene，BM25 算法
- **Elasticsearch** - 分布式 BM25
- **H2** - 嵌入式数据库全文检索
- **SQLite** - FTS5 全文检索
- **MongoDB** - Text Index
- **Redis** - 关键词倒排索引

**使用方式**：
```java
// 方式 1：直接文本搜索
List<SearchResult> results = ragService.searchByText("Spring Boot", 10);

// 方式 2：使用 Query 对象（TEXT 模式）
Query query = Query.builder()
    .text("Spring Boot")
    .mode(SearchMode.TEXT)  // 不使用向量
    .topK(10)
    .highlight(true)
    .build();

List<SearchResult> results = ragService.search(query);
```

**优势**：
- ✅ 无需向量模型（零依赖）
- ✅ 速度快（1-10ms vs 50-200ms）
- ✅ 关键词匹配精准
- ✅ 资源占用少
- ✅ 适合专业术语检索

**劣势**：
- ❌ 无法理解语义（"汽车"和"车辆"不会匹配）
- ❌ 依赖关键词匹配

### 2. 对比：向量检索 vs 全文检索

| 维度 | 向量检索（Embedding） | 全文检索（BM25/Lucene） |
|------|---------------------|---------------------|
| 语义理解 | ✅ 优秀 | ❌ 无法理解 |
| 关键词匹配 | ⚠️ 一般 | ✅ 精准 |
| 计算成本 | ❌ 高（需要模型推理） | ✅ 低（索引查找） |
| 查询延迟 | 50-200ms | 1-10ms |
| 内存占用 | 2GB（含模型） | 100MB |
| 冷启动 | ❌ 需要加载模型 | ✅ 即开即用 |

### 3. 快速开始：不使用向量的配置

**application.yml（Lucene 全文检索）**：
```yaml
omni-agent:
  rag:
    type: file  # 使用 Lucene
    file:
      index-path: ./data/lucene-index
      highlight-enabled: true
```

**无需配置 EmbeddingService！**

### 4. 使用场景建议

**推荐使用全文检索（不用向量）**：
- ✅ 关键词明确（产品型号、专业术语）
- ✅ 性能要求高（实时搜索）
- ✅ 资源受限（边缘设备）
- ✅ 文档量小（< 10,000 条）
- ✅ 代码搜索

**推荐使用向量检索**：
- ✅ 语义理解重要（智能客服）
- ✅ 查询多样化（用户问法不同）
- ✅ 跨语言检索
- ✅ 推荐系统

---

## 📖 详细文档

- [RAG_WITHOUT_EMBEDDING.md](./RAG_WITHOUT_EMBEDDING.md) - 不使用向量模型的完整指南
- [RAG_COMPARISON_GUIDE.md](./RAG_COMPARISON_GUIDE.md) - 向量检索 vs 全文检索详细对比
- [README_RAG_EXAMPLE.md](../omni-agent-example-basic/README_RAG_EXAMPLE.md) - 代码示例说明

---

**最后更新**: 2025-12-25  
**维护者**: OmniAgent Team

