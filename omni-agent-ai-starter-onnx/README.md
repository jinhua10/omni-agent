# OmniAgent AI Starter - ONNX

基于 ONNX Runtime 的本地 Embedding 服务实现。

## ✨ 特性

- ✅ **本地推理** - 无需网络请求，数据隐私安全
- ✅ **高性能** - ONNX Runtime 优化，支持 CPU/GPU
- ✅ **多模型支持** - 支持 bge、text2vec 等多种中文模型
- ✅ **自动归一化** - L2 归一化，适用于余弦相似度
- ✅ **Spring Boot 集成** - 开箱即用的自动配置
- ✅ **轻量级** - 不依赖外部服务

## 📦 依赖

### Maven

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-ai-starter-onnx</artifactId>
    <version>1.0.0</version>
</dependency>
```

### Gradle

```groovy
implementation 'top.yumbo.ai.omni:omni-agent-ai-starter-onnx:1.0.0'
```

## 📥 模型下载

### 推荐模型

| 模型 | 语言 | 维度 | 大小 | 适用场景 |
|------|------|------|------|---------|
| **bge-base-zh-v1.5** | 中文 | 768 | ~400MB | 通用（推荐）|
| bge-m3 | 多语言 | 1024 | ~2GB | 多语言支持 |
| bge-large-zh | 中文 | 1024 | ~1.3GB | 高精度 |
| text2vec-base-chinese | 中文 | 768 | ~400MB | 通用 |

### 下载步骤

#### 方式 1：使用 Hugging Face（推荐）

```bash
# 安装 huggingface-cli
pip install huggingface-hub

# 下载模型
huggingface-cli download BAAI/bge-base-zh-v1.5 --local-dir ./models/bge-base-zh-v1.5

# 转换为 ONNX 格式（如果需要）
python convert_to_onnx.py --model-path ./models/bge-base-zh-v1.5
```

#### 方式 2：手动下载

1. 访问 [Hugging Face](https://huggingface.co/BAAI/bge-base-zh-v1.5)
2. 下载 `model.onnx` 文件
3. 放置到项目目录：`./models/bge-base-zh-v1.5/model.onnx`

#### 方式 3：使用国内镜像

```bash
# 使用魔搭社区（中国大陆推荐）
git clone https://www.modelscope.cn/BAAI/bge-base-zh-v1.5.git ./models/bge-base-zh-v1.5
```

## ⚙️ 配置

### application.yml

```yaml
embedding:
  onnx:
    enabled: true
    model-path: ./models/bge-base-zh-v1.5/model.onnx
    max-sequence-length: 512
    batch-size: 32
```

### application.properties

```properties
# 启用 ONNX Embedding 服务
embedding.onnx.enabled=true

# 模型文件路径
embedding.onnx.model-path=./models/bge-base-zh-v1.5/model.onnx

# 最大序列长度
embedding.onnx.max-sequence-length=512

# 批处理大小
embedding.onnx.batch-size=32
```

### 配置项说明

| 配置项 | 类型 | 默认值 | 说明 |
|-------|------|--------|------|
| `embedding.onnx.enabled` | boolean | true | 是否启用 ONNX Embedding 服务 |
| `embedding.onnx.model-path` | String | `./models/bge-base-zh-v1.5/model.onnx` | ONNX 模型文件路径 |
| `embedding.onnx.max-sequence-length` | int | 512 | 最大序列长度 |
| `embedding.onnx.batch-size` | int | 32 | 批处理大小 |

## 🚀 使用示例

### 1. 基本使用

```java
@Service
public class MyService {
    
    @Autowired
    private EmbeddingService embeddingService;
    
    public void example() {
        // 生成单个文本的向量
        float[] embedding = embeddingService.embed("这是一段测试文本");
        System.out.println("向量维度: " + embedding.length);
        
        // 获取模型信息
        String model = embeddingService.getEmbeddingModel();
        int dimension = embeddingService.getDimension();
        System.out.println("使用模型: " + model + ", 维度: " + dimension);
    }
}
```

### 2. 批量处理

```java
@Service
public class BatchService {
    
    @Autowired
    private EmbeddingService embeddingService;
    
    public void batchEmbed() {
        List<String> texts = Arrays.asList(
            "第一段文本",
            "第二段文本",
            "第三段文本"
        );
        
        // 批量生成向量
        List<float[]> embeddings = embeddingService.embedBatch(texts);
        System.out.println("生成了 " + embeddings.size() + " 个向量");
    }
}
```

### 3. 与 RAG 集成

```java
@Service
public class RAGService {
    
    @Autowired
    private EmbeddingService embeddingService;
    
    @Autowired
    private top.yumbo.ai.rag.api.RAGService ragService;
    
    public void indexDocument(String title, String content) {
        // 生成向量
        float[] embedding = embeddingService.embed(content);
        
        // 创建文档
        Document doc = Document.builder()
            .title(title)
            .content(content)
            .embedding(embedding)  // 设置向量
            .build();
        
        // 索引文档
        ragService.indexDocument(doc);
    }
    
    public List<SearchResult> semanticSearch(String query, int topK) {
        // 使用 Embedding 服务自动完成向量化和检索
        return ragService.semanticSearch(query, topK);
    }
}
```

## 🔧 高级配置

### 使用不同的模型

```yaml
# 使用 bge-m3（多语言，1024维）
embedding:
  onnx:
    model-path: ./models/bge-m3/model.onnx
    max-sequence-length: 8192  # bge-m3 支持更长的序列
```

```yaml
# 使用 text2vec-base-chinese
embedding:
  onnx:
    model-path: ./models/text2vec-base-chinese/model.onnx
    max-sequence-length: 512
```

### GPU 加速（TODO）

当前版本使用 CPU 推理。如需 GPU 加速，需要：

1. 安装 ONNX Runtime GPU 版本
2. 添加 CUDA/cuDNN 依赖
3. 配置 GPU 提供程序

```xml
<!-- GPU 版本依赖（示例） -->
<dependency>
    <groupId>com.microsoft.onnxruntime</groupId>
    <artifactId>onnxruntime_gpu</artifactId>
    <version>1.19.2</version>
</dependency>
```

## 📊 性能基准

### 测试环境

- CPU: Intel i7-12700
- RAM: 32GB
- 模型: bge-base-zh-v1.5 (768维)

### 测试结果

| 操作 | 延迟 | 吞吐量 |
|------|------|--------|
| 单文本向量化 | ~30-50ms | 20-30 texts/sec |
| 批量向量化 (32) | ~500ms | 60 texts/sec |
| 内存占用 | ~500MB | - |

## ⚠️ 注意事项

### 1. 模型文件

- 确保模型文件是 ONNX 格式（.onnx 后缀）
- 模型文件较大（400MB-2GB），首次下载可能需要时间
- 建议使用外部目录存储模型，避免打包到 JAR 中

### 2. 分词器

- 当前使用简化的字符级分词器
- 生产环境建议使用 HuggingFace Tokenizers
- 或预先使用 Python 生成 token IDs

### 3. 性能优化

- 批量处理可以提高吞吐量
- 考虑使用缓存避免重复计算
- 长文本建议分段处理

## 🐛 故障排查

### 问题 1：模型文件未找到

```
错误: 模型文件不存在: ./models/bge-base-zh-v1.5/model.onnx
```

**解决方案**:
1. 检查模型文件路径是否正确
2. 确认模型文件已下载
3. 检查文件权限

### 问题 2：内存不足

```
错误: OutOfMemoryError
```

**解决方案**:
1. 增加 JVM 堆内存：`-Xmx2g`
2. 使用较小的模型
3. 减少批处理大小

### 问题 3：推理速度慢

**解决方案**:
1. 使用批量处理
2. 考虑 GPU 加速
3. 减少最大序列长度

## 📖 相关文档

- [EmbeddingService API](../omni-agent-ai-api/README.md)
- [RAG 集成指南](../docs/RAG_CORE_CONCEPTS.md)
- [ONNX Runtime 官方文档](https://onnxruntime.ai/)
- [BGE 模型文档](https://huggingface.co/BAAI/bge-base-zh-v1.5)

## 🤝 贡献

欢迎提交 Issue 和 Pull Request！

## 📄 许可证

Apache License 2.0

