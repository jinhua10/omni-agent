# Ollama 离线 Vision 图像识别使用指南

## 概述

Ollama 支持多个**离线**的 Vision 多模态模型，可以在本地进行图像识别，无需连接外部 API，保证数据安全和隐私。

## 支持的 Vision 模型

Ollama 支持以下离线 Vision 模型：

### 1. **LLaVA** (推荐) ⭐
- **模型名称**: `llava`, `llava:7b`, `llava:13b`, `llava:34b`
- **描述**: Large Language and Vision Assistant，最流行的开源多模态模型
- **特点**: 
  - 基于 Llama 2 的多模态版本
  - 支持图像理解和对话
  - 中文和英文双语支持
  - 模型大小适中，性能优秀

### 2. **BakLLaVA**
- **模型名称**: `bakllava`
- **描述**: 基于 Mistral 7B 的多模态模型
- **特点**: 更快的推理速度

### 3. **LLaVA-Phi3**
- **模型名称**: `llava-phi3`
- **描述**: 基于 Phi-3 的轻量级多模态模型
- **特点**: 小巧高效，适合资源受限环境

### 4. **LLaVA-Llama3**
- **模型名称**: `llava-llama3`
- **描述**: 基于 Llama 3 的最新多模态模型
- **特点**: 最新架构，性能更强

## 安装步骤

### 1. 安装 Ollama

**Windows/macOS/Linux**:
```bash
# 从官网下载安装: https://ollama.ai/download

# 或使用命令行安装（Linux/macOS）:
curl -fsSL https://ollama.ai/install.sh | sh
```

### 2. 下载 Vision 模型

```bash
# 下载 LLaVA 7B 模型（推荐，约 4.7GB）
ollama pull llava

# 或者下载其他版本
ollama pull llava:13b    # 13B版本，更强大但更大
ollama pull llava:34b    # 34B版本，最强但需要更多资源

# 或下载其他 Vision 模型
ollama pull bakllava
ollama pull llava-phi3
ollama pull llava-llama3
```

### 3. 验证模型安装

```bash
# 列出已安装的模型
ollama list

# 测试 Vision 模型
ollama run llava "描述这张图片" --image /path/to/your/image.jpg
```

## 配置 OmniAgent

### application.yml 配置

```yaml
omni-agent:
  # 使用 Ollama 作为 AI 服务
  ai:
    type: ollama
    ollama:
      base-url: http://localhost:11434  # Ollama 服务地址
      model: llava                       # 使用 LLaVA Vision 模型
      temperature: 0.7
      max-tokens: 2000
      timeout: 60000  # Vision 处理可能需要更长时间

  # Vision LLM 配置
  vision-llm:
    enabled: true
    model: llava  # 使用 Ollama 的 Vision 模型
    system-prompt: |
      请仔细分析这张图片并提取其中的关键信息。
      如果图片包含文字，请完整准确地提取所有文字内容。
      如果是图表、流程图或架构图，请详细描述其结构和含义。
    
    # 智能批处理配置（Vision 处理较慢，建议减小批次）
    batch-processing:
      enabled: true
      max-context-tokens: 4096        # LLaVA 的上下文限制
      estimated-tokens-per-slide: 1000
      min-batch-size: 1
      max-batch-size: 2               # 降低批次大小，减少内存占用
```

## Ollama Vision API 实现

OmniAgent 已经在 `OllamaAIService` 中实现了 Vision 支持：

```java
@Override
public String analyzeImages(List<byte[]> imagesData, String prompt) {
    // Ollama Vision API 格式
    {
        "model": "llava",
        "messages": [
            {
                "role": "user",
                "content": "What's in this image?",
                "images": ["base64_encoded_image_1", "base64_encoded_image_2"]
            }
        ]
    }
}
```

## 使用示例

### 1. 通过 API 使用

```bash
# 上传文档并提取内容
curl -X POST "http://localhost:3000/api/documents/processing/presentation.ppt/extract" \
  -H "Content-Type: application/json" \
  -d '{"model": "vision-llm", "streaming": true}'
```

### 2. 直接使用 AIService

```java
@Autowired
private AIService aiService;

public void analyzeImage() {
    // 读取图片
    byte[] imageData = Files.readAllBytes(Paths.get("image.jpg"));
    
    // 分析图片
    String result = aiService.analyzeImage(imageData, "请描述这张图片的内容");
    
    System.out.println(result);
}
```

### 3. 分析 PPT 幻灯片

```java
// VisionLLMDocumentProcessor 会自动使用 Ollama Vision 模型
DocumentProcessor.ProcessingContext context = 
    DocumentProcessor.ProcessingContext.builder()
        .filePath("presentation.ppt")
        .fileExtension("ppt")
        .build();

DocumentProcessor.ProcessingResult result = 
    documentProcessorManager.processDocument(context);

System.out.println(result.getContent());
```

## 性能优化建议

### 1. 硬件要求

- **最低配置**:
  - CPU: 4核心
  - RAM: 8GB
  - 磁盘: 10GB 可用空间
  - GPU: 可选（有GPU会显著加速）

- **推荐配置**:
  - CPU: 8核心以上
  - RAM: 16GB 以上
  - 磁盘: 20GB 可用空间
  - GPU: NVIDIA GPU (8GB+ VRAM)

### 2. GPU 加速

如果有 NVIDIA GPU，Ollama 会自动使用 CUDA 加速：

```bash
# 检查 GPU 是否被使用
nvidia-smi

# Ollama 会自动检测并使用 GPU
ollama run llava
```

### 3. 批处理配置

对于大型文档，建议调整批处理参数：

```yaml
omni-agent:
  vision-llm:
    batch-processing:
      max-batch-size: 1  # 一次处理1页，避免内存不足
      max-context-tokens: 2048  # 降低上下文大小
```

### 4. 并发控制

```yaml
omni-agent:
  thread-pool:
    vision-llm:
      core-pool-size: 1      # 降低并发，避免资源竞争
      max-pool-size: 2
```

## 对比：在线 vs 离线 Vision

| 特性 | Ollama (离线) | 千问VL/GPT-4V (在线) |
|------|--------------|---------------------|
| **隐私安全** | ✅ 完全本地处理 | ❌ 数据发送到云端 |
| **离线可用** | ✅ 无需网络 | ❌ 需要网络连接 |
| **费用** | ✅ 免费 | ❌ 按使用量收费 |
| **速度** | ⚠️ 取决于硬件 | ✅ 通常较快 |
| **准确性** | ⚠️ 中等 | ✅ 非常高 |
| **中文支持** | ✅ 支持 | ✅ 优秀 |
| **模型更新** | ⚠️ 需手动下载 | ✅ 自动最新 |

## 常见问题

### Q1: Ollama Vision 模型太大，占用太多磁盘空间？

**A**: 可以使用更小的模型：
```bash
# 使用 LLaVA-Phi3 (更小，约2GB)
ollama pull llava-phi3

# 配置使用
omni-agent:
  ai:
    ollama:
      model: llava-phi3
```

### Q2: Vision 处理速度太慢？

**A**: 
1. 使用 GPU 加速
2. 降低批处理大小
3. 使用更小的模型（如 llava-phi3）
4. 减少图片分辨率

### Q3: 如何在没有 GPU 的机器上使用？

**A**: 
- Ollama 可以在 CPU 上运行，但速度会较慢
- 建议使用较小的模型（llava-phi3）
- 调整超时时间：`timeout: 120000`（2分钟）

### Q4: 如何切换到在线 Vision API？

**A**: 修改配置即可：
```yaml
omni-agent:
  ai:
    type: online-api  # 从 ollama 改为 online-api
    online:
      provider: qianwen
      api-key: ${AI_API_KEY}
      default-model: qwen-vl-plus
```

## 测试 Vision 功能

### 1. 启动 Ollama 服务

```bash
# Ollama 通常会作为后台服务自动启动
# 如果没有启动，手动启动：
ollama serve
```

### 2. 启动 OmniAgent

```bash
cd omni-agent-example-basic
mvn spring-boot:run
```

### 3. 测试图像识别

```bash
# 上传并分析图片
curl -X POST "http://localhost:3000/api/documents/processing/diagram.png/extract" \
  -H "Content-Type: application/json" \
  -d '{"model": "vision-llm", "streaming": true}'
```

## 支持的文档类型

使用 Ollama Vision 可以离线处理：

- ✅ **PDF** - 每页转图片分析
- ✅ **PPT/PPTX** - 幻灯片图片分析
- ✅ **Word (DOCX)** - 文档中的图片
- ✅ **Excel (XLSX)** - 工作表图表
- ✅ **纯图片** - PNG, JPG, JPEG, BMP

## 总结

使用 Ollama Vision 的优势：

1. ✅ **完全离线**：无需网络，数据不离开本地
2. ✅ **数据安全**：敏感文档不会上传到云端
3. ✅ **免费使用**：无API费用
4. ✅ **易于部署**：一条命令下载模型即可

适用场景：

- 🔒 处理敏感文档（企业内部文档、个人隐私）
- 🌐 网络受限环境
- 💰 预算有限的项目
- 🏢 需要完全自主可控的系统

如果对准确性要求极高，或者有充足的网络和预算，可以选择在线 Vision API（如千问VL、GPT-4V）。

