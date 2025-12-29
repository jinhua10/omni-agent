# AbstractDocumentProcessor 批处理优化说明

## 📄 概述

将 VisionLLMDocumentProcessor 中的批处理逻辑迁移到 AbstractDocumentProcessor，使所有继承它的文档处理器（ExcelProcessor、PDFProcessor、WordProcessor、PPTProcessor 等）都能享受到批处理带来的性能优势。

## ✨ 核心优化

### 1. 智能分批 (Smart Batching)

**功能**：根据配置动态决定批次大小，尽可能多张图片一起处理。

**优势**：
- 减少 API 调用次数
- 降低网络开销
- 提高处理效率

**实现**：
```java
protected List<List<ContentBlock>> smartBatchingForImages(List<ContentBlock> imageBlocks)
```

**配置**：
```yaml
omni-agent:
  vision-llm:
    batch-processing:
      enabled: true
      max-batch-size: 5  # 每批最多处理 5 张图片
```

### 2. 并行处理 (Parallel Processing)

**功能**：使用线程池并行处理多个批次，大幅提升处理速度。

**优势**：
- 多批次同时处理
- 充分利用 CPU 和网络资源
- 大幅缩短总处理时间

**实现**：
```java
protected void processImageBatchesInParallel(List<List<ContentBlock>> batches, ProcessingContext context)
```

**自动启用条件**：
- 配置了 `visionLlmExecutor` 线程池
- 批次数量 > 1

### 3. 串行处理 (Sequential Processing)

**功能**：按顺序依次处理每个批次，保证顺序正确。

**使用场景**：
- 未配置线程池
- 只有一个批次
- 需要严格保证顺序

**实现**：
```java
protected void processImageBatchesSequentially(List<List<ContentBlock>> batches, ProcessingContext context)
```

### 4. 批次标记 (Batch Markers)

**功能**：在流式输出中添加批次标记，前端可以按批次显示内容。

**标记类型**：

#### BATCH_INFO（批次信息）
```json
BATCH_INFO:{"totalBatches":3,"totalImages":15}
```

#### BATCH_START（批次开始）
```json
BATCH_START:{"batchIndex":0,"batchNumber":1,"totalBatches":3}
```

#### BATCH_CONTENT（批次内容）
```
BATCH_CONTENT:0:这是第一个批次的内容...
```

#### BATCH_END（批次结束）
```json
BATCH_END:{"batchIndex":0,"batchNumber":1}
```

### 5. 重试机制 (Retry Mechanism)

**功能**：自动重试失败的图片分析，提高成功率。

**特性**：
- 最多重试 3 次
- 递增等待时间（2s、4s、6s）
- 识别超时错误并重试
- 不支持的功能不重试

**实现**：
```java
protected String analyzeImageWithRetry(ExtractedImage image, ProcessingContext context, int batchIndex)
```

## 🏗️ 架构设计

### 处理流程

```
1. 收集所有图片块
   ↓
2. 智能分批（smartBatchingForImages）
   ↓
3. 发送批次信息（BATCH_INFO）
   ↓
4. 选择处理方式
   ├─ 并行处理（processImageBatchesInParallel）
   └─ 串行处理（processImageBatchesSequentially）
   ↓
5. 对每个批次：
   a. 发送 BATCH_START
   b. 处理批次中的图片
      - 应用 ImageHandler
      - 调用 Vision LLM（带重试）
      - 发送 BATCH_CONTENT
   c. 发送 BATCH_END
   ↓
6. 完成所有批次
```

### 并行处理流程

```
批次1 (线程1) ──┐
批次2 (线程2) ──┼─→ CompletableFuture.allOf() ─→ 等待所有完成
批次3 (线程3) ──┘
```

**保证顺序**：虽然并行处理，但通过 `batchIndex` 标记确保前端能按顺序显示。

### 核心组件

#### 1. 批处理配置
```java
@Autowired(required = false)
protected VisionLLMBatchProcessingProperties batchProcessingConfig;
```

#### 2. 线程池
```java
@Autowired(required = false)
@Qualifier("visionLlmExecutor")
protected Executor visionLlmExecutor;
```

#### 3. Vision AI Service
```java
@Autowired(required = false)
protected AIService visionAIService;
```

## 📊 性能对比

### 场景：处理 30 张图片的 PPT

#### 传统方式（逐张处理）
```
图片1 → 分析(3s) → 图片2 → 分析(3s) → ... → 图片30 → 分析(3s)
总耗时：30 × 3s = 90s
```

#### 智能分批（每批5张）
```
批次1(5张) → 分析(4s)
批次2(5张) → 分析(4s)
批次3(5张) → 分析(4s)
批次4(5张) → 分析(4s)
批次5(5张) → 分析(4s)
批次6(5张) → 分析(4s)
总耗时：6 × 4s = 24s
```
**提升**：~73% 的时间减少

#### 智能分批 + 并行处理（3个线程）
```
批次1(5张) ──┐
批次2(5张) ──┼─→ 分析(4s)
批次3(5张) ──┘

批次4(5张) ──┐
批次5(5张) ──┼─→ 分析(4s)
批次6(5张) ──┘

总耗时：2 × 4s = 8s
```
**提升**：~91% 的时间减少

## 🎯 适用场景

### 最佳场景
- ✅ 大量图片的文档（PDF、PPT）
- ✅ 图片较大或复杂
- ✅ 网络延迟较高
- ✅ 需要实时反馈（流式输出）

### 不适用场景
- ❌ 单张或少量图片
- ❌ 图片很小且简单
- ❌ 对顺序有严格要求（使用串行模式）

## 🔧 配置示例

### 完整配置

```yaml
omni-agent:
  # Vision LLM 配置
  vision-llm:
    enabled: true
    model: "qwen-vl-plus"
    system-prompt: "请分析这张图片并提取其中的关键信息。"
    
    # 批处理配置
    batch-processing:
      enabled: true              # 启用批处理
      max-batch-size: 5          # 每批最多 5 张图片
      max-context-tokens: 8000   # 最大上下文 token 数
      
  # 线程池配置
  executor:
    vision-llm:
      core-pool-size: 3          # 核心线程数
      max-pool-size: 6           # 最大线程数
      queue-capacity: 100        # 队列容量
      thread-name-prefix: "vision-llm-"
```

### 最小配置（使用默认值）

```yaml
omni-agent:
  vision-llm:
    enabled: true
    model: "qwen-vl-plus"
```

## 📝 代码示例

### 使用批处理

所有继承 `AbstractDocumentProcessor` 的处理器都自动获得批处理能力，无需额外代码：

```java
@Component
public class MyDocumentProcessor extends AbstractDocumentProcessor {
    
    @Override
    protected ExtractedContent extractContent(ProcessingContext context) throws Exception {
        ExtractedContent content = new ExtractedContent();
        
        // 添加图片
        content.addImageBlock(image1, 0);
        content.addImageBlock(image2, 1);
        content.addImageBlock(image3, 2);
        
        // 批处理会自动应用
        return content;
    }
}
```

### 流式输出

```java
ProcessingContext context = ProcessingContext.builder()
    .filePath("document.pptx")
    .build();

// 添加流式回调
Map<String, Object> options = new HashMap<>();
options.put("streaming", true);
options.put("streamCallback", (Consumer<String>) content -> {
    if (content.startsWith("BATCH_INFO:")) {
        // 处理批次信息
        System.out.println("批次信息: " + content);
    } else if (content.startsWith("BATCH_START:")) {
        // 批次开始
        System.out.println("批次开始: " + content);
    } else if (content.startsWith("BATCH_CONTENT:")) {
        // 批次内容
        String[] parts = content.split(":", 3);
        int batchIndex = Integer.parseInt(parts[1]);
        String text = parts[2];
        System.out.println("批次 " + batchIndex + ": " + text);
    } else if (content.startsWith("BATCH_END:")) {
        // 批次结束
        System.out.println("批次结束: " + content);
    }
});

context.setOptions(options);

ProcessingResult result = processor.process(context);
```

## 🐛 故障处理

### 超时重试

批处理内置了超时重试机制：
- 检测到超时错误自动重试
- 最多重试 3 次
- 递增等待时间

### 部分失败

如果某个批次失败：
- 不影响其他批次
- 记录错误日志
- 在元数据中标记失败

### 监控日志

```
🖼️ 准备处理 3 个图片块，共 15 张图片
📦 智能分批完成: 3 个批次
🚀 并行处理 3 个批次
⚙️ [Thread: vision-llm-1] 处理批次 #1
⚙️ [Thread: vision-llm-2] 处理批次 #2
⚙️ [Thread: vision-llm-3] 处理批次 #3
✅ [Thread: vision-llm-1] 批次 #1 完成
✅ [Thread: vision-llm-2] 批次 #2 完成
✅ [Thread: vision-llm-3] 批次 #3 完成
✅ 并行处理完成: 耗时 4523ms, 平均每批次 1507ms
```

## 🔄 向后兼容

- ✅ 不影响现有代码
- ✅ 自动降级到串行模式（如果未配置线程池）
- ✅ 保持原有的接口和行为

## 📊 监控指标

建议监控以下指标：
- 批次数量
- 每批次处理时间
- 总处理时间
- 重试次数
- 失败率

## 🚀 未来优化

1. **动态批次大小**：根据 API 响应时间动态调整
2. **智能负载均衡**：根据服务器负载分配批次
3. **缓存机制**：缓存已分析的图片
4. **断点续传**：支持中断后继续处理

---

**版本**: 3.0.0  
**作者**: OmniAgent Team  
**更新日期**: 2025-01-28

