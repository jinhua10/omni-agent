# 流式输出卡住问题排查指南

## 🐛 问题现象

**请求**：
```
POST /api/documents/processing/绿色环保能源灯泡——.ppt/extract
payload: {model: "vision-llm", streaming: true}
```

**返回内容**：
```
event:message data:{"type":"progress","percent":10,"message":"正在读取文档..."}
event:message data:{"type":"progress","percent":30,"message":"正在解析文档格式..."}
event:message data:{"type":"progress","percent":35,"message":"正在实时提取文本..."}
```

**之后就没有内容了** ❌

## 🔍 可能的原因

### 1. 回调未被调用
VisionLLMDocumentProcessor 的流式回调可能没有被正确触发。

### 2. Vision LLM API 调用失败
`chatWithVisionFlux` 可能在调用 Vision API 时出错，但错误被吞掉了。

### 3. 流式响应卡住
Flux 流可能卡在某个地方，没有 emit 任何内容。

## 📋 详细的调试步骤

### 步骤 1：查看后端完整日志

现在代码中已经添加了详细的日志，请查看后端日志，寻找以下关键信息：

#### 1.1 检查是否进入流式模式
```
✅ [VisionLLM] 检测到流式回调
✅ [VisionLLM] 流式模式: true
🚀 [VisionLLM] 启动流式处理，页面 1
```

**如果看到**：
- ✅ 说明流式模式被正确识别
- ❌ 如果没看到，说明回调或 streaming 参数未正确传递

#### 1.2 检查是否开始调用 Vision API
```
🔄 [VisionLLM] 开始调用 chatWithVisionFlux
```

**如果看到**：
- ✅ 说明开始调用 Vision API
- ❌ 如果没看到，说明在此之前就卡住了

#### 1.3 检查是否收到 token
```
📥 [VisionLLM] 收到 token: 123 字符
📤 [STREAM] 发送流式内容: 123 字符
✅ [STREAM] 成功发送流式内容
```

**如果看到**：
- ✅ 说明 Vision API 正常返回，且成功发送
- ❌ 如果没看到，说明 Vision API 没有返回内容

#### 1.4 检查是否有错误
```
❌ [VisionLLM] Vision 分析失败: xxx
❌ [STREAM] 发送流式内容失败: xxx
```

**如果看到**：
- 说明具体错误原因

### 步骤 2：根据日志定位问题

根据步骤 1 的日志，可以定位到具体问题：

#### 情况 A：未进入流式模式

**日志特征**：
```
⚠️ [VisionLLM] context 或 options 为空
```

**原因**：ProcessingContext 的 options 未正确传递

**解决方案**：
检查 `extractTextWithProcessorStreaming` 方法中是否正确设置了 options：
```java
Map<String, Object> options = new HashMap<>();
options.put("model", model);
options.put("streaming", true);  // ⭐ 必须设置
options.put("streamCallback", streamCallback);  // ⭐ 必须设置
```

#### 情况 B：进入流式模式，但未收到 token

**日志特征**：
```
🔄 [VisionLLM] 开始调用 chatWithVisionFlux
// 之后没有 "📥 收到 token" 日志
```

**原因**：Vision API 调用失败或返回空内容

**可能的子原因**：

##### B.1：Vision API 配置错误
检查配置：
```yaml
omni-agent:
  vision-llm:
    enabled: true
    model: qwen-vl-plus  # 确认模型名称正确
    endpoint: https://...  # 确认 endpoint 正确
    api-key: sk-xxx  # 确认 API key 正确
```

##### B.2：Vision API 网络问题
检查日志是否有网络错误：
```
java.net.ConnectException: Connection refused
java.net.SocketTimeoutException: Read timed out
```

##### B.3：Vision API 返回错误
检查是否有 API 错误响应：
```
400 Bad Request: {...}
401 Unauthorized
429 Too Many Requests
```

#### 情况 C：收到 token 但未发送到前端

**日志特征**：
```
📥 [VisionLLM] 收到 token: 123 字符
⚠️ 收到空内容，跳过发送  # ⭐ 这个不应该出现
```

或

```
📥 [VisionLLM] 收到 token: 123 字符
❌ [STREAM] 发送流式内容失败: xxx
```

**原因**：SSE 发送失败

**解决方案**：
1. 检查异常详情
2. 检查 SseEmitter 是否被提前关闭
3. 检查是否有反向代理缓冲了响应

### 步骤 3：使用测试端点验证 SSE 基础功能

在排查实际文档提取前，先确认 SSE 基础功能正常：

```bash
curl -N http://localhost:3000/api/documents/processing/test-streaming
```

**预期输出**：
```
event:message
data:{"type":"content","content":"第 1 条测试消息\n"}

event:message
data:{"type":"content","content":"第 2 条测试消息\n"}
...
```

**如果测试端点正常**：
- ✅ SSE 基础功能 OK
- 问题在于文档提取流程

**如果测试端点也失败**：
- ❌ SSE 基础设施有问题
- 检查网络、反向代理、防火墙

## 🔧 常见解决方案

### 解决方案 1：检查 Vision LLM 配置

确保配置文件中有正确的 Vision LLM 配置：

```yaml
omni-agent:
  vision-llm:
    enabled: true
    model: qwen-vl-plus
    endpoint: https://dashscope.aliyuncs.com/api/v1/services/aigc/multimodal-generation/generation
    api-key: ${QIANWEN_API_KEY}
```

### 解决方案 2：检查 visionAIService Bean

确认是否有 visionAIService Bean 被创建：

```bash
# 启动时日志中查找
Auto-configuring OnlineAPIAIService: provider=qianwen, model=qwen-vl-plus
✅ Vision AI Service 配置成功
```

如果看到：
```
⚠️ Vision LLM 配置未找到，Vision 功能可能无法正常工作
```

说明 Vision AI Service 未正确配置。

### 解决方案 3：增加超时时间

Vision LLM 处理可能需要较长时间，增加超时：

```java
// DocumentProcessingController.java
SseEmitter emitter = new SseEmitter(10 * 60 * 1000L); // 从 5 分钟改为 10 分钟
```

### 解决方案 4：禁用并行处理

如果使用了并行批次处理，可能导致流式输出混乱，尝试禁用：

```yaml
omni-agent:
  vision-llm:
    batch-processing:
      enabled: false  # 禁用批处理
```

或在代码中强制串行：

```java
// VisionLLMDocumentProcessor.java
// 流式模式强制串行
if (streamingEnabled) {
    batchResults = processPageBatchesSequentially(batches);
}
```

## 📊 调试日志模板

**完整的日志应该是这样的（正常情况）**：

```
🔍 开始文本提取: documentId=xxx, model=vision-llm, forceReExtract=false
正在读取文档...
正在解析文档格式...
正在实时提取文本...
✅ [VisionLLM] 检测到流式回调
✅ [VisionLLM] 流式模式: true
🔍 [VisionLLM] 调用 Vision API 分析页面 1, 图片数: 1, 流式模式: true, 回调存在: true, 使用服务: visionAIService
🚀 [VisionLLM] 启动流式处理，页面 1
📤 [VisionLLM] 发送页面分隔符
🔄 [VisionLLM] 开始调用 chatWithVisionFlux
📥 [VisionLLM] 收到 token: 15 字符
📤 [STREAM] 发送流式内容: 15 字符
✅ [STREAM] 成功发送流式内容
📥 [VisionLLM] 收到 token: 28 字符
📤 [STREAM] 发送流式内容: 28 字符
✅ [STREAM] 成功发送流式内容
...
✅ [VisionLLM] Flux 完成
✅ [VisionLLM] 页面 1 (stream) 分析完成，内容长度: 456 chars
```

## 🚨 紧急排查清单

如果问题紧急，按以下顺序快速检查：

1. ☐ 查看日志是否有 "✅ [VisionLLM] 检测到流式回调"
2. ☐ 查看日志是否有 "🔄 [VisionLLM] 开始调用 chatWithVisionFlux"
3. ☐ 查看日志是否有 "📥 [VisionLLM] 收到 token"
4. ☐ 查看日志是否有任何错误信息（❌）
5. ☐ 测试 `/test-streaming` 端点是否正常
6. ☐ 检查 Vision API 配置是否正确
7. ☐ 检查网络是否能访问 Vision API
8. ☐ 尝试增加超时时间

## 📞 需要提供的信息

如果问题仍未解决，请提供：

1. **完整的后端日志**（从"开始文本提取"到最后一行）
2. **配置文件**（omni-agent.vision-llm 部分）
3. **测试文档信息**（文件类型、大小、页数）
4. **test-streaming 端点测试结果**
5. **前端 Network 标签页截图**
6. **是否使用了反向代理**（如 nginx）

---

**更新时间**：2025-12-24  
**版本**：v2.0 - 增加详细日志追踪

