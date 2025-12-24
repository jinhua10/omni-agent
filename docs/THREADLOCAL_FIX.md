# ThreadLocal 问题修复 - 流式回调在并行处理中失效

## 🐛 问题根本原因

**症状**：后台有 LLM 解析日志，但前端 UI 不显示实时内容

**根本原因**：**ThreadLocal 在子线程中无法访问**

### 详细说明

#### 1. 代码流程
```
主线程：process(context)
  ↓ 设置 ThreadLocal
  processingContextThreadLocal.set(context)
  ↓ 并行处理
  CompletableFuture.supplyAsync(() -> {
      // ⚠️ 这里是线程池中的其他线程
      processPageBatch(pages)
        ↓ 尝试获取 ThreadLocal
        this.processingContextThreadLocal.get()
          ↓ 
          返回 null ❌  // 子线程访问不到主线程的 ThreadLocal
  }, visionLlmExecutor)
```

#### 2. 为什么会失效

**ThreadLocal 的特性**：
- ThreadLocal 变量是**线程隔离**的
- 每个线程有自己的副本
- **子线程默认不会继承父线程的 ThreadLocal 值**

**在并行处理中**：
- 主线程：`processingContextThreadLocal.set(context)` ✅
- 子线程（线程池）：`processingContextThreadLocal.get()` → `null` ❌

#### 3. 导致的后果

```java
ProcessingContext ctx = this.processingContextThreadLocal.get();
// ctx == null ❌

if (ctx != null && ctx.getOptions() != null) {
    // 永远不会进入这里 ❌
    streamCallback = ctx.getOptions().get("streamCallback");
}

// 结果：streamCallback 永远是 null
// 结果：回调永远不会被调用
// 结果：前端收不到实时内容
```

## ✅ 解决方案

### 方案：直接传递参数，不使用 ThreadLocal

**改动前**：
```java
// process 方法
processingContextThreadLocal.set(context);  // 主线程设置
processPageBatchesInParallel(batches);      // 子线程无法访问

// processPageBatch 方法
ProcessingContext ctx = this.processingContextThreadLocal.get();  // ❌ 子线程中为 null
```

**改动后**：
```java
// process 方法
ProcessingContext contextForBatch = context;
processPageBatchesInParallel(batches, contextForBatch);  // ✅ 直接传递

// processPageBatch 方法
private String processPageBatch(List<DocumentPage> pages, ProcessingContext context) {
    // ✅ 直接使用参数，不依赖 ThreadLocal
    if (context != null && context.getOptions() != null) {
        streamCallback = context.getOptions().get("streamCallback");
    }
}
```

## 📝 修改清单

### 1. VisionLLMDocumentProcessor.java

#### 改动 1：process 方法
```java
// ⭐ 为了解决 ThreadLocal 在子线程中无法访问的问题，传递 context
ProcessingContext contextForBatch = context;
if (visionLlmExecutor != null && batches.size() > 1) {
    batchResults = processPageBatchesInParallel(batches, contextForBatch);
} else {
    batchResults = processPageBatchesSequentially(batches, contextForBatch);
}
```

#### 改动 2：processPageBatch 方法签名
```java
// 之前
private String processPageBatch(List<DocumentPage> pages)

// 之后
private String processPageBatch(List<DocumentPage> pages, ProcessingContext context)
```

#### 改动 3：processPageBatch 内部逻辑
```java
// 之前
ProcessingContext ctx = this.processingContextThreadLocal.get();  // ❌

// 之后
// ✅ 直接使用参数 context
if (context != null && context.getOptions() != null) {
    streamCallback = context.getOptions().get("streamCallback");
}
```

#### 改动 4：并行处理方法
```java
// 之前
private List<BatchProcessingResult> processPageBatchesInParallel(List<List<DocumentPage>> batches)

// 之后
private List<BatchProcessingResult> processPageBatchesInParallel(
    List<List<DocumentPage>> batches, 
    ProcessingContext context)  // ⭐ 新增参数
```

#### 改动 5：串行处理方法
```java
// 之前
private List<BatchProcessingResult> processPageBatchesSequentially(List<List<DocumentPage>> batches)

// 之后
private List<BatchProcessingResult> processPageBatchesSequentially(
    List<List<DocumentPage>> batches, 
    ProcessingContext context)  // ⭐ 新增参数
```

## 🎯 修复效果

### 修复前
```
主线程设置 ThreadLocal ✅
  ↓
子线程获取 ThreadLocal → null ❌
  ↓
回调未触发 ❌
  ↓
前端无内容 ❌
```

### 修复后
```
主线程传递 context 参数 ✅
  ↓
子线程接收 context 参数 ✅
  ↓
回调正确触发 ✅
  ↓
前端实时显示内容 ✅
```

## 🧪 验证方法

### 1. 查看日志

**修复后应该看到**：
```
✅ [VisionLLM] 检测到流式回调
✅ [VisionLLM] 流式模式: true
🚀 [VisionLLM] 启动流式处理，页面 1
📤 [VisionLLM] 发送页面分隔符
🔄 [VisionLLM] 开始调用 chatWithVisionFlux
📥 [VisionLLM] 收到 token: 123 字符
📤 [STREAM] 发送流式内容: 123 字符
✅ [STREAM] 成功发送流式内容
```

**如果仍然没有看到"收到 token"**：
- 问题不在 ThreadLocal
- 问题在 Vision API 调用

### 2. 前端验证

**浏览器控制台应该看到**：
```javascript
📥 收到SSE事件: content {type: "content", content: "..."}
📄 累加文本内容，长度: 123 模式: 流式
```

**前端 UI 应该看到**：
- 文本区域内容逐步增长
- 不再是等待全部完成才显示

### 3. Network 验证

**浏览器 Network 标签页**：
- 找到 `/extract` 请求
- 查看 EventStream
- 应该看到连续的 `data:{"type":"content",...}` 消息

## 💡 为什么不用 InheritableThreadLocal？

**InheritableThreadLocal** 可以让子线程继承父线程的值，但有问题：

### 问题 1：线程池复用
```java
// 第一次请求
主线程 A: InheritableThreadLocal.set(contextA)
子线程 1: 继承 contextA ✅

// 第二次请求
主线程 B: InheritableThreadLocal.set(contextB)
子线程 1（复用）: 仍然是 contextA ❌  // 线程池复用，不会重新继承
```

### 问题 2：内存泄漏风险
- InheritableThreadLocal 容易导致内存泄漏
- 子线程长时间持有引用

### 问题 3：代码不清晰
- 隐式传递，不易理解
- 调试困难

**结论**：**直接传参是最简单、最可靠的方案**

## 🔧 其他注意事项

### 1. ThreadLocal 仍然保留

虽然我们不再依赖 ThreadLocal 传递 context 到子线程，但 ThreadLocal 仍然有用：

```java
@Override
public ProcessingResult process(ProcessingContext context) throws DocumentProcessingException {
    processingContextThreadLocal.set(context);  // ⭐ 仍然设置
    try {
        // ... 处理逻辑
    } finally {
        processingContextThreadLocal.remove();  // ⭐ 清理
    }
}
```

**原因**：
- 主线程中的其他方法可能需要访问
- 保持向后兼容

### 2. 串行处理也修改

虽然串行处理在主线程中执行，ThreadLocal 可以访问，但为了代码一致性，也改为传参：

```java
// 串行和并行使用相同的签名
processPageBatchesSequentially(batches, context);
processPageBatchesInParallel(batches, context);
```

## 📊 性能影响

**传递参数 vs ThreadLocal**：

| 方面 | ThreadLocal | 传参 |
|------|-------------|------|
| 性能 | 微小优势 | 几乎相同 |
| 可靠性 | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| 可维护性 | ⭐⭐ | ⭐⭐⭐⭐⭐ |
| 调试难度 | 难 | 易 |
| 内存泄漏风险 | 有 | 无 |

**结论**：传参方案更好

## 🚀 下一步

1. **重启应用**
2. **重新测试文档提取**
3. **查看日志确认回调被调用**
4. **查看前端是否实时显示**

如果仍然有问题，按照 `STREAMING_STUCK_DEBUG.md` 排查其他可能原因。

---

**更新时间**：2025-12-24  
**问题类型**：ThreadLocal 子线程访问失效  
**解决方案**：直接传参  
**影响范围**：所有并行处理的流式回调

