# 并行处理批次索引混乱问题 - 最终修复

## 🐛 问题现象

**接口输出**：
```
event:message data:{"batchIndex":1,"type":"content","content":"\n\n---\n\n## 📄 页面 1\n\n"}
event:message data:{"batchIndex":1,"type":"content","content":"\n\n---\n\n## 📄 页面 5\n\n"}
```

**分析**：
- 批次 1 中同时出现了"页面 1"和"页面 5"
- 说明不同批次的页面被发送到了同一个批次索引

## 🔍 问题根源

### 共享 Context 导致批次索引被覆盖

**错误代码**：
```java
// ❌ processPageBatch 方法中
private String processPageBatch(List<DocumentPage> pages, ProcessingContext context, int batchIndex) {
    // 将批次索引存储到共享的 context 中
    context.getOptions().put("currentBatchIndex", batchIndex);  // ❌ 多线程不安全！
    
    for (DocumentPage page : pages) {
        // 从 context 中读取批次索引
        recognizePageWithVisionLLM(page, prompt, context);  // 读取时可能已被其他线程覆盖
    }
}
```

**并行处理时的问题**：
```
时间线（3个批次并行）：

T1: 线程A 执行批次0 → context.put("currentBatchIndex", 0)
T2: 线程B 执行批次1 → context.put("currentBatchIndex", 1)  // 覆盖！
T3: 线程C 执行批次2 → context.put("currentBatchIndex", 2)  // 覆盖！

T4: 线程A 处理页面1 → 从 context 读取 = 2  // ❌ 错误！应该是 0
    → 发送 BATCH_CONTENT:2:页面1

T5: 线程B 处理页面5 → 从 context 读取 = 2  // ❌ 错误！应该是 1
    → 发送 BATCH_CONTENT:2:页面5

结果：页面1和页面5都被发送到批次2 ❌
```

### 为什么会共享 Context？

在并行处理中，我们使用了 `CompletableFuture.supplyAsync`：

```java
for (int i = 0; i < batches.size(); i++) {
    final int batchIndex = i;
    final List<DocumentPage> batch = batches.get(i);
    
    CompletableFuture.supplyAsync(() -> {
        // ⭐ 所有线程共享同一个 context 对象
        processPageBatch(batch, context, batchIndex);
    }, visionLlmExecutor);
}
```

**所有线程都引用同一个 `context` 对象**，因此：
- `context.getOptions()` 返回同一个 `Map`
- 多个线程同时 `put("currentBatchIndex", ...)` 会互相覆盖
- 读取时得到的是最后一个写入的值

---

## ✅ 解决方案

### 方案：直接传递批次索引参数

**核心思想**：不要将批次索引存储在共享的 `context` 中，而是通过方法参数直接传递。

### 修改 1：`recognizePageWithVisionLLM` 方法签名

**修改前**：
```java
private String recognizePageWithVisionLLM(
    DocumentPage page, 
    String prompt, 
    ProcessingContext context) {  // ❌ 从 context 中读取批次索引
    
    // 从共享 context 中获取批次索引
    int batchIndex = (Integer) context.getOptions().get("currentBatchIndex");
}
```

**修改后**：
```java
private String recognizePageWithVisionLLM(
    DocumentPage page, 
    String prompt, 
    ProcessingContext context,
    int batchIndex) {  // ✅ 直接作为参数传递
    
    // 直接使用参数，不读取共享 context
    log.info("处理页面 {}, 批次 {}", page.getPageNumber(), batchIndex);
}
```

### 修改 2：`processPageBatch` 方法

**修改前**：
```java
private String processPageBatch(..., int batchIndex) {
    // ❌ 存储到共享 context
    context.getOptions().put("currentBatchIndex", batchIndex);
    
    for (DocumentPage page : pages) {
        // 不传递 batchIndex，让方法从 context 中读取
        recognizePageWithVisionLLM(page, prompt, context);
    }
}
```

**修改后**：
```java
private String processPageBatch(..., int batchIndex) {
    // ✅ 不存储到共享 context
    
    for (DocumentPage page : pages) {
        // ✅ 直接传递 batchIndex 参数
        recognizePageWithVisionLLM(page, prompt, context, batchIndex);
    }
}
```

### 修改 3：流式处理部分

**修改前**：
```java
if (finalStreamingEnabled && finalStreamCallback != null) {
    // ❌ 从共享 context 中读取
    int currentBatchIndex = -1;
    if (context != null && context.getOptions() != null) {
        Object batchIndexObj = context.getOptions().get("currentBatchIndex");
        if (batchIndexObj instanceof Integer) {
            currentBatchIndex = (Integer) batchIndexObj;
        }
    }
    final int batchIndex = currentBatchIndex;
    
    // 使用读取到的 batchIndex（可能是错误的）
    finalStreamCallback.accept("BATCH_CONTENT:" + batchIndex + ":...");
}
```

**修改后**：
```java
if (finalStreamingEnabled && finalStreamCallback != null) {
    // ✅ 直接使用方法参数
    log.info("启动流式处理，页面 {}, 批次 {}", page.getPageNumber(), batchIndex);
    
    // 使用正确的 batchIndex
    finalStreamCallback.accept("BATCH_CONTENT:" + batchIndex + ":...");
}
```

---

## 📊 修复效果对比

### 修复前（错误）
```
批次0（线程A）：context.put(0) → 处理页面1 → context.get() = 2 ❌
    → 发送：BATCH_CONTENT:2:页面1

批次1（线程B）：context.put(1) → 处理页面5 → context.get() = 2 ❌
    → 发送：BATCH_CONTENT:2:页面5

批次2（线程C）：context.put(2) → 处理页面8 → context.get() = 2 ✓
    → 发送：BATCH_CONTENT:2:页面8

结果：所有页面都发送到批次2 ❌
```

### 修复后（正确）
```
批次0（线程A）：参数 batchIndex=0 → 处理页面1
    → 发送：BATCH_CONTENT:0:页面1 ✅

批次1（线程B）：参数 batchIndex=1 → 处理页面5
    → 发送：BATCH_CONTENT:1:页面5 ✅

批次2（线程C）：参数 batchIndex=2 → 处理页面8
    → 发送：BATCH_CONTENT:2:页面8 ✅

结果：每个页面发送到正确的批次 ✅
```

---

## 🔧 改动文件清单

**文件**：`VisionLLMDocumentProcessor.java`

### 改动 1：方法签名 (Line ~1009)
```java
// 添加 batchIndex 参数
private String recognizePageWithVisionLLM(
    DocumentPage page, 
    String prompt, 
    ProcessingContext context,
    int batchIndex)  // ⭐ 新增参数
```

### 改动 2：processPageBatch (Line ~907-920)
```java
// ❌ 删除：存储到共享 context
// context.getOptions().put("currentBatchIndex", batchIndex);

// ✅ 添加：直接传递参数
recognizePageWithVisionLLM(page, pagePrompt, context, batchIndex);
```

### 改动 3：流式处理 (Line ~1057-1066)
```java
// ❌ 删除：从共享 context 读取
// int currentBatchIndex = -1;
// if (context != null && context.getOptions() != null) {
//     Object batchIndexObj = context.getOptions().get("currentBatchIndex");
//     ...
// }

// ✅ 直接使用方法参数
log.info("启动流式处理，页面 {}, 批次 {}", page.getPageNumber(), batchIndex);
```

---

## 💡 关键教训

### 1. 并行处理中避免共享可变状态

**错误模式**：
```java
// ❌ 多个线程共享同一个 Map
Map<String, Object> sharedMap = new HashMap<>();

CompletableFuture.supplyAsync(() -> {
    sharedMap.put("key", value1);  // 线程A
});

CompletableFuture.supplyAsync(() -> {
    sharedMap.put("key", value2);  // 线程B，覆盖线程A
});
```

**正确模式**：
```java
// ✅ 通过参数传递，每个线程有独立的值
CompletableFuture.supplyAsync(() -> {
    processWithValue(value1);  // 线程A
});

CompletableFuture.supplyAsync(() -> {
    processWithValue(value2);  // 线程B
});
```

### 2. ThreadLocal 也不是解决方案

虽然之前我们尝试过 `ThreadLocal`，但在线程池中也有问题：
- 线程复用导致值残留
- 需要手动清理

**最佳实践**：直接通过方法参数传递。

### 3. 调试并发问题的技巧

**添加日志**：
```java
log.info("📤 [Thread: {}] 发送页面 {}, 批次 {}", 
    Thread.currentThread().getName(), 
    page.getPageNumber(), 
    batchIndex);
```

**检查点**：
- 线程名称
- 时间戳
- 批次索引
- 页面编号

通过日志可以清楚地看到问题。

---

## ✅ 验证清单

修复后，应该满足：

- [ ] 后端日志显示：每个页面的批次索引正确
- [ ] 接口返回：`{"batchIndex":0,"content":"## 📄 页面 1"}` ✅
- [ ] 接口返回：`{"batchIndex":1,"content":"## 📄 页面 5"}` ✅
- [ ] 前端显示：批次 1 只有页面 1-3
- [ ] 前端显示：批次 2 只有页面 4-6
- [ ] 前端显示：批次 3 只有页面 7-10
- [ ] 合并后：页面按顺序 1→2→3...→10

---

## 🧪 测试方法

### 1. 查看后端日志

**应该看到**：
```
🔍 [VisionLLM] 处理第 1 页，包含 3 张图片，批次 0
📤 [VisionLLM] 发送页面标记: 页面 1, 批次 0
📥 [VisionLLM] 收到 token: 50 字符，批次 0

🔍 [VisionLLM] 处理第 5 页，包含 2 张图片，批次 1
📤 [VisionLLM] 发送页面标记: 页面 5, 批次 1
📥 [VisionLLM] 收到 token: 45 字符，批次 1
```

**关键检查**：
- ✅ 页面 1 → 批次 0
- ✅ 页面 5 → 批次 1
- ✅ 不同页面在正确的批次

### 2. 查看 Network

**应该看到**：
```
data:{"batchIndex":0,"type":"content","content":"## 📄 页面 1"}
data:{"batchIndex":0,"type":"content","content":"内容..."}
data:{"batchIndex":1,"type":"content","content":"## 📄 页面 5"}
data:{"batchIndex":1,"type":"content","content":"内容..."}
```

### 3. 查看前端控制台

**应该看到**：
```
📄 累加文本内容: {批次索引: 0, 内容预览: "## 📄 页面 1"}
📊 批次状态更新: [
    {批次: 1, 内容长度: 123},  // ✅ 批次 1 有内容
    {批次: 2, 内容长度: 0},
    {批次: 3, 内容长度: 0}
]

📄 累加文本内容: {批次索引: 1, 内容预览: "## 📄 页面 5"}
📊 批次状态更新: [
    {批次: 1, 内容长度: 123},
    {批次: 2, 内容长度: 98},  // ✅ 批次 2 有内容
    {批次: 3, 内容长度: 0}
]
```

---

**版本**：v4.1.5 - 并行批次索引修复版  
**修复时间**：2025-12-24  
**问题**：并行处理时批次索引混乱  
**原因**：共享 context 导致批次索引被覆盖  
**解决**：通过方法参数直接传递批次索引

