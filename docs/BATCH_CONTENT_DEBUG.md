# 批次内容错乱问题 - 调试指南

## 🐛 问题现象

**用户反馈**：
```
批次 1: "---  ## 📄 页面 5    ---  ## 📄 页面 1  ### 幻灯### 幻灯片片文字文字内容内容"
批次 2: 没有内容
```

**分析**：内容混乱，页面 5 和页面 1 的内容都出现在批次 1 中。

---

## 🔍 已实施的修复

### 修复 1：分离批次内容累加
**问题**：之前所有批次的内容都累加到 `extractionResult`，导致重复和混乱。

**解决**：
```javascript
// ❌ 之前：所有内容都累加到 extractionResult
setExtractionResult(prev => prev + newContent)
setBatches(prev => prev.map(...))  // 同时也累加到批次

// ✅ 现在：只累加到对应批次
if (batchIdx >= 0) {
    setBatches(prev => prev.map(b =>
        b.index === batchIdx
            ? { ...b, content: b.content + newContent }
            : b
    ))
} else {
    // 降级：无批次信息时才累加到 extractionResult
    setExtractionResult(prev => prev + newContent)
}
```

### 修复 2：添加详细调试日志
**位置**：`TextExtractionConfig.jsx:375-395`

**日志输出**：
```javascript
console.log('📄 累加文本内容:', {
    长度: newContent.length,
    批次索引: batchIdx,
    消息中的索引: data.batchIndex,
    全局索引: currentBatchIndex,
    内容预览: newContent.substring(0, 50)
})

console.log('📊 批次状态更新:', updated.map(b => ({
    批次: b.number,
    状态: b.status,
    内容长度: b.content.length
})))
```

---

## 🧪 调试步骤

### 步骤 1：查看浏览器控制台

**打开开发者工具** → **Console 标签页**

**观察日志输出**：

#### 正常情况应该看到：
```
📦 收到批次信息: {totalBatches: 3, totalPages: 10}
🚀 批次开始: {batchIndex: 0, batchNumber: 1}
📄 累加文本内容: {
    长度: 50,
    批次索引: 0,
    消息中的索引: 0,  ← 关键：应该有值
    全局索引: 0,
    内容预览: "---\n\n## 📄 页面 1\n\n### 幻灯片标题..."
}
📊 批次状态更新: [
    {批次: 1, 状态: "processing", 内容长度: 50},
    {批次: 2, 状态: "pending", 内容长度: 0},
    {批次: 3, 状态: "pending", 内容长度: 0}
]
```

#### 异常情况会看到：
```
📄 累加文本内容: {
    批次索引: -1,        ← ❌ 错误：索引为 -1
    消息中的索引: undefined,  ← ❌ 消息中没有索引
    全局索引: -1
}
⚠️ 未找到批次索引，使用旧协议
```

### 步骤 2：检查 Network 标签页

**Network** → **找到 `/extract` 请求** → **查看 EventStream**

**正常的 SSE 消息应该是**：
```
event:message
data:{"type":"batchInfo","totalBatches":3,"totalPages":10}

event:message
data:{"type":"batchStart","batchIndex":0,"batchNumber":1}

event:message
data:{"type":"content","batchIndex":0,"content":"---\n\n## 📄 页面 1\n\n"}

event:message
data:{"type":"content","batchIndex":0,"content":"### 幻灯片标题\n\n"}
```

**关键检查**：
- ✅ `data.type === "content"` 的消息中必须有 `batchIndex` 字段
- ✅ `batchIndex` 的值应该是 0、1、2 等整数
- ✅ 相同 `batchIndex` 的内容应该连续

### 步骤 3：检查后端日志

**查看后端日志文件或控制台**

**应该看到**：
```
📤 [STREAM] 发送批次 0 的流式内容: 50 字符
📥 [VisionLLM] 收到 token: 28 字符，批次 0
📤 [STREAM] 发送批次 0 的流式内容: 28 字符
```

**如果看到**：
```
📤 [STREAM] 发送流式内容: 50 字符  ← ❌ 没有批次信息
```
说明后端没有使用 `BATCH_CONTENT` 协议。

---

## 🔧 可能的原因和解决方案

### 原因 1：后端未发送批次索引

**症状**：
- 控制台显示 `消息中的索引: undefined`
- Network 中的消息没有 `batchIndex` 字段

**检查**：
```java
// VisionLLMDocumentProcessor.java
// 确保使用 BATCH_CONTENT 格式
finalStreamCallback.accept("BATCH_CONTENT:" + batchIndex + ":" + token);
```

**验证**：后端日志应该显示 `发送批次 X 的流式内容`

### 原因 2：Controller 解析失败

**症状**：
- 后端日志显示 `发送批次 X 的流式内容`
- 但前端收到的消息没有 `batchIndex`

**检查**：
```java
// DocumentProcessingController.java:180-200
if (chunk.startsWith("BATCH_CONTENT:")) {
    int firstColon = chunk.indexOf(':', 14);
    // ... 解析逻辑
}
```

**验证**：添加日志查看解析是否成功

### 原因 3：context.getOptions() 中没有 currentBatchIndex

**症状**：
- 后端日志显示批次索引为 -1

**检查**：
```java
// processPageBatch 方法中
if (context != null && context.getOptions() != null) {
    context.getOptions().put("currentBatchIndex", batchIndex);
}
```

### 原因 4：前端批次初始化有问题

**症状**：
- 批次数组为空或长度不对

**检查**：
```javascript
// 收到 BATCH_INFO 后
const initialBatches = Array.from({ length: data.totalBatches }, (_, i) => ({
    index: i,
    number: i + 1,
    content: '',
    status: 'pending',
}))
```

---

## 📊 完整的数据流追踪

### 正常流程
```
后端：processPageBatch(batch, context, 0)
    ↓
后端：context.options.put("currentBatchIndex", 0)
    ↓
后端：finalStreamCallback.accept("BATCH_CONTENT:0:内容")
    ↓
Controller：检测到 BATCH_CONTENT:
    ↓
Controller：提取索引 0 和内容
    ↓
Controller：发送 {"type":"content","batchIndex":0,"content":"..."}
    ↓
前端：data.batchIndex === 0
    ↓
前端：setBatches(更新批次 0 的内容)
    ↓
UI：批次 1 面板显示新内容
```

### 异常流程（会导致混乱）
```
后端：并行处理批次 0 和批次 2
    ↓
后端：批次 0 发送 "BATCH_CONTENT:0:内容A"
后端：批次 2 发送 "BATCH_CONTENT:2:内容B"
    ↓
Controller：正确解析，分别发送 batchIndex:0 和 batchIndex:2
    ↓
前端：✅ 应该正确累加到各自批次
    ↓
如果前端出错 → 内容混乱
```

---

## ✅ 验证清单

请按顺序检查以下项目：

- [ ] 浏览器控制台是否显示 `消息中的索引: 0/1/2`（有具体数字）
- [ ] Network 中的 SSE 消息是否包含 `"batchIndex":0`
- [ ] 后端日志是否显示 `发送批次 X 的流式内容`
- [ ] 批次状态更新日志中，各批次的内容长度是否正确增长
- [ ] 批次面板中，每个批次是否显示各自的内容（不交叉）
- [ ] 所有批次完成后，点击"合并批次"，内容是否按顺序正确合并

---

## 🚀 立即测试

1. **清除浏览器缓存**（Ctrl + Shift + Delete）
2. **重启后端应用**
3. **刷新前端页面**（Ctrl + F5）
4. **打开开发者工具**（F12）
5. **上传文档，开始提取**
6. **观察控制台日志**
7. **对照上面的"正常情况"检查每一项**
8. **截图或复制日志**发给我进行分析

---

**版本**：v4.1.2 - 批次内容错乱修复版  
**修复时间**：2025-12-24  
**关键改进**：分离批次内容累加 + 详细调试日志

