# 页面突然看不到批次内容问题 - 快速修复

## 🐛 问题现象

**用户反馈**：页面突然看不到之前实时显示的批次内容

## 🔍 问题根源

之前的修复中，我们修改了内容累加逻辑：
```javascript
// 只累加到批次，不累加到 extractionResult
if (batchIdx >= 0) {
    setBatches(...)  // ✅ 更新批次
    // ❌ 不再更新 extractionResult
}
```

**但是**，显示逻辑是：
```javascript
{batches.length > 0 ? (
    <显示批次面板>
) : (
    <显示 extractionResult>  // ❌ extractionResult 为空！
)}
```

**导致**：
1. 刚开始提取时，`batches` 为空数组（还未收到 BATCH_INFO）
2. 显示 `extractionResult`，但 `extractionResult` 不再累加内容
3. 页面空白，看不到任何内容

## ✅ 解决方案

### 修复 1：预览模式显示逻辑
```javascript
{batches.length > 0 && !isMerged ? (
    // 有批次且未合并：显示批次面板
    <Collapse items={batches} />
) : (
    // 无批次或已合并：显示完整内容
    <ReactMarkdown>{extractionResult || '等待提取...'}</ReactMarkdown>
)}
```

**改进**：
- ✅ 添加 `&& !isMerged` 条件，合并后显示完整内容
- ✅ 添加 `|| '等待提取...'` 默认值，避免空白

### 修复 2：源码模式显示逻辑
```javascript
<TextArea
    value={
        batches.length > 0 && !isMerged
            ? batches.sort(...).map(b => b.content).join('\n\n')  // 临时合并
            : extractionResult  // 已合并或无批次
    }
/>
```

**改进**：
- ✅ 源码模式实时显示批次内容（临时合并）
- ✅ 合并后显示 extractionResult

## 📊 显示逻辑流程

### 流程 1：正常提取（有批次）
```
开始提取
    ↓
batches = [] (空) + extractionResult = ""
    → 显示 "等待提取..."
    ↓
收到 BATCH_INFO → batches = [{}, {}, {}]
    → 显示批次面板（3个折叠面板）
    ↓
收到内容 → batches[0].content = "..."
    → 批次 1 显示内容
    ↓
所有完成 → 点击"合并批次"
    → setExtractionResult(合并内容)
    → setIsMerged(true)
    → setBatches([])
    → 显示完整文档
```

### 流程 2：旧协议提取（无批次）
```
开始提取
    ↓
batches = [] + extractionResult = ""
    → 显示 "等待提取..."
    ↓
收到内容（无 batchIndex）
    → extractionResult += content  // 降级处理
    → 显示 extractionResult
```

## 🎨 UI 状态对照表

| 阶段 | batches | isMerged | extractionResult | 显示内容 |
|------|---------|----------|------------------|---------|
| 提取前 | [] | false | "" | "等待提取..." |
| 收到批次信息 | [{},{},{}] | false | "" | 批次面板 |
| 提取中 | [{content:...}...] | false | "" | 批次面板（有内容） |
| 提取完成 | [{content:...}...] | false | "" | 批次面板 + "合并"按钮 |
| 点击合并 | [] | true | "完整内容" | 完整文档 |
| 旧协议提取 | [] | false | "累加内容" | 累加内容 |

## ✅ 改动文件

**文件**：`UI/src/components/document/TextExtractionConfig.jsx`

**改动 1**：预览模式 (Line ~697)
```javascript
// 添加 && !isMerged 条件
{batches.length > 0 && !isMerged ? (
    <批次面板>
) : (
    <ReactMarkdown>{extractionResult || '等待提取...'}</ReactMarkdown>
)}
```

**改动 2**：源码模式 (Line ~726)
```javascript
// 临时合并批次内容
value={
    batches.length > 0 && !isMerged
        ? batches.sort(...).map(b => b.content).join('\n\n')
        : extractionResult
}
```

## 🧪 测试验证

1. **刷新页面**（Ctrl + F5）
2. **开始提取**
3. **观察**：
   - ✅ 一开始显示 "等待提取..."
   - ✅ 收到批次信息后显示批次面板
   - ✅ 批次内容实时显示在对应面板中
   - ✅ 切换到"源码"模式也能看到内容（临时合并）
4. **等待完成**
5. **点击"合并批次"**
6. **验证**：
   - ✅ 批次面板消失，显示完整文档
   - ✅ 源码模式显示完整 Markdown

## 🎯 关键改进

### 改进 1：空白页面问题
- ❌ 之前：提取初期页面空白
- ✅ 现在：显示 "等待提取..."

### 改进 2：源码模式问题
- ❌ 之前：源码模式看不到批次内容
- ✅ 现在：临时合并显示

### 改进 3：合并后问题
- ❌ 之前：合并后 `isMerged=true` 但仍显示批次面板（已清空）
- ✅ 现在：合并后显示完整文档

---

**版本**：v4.1.3 - 显示逻辑修复版  
**修复时间**：2025-12-24  
**问题**：页面空白，看不到内容  
**原因**：显示逻辑未考虑批次初始化和合并状态  
**解决**：完善显示逻辑，支持所有状态

