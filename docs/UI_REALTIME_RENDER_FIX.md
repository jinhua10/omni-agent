# UI 不实时渲染批次内容问题 - 修复说明

## 🐛 问题现象

**接口返回**：`event:message data:{"batchIndex":1,"type":"content","content":" Smart"}`  
**UI 表现**：没有实时渲染输出的内容

## 🔍 问题根源

### 问题 1：右侧面板显示条件错误
```javascript
// ❌ 修复前
{documentId && extractionResult ? (
    <显示提取结果面板>
) : (
    <显示模型说明>
)}
```

**问题**：当有批次时，`extractionResult` 为空（内容都在批次中），导致不显示预览面板！

### 问题 2：Collapse 使用 defaultActiveKey
```javascript
// ❌ 修复前
<Collapse
    defaultActiveKey={batches.map(b => b.index)}
    items={...}
/>
```

**问题**：`defaultActiveKey` 只在组件**首次挂载**时生效，后续批次内容更新时不会触发重新渲染！

### 问题 3：字符统计未考虑批次
```javascript
// ❌ 修复前
<Tag color="blue">{extractionResult.length} 字符</Tag>
```

**问题**：当内容在批次中时，`extractionResult.length` 为 0。

---

## ✅ 解决方案

### 修复 1：右侧面板显示条件
```javascript
// ✅ 修复后
{documentId && (extractionResult || batches.length > 0 || extracting) ? (
    <显示提取结果面板>
) : (
    <显示模型说明>
)}
```

**改进**：
- 只要有 `extractionResult` 或 `batches` 或 `extracting`，就显示预览面板
- 支持提取过程中的实时显示

### 修复 2：Collapse 改用 activeKey
```javascript
// ✅ 修复后
<Collapse
    activeKey={batches.map(b => b.index)}  // ⭐ 使用 activeKey
    items={...}
/>
```

**改进**：
- `activeKey` 会随着 `batches` 状态变化而更新
- 批次内容更新时，Collapse 会重新渲染
- 所有批次始终保持展开状态

### 修复 3：字符统计支持批次
```javascript
// ✅ 修复后
<Tag color="blue">
    {batches.length > 0 && !isMerged
        ? `${batches.reduce((sum, b) => sum + b.content.length, 0)} 字符`
        : `${extractionResult.length} 字符`
    }
</Tag>
```

**改进**：
- 有批次时：统计所有批次内容的总长度
- 无批次或已合并：显示 `extractionResult` 长度

---

## 📊 渲染流程对比

### 修复前（不工作）
```
接口返回: {"batchIndex":1,"type":"content","content":" Smart"}
    ↓
前端处理: batchIdx = 1
    ↓
更新状态: batches[1].content += " Smart" ✅
    ↓
Collapse 组件: defaultActiveKey={[0,1,2]} (不更新 ❌)
    ↓
渲染: 不触发重新渲染 ❌
    ↓
UI: 看不到内容 ❌
```

### 修复后（工作）
```
接口返回: {"batchIndex":1,"type":"content","content":" Smart"}
    ↓
前端处理: batchIdx = 1
    ↓
更新状态: batches[1].content += " Smart" ✅
    ↓
Collapse 组件: activeKey={[0,1,2]} (响应式更新 ✅)
    ↓
渲染: 批次 2 内容变化，触发重新渲染 ✅
    ↓
UI: 实时显示 " Smart" ✅
```

---

## 🎯 关键改进

| 改进项 | 修复前 | 修复后 |
|--------|--------|--------|
| 面板显示条件 | `extractionResult` 非空 | `extractionResult` 或 `batches` 非空 |
| Collapse 展开控制 | `defaultActiveKey`（静态） | `activeKey`（响应式） |
| 字符统计 | 只统计 `extractionResult` | 批次时统计批次总长度 |
| 实时渲染 | ❌ 不工作 | ✅ 工作 |

---

## 🧪 测试验证

### 步骤 1：清除缓存并重启
```bash
# 清除浏览器缓存
Ctrl + Shift + Delete

# 重启前端（如果在开发模式）
npm run dev
```

### 步骤 2：打开开发者工具
```
F12 → Console 标签页
```

### 步骤 3：开始提取
```
1. 上传文档
2. 点击"开始提取"
3. 观察控制台日志
```

### 步骤 4：验证实时渲染

**控制台应该看到**：
```
📦 收到批次信息: {totalBatches: 3, totalPages: 10}
🚀 批次开始: {batchIndex: 0, batchNumber: 1}
📄 累加文本内容: {
    长度: 6,
    批次索引: 0,
    消息中的索引: 0,
    内容预览: " Smart"
}
📊 批次状态更新: [
    {批次: 1, 状态: "processing", 内容长度: 6},  ← ✅ 长度增加
    {批次: 2, 状态: "pending", 内容长度: 0},
    {批次: 3, 状态: "pending", 内容长度: 0}
]
```

**UI 应该看到**：
- ✅ 批次 1 面板展开
- ✅ 批次 1 内容实时显示 " Smart"
- ✅ 字符统计实时更新（6 字符）

---

## 🔧 React 组件渲染机制

### defaultActiveKey vs activeKey

**defaultActiveKey（静态）**：
```javascript
// 只在组件首次渲染时使用
<Collapse defaultActiveKey={[0, 1, 2]}>
    {/* 后续 props 变化不会触发更新 */}
</Collapse>
```

**activeKey（响应式）**：
```javascript
// 每次 state 变化都会更新
<Collapse activeKey={batches.map(b => b.index)}>
    {/* batches 变化 → activeKey 变化 → 重新渲染 ✅ */}
</Collapse>
```

### React 渲染时机
```
状态更新: setBatches(...)
    ↓
React 检测到 batches 变化
    ↓
重新执行 render 函数
    ↓
activeKey 重新计算: batches.map(b => b.index)
    ↓
Collapse 组件接收新的 activeKey
    ↓
Collapse 内部更新
    ↓
子组件 ReactMarkdown 重新渲染
    ↓
UI 显示最新内容 ✅
```

---

## ✅ 改动文件清单

**文件**：`UI/src/components/document/TextExtractionConfig.jsx`

### 改动 1：右侧面板显示条件 (Line 591)
```javascript
// 支持批次、extractionResult 或提取中时显示
{documentId && (extractionResult || batches.length > 0 || extracting) ? (
```

### 改动 2：字符统计逻辑 (Line 595-601)
```javascript
// 批次时统计所有批次的总长度
{batches.length > 0 && !isMerged
    ? `${batches.reduce((sum, b) => sum + b.content.length, 0)} 字符`
    : `${extractionResult.length} 字符`
}
```

### 改动 3：Collapse activeKey (Line 705)
```javascript
// 使用 activeKey 而不是 defaultActiveKey
activeKey={batches.map(b => b.index)}
```

---

## 🎉 预期效果

修复后，你应该看到：

1. **提取开始**：
   - 右侧立即显示预览面板
   - 显示 3 个批次折叠面板（全部展开）

2. **提取过程中**：
   - 批次 1 开始 → 状态变为"处理中⟳"
   - 接收到内容 → 批次 1 面板实时显示内容
   - 字符统计实时更新

3. **并行处理**：
   - 批次 1、3 同时处理
   - 各自面板独立显示内容
   - 不交叉、不混乱

4. **提取完成**：
   - 所有批次状态变为"已完成✓"
   - 显示"合并批次"按钮
   - 点击合并 → 显示完整文档

---

**版本**：v4.1.4 - UI 实时渲染修复版  
**修复时间**：2025-12-24  
**问题**：批次内容不实时渲染  
**原因**：Collapse 使用 defaultActiveKey + 面板显示条件错误  
**解决**：改用 activeKey + 完善显示逻辑

