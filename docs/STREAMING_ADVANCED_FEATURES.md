# 流式提取高级功能 - 完整实现文档

## 🎯 新增功能概览

基于基础流式提取功能，新增以下高级特性：

1. ✅ **批次级别分区显示** - 每个批次独立折叠面板
2. ✅ **实时自动保存** - 3秒防抖，自动保存到服务器
3. ✅ **多格式导出** - Markdown / HTML 文件导出
4. ✅ **批次状态可视化** - 实时显示处理进度

---

## 📋 功能详情

### 1. 批次级别分区显示

#### 功能说明
每个处理批次有独立的折叠面板，用户可以：
- 查看每个批次的处理状态
- 独立展开/折叠每个批次
- 批次处理完成后自动标记

#### UI 效果
```
┌─ 批次 1 [已完成 ✓] ───────────────┐
│ ## 📄 页面 1                      │
│ ### 幻灯片标题                     │
│ 内容...                           │
└────────────────────────────────────┘

┌─ 批次 2 [处理中 ⟳] ───────────────┐
│ ## 📄 页面 4                      │
│ 正在分析...                        │
└────────────────────────────────────┘

┌─ 批次 3 [等待中] ─────────────────┐
│ 等待前面批次完成...                │
└────────────────────────────────────┘
```

#### 技术实现

**后端**：
```java
// VisionLLMDocumentProcessor.java
// 批次开始时发送标记
String batchMarker = String.format(
    "BATCH_START:{\"batchIndex\":%d,\"batchNumber\":%d,\"totalBatches\":%d}\n",
    i, i + 1, batches.size()
);
callback.accept(batchMarker);

// 批次结束时发送标记
String batchEndMarker = String.format(
    "BATCH_END:{\"batchIndex\":%d,\"batchNumber\":%d}\n",
    i, i + 1
);
callback.accept(batchEndMarker);
```

**前端**：
```javascript
// TextExtractionConfig.jsx
const [batches, setBatches] = useState([])

// 处理批次开始消息
else if (data.type === 'batchStart') {
    currentBatchIndex = data.batchIndex
    setBatches(prev => prev.map(b => 
        b.index === data.batchIndex 
            ? { ...b, status: 'processing' } 
            : b
    ))
}

// 渲染折叠面板
<Collapse
    items={batches.map(batch => ({
        label: (
            <Space>
                <span>批次 {batch.number}</span>
                {batch.status === 'processing' && 
                    <Tag icon={<LoadingOutlined />} color="processing">处理中</Tag>}
                {batch.status === 'completed' && 
                    <Tag icon={<CheckCircleFilled />} color="success">已完成</Tag>}
            </Space>
        ),
        children: <ReactMarkdown>{batch.content}</ReactMarkdown>
    }))}
/>
```

---

### 2. 实时自动保存

#### 功能说明
- 用户编辑源码后，3秒内无新编辑自动保存
- 显示最后保存时间
- 可手动开关自动保存功能

#### UI 效果
```
┌─────────────────────────────────────────┐
│ [预览] [源码]  [💾自动保存 ON]  已保存✓ │
│                    上次保存: 14:32:15    │
└─────────────────────────────────────────┘
```

#### 技术实现

```javascript
// 状态管理
const [autoSaveEnabled, setAutoSaveEnabled] = useState(true)
const [lastSaved, setLastSaved] = useState(null)

// 自动保存 Hook（3秒防抖）
useEffect(() => {
    if (!autoSaveEnabled || !extractionResult || !documentId) return

    const timer = setTimeout(() => {
        saveExtractionResult()
    }, 3000) // 3秒防抖

    return () => clearTimeout(timer)
}, [extractionResult, autoSaveEnabled, documentId])

// 保存函数
const saveExtractionResult = async () => {
    const encodedDocId = encodeURIComponent(documentId)
    await fetch(`/api/system/rag-config/document/${encodedDocId}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
            extractedText: extractionResult,
            textExtractionModel: selectedModel,
        }),
    })
    setLastSaved(new Date())
}
```

#### 使用场景
- **长文档编辑**：避免丢失修改
- **断网保护**：定期保存，减少数据丢失风险
- **多次修改**：不需要手动点保存

---

### 3. 多格式导出

#### 功能说明
支持导出为多种格式：
- **Markdown**：纯文本，保留完整语法
- **HTML**：带样式的网页文件

#### UI 效果
```
┌─────────────────┐
│ 导出 ▼         │
├─────────────────┤
│ 📥 导出 Markdown│
│ 📥 导出 HTML    │
└─────────────────┘
```

#### 技术实现

**Markdown 导出**：
```javascript
const exportAsMarkdown = () => {
    const blob = new Blob([extractionResult], { 
        type: 'text/markdown;charset=utf-8' 
    })
    const url = URL.createObjectURL(blob)
    const link = document.createElement('a')
    link.href = url
    link.download = `${documentId || 'extraction'}.md`
    link.click()
    URL.revokeObjectURL(url)
}
```

**HTML 导出**：
```javascript
const exportAsHTML = () => {
    const htmlContent = `<!DOCTYPE html>
<html lang="zh-CN">
<head>
    <meta charset="UTF-8">
    <title>${documentId}</title>
    <style>
        /* GitHub 风格样式 */
        body { font-family: sans-serif; max-width: 900px; margin: 40px auto; }
        h2 { border-bottom: 1px solid #eaecef; padding-bottom: 0.3em; }
        code { background-color: #f6f8fa; padding: 2px 6px; }
        /* ...更多样式... */
    </style>
</head>
<body>
    ${convertMarkdownToHTML(extractionResult)}
</body>
</html>`
    
    const blob = new Blob([htmlContent], { type: 'text/html' })
    // ...下载逻辑
}
```

#### 使用场景
- **Markdown**：用于 GitHub、博客、文档系统
- **HTML**：用于分享、打印、归档

---

### 4. 批次状态可视化

#### 状态类型

| 状态 | 图标 | 颜色 | 说明 |
|------|------|------|------|
| 等待中 | - | 灰色 | 批次尚未开始 |
| 处理中 | ⟳ | 蓝色 | 正在调用 Vision LLM |
| 已完成 | ✓ | 绿色 | 批次处理完成 |

#### UI 实现

```javascript
{batch.status === 'pending' && 
    <Tag color="default">等待中</Tag>}
{batch.status === 'processing' && 
    <Tag icon={<LoadingOutlined />} color="processing">处理中</Tag>}
{batch.status === 'completed' && 
    <Tag icon={<CheckCircleFilled />} color="success">已完成</Tag>}
```

#### 工作流程
```
批次 1: 等待中 → 处理中 ⟳ → 已完成 ✓
            ↓
批次 2: 等待中 → 处理中 ⟳ → 已完成 ✓
            ↓
批次 3: 等待中 → 处理中 ⟳ → 已完成 ✓
```

---

## 🔄 完整工作流程

### 用户操作流程
```
1. 上传文档 → 选择 Vision LLM → 开启流式
               ↓
2. 点击"开始提取" → 收到批次信息 "3 批次 / 10 页面"
               ↓
3. 批次 1 开始 → 折叠面板显示"处理中⟳"
               ↓ (实时显示内容)
   批次 1 完成 → 折叠面板显示"已完成✓"
               ↓
4. 批次 2 开始 → 重复上述过程
               ↓
5. 全部完成 → 可切换预览/源码
               ↓
6. 编辑源码 → 3秒后自动保存 → 显示"已保存✓"
               ↓
7. 导出 → 选择 Markdown 或 HTML → 下载文件
```

### 数据流
```
后端处理
  ↓
BATCH_INFO → 初始化批次数组
  ↓
BATCH_START → 更新批次状态 = processing
  ↓
content → 累加到对应批次.content
  ↓
BATCH_END → 更新批次状态 = completed
  ↓
complete → 全部完成
```

---

## 📊 性能优化

### 1. 防抖机制
```javascript
// 避免频繁保存
setTimeout(() => saveExtractionResult(), 3000)
```

### 2. 批次级别渲染
```javascript
// 只渲染可见批次，提升性能
<Collapse defaultActiveKey={[0]}>  // 只展开第一个
```

### 3. 内存管理
```javascript
// 清理 Blob URL
URL.revokeObjectURL(url)
```

---

## 🎨 UI/UX 优化

### 1. 视觉层次
- **批次面板**：清晰的折叠/展开动画
- **状态标签**：颜色区分，图标辅助
- **自动保存**：非侵入式提示

### 2. 交互反馈
- **加载动画**：处理中显示旋转图标
- **完成提示**：成功后显示绿色对勾
- **保存确认**：显示保存时间戳

### 3. 响应式设计
- 大屏：折叠面板 + 预览并排
- 小屏：垂直布局，折叠面板全宽

---

## 🚀 使用指南

### 基础使用
1. 开启流式模式
2. 开始提取
3. 观察批次进度
4. 查看/编辑结果

### 高级使用
1. **批次管理**
   - 展开/折叠特定批次
   - 查看批次状态
   - 批次内容独立显示

2. **自动保存**
   - 编辑源码
   - 等待 3 秒
   - 查看"已保存"标签

3. **导出分享**
   - 点击"导出"下拉菜单
   - 选择格式（Markdown/HTML）
   - 文件自动下载

---

## 📝 代码位置

### 后端
- **批次标记发送**：`VisionLLMDocumentProcessor.java:1422-1445`
- **Controller 处理**：`DocumentProcessingController.java:159-181`

### 前端
- **批次状态管理**：`TextExtractionConfig.jsx:91-92`
- **自动保存逻辑**：`TextExtractionConfig.jsx:124-147`
- **导出功能**：`TextExtractionConfig.jsx:149-189`
- **批次面板渲染**：`TextExtractionConfig.jsx:632-653`

---

## 🧪 测试场景

### 测试 1：批次显示
1. 上传 10 页 PPT
2. 观察批次数量（应为 2-3 个）
3. 每个批次独立显示
4. 状态正确切换

### 测试 2：自动保存
1. 切换到"源码"模式
2. 编辑内容
3. 等待 3 秒
4. 查看"已保存"提示

### 测试 3：导出功能
1. 完成提取
2. 点击"导出" → "Markdown"
3. 验证文件内容完整
4. 点击"导出" → "HTML"
5. 在浏览器中打开验证样式

### 测试 4：批次状态
1. 提取过程中观察
2. "等待中" → "处理中" → "已完成"
3. 状态图标和颜色正确

---

## ✅ 验证清单

- [x] 批次面板正常展开/折叠
- [x] 批次状态实时更新
- [x] 自动保存功能正常
- [x] 保存时间显示正确
- [x] Markdown 导出成功
- [x] HTML 导出成功且样式正确
- [x] 批次内容独立累加
- [x] 所有功能编译通过

---

## 🎓 最佳实践

### 1. 大文档处理
- 使用流式模式
- 启用自动保存
- 批次级别查看

### 2. 内容编辑
- 切换到源码模式
- 编辑后等待自动保存
- 定期导出备份

### 3. 分享协作
- 导出为 HTML（带样式）
- 或导出为 Markdown（纯文本）

---

**版本**：v4.0 - 高级功能完整版  
**更新时间**：2025-12-24  
**作者**：OmniAgent Team

