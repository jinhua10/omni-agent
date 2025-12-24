# 流式提取优化 - 完整解决方案

## 🎯 解决的问题

### 问题 1：并行处理导致内容混乱
**现象**：
```
=== 页面 5 ===  === 页面 1 === ### 幻### 幻灯灯片片文字文字内容内容
```

**原因**：多个页面并行处理，token 交错输出

**解决**：✅ 流式模式强制使用串行处理

### 问题 2：Markdown 语法被打散
**现象**：
```
### 幻### 幻灯灯片片
`` SmartArt`   ``### 图`  表说明
```

**原因**：并行输出时，Markdown 语法关键字被拆分到不同的 token

**解决**：✅ 串行处理 + 完整页面输出 + Markdown 分隔符

### 问题 3：前端无法预知批次数量
**原因**：后端开始处理后才知道有多少批次

**解决**：✅ 提前发送批次信息（`BATCH_INFO` 消息）

### 问题 4：无法编辑 Markdown 源码
**原因**：只有预览模式

**解决**：✅ 添加预览/源码双模式切换

---

## ✅ 完整解决方案

### 1. 后端改动

#### 1.1 强制流式模式使用串行处理
```java
// VisionLLMDocumentProcessor.java
boolean isStreamingMode = context != null 
    && context.getOptions() != null 
    && Boolean.TRUE.equals(context.getOptions().get("streaming"));

if (isStreamingMode) {
    // ✅ 流式模式：串行处理，确保顺序
    batchResults = processPageBatchesSequentially(batches, context);
} else {
    // 非流式模式才使用并行
    batchResults = processPageBatchesInParallel(batches, context);
}
```

**效果**：
- ✅ 避免内容交错
- ✅ Markdown 语法完整
- ✅ 按页面顺序输出

#### 1.2 发送批次信息
```java
// 分批后立即发送批次信息
String batchInfo = String.format(
    "BATCH_INFO:{\"totalBatches\":%d,\"totalPages\":%d}\n",
    batches.size(), pages.size()
);
callback.accept(batchInfo);
```

**消息格式**：
```
BATCH_INFO:{"totalBatches":3,"totalPages":10}
```

**效果**：
- ✅ 前端提前知道总批次数
- ✅ 可以预分配 UI 区域
- ✅ 显示进度更准确

#### 1.3 使用 Markdown 格式的页面分隔
```java
// 之前：纯文本分隔
callback.accept("\n=== 页面 " + page.getPageNumber() + " ===\n");

// 之后：Markdown 格式分隔
String pageHeader = String.format("\n\n---\n\n## 📄 页面 %d\n\n", page.getPageNumber());
callback.accept(pageHeader);
```

**效果**：
- ✅ 符合 Markdown 语法（`---` 是水平线，`##` 是二级标题）
- ✅ 渲染后美观
- ✅ 易于区分不同页面

#### 1.4 Controller 识别批次信息
```java
// DocumentProcessingController.java
if (chunk.startsWith("BATCH_INFO:")) {
    String batchInfoJson = chunk.substring("BATCH_INFO:".length()).trim();
    emitter.send(SseEmitter.event()
        .name("message")
        .data("{\"type\":\"batchInfo\"," + batchInfoJson.substring(1)));
    return;
}
```

**SSE 消息**：
```
event:message
data:{"type":"batchInfo","totalBatches":3,"totalPages":10}
```

### 2. 前端改动

#### 2.1 处理批次信息
```javascript
// TextExtractionConfig.jsx
else if (data.type === 'batchInfo') {
    console.log('📦 收到批次信息:', data)
    setBatchInfo({
        totalBatches: data.totalBatches,
        totalPages: data.totalPages,
    })
}
```

#### 2.2 Markdown 渲染
```javascript
import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'

// 预览模式
<ReactMarkdown remarkPlugins={[remarkGfm]}>
    {extractionResult}
</ReactMarkdown>
```

**支持的 Markdown 语法**：
- ✅ 标题（`#`, `##`, `###`）
- ✅ 列表（`-`, `*`, `1.`）
- ✅ 代码块（`` ``` ``）
- ✅ 引用（`>`）
- ✅ 表格（GFM）
- ✅ 水平线（`---`）
- ✅ 粗体/斜体（`**`, `*`）

#### 2.3 预览/源码切换
```javascript
const [activeTab, setActiveTab] = useState('preview')

<Space>
    <Button onClick={() => setActiveTab('preview')}>预览</Button>
    <Button onClick={() => setActiveTab('source')}>源码</Button>
</Space>

{activeTab === 'preview' ? (
    <ReactMarkdown>{extractionResult}</ReactMarkdown>
) : (
    <TextArea value={extractionResult} onChange={...} />
)}
```

**操作**：
- 点击"预览"：查看渲染后的 Markdown
- 点击"源码"：编辑 Markdown 源码（可修改）

#### 2.4 显示批次信息
```javascript
{batchInfo && (
    <Tag color="purple">
        {batchInfo.totalPages} 页 / {batchInfo.totalBatches} 批
    </Tag>
)}
```

### 3. Markdown 样式优化

#### 3.1 GitHub 风格样式
```css
.markdown-preview h2 {
    font-size: 1.5em;
    border-bottom: 1px solid #eaecef;
    padding-bottom: 0.3em;
}

.markdown-preview code {
    background-color: rgba(27, 31, 35, 0.05);
    border-radius: 3px;
}

.markdown-preview pre {
    background-color: #f6f8fa;
    border-radius: 6px;
}
```

---

## 📊 效果对比

### 修复前
```
并行处理 → 内容混乱：
=== 页面 5 ===  === 页面 1 === ### 幻### 幻灯灯片片

前端无法预知批次数量
无法编辑 Markdown 源码
Markdown 渲染错误
```

### 修复后
```
串行处理 → 内容有序：

---

## 📄 页面 1

### 幻灯片文字内容

- 项目 1
- 项目 2

---

## 📄 页面 2

...

前端提前知道批次：10 页 / 3 批
可以切换预览/源码模式
Markdown 渲染正确
```

---

## 🔄 工作流程

### 1. 后端处理流程
```
用户点击"开始提取"
    ↓
DocumentProcessingController 接收请求
    ↓
VisionLLMDocumentProcessor.process()
    ↓
智能分批（smartBatching）
    ↓
检测流式模式 → 是
    ↓
发送批次信息：BATCH_INFO:{...}
    ↓
串行处理每个批次
    ↓
每页完成时发送：
    "---\n\n## 📄 页面 N\n\n{内容}"
    ↓
全部完成 → complete
```

### 2. 前端处理流程
```
接收 SSE 消息
    ↓
type === 'batchInfo'
    → 保存批次信息，显示标签
    ↓
type === 'content'
    → 累加到 extractionResult
    → 实时渲染（预览模式）或显示源码
    ↓
type === 'complete'
    → 提取完成，可编辑/保存
```

---

## 🎨 UI 效果

### 标题栏
```
┌─────────────────────────────────────────────────────────┐
│ 📄 提取结果  [123 字符]  [精度: 95.3%]  [10 页 / 3 批] │
│                                    [预览] [源码]        │
└─────────────────────────────────────────────────────────┘
```

### 预览模式
```
┌─────────────────────────────────────────────┐
│                                             │
│ ─────────────────────────────────────      │
│                                             │
│ ## 📄 页面 1                                │
│                                             │
│ ### 幻灯片标题                               │
│                                             │
│ 这是幻灯片的内容...                          │
│                                             │
│ ─────────────────────────────────────      │
│                                             │
│ ## 📄 页面 2                                │
│                                             │
│ ...                                         │
└─────────────────────────────────────────────┘
```

### 源码模式
```
┌─────────────────────────────────────────────┐
│ ---                                         │
│                                             │
│ ## 📄 页面 1                                │
│                                             │
│ ### 幻灯片标题                               │
│                                             │
│ 这是幻灯片的内容...                          │
│                                             │
│ ---                                         │
│                                             │
│ ## 📄 页面 2                                │
│ ...                                         │
└─────────────────────────────────────────────┘
```

---

## 🚀 使用指南

### 1. 启动应用
```bash
# 后端已编译
cd D:\Jetbrains\omni-agent
mvn spring-boot:run

# 前端
cd UI
npm run dev
```

### 2. 测试流式提取
1. 上传测试文档（PPT）
2. 进入"文本提取"
3. 选择"Vision LLM"模型
4. 确保"流式"开关打开
5. 点击"开始提取"

### 3. 观察效果
- **批次信息**：右上角显示 "10 页 / 3 批"
- **实时输出**：内容逐页显示
- **Markdown 渲染**：自动渲染标题、列表等
- **预览/源码切换**：点击切换查看模式

### 4. 编辑源码
1. 点击"源码"按钮
2. 直接编辑 Markdown 文本
3. 点击"预览"查看渲染效果
4. 保存（保存到数据库）

---

## 📝 关键代码位置

### 后端
1. **批次信息发送**：`VisionLLMDocumentProcessor.java:148-159`
2. **串行处理强制**：`VisionLLMDocumentProcessor.java:161-173`
3. **Markdown 页面标记**：`VisionLLMDocumentProcessor.java:1061-1064`
4. **Controller 批次识别**：`DocumentProcessingController.java:143-153`

### 前端
1. **批次信息处理**：`TextExtractionConfig.jsx:213-220`
2. **Markdown 渲染**：`TextExtractionConfig.jsx:436-442`
3. **预览/源码切换**：`TextExtractionConfig.jsx:425-435`
4. **Markdown 样式**：`TextExtractionConfig.css:77-169`

---

## ✅ 验证清单

- [x] 后端编译通过
- [x] 流式模式强制串行
- [x] 批次信息正确发送
- [x] Markdown 格式完整
- [x] 前端 Markdown 渲染
- [x] 预览/源码切换
- [x] 批次信息显示
- [x] 样式美观

---

## 🔧 未来改进方向

1. **批次级别的分区显示**
   - 每个批次一个可折叠面板
   - 批次完成后才展开下一批

2. **实时保存**
   - 编辑源码后自动保存
   - 本地存储草稿

3. **导出功能**
   - 导出为 Markdown 文件
   - 导出为 PDF/HTML

4. **语法高亮**
   - 源码模式支持语法高亮
   - 代码块高亮渲染

---

**更新时间**：2025-12-24  
**版本**：v3.0 - Markdown 流式提取  
**作者**：OmniAgent Team

