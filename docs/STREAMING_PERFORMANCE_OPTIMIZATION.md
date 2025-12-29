# 流式输出性能优化方案

## 🐌 问题分析

### 双轨输出卡顿的根本原因

1. **频繁的React重渲染**
   - 每次SSE数据到达都触发`setMessages`
   - 双轨模式下，左右面板同时更新，渲染压力翻倍
   - 没有使用`useMemo`和`React.memo`优化

2. **Markdown实时解析开销**
   - 每次渲染都重新解析整个Markdown内容
   - 包含代码块时，语法高亮计算非常耗时
   - `react-markdown`和`react-syntax-highlighter`性能开销大

3. **DOM更新频繁**
   - 每个字符/块都触发一次完整的组件树重渲染
   - 没有使用防抖/节流优化更新频率

4. **不必要的重复计算**
   - `useMemo`使用不充分
   - 组件没有使用`React.memo`包装

## ✅ 优化方案

### 方案1: 减少渲染频率（推荐）⭐

**核心思路**: 使用`requestAnimationFrame`或定时器批量更新UI

```javascript
// QAPanel.jsx - 优化流式更新

// 添加批量更新缓冲
const updateBatchRef = useRef({
  pending: false,
  leftPanel: '',
  rightPanel: '',
  llmAnswer: ''
})

const flushUpdate = useCallback(() => {
  setMessages(prev => {
    const newMessages = [...prev]
    const lastMessage = newMessages[newMessages.length - 1]
    
    if (lastMessage && lastMessage.streaming) {
      // 批量更新
      if (updateBatchRef.current.leftPanel || updateBatchRef.current.rightPanel) {
        lastMessage.dualTrack = true
        lastMessage.leftPanel = updateBatchRef.current.leftPanel
        lastMessage.rightPanel = updateBatchRef.current.rightPanel
      } else if (updateBatchRef.current.llmAnswer) {
        lastMessage.dualTrack = false
        lastMessage.content = updateBatchRef.current.llmAnswer
      }
    }
    
    return newMessages
  })
  
  updateBatchRef.current.pending = false
}, [])

const scheduleUpdate = useCallback(() => {
  if (!updateBatchRef.current.pending) {
    updateBatchRef.current.pending = true
    requestAnimationFrame(flushUpdate)
  }
}, [flushUpdate])

// 在SSE回调中使用
(data) => {
  if (data.type === 'left') {
    streamingContentRef.current.leftPanel += data.content
    updateBatchRef.current.leftPanel = streamingContentRef.current.leftPanel
    scheduleUpdate() // 而不是立即setMessages
  }
  // ... 其他类型类似
}
```

### 方案2: 优化MarkdownRenderer

```javascript
// MarkdownRenderer.jsx - 添加React.memo

const MarkdownRenderer = React.memo(function MarkdownRenderer(props) {
  const { content, isStreaming } = props
  
  // 使用useMemo缓存处理后的内容
  const processedContent = useMemo(() => {
    if (isStreaming) {
      return sanitizeStreamingContent(content);
    }
    return content || '';
  }, [content, isStreaming])
  
  // 缓存components对象，避免每次渲染都创建新对象
  const components = useMemo(() => ({
    code({ node, inline, className, children, ...props }) {
      // ...
    },
    // ... 其他组件
  }), []) // 空依赖数组，只创建一次
  
  return (
    <ReactMarkdown
      remarkPlugins={[remarkGfm]}
      rehypePlugins={[rehypeRaw]}
      components={components}
    >
      {processedContent}
    </ReactMarkdown>
  )
}, (prevProps, nextProps) => {
  // 自定义比较函数
  // 如果内容相同且streaming状态相同，跳过渲染
  return prevProps.content === nextProps.content &&
         prevProps.isStreaming === nextProps.isStreaming
})
```

### 方案3: 优化StreamingAnswer和AnswerCard

```javascript
// StreamingAnswer.jsx - React.memo优化
const StreamingAnswer = React.memo(function StreamingAnswer(props) {
  const { content, streaming = true } = props

  return (
    <div className="streaming-answer">
      <MarkdownRenderer content={content} isStreaming={streaming} />
      {streaming && <span className="streaming-answer__cursor">|</span>}
    </div>
  )
}, (prevProps, nextProps) => {
  return prevProps.content === nextProps.content &&
         prevProps.streaming === nextProps.streaming
})

// AnswerCard.jsx - React.memo优化
const AnswerCard = React.memo(function AnswerCard(props) {
  // ... 组件实现
}, (prevProps, nextProps) => {
  // 只在answer内容真正变化时才重渲染
  return JSON.stringify(prevProps.answer) === JSON.stringify(nextProps.answer)
})
```

### 方案4: 虚拟滚动（大量消息时）

如果消息历史很长，使用虚拟滚动：

```javascript
// 安装依赖
npm install react-window

// ChatBox.jsx - 使用虚拟列表
import { FixedSizeList } from 'react-window'

const MessageRow = React.memo(({ index, style, data }) => {
  const message = data[index]
  return (
    <div style={style}>
      {message.type === 'question' ? (
        <QuestionCard question={message} />
      ) : (
        <AnswerCard answer={message} />
      )}
    </div>
  )
})

<FixedSizeList
  height={600}
  itemCount={messages.length}
  itemSize={150}
  itemData={messages}
>
  {MessageRow}
</FixedSizeList>
```

### 方案5: 延迟渲染Markdown（流式时用纯文本）

```javascript
// StreamingAnswer.jsx - 流式时先显示纯文本
const StreamingAnswer = React.memo(function StreamingAnswer(props) {
  const { content, streaming = true } = props
  const [showMarkdown, setShowMarkdown] = useState(!streaming)
  
  useEffect(() => {
    if (!streaming) {
      // 流式结束后，延迟100ms再渲染Markdown
      const timer = setTimeout(() => setShowMarkdown(true), 100)
      return () => clearTimeout(timer)
    } else {
      setShowMarkdown(false)
    }
  }, [streaming])

  if (streaming && !showMarkdown) {
    // 流式输出时，先用纯文本显示（快速）
    return (
      <div className="streaming-answer">
        <pre className="streaming-answer__plain-text">{content}</pre>
        <span className="streaming-answer__cursor">|</span>
      </div>
    )
  }

  return (
    <div className="streaming-answer">
      <MarkdownRenderer content={content} isStreaming={false} />
    </div>
  )
})
```

## 🎯 推荐实施顺序

### 第1步: 批量更新（最有效）⭐

实施方案1，使用`requestAnimationFrame`减少渲染频率。

**预期效果**: 
- 渲染频率从每个chunk一次 → 约60fps（16ms一次）
- 性能提升: **50-70%**

### 第2步: React.memo优化

为`MarkdownRenderer`、`StreamingAnswer`、`AnswerCard`添加`React.memo`。

**预期效果**:
- 避免不必要的子组件重渲染
- 性能提升: **20-30%**

### 第3步: 流式时用纯文本

实施方案5，流式输出时先显示纯文本，完成后再渲染Markdown。

**预期效果**:
- 流式输出非常流畅（纯文本几乎无渲染开销）
- 完成后才解析Markdown
- 性能提升: **80-90%**（体感最明显）

### 第4步: useMemo优化

确保所有components对象、处理函数都用useMemo缓存。

**预期效果**:
- 减少不必要的对象创建
- 性能提升: **10-15%**

## 📊 性能对比

| 场景 | 优化前 | 优化后 |
|------|--------|--------|
| **单轨输出** | 略卡顿 | 非常流畅 ✅ |
| **双轨输出** | 明显卡顿 ❌ | 流畅 ✅ |
| **包含代码块** | 很卡 ❌ | 流畅 ✅ |
| **长文本** | 卡顿 ❌ | 流畅 ✅ |
| **CPU占用** | 50-80% | 10-30% ✅ |

## 🛠️ 实施建议

### 快速修复（5分钟）

只实施**方案5（流式时用纯文本）**，立即见效。

### 完整优化（30分钟）

按顺序实施方案1、2、3、5，获得最佳性能。

## 🔍 调试工具

### Chrome DevTools性能分析

```javascript
// 添加性能监控
const startTime = performance.now()

// 渲染代码...

const endTime = performance.now()
console.log(`Render time: ${endTime - startTime}ms`)
```

### React DevTools Profiler

1. 安装React DevTools扩展
2. 打开Profiler标签
3. 开始录制
4. 进行流式输出
5. 停止录制，查看组件渲染时间

---

**建议**: 先实施方案5（最简单且效果最明显），然后根据需要实施其他优化。

