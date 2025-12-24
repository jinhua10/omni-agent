# 文档提取流式/非流式输出功能

## 📋 功能概述

为文档管理的文本提取功能增加了"流式/非流式"输出模式开关，用户可以根据文档类型和个人偏好选择合适的输出方式。

## ✨ 核心特性

### 1. 流式输出模式（默认）
- **实时显示**：边提取边显示内容，大幅减少等待时间
- **适用场景**：PPT、PDF 等大文档，复杂的 Vision LLM 处理
- **用户体验**：可以看到提取进度，内容逐步增长
- **技术实现**：使用 SSE（Server-Sent Events）+ Flux 流式推送

### 2. 批量输出模式
- **批次显示**：按页面/批次完成后显示内容
- **适用场景**：TXT、Markdown 等小文档，快速处理
- **用户体验**：等待时间短，整体显示更清晰
- **技术实现**：完成一页就发送一次，而不是等全部完成

## 🎯 关键改动

### 前端改动（UI）

#### 1. TextExtractionConfig.jsx
```javascript
// 新增状态
const [streamingMode, setStreamingMode] = useState(true) // 流式/非流式开关

// UI 组件
<Switch
  checked={streamingMode}
  onChange={setStreamingMode}
  disabled={extracting}
  checkedChildren="流式"
  unCheckedChildren="批量"
/>
```

**位置**：在模型选择器下方，开始提取按钮上方

**说明文案**：
- 流式：💡 边提取边显示，减少等待时间，适合PPT、PDF等大文档
- 批量：💡 提取完成后统一显示，适合TXT、Markdown等小文档

#### 2. handleAutoExtract 函数改动
```javascript
body: JSON.stringify({
  model: selectedModel,
  streaming: streamingMode  // ⭐ 使用开关控制
})
```

### 后端改动（Java）

#### 1. VisionLLMDocumentProcessor.java

**核心逻辑**：
- 流式模式：在 `recognizePageWithVisionLLM` 方法内通过 `chatWithVisionFlux` 实时推送每个 token
- 非流式模式：在 `processPageBatch` 方法内每页完成后通过回调发送完整页面内容

```java
// 非流式模式下，每页处理完也立即通过回调发送（分批显示）
if (!isStreaming) {
    Object cb = ctx.getOptions().get("streamCallback");
    if (cb instanceof java.util.function.Consumer) {
        callback.accept("\n=== 页面 " + page.getPageNumber() + " ===\n");
        callback.accept(pageContent);
    }
}
```

#### 2. DocumentProcessingController.java

**改动点**：
```java
if (Boolean.TRUE.equals(request.getStreaming())) {
    // 流式模式：实时推送
    extractedText = extractTextWithProcessorStreaming(...);
} else {
    // 非流式模式：批次推送
    extractedText = extractTextWithProcessor(...);
}
```

## 🔄 工作流程

### 流式模式工作流
```
用户点击"开始提取" (streaming=true)
    ↓
后端启动处理，逐页调用 Vision LLM
    ↓
每个 token 生成 → 立即通过 SSE 推送到前端
    ↓
前端实时累加显示（用户看到内容逐字增长）
    ↓
全部完成 → 保存到数据库
```

### 批量模式工作流
```
用户点击"开始提取" (streaming=false)
    ↓
后端启动处理，逐页调用 Vision LLM
    ↓
每页完成 → 通过 SSE 推送该页完整内容
    ↓
前端按页显示（用户看到内容逐页增长）
    ↓
全部完成 → 保存到数据库
```

## 📊 对比分析

| 特性 | 流式模式 | 批量模式 |
|------|---------|---------|
| 显示速度 | ⚡ 实时显示，秒级响应 | 🐢 按页显示，需等待单页完成 |
| 适用文档 | 大文档（PPT、PDF、长文） | 小文档（TXT、MD、短文） |
| 网络流量 | 持续小包 | 分批中包 |
| 后端负载 | 均匀分布 | 突发性 |
| 用户体验 | 🌟🌟🌟🌟🌟 | 🌟🌟🌟🌟 |
| 技术复杂度 | 较高（Flux + SSE） | 中等（SSE） |

## 🎨 UI 设计

### 开关位置
```
┌─────────────────────────────────┐
│  模型选择：Vision LLM           │
├─────────────────────────────────┤
│  ⚡ 流式输出    [流式 | 批量]   │
│  💡 边提取边显示，适合大文档     │
├─────────────────────────────────┤
│  [开始提取]  [重置]  [返回]     │
└─────────────────────────────────┘
```

### 颜色方案
- 流式开关背景：渐变蓝色 (`#f0f9ff` → `#e0f2fe`)
- 边框：天蓝色 (`#bae6fd`)
- Hover 效果：亮蓝色边框 + 阴影

## 🔧 技术实现细节

### 1. 分批处理逻辑
无论流式还是非流式，后端都会智能分批：
- 根据 `VisionLLMBatchProcessingProperties` 配置
- 默认每批 5 页（可配置）
- 支持并行处理多个批次

### 2. 回调机制
通过 `ProcessingContext.options` 传递：
```java
options.put("streaming", true/false);
options.put("streamCallback", Consumer<String>);
```

### 3. ThreadLocal 支持并行
```java
private final ThreadLocal<ProcessingContext> processingContextThreadLocal = new ThreadLocal<>();
```
确保并行批次也能访问到回调。

## 📈 性能优化

### 流式模式优化
1. **减少等待时间**：首 token 延迟从分钟级降到秒级
2. **用户留存**：避免用户因长时间无响应而离开
3. **内存友好**：不需要在内存中累积大量内容

### 批量模式优化
1. **减少网络包数**：每页一次推送，比流式少
2. **降低渲染压力**：前端不需要频繁更新
3. **适合小文档**：快速完成，整体显示

## 🐛 已知问题和解决方案

### 问题1：并行批次导致输出乱序
**现象**：流式模式下，多个批次并行处理时，页面输出可能交错

**解决方案**：
- 当 `streaming=true` 时，强制使用串行处理（`processPageBatchesSequentially`）
- 或者在前端按页码排序后显示

### 问题2：非流式模式仍然很慢
**现象**：批量模式下，仍需等待每页 Vision LLM 完成

**解决方案**：
- 这是 Vision LLM 本身的推理时间，无法完全避免
- 建议用户使用流式模式获得更好体验

## 🚀 未来改进方向

1. **智能模式切换**：根据文档大小自动推荐模式
2. **进度条优化**：显示当前处理到第几页/共几页
3. **暂停/恢复**：支持暂停提取，稍后继续
4. **优先级队列**：多文档提取时支持调整优先级

## 📝 使用示例

### 用户操作流程
1. 上传文档到文档管理
2. 点击"文本提取"
3. 选择提取模型（如 Vision LLM）
4. **新功能**：切换"流式/批量"开关
   - 大文档（PPT、PDF）→ 选择"流式"
   - 小文档（TXT、MD）→ 选择"批量"
5. 点击"开始提取"
6. 实时查看提取结果

### 开发者配置
```yaml
# application.yml
omni-agent:
  vision-llm:
    enabled: true
    model: qwen-vl-plus
    batch-processing:
      enabled: true
      max-batch-size: 5
      max-slides-per-batch: 10
```

## ✅ 测试清单

- [x] 流式模式 + 小文档（1-2页）
- [x] 流式模式 + 大文档（10+页）
- [x] 批量模式 + 小文档
- [x] 批量模式 + 大文档
- [x] 切换模式后重新提取
- [x] 提取过程中切换模式（应禁用）
- [x] 网络中断恢复
- [x] 并发多文档提取

## 📞 联系方式

如有问题或建议，请联系：
- 项目维护者：OmniAgent Team
- 文档更新时间：2025-12-24

