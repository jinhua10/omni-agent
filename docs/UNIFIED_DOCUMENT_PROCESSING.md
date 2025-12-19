# 📄 统一文档处理流程 - 全文档类型支持

**更新时间**: 2025-12-19  
**状态**: ✅ 已完成  
**版本**: v2.0

---

## 🎯 概述

现在**所有文档类型**（PPT、PDF、Word、Excel等）都使用**统一的处理流程**：

```
文档上传
  ↓
1. DocumentProcessorManager 处理（智能识别文档类型）
  ↓
2. ChunkingStrategyManager 分块（自动选择最佳策略）
  ↓
3. 逐块索引到 RAG
```

---

## 📦 支持的文档类型

### 1. Office 文档 ✅

| 文件类型 | 扩展名 | 处理器 | 说明 |
|---------|--------|--------|------|
| **PowerPoint** | .ppt, .pptx | VisionLLMProcessor | 页面级图片识别 |
| **Word** | .doc, .docx | VisionLLMProcessor | 图片+文本提取 |
| **Excel** | .xls, .xlsx | VisionLLMProcessor | 表格识别 |
| **PDF** | .pdf | VisionLLMProcessor | 页面级图片识别 |

**处理特点**:
- 🔍 使用 Vision LLM 识别图片内容
- 📄 以页面/幻灯片为单位处理
- 🎯 保持布局和位置信息
- 📊 流程图、架构图完整识别

### 2. 文本文档 ✅

| 文件类型 | 扩展名 | 处理器 | 说明 |
|---------|--------|--------|------|
| **文本** | .txt | PlainTextProcessor | 直接读取 |
| **Markdown** | .md, .markdown | PlainTextProcessor | 保留格式 |
| **日志** | .log | PlainTextProcessor | 纯文本处理 |

### 3. 代码文件 ✅

| 文件类型 | 扩展名 | 处理器 | 说明 |
|---------|--------|--------|------|
| **Java** | .java | PlainTextProcessor | 语义分块 |
| **Python** | .py | PlainTextProcessor | 语义分块 |
| **JavaScript** | .js, .ts | PlainTextProcessor | 语义分块 |
| **Go** | .go | PlainTextProcessor | 语义分块 |
| **Rust** | .rs | PlainTextProcessor | 语义分块 |
| **C/C++** | .c, .cpp, .h | PlainTextProcessor | 语义分块 |
| **C#** | .cs | PlainTextProcessor | 语义分块 |
| **其他** | 40+ 种 | PlainTextProcessor | 完整支持 |

### 4. 配置文件 ✅

| 文件类型 | 扩展名 | 处理器 | 说明 |
|---------|--------|--------|------|
| **YAML** | .yml, .yaml | PlainTextProcessor | 结构化处理 |
| **JSON** | .json | PlainTextProcessor | 结构化处理 |
| **XML** | .xml | PlainTextProcessor | 结构化处理 |
| **Properties** | .properties | PlainTextProcessor | 键值对 |
| **INI** | .ini, .conf | PlainTextProcessor | 配置解析 |

### 5. Web 文件 ✅

| 文件类型 | 扩展名 | 处理器 | 说明 |
|---------|--------|--------|------|
| **HTML** | .html, .htm | PlainTextProcessor | 提取文本 |
| **CSS** | .css, .scss, .sass | PlainTextProcessor | 样式代码 |

### 6. 图片文件 ✅

| 文件类型 | 扩展名 | 处理器 | 说明 |
|---------|--------|--------|------|
| **图片** | .png, .jpg, .jpeg, .bmp, .tiff, .gif | VisionLLMProcessor | OCR + 图像理解 |

### 7. 媒体文件 🚧 (未来支持)

| 文件类型 | 扩展名 | 处理器 | 说明 |
|---------|--------|--------|------|
| **视频** | .mp4, .avi, .mov | MediaFileProcessor | 字幕提取 + 关键帧 |
| **音频** | .mp3, .wav, .aac | MediaFileProcessor | 语音转文字 |
| **字幕** | .srt, .ass, .vtt | MediaFileProcessor | 直接提取 |

---

## 🔄 统一处理流程

### 单个文件上传 (`/api/documents/upload`)

```java
// 1. 文档处理
DocumentProcessor.ProcessingContext context = DocumentProcessor.ProcessingContext.builder()
    .fileBytes(file.getBytes())
    .fileExtension("pdf")  // 或 "pptx", "docx", "xlsx"
    .originalFileName("document.pdf")
    .fileSize(file.getSize())
    .build();

DocumentProcessor.ProcessingResult result = 
    documentProcessorManager.processDocument(context);

String content = result.getContent();

// 2. 智能分块
List<Chunk> chunks = chunkingStrategyManager.chunkWithAutoStrategy(
    documentId, content, filename);

// 3. 逐块索引
for (Chunk chunk : chunks) {
    ragService.indexDocument(createDocument(chunk));
}
```

### 批量文件上传 (`/api/documents/upload-batch`)

**相同的流程应用到每个文件**:
- ✅ PPT 文件 → Vision LLM 识别
- ✅ PDF 文件 → Vision LLM 识别
- ✅ Word 文件 → Vision LLM 识别
- ✅ Excel 文件 → Vision LLM 识别
- ✅ 代码文件 → 纯文本提取
- ✅ 所有文件都使用智能分块

---

## 📊 处理策略对比

### Office 文档处理

#### PPT (PowerPoint)
```
presentation.pptx (10页，每页3张图片)
  ↓
VisionLLMProcessor
  ├─ 提取10页幻灯片
  ├─ 每页3张图片组合分析
  ├─ Vision LLM 识别内容
  └─ 生成完整文本 (5678 chars)
  ↓
ChunkingStrategyManager
  ├─ 文档类型: GENERAL
  ├─ 选择策略: PPL
  └─ 生成15个块
  ↓
索引15个文档块
```

#### PDF (文档)
```
report.pdf (20页)
  ↓
VisionLLMProcessor
  ├─ 提取20页
  ├─ 每页转为图片
  ├─ Vision LLM 识别
  └─ 生成完整文本
  ↓
ChunkingStrategyManager
  ├─ 文档类型: GENERAL
  ├─ 选择策略: Paragraph
  └─ 生成30个块
  ↓
索引30个文档块
```

#### Word (文档)
```
document.docx (10页)
  ↓
VisionLLMProcessor
  ├─ 提取文本
  ├─ 提取图片
  ├─ Vision LLM 识别图片
  └─ 组合文本+图片内容
  ↓
ChunkingStrategyManager
  ├─ 选择策略: Paragraph
  └─ 生成20个块
  ↓
索引20个文档块
```

#### Excel (表格)
```
data.xlsx (5个工作表)
  ↓
VisionLLMProcessor
  ├─ 提取每个工作表
  ├─ 表格转图片
  ├─ Vision LLM 识别
  └─ 提取结构化数据
  ↓
ChunkingStrategyManager
  ├─ 选择策略: Fixed Size
  └─ 生成10个块
  ↓
索引10个文档块
```

### 代码文件处理

```
Application.java (500行)
  ↓
PlainTextProcessor
  └─ 直接读取文本
  ↓
ChunkingStrategyManager
  ├─ 文档类型: CODE
  ├─ 选择策略: Semantic
  └─ 按函数/类分块
  ↓
索引8个代码块
```

---

## 🎯 分块策略自动选择

### 策略选择规则

| 文档类型 | 自动选择的策略 | 原因 |
|---------|--------------|------|
| **PPT/PDF** | PPL (如果配置) | PPL 智能分割点识别 |
| **代码文件** | Semantic | 保持代码完整性 |
| **长文章** | Paragraph | 按段落自然分块 |
| **技术文档** | Semantic | 保持语义完整 |
| **其他** | Fixed Size | 兜底策略 |

### 策略优先级

```
1. PPL (priority=10) - 最高
   条件: ppl-onnx.enabled=true

2. Semantic (priority=20)
   条件: 代码文件、技术文档

3. Paragraph (priority=30)
   条件: 通用文档、长文章

4. Fixed Size (priority=50) - 兜底
   条件: 其他所有情况
```

---

## 📝 API 使用示例

### 上传 PDF 文件

```bash
curl -X POST http://localhost:8080/api/documents/upload \
  -F "file=@report.pdf" \
  -F "autoIndex=true"
```

**后台日志**:
```
[INFO] 上传文档: filename=report.pdf, size=2458624 bytes
[INFO] 🔄 使用 DocumentProcessorManager 处理文档...
[INFO] 🔍 [VisionLLM] 开始处理文档: report.pdf
[INFO] 📄 [VisionLLM] 提取了 20 个页面
[INFO] ✅ 文档处理成功: processor=VisionLLMProcessor, 内容长度=12345 chars
[INFO] 📦 使用 ChunkingStrategyManager 进行分块...
[INFO] ✅ 分块完成: 共 30 个块, 策略: paragraph
[INFO] ✅ 索引完成: 共索引 30 个文档块
```

### 上传 Word 文件

```bash
curl -X POST http://localhost:8080/api/documents/upload \
  -F "file=@document.docx" \
  -F "autoIndex=true"
```

**后台日志**:
```
[INFO] 上传文档: filename=document.docx, size=1234567 bytes
[INFO] 🔄 使用 DocumentProcessorManager 处理文档...
[INFO] ✅ 文档处理成功: processor=VisionLLMProcessor, 内容长度=8901 chars
[INFO] 📦 使用 ChunkingStrategyManager 进行分块...
[INFO] ✅ 分块完成: 共 20 个块, 策略: paragraph
[INFO] ✅ 索引完成: 共索引 20 个文档块
```

### 上传 Excel 文件

```bash
curl -X POST http://localhost:8080/api/documents/upload \
  -F "file=@data.xlsx" \
  -F "autoIndex=true"
```

**后台日志**:
```
[INFO] 上传文档: filename=data.xlsx, size=567890 bytes
[INFO] 🔄 使用 DocumentProcessorManager 处理文档...
[INFO] ✅ 文档处理成功: processor=VisionLLMProcessor, 内容长度=6789 chars
[INFO] 📦 使用 ChunkingStrategyManager 进行分块...
[INFO] ✅ 分块完成: 共 10 个块, 策略: fixed_size
[INFO] ✅ 索引完成: 共索引 10 个文档块
```

### 批量上传多种文档

```bash
curl -X POST http://localhost:8080/api/documents/upload-batch \
  -F "files=@presentation.pptx" \
  -F "files=@report.pdf" \
  -F "files=@data.xlsx" \
  -F "files=@code.java" \
  -F "autoIndex=true"
```

**返回**:
```json
{
  "success": true,
  "message": "批量上传完成: 成功 4, 失败 0",
  "successCount": 4,
  "failureCount": 0,
  "results": [
    {
      "success": true,
      "message": "上传成功，已分块并索引（15 个块）",
      "fileName": "presentation.pptx",
      "documentId": "doc_1734589234567_presentation_pptx",
      "fileSize": 2458624
    },
    {
      "success": true,
      "message": "上传成功，已分块并索引（30 个块）",
      "fileName": "report.pdf",
      "documentId": "doc_1734589234568_report_pdf",
      "fileSize": 3456789
    },
    {
      "success": true,
      "message": "上传成功，已分块并索引（10 个块）",
      "fileName": "data.xlsx",
      "documentId": "doc_1734589234569_data_xlsx",
      "fileSize": 567890
    },
    {
      "success": true,
      "message": "上传成功，已分块并索引（8 个块）",
      "fileName": "code.java",
      "documentId": "doc_1734589234570_code_java",
      "fileSize": 12345
    }
  ]
}
```

---

## 🔧 配置

### application.yml

```yaml
omni-agent:
  # Vision LLM 配置（用于 Office 文档和图片）
  vision-llm:
    enabled: true
    model: qwen-vl-plus
    api-key: ${QW_API_KEY}
    batch-size: 3  # 一次处理3页
    
  # PPL 分块配置
  ppl-onnx:
    enabled: true
    model-path: models/ppl-model.onnx
    threshold: 0.5
```

---

## 📊 性能对比

### 处理效果对比

| 文档类型 | 旧流程 | 新流程 | 改进 |
|---------|--------|--------|------|
| **PPT** | 简单文本提取 | Vision LLM识别 | ✅ 图片内容完整 |
| **PDF** | 文本提取 | 页面级识别 | ✅ 布局保留 |
| **Word** | 文本+图片分离 | 组合分析 | ✅ 上下文完整 |
| **Excel** | 表格文本 | 结构化识别 | ✅ 数据关系 |
| **代码** | 整文档 | 语义分块 | ✅ 函数完整 |

### 索引粒度对比

| 文档 | 旧流程 | 新流程 | 检索效果 |
|------|--------|--------|---------|
| **10页PPT** | 1个大文档 | 15个块 | ✅ 精准检索 |
| **20页PDF** | 1个大文档 | 30个块 | ✅ 段落级检索 |
| **Word文档** | 1个大文档 | 20个块 | ✅ 章节级检索 |
| **Excel表** | 1个大文档 | 10个块 | ✅ 表格级检索 |

---

## ✅ 验证清单

- [x] PPT 文件使用统一流程
- [x] PDF 文件使用统一流程
- [x] Word 文件使用统一流程
- [x] Excel 文件使用统一流程
- [x] 代码文件使用统一流程
- [x] 配置文件使用统一流程
- [x] 图片文件使用统一流程
- [x] 单文件上传集成 ✅
- [x] 批量上传集成 ✅
- [x] 编译通过 ✅
- [x] 降级机制完善 ✅

---

## 🎉 总结

### 核心改进

1. ✅ **统一处理**: 所有文档类型使用相同流程
2. ✅ **智能识别**: 自动选择最佳处理器和分块策略
3. ✅ **完整内容**: Vision LLM 识别图片、表格、流程图
4. ✅ **精准检索**: 块级索引，检索更准确
5. ✅ **优雅降级**: 多层降级保证稳定性

### 支持的文档

- 📄 **Office**: PPT, Word, Excel, PDF
- 📝 **文本**: txt, md, log
- 💻 **代码**: 40+ 种编程语言
- ⚙️ **配置**: yml, json, xml, properties
- 🌐 **Web**: html, css
- 🖼️ **图片**: png, jpg, gif, etc.
- 🎬 **媒体**: 视频/音频（未来）

### 用户价值

- 🎯 **一致体验**: 所有文档处理方式一致
- 📈 **高质量**: Vision LLM 提供准确识别
- ⚡ **高效率**: 智能分块优化检索
- 🛡️ **高可靠**: 多层降级保证成功

---

**更新完成时间**: 2025-12-19  
**状态**: ✅ 生产就绪  
**覆盖范围**: PPT, PDF, Word, Excel, 代码, 配置, 图片等所有文档类型

🎉 **所有文档类型现在都使用统一的智能处理流程！** 📄✨

