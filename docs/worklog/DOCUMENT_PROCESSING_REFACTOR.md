# 📊 Excel 和 Word 文档处理架构重构

## 📅 日期
2025-12-27

## 🎯 目标

解决之前的问题：
- ❌ **错误做法**：将 Excel/Word 转换为图片再调用 Vision LLM
- ✅ **正确做法**：提取文本和表格数据 + 提取图片 + Vision LLM 分析图片 + 嵌入原位置

---

## 🏗️ 新架构设计

### 1. 抽象基类：`AbstractDocumentProcessor`

**核心流程**：
```
1. extractContent()    // 提取文档内容（文本块 + 图片块）
   ↓
2. processImages()     // 使用 Vision LLM 分析图片
   ↓
3. mergeContent()      // 将图片描述嵌入到原文本位置
   ↓
4. collectImages()     // 收集所有图片（用于保存）
```

**核心数据结构**：
```java
// 内容块（文本或图片）
class ContentBlock {
    String text;              // 文本内容
    List<ExtractedImage> images;  // 图片列表
    int position;             // 位置序号（用于排序）
}

// 提取的内容
class ExtractedContent {
    List<ContentBlock> blocks;    // 按位置排序的内容块
    Map<String, Object> metadata; // 元数据
}
```

---

### 2. Excel 处理器：`ExcelDocumentProcessor`

**处理流程**：
```
Excel 文件
   ↓
读取所有工作表
   ↓
对每个工作表：
   ├─ 提取表格数据 → Markdown 格式
   ├─ 提取内嵌图片 → 记录位置
   └─ 按位置添加到 ContentBlock
   ↓
调用 Vision LLM 分析图片
   ↓
合并：表格 + 图片描述
```

**输出示例**：
```markdown
## 工作表: 销售数据

| 月份 | 销售额 | 增长率 |
| --- | --- | --- |
| 1月 | 100万 | 10% |
| 2月 | 120万 | 20% |

📷 **[图片 - 页码 1]**

这是一个销售趋势图，展示了1-2月的销售增长情况。
横轴为月份，纵轴为销售额，可以看到明显的上升趋势。
```

**优势**：
- ✅ 保留表格结构（Excel 的核心数据）
- ✅ 图片转换为文本描述（可检索）
- ✅ 描述嵌入原位置（上下文完整）

---

### 3. Word 处理器：`WordDocumentProcessor` ✅

**处理流程**：
```
Word 文件
   ↓
读取文档内容
   ↓
对每个段落/表格/图片：
   ├─ 提取段落文本
   │  ├─ 识别标题样式 → Markdown 格式（# 标题）
   │  └─ 记录位置
   ├─ 提取表格 → Markdown 表格格式
   ├─ 提取内嵌图片 → 记录位置
   └─ 按位置添加到 ContentBlock
   ↓
调用 Vision LLM 分析图片
   ↓
合并：文本 + 图片描述
```

**输出示例**：
```markdown
# 产品介绍文档

本产品是一款创新的智能设备...

## 产品特点

- 高性能处理器
- 长续航电池

📷 **[图片 - 页码 1]**

产品外观图：银色金属机身，圆润边角设计，
正面是一块6.5英寸全面屏，背面有三摄像头模组。

## 技术参数

| 参数 | 数值 |
| --- | --- |
| 处理器 | A15 芯片 |
| 内存 | 8GB |
```

**支持特性**：
- ✅ 段落文本提取
- ✅ 标题识别（Heading1-6 → Markdown #）
- ✅ 表格提取（转 Markdown）
- ✅ 图片提取和位置记录
- ✅ .docx 和 .doc 格式支持

---

### 4. PDF 处理器：`PDFDocumentProcessor` ✅

**处理流程**：
```
PDF 文件
   ↓
逐页处理
   ↓
对每一页：
   ├─ 提取页码标记
   ├─ 提取文本内容
   ├─ 提取内嵌图片 → 记录位置
   └─ 按位置添加到 ContentBlock
   ↓
调用 Vision LLM 分析图片
   ↓
合并：文本 + 图片描述
```

**输出示例**：
```markdown
## 第 1 页

产品技术白皮书

本文档介绍了产品的核心技术架构和设计理念...

📷 **[图片 - 页码 1]**

架构图：展示了系统的三层架构，包括前端层、
业务逻辑层和数据持久层，各层之间通过REST API通信。

---

## 第 2 页

性能测试报告

在标准测试环境下，系统的响应时间平均为...

📷 **[图片 - 页码 2]**

性能曲线图：横轴为并发用户数，纵轴为响应时间，
可以看到在500并发时系统仍保持稳定。
```

**支持特性**：
- ✅ 按页提取文本（使用 PDFTextStripper）
- ✅ 提取内嵌图片（PDImageXObject）
- ✅ 保留页码信息
- ✅ 图片格式自动检测
- ✅ 使用 PDFBox 库

---

## 🔧 关键实现

### Vision LLM 图片分析

**提示词模板**：
```
请描述这张图片的内容，包括：
1. 主要内容和对象
2. 图表数据（如果有）
3. 文字信息（如果有）
4. 整体含义和作用

请用简洁的语言描述，便于理解。
```

**调用方式**：
```java
String description = visionAIService.analyzeImages(
    List.of(imageData), 
    prompt
);

// 保存到图片元数据
image.getMetadata().put("visionDescription", description);
```

---

### 内容合并策略

**按位置排序**：
```java
// 所有 ContentBlock 按 position 排序
blocks.sort((a, b) -> Integer.compare(a.getPosition(), b.getPosition()));

// 依次输出
for (ContentBlock block : blocks) {
    if (block.isText()) {
        output.append(block.getText());
    } else if (block.isImage()) {
        output.append("\n\n📷 **[图片]**\n\n");
        output.append(block.getImages().get(0).getMetadata().get("visionDescription"));
        output.append("\n\n");
    }
}
```

---

## 📊 对比：旧 vs 新

### PPT 处理（保持不变）✅
```
PPT → 每页渲染为图片 → Vision LLM 分析
```
**原因**：PPT 本身就是视觉展示，转图片合理

### Excel 处理（已修复）✅

**旧方法** ❌：
```
Excel → 提取内嵌图片 → Vision LLM
（丢失了表格数据！）
```

**新方法** ✅：
```
Excel → 提取表格数据（Markdown）
      → 提取内嵌图片
      → Vision LLM 分析图片
      → 合并（表格 + 图片描述）
```

### Word 处理（已完成）✅

**旧方法** ❌：
```
Word → 提取内嵌图片 → Vision LLM
（丢失了文本内容！）
```

**新方法** ✅：
```
Word → 提取文本段落
     → 提取内嵌图片（记录位置）
     → Vision LLM 分析图片
     → 合并（文本 + 图片描述，按位置）
```

### PDF 处理（已完成）✅

**旧方法** ❌：
```
PDF → 每页渲染为图片 → Vision LLM
（丢失了文本内容！）
```

**新方法** ✅：
```
PDF → 逐页提取文本
    → 提取内嵌图片（记录位置）
    → Vision LLM 分析图片
    → 合并（文本 + 图片描述，按位置）
```

---

## 📁 文件清单

### 新增文件
1. ✅ `AbstractDocumentProcessor.java` - 抽象基类
2. ✅ `ExcelDocumentProcessor.java` - Excel 处理器（已完成）
3. ✅ `WordDocumentProcessor.java` - Word 处理器（已完成）
4. ✅ `PDFDocumentProcessor.java` - PDF 处理器（已完成）

### 修改文件
5. ✅ `VisionLLMDocumentProcessor.java` - 移除 Excel、Word 和 PDF 支持

---

## 🎯 优势总结

### 1. 正确处理数据
- ✅ Excel：保留表格数据（核心价值）
- ✅ Word：保留文本内容（核心价值）
- ✅ 图片：转换为文本描述（可检索）

### 2. 上下文完整
- ✅ 图片描述嵌入原位置
- ✅ 保持文档结构
- ✅ 便于 RAG 检索

### 3. 架构清晰
- ✅ 统一的处理流程（基类）
- ✅ 易于扩展（新文档类型）
- ✅ 职责分离（文本提取 vs 图片分析）

### 4. 性能优化
- ✅ 只对图片调用 Vision LLM
- ✅ 文本直接提取（无需 AI）
- ✅ 减少 AI 调用成本

---

## 📝 下一步

1. ✅ Excel 处理器 - 已完成
2. ✅ Word 处理器 - 已完成
3. ✅ PDF 处理器 - 已完成
4. ⏳ 测试和优化

---

## 📊 处理器优先级总结

| 文档类型 | 处理器 | 优先级 | 处理方式 |
|---------|--------|--------|---------|
| **Excel** (.xls, .xlsx) | ExcelProcessor | 30 | 提取表格 + 图片分析 ✅ |
| **Word** (.doc, .docx) | WordProcessor | 30 | 提取文本 + 图片分析 ✅ |
| **PDF** (.pdf) | PDFProcessor | 30 | 提取文本 + 图片分析 ✅ |
| **PPT** (.ppt, .pptx) | VisionLLMProcessor | 10 | 每页转图片 + 分析 ✅ |
| **图片** (png, jpg...) | VisionLLMProcessor | 10 | 直接分析 ✅ |

**说明**：
- 优先级数字越大，优先级越高
- Excel、Word、PDF 使用专用处理器（优先级 30）
- PPT 和纯图片使用 VisionLLM 处理器（优先级 10）
- 处理器按优先级从低到高排序，优先匹配高优先级

---

**Author**: OmniAgent Team  
**Date**: 2025-12-27  
**Version**: 3.0.0

