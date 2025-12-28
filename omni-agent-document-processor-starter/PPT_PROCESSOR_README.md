# PPTProcessor 实现说明

## 📄 概述

`PPTProcessor` 是专门用于处理 Microsoft PowerPoint 文档（.ppt 和 .pptx）的文档处理器。它继承自 `AbstractDocumentProcessor`，并保留了原有的 Vision LLM 处理逻辑。

## ✨ 主要特性

### 1. 支持的格式
- ✅ **PowerPoint 2007+** (.pptx) - 基于 XML 的新格式
- ✅ **PowerPoint 97-2003** (.ppt) - 二进制格式

### 2. 核心功能

#### 🎯 高质量幻灯片渲染
- 将每张幻灯片渲染为高分辨率 PNG 图片
- 渲染分辨率缩放 2 倍（可配置），确保清晰度
- 应用高质量渲染参数（抗锯齿、双三次插值等）

#### 📝 文本提取
- 提取每张幻灯片中的文本内容
- 将文本作为上下文传递给 Vision LLM
- 帮助 AI 更好地理解幻灯片主题

#### 🤖 Vision LLM 集成
- 使用 Vision LLM 分析幻灯片图片
- 提取图表、流程图、架构图等视觉信息
- 识别图片中的关键元素和布局

#### 🔗 上下文增强
- 将前 3 张幻灯片的文本作为文档上下文
- 帮助 Vision LLM 理解文档主题
- 提高分析准确性

## 🏗️ 技术实现

### 处理流程

```
1. 读取 PPT 文件
   ↓
2. 提取所有幻灯片的文本内容
   ↓
3. 对每张幻灯片：
   a. 添加幻灯片标题
   b. 添加文本内容
   c. 渲染为高分辨率图片
   d. 创建 ExtractedImage 对象（包含元数据）
   e. 添加到内容块
   ↓
4. Vision LLM 分析所有图片（由父类处理）
   ↓
5. 合并文本和 Vision LLM 分析结果
   ↓
6. 返回最终结果
```

### 核心方法

#### `extractPptxContent()`
处理 .pptx 格式的文档：
- 使用 `XMLSlideShow` (Apache POI)
- 提取文本和渲染图片
- 构建元数据

#### `extractPptContent()`
处理 .ppt 格式的文档：
- 使用 `HSLFSlideShow` (Apache POI)
- 提取文本和渲染图片
- 构建元数据

#### `renderSlide()`
渲染幻灯片为图片：
- 创建高分辨率 BufferedImage
- 设置高质量渲染参数
- 应用缩放变换
- 绘制幻灯片内容

#### `buildImageAnalysisPrompt()`
构建 PPT 专用的 Vision LLM 提示词：
- 包含文档主题信息
- 包含当前幻灯片文本
- 包含幻灯片位置信息
- 指导 AI 关注视觉元素

## 📊 元数据结构

每张幻灯片图片包含以下元数据：

```java
{
    "slideText": "当前幻灯片的文本内容",
    "fileName": "原始文件名",
    "totalSlides": 总幻灯片数,
    "slideNumber": 当前幻灯片编号,
    "imageIndex": 0,
    "documentContext": "前3张幻灯片的文本（仅前3张）"
}
```

## 🔧 配置选项

### application.yml / application.properties

```yaml
# 启用/禁用 PPT 处理器
omni-agent:
  ppt:
    enabled: true  # 默认启用
    
# Vision AI Service 配置（必需）
omni-agent:
  vision-llm:
    enabled: true
    model: "qwen-vl-plus"
```

### 代码配置

```java
// 渲染分辨率缩放倍数
private static final double RENDER_SCALE = 2.0;
```

## 📝 输出格式

处理后的文档内容格式：

```markdown
## 幻灯片 1

**文本内容：**
标题文本和正文内容...

📷 **[图片 - 页码 1]**

[Vision LLM 分析结果]
- 流程图描述
- 关键视觉元素
- 布局特点
...

---

## 幻灯片 2

...
```

## 🔄 与 VisionLLMDocumentProcessor 的关系

### 保留的逻辑
1. ✅ 幻灯片渲染为图片
2. ✅ 高分辨率渲染（2x 缩放）
3. ✅ 文本提取作为上下文
4. ✅ Vision LLM 分析
5. ✅ 元数据传递

### 优化改进
1. 🎯 **架构优化**：继承 `AbstractDocumentProcessor`，享受扩展机制
2. 🎯 **职责分离**：PPT 处理逻辑独立，不与其他格式混合
3. 🎯 **可扩展性**：支持 PreProcessor、PostProcessor 等扩展
4. 🎯 **配置灵活**：独立的配置项，易于管理

### 迁移建议

将 `VisionLLMDocumentProcessor` 中的 PPT 处理逻辑迁移到 `PPTProcessor` 后：

1. 在 `VisionLLMDocumentProcessor` 中移除 PPT 相关代码
2. 更新 `SUPPORTED_EXTENSIONS`，移除 "ppt" 和 "pptx"
3. 删除 `extractPptxPages()` 和 `extractPptPages()` 方法
4. 保留图片处理逻辑

## 🚀 使用示例

### 基本使用

```java
@Autowired
private DocumentProcessor pptProcessor;

ProcessingContext context = ProcessingContext.builder()
    .filePath("presentation.pptx")
    .originalFileName("presentation.pptx")
    .fileExtension("pptx")
    .build();

ProcessingResult result = pptProcessor.process(context);
String content = result.getContent();
List<ExtractedImage> images = result.getImages();
```

### 扩展使用

添加自定义处理逻辑：

```java
@Component
@Order(1)
public class PPTWatermarkImageHandler implements ImageHandler {
    
    @Override
    public boolean supports(String processorName) {
        return "PPTProcessor".equals(processorName);
    }
    
    @Override
    public ProcessedImage handle(ProcessingContext context, ExtractedImage image) {
        // 为 PPT 幻灯片图片添加水印
        byte[] watermarkedData = addWatermark(image.getData());
        return ProcessedImage.builder()
            .data(watermarkedData)
            .format(image.getFormat())
            .build();
    }
}
```

## 📦 依赖

```xml
<!-- Apache POI for PowerPoint -->
<dependency>
    <groupId>org.apache.poi</groupId>
    <artifactId>poi</artifactId>
</dependency>
<dependency>
    <groupId>org.apache.poi</groupId>
    <artifactId>poi-ooxml</artifactId>
</dependency>
<dependency>
    <groupId>org.apache.poi</groupId>
    <artifactId>poi-scratchpad</artifactId>
</dependency>
```

## ⚡ 性能优化建议

1. **批处理**：对于多个 PPT 文件，使用线程池并行处理
2. **缓存**：缓存已渲染的幻灯片图片
3. **分辨率调整**：根据需求调整 `RENDER_SCALE`（更高清晰度 = 更多内存和时间）
4. **Vision LLM 批处理**：利用父类的批处理机制

## 🐛 常见问题

### Q1: 幻灯片图片模糊？
**A**: 增加 `RENDER_SCALE` 值（例如 3.0 或 4.0）

### Q2: 内存占用过高？
**A**: 减少 `RENDER_SCALE` 值或分批处理大文件

### Q3: Vision LLM 分析不准确？
**A**: 
- 检查幻灯片文本是否正确提取
- 优化 `buildImageAnalysisPrompt()` 提示词
- 确保 Vision AI Service 正确配置

### Q4: 不支持某些 PPT 特效？
**A**: Apache POI 对某些高级特效支持有限，考虑使用其他渲染方案

## 📞 技术支持

如有问题，请联系 OmniAgent 团队或提交 Issue。

---

**版本**: 3.0.0  
**作者**: OmniAgent Team  
**更新日期**: 2025-01-28

