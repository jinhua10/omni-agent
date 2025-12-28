# OmniAgent Document Processor Starter

## 📚 概述

`omni-agent-document-processor-starter` 是一个开箱即用的文档处理 Starter，支持多种文档格式的智能处理。

## ✨ 特性

### 支持的文档格式

| 格式 | 处理器 | 默认启用 | 说明 |
|-----|--------|---------|------|
| **Excel** (.xls, .xlsx) | ExcelProcessor | ✅ | 提取表格数据、图表、图片 |
| **Word** (.doc, .docx) | WordProcessor | ✅ | 提取文本、表格、图片 |
| **PDF** (.pdf) | PDFProcessor | ✅ | 提取文本、图片，支持 OCR |
| **PowerPoint** (.ppt, .pptx) | PPTProcessor | ✅ | 渲染幻灯片为图片，Vision LLM 分析 |
| **Text** (.txt, .md, .json, .xml, .csv, .log) | TextProcessor | ✅ | 直接读取文本内容 |
| **Media** (图片、音频、视频) | MediaFileProcessor | ❌ | 需要显式启用 |
| **Vision LLM** (所有格式) | VisionLLMDocumentProcessor | ❌ | 需要显式启用 |

### 核心功能

- ✅ **自动注册**：通过 Spring Boot 自动配置
- ✅ **可扩展**：支持自定义扩展（PreProcessor、PostProcessor、ContentEnhancer、ImageHandler、MetadataExtractor）
- ✅ **批处理**：智能分批和并行处理，大幅提升性能
- ✅ **Vision LLM**：集成 Vision AI，分析图片内容
- ✅ **流式输出**：支持流式处理和批次标记
- ✅ **配置化**：灵活的配置选项

## 🚀 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-document-processor-starter</artifactId>
    <version>3.0.0</version>
</dependency>
```

### 2. 配置（可选）

```yaml
omni-agent:
  # 文档处理器总开关（默认启用）
  document-processor:
    enabled: true
    
  # 各个处理器配置（默认都启用，除了 media 和 vision-llm）
  excel:
    enabled: true
  word:
    enabled: true
  pdf:
    enabled: true
  ppt:
    enabled: true
  text:
    enabled: true
    
  # 媒体文件处理器（默认不启用）
  media:
    enabled: false
    
  # Vision LLM 处理器（默认不启用）
  vision-llm:
    enabled: false
    model: "qwen-vl-plus"
    
    # 批处理配置
    batch-processing:
      enabled: true
      max-batch-size: 5
```

### 3. 使用

```java
@Autowired
private DocumentProcessor documentProcessor;

public void processDocument(String filePath) {
    ProcessingContext context = ProcessingContext.builder()
        .filePath(filePath)
        .originalFileName("document.pdf")
        .fileExtension("pdf")
        .build();
        
    ProcessingResult result = documentProcessor.process(context);
    
    String content = result.getContent();
    List<ExtractedImage> images = result.getImages();
    Map<String, Object> metadata = result.getMetadata();
}
```

## 📋 配置详解

### Excel 处理器

```yaml
omni-agent:
  excel:
    enabled: true
```

**功能**：
- 提取所有工作表的表格数据（Markdown 格式）
- 提取内嵌图片
- 使用 Vision LLM 分析图片（如果启用）

### Word 处理器

```yaml
omni-agent:
  word:
    enabled: true
```

**功能**：
- 提取文本内容（段落、标题、列表）
- 提取表格（Markdown 格式）
- 提取内嵌图片
- 使用 Vision LLM 分析图片（如果启用）

### PDF 处理器

```yaml
omni-agent:
  pdf:
    enabled: true
    enable-ocr: false  # 是否启用 OCR
```

**功能**：
- 提取每页文本
- 提取内嵌图片
- OCR 识别（如果启用）
- 使用 Vision LLM 分析图片（如果启用）

### PowerPoint 处理器

```yaml
omni-agent:
  ppt:
    enabled: true
```

**功能**：
- 渲染每张幻灯片为高分辨率图片（2x 缩放）
- 提取幻灯片文本作为上下文
- 使用 Vision LLM 分析幻灯片图片
- 前 3 张幻灯片作为文档上下文

### Text 处理器

```yaml
omni-agent:
  text:
    enabled: true
```

**功能**：
- 直接读取文本文件内容
- 支持格式：txt, md, json, xml, csv, log, yaml, yml

### Vision LLM 处理器

```yaml
omni-agent:
  vision-llm:
    enabled: true
    model: "qwen-vl-plus"
    system-prompt: "请分析这张图片并提取其中的关键信息。"
    
    # 批处理配置（重要！）
    batch-processing:
      enabled: true
      max-batch-size: 5          # 每批最多处理的幻灯片/页面数
      max-context-tokens: 8000   # 最大上下文 token 数
      
  # 线程池配置（用于并行处理）
  executor:
    vision-llm:
      core-pool-size: 3
      max-pool-size: 6
      queue-capacity: 100
      thread-name-prefix: "vision-llm-"
```

**功能**：
- 将文档页面渲染为图片
- 使用 Vision LLM 分析图片内容
- 智能批处理和并行处理
- 流式输出支持

## 🎨 扩展机制

### 1. 前置处理器 (PreProcessor)

在文档处理前执行：

```java
@Component
@Order(1)
public class MyPreProcessor implements PreProcessor {
    @Override
    public String getName() {
        return "MyPreProcessor";
    }
    
    @Override
    public ProcessingContext preProcess(ProcessingContext context) {
        // 自定义前置处理逻辑
        return context;
    }
}
```

### 2. 后置处理器 (PostProcessor)

在文档处理后执行：

```java
@Component
@Order(10)
public class MyPostProcessor implements PostProcessor {
    @Override
    public String getName() {
        return "MyPostProcessor";
    }
    
    @Override
    public ProcessingResult postProcess(ProcessingContext context, ProcessingResult result) {
        // 自定义后置处理逻辑
        return result;
    }
}
```

### 3. 内容增强器 (ContentEnhancer)

增强提取的内容：

```java
@Component
@Order(20)
public class MyContentEnhancer implements ContentEnhancer {
    @Override
    public String getName() {
        return "MyContentEnhancer";
    }
    
    @Override
    public EnhancedContent enhance(ProcessingContext context, String originalContent) {
        // 提取关键词、生成摘要等
        return EnhancedContent.builder()
            .content(originalContent)
            .keywords(extractKeywords(originalContent))
            .summary(generateSummary(originalContent))
            .build();
    }
}
```

### 4. 图片处理器 (ImageHandler)

处理提取的图片：

```java
@Component
@Order(5)
public class MyImageHandler implements ImageHandler {
    @Override
    public String getName() {
        return "MyImageHandler";
    }
    
    @Override
    public ProcessedImage handle(ProcessingContext context, ExtractedImage image) {
        // 压缩图片、添加水印等
        return ProcessedImage.builder()
            .data(compressImage(image.getData()))
            .format(image.getFormat())
            .build();
    }
}
```

详细文档请参考：`EXTENSION_GUIDE.md`

## 📊 性能优化

### 批处理

智能分批和并行处理，大幅提升性能：

**场景**：处理 30 张图片的 PPT

| 方式 | 耗时 | 提升 |
|-----|------|------|
| 传统方式 | 90s | - |
| 智能分批 | 24s | 73% ↑ |
| 分批+并行 | 8s | 91% ↑ |

### 流式输出

支持流式处理和批次标记：

```java
Map<String, Object> options = new HashMap<>();
options.put("streaming", true);
options.put("streamCallback", (Consumer<String>) content -> {
    if (content.startsWith("BATCH_INFO:")) {
        // 批次信息
    } else if (content.startsWith("BATCH_START:")) {
        // 批次开始
    } else if (content.startsWith("BATCH_CONTENT:")) {
        // 批次内容
    } else if (content.startsWith("BATCH_END:")) {
        // 批次结束
    }
});

context.setOptions(options);
```

详细文档请参考：`BATCH_PROCESSING_OPTIMIZATION.md`

## 📁 项目结构

```
omni-agent-document-processor-starter/
├── src/main/java/
│   └── top/yumbo/ai/omni/document/processor/starter/
│       ├── CompositeDocumentProcessor.java      # 组合处理器
│       ├── config/
│       │   ├── DocumentProcessorAutoConfiguration.java
│       │   └── DocumentProcessorProperties.java
│       └── processor/
│           ├── ExcelProcessor.java
│           ├── PDFProcessor.java
│           ├── WordProcessor.java
│           ├── PPTProcessor.java
│           ├── TextProcessor.java
│           ├── MediaFileProcessor.java
│           └── VisionLLMDocumentProcessor.java
└── src/main/resources/
    └── META-INF/
        └── spring.factories                      # 自动配置
```

## 🔧 故障排查

### 问题 1：处理器没有生效

**检查**：
1. 确认配置项是否正确
2. 查看日志，确认处理器是否被注册
3. 检查文件扩展名是否正确

### 问题 2：Vision LLM 不工作

**检查**：
1. 确认 `omni-agent.vision-llm.enabled=true`
2. 确认 Vision AI Service 已配置
3. 查看日志中的错误信息

### 问题 3：性能慢

**优化**：
1. 启用批处理：`omni-agent.vision-llm.batch-processing.enabled=true`
2. 配置线程池
3. 调整批次大小

## 📚 相关文档

- [扩展机制使用指南](../omni-agent-document-processor-api/EXTENSION_GUIDE.md)
- [批处理优化说明](../omni-agent-document-processor-api/BATCH_PROCESSING_OPTIMIZATION.md)
- [PPT 处理器文档](PPT_PROCESSOR_README.md)
- [完整总结](../DOCUMENT_PROCESSOR_COMPLETE_SUMMARY.md)

## 📞 技术支持

如有问题，请联系 OmniAgent 团队或提交 Issue。

---

**版本**: 3.0.0  
**作者**: OmniAgent Team  
**更新日期**: 2025-01-28

