# 📄 可扩展文档处理架构设计

**设计时间**: 2025-12-19  
**状态**: ✅ 架构完成，部分待实现  
**设计者**: AI Assistant

---

## 📋 需求分析

### 当前需求
- 处理 Office 文档：PDF, Word, Excel, PPT
- 使用 Vision LLM 将文档转为文本

### 未来扩展
- 任意文件类型
- 媒体文件（视频/音频）
- LLM 直接文件分析
- 压缩文件、代码文件等

---

## 🏗️ 架构设计

### 核心接口：DocumentProcessor

```java
public interface DocumentProcessor {
    // 是否支持该文件类型
    boolean supports(String fileExtension);
    
    // 处理器名称和优先级
    String getName();
    int getPriority();
    
    // 同步处理
    ProcessingResult process(ProcessingContext context);
    
    // 异步处理（大文件）
    String processAsync(ProcessingContext context, ProgressCallback callback);
    
    // 验证
    ValidationResult validate(ProcessingContext context);
}
```

### 管理服务：DocumentProcessorManager

```java
@Service
public class DocumentProcessorManager {
    // 自动注入所有 DocumentProcessor Bean
    @Autowired(required = false)
    public DocumentProcessorManager(List<DocumentProcessor> processors);
    
    // 查找合适的处理器
    DocumentProcessor findProcessor(String fileExtension);
    
    // 处理文档
    ProcessingResult processDocument(ProcessingContext context);
    String processDocumentAsync(ProcessingContext context, ProgressCallback callback);
}
```

---

## 📦 已实现的处理器

### 1. PlainTextDocumentProcessor ✅

**支持的文件类型**:
- 文本文件：txt, md, log
- 代码文件：java, py, js, ts, go, rs, c, cpp, cs, php, rb, swift, kt
- 配置文件：yml, yaml, json, xml, properties, ini
- Web 文件：html, css, scss
- 脚本文件：sh, bash, bat, ps1, sql

**优先级**: 50 (中等)

**实现状态**: ✅ 完全实现

### 2. VisionLLMDocumentProcessor 🚧

**支持的文件类型**:
- Office 文档：pdf, doc, docx, xls, xlsx, ppt, pptx
- 图片文件：png, jpg, jpeg, bmp, tiff, gif

**优先级**: 10 (高)

**实现状态**: 🚧 框架完成，待实现：
- [ ] 文档转图片功能
- [ ] Vision LLM API 调用

**处理流程**:
```
1. 文档 → 图片转换
   ├─ PDF: Apache PDFBox / pdf2image
   ├─ Word/Excel/PPT: Apache POI + Java2D
   └─ 图片: 直接读取

2. Vision LLM 识别
   ├─ 图片 Base64 编码
   ├─ 调用 Vision API (千问VL/GPT-4V/Claude)
   └─ 提取文本内容

3. 结果整合
   └─ 合并所有页面/图片的识别结果
```

### 3. MediaFileProcessor 🔮

**支持的文件类型**:
- 视频：mp4, avi, mov, mkv, flv, wmv, webm
- 音频：mp3, wav, aac, flac, ogg, m4a
- 字幕：srt, ass, vtt, sub

**优先级**: 20 (较高)

**实现状态**: 🔮 未来计划

**技术方案**:

#### 视频处理
```
视频文件 (video.mp4)
  ↓
1. 提取音频轨道 (FFmpeg)
  ↓
2. 语音识别 (Whisper / 云服务)
  ↓
3. 生成字幕文本
  ↓
4. 提取关键帧 (OpenCV)
  ↓
5. Vision LLM 分析关键帧
  ↓
6. 合并：字幕 + 视觉描述
```

#### 音频处理
```
音频文件 (audio.mp3)
  ↓
1. 音频预处理 (降噪、归一化)
  ↓
2. 语音识别 (Whisper)
  ├─ 单人：直接转文字
  └─ 多人：说话人分离
  ↓
3. 生成文本
```

**性能考虑**:
- ✅ 必须异步处理
- ✅ 大文件分段并行
- ✅ 进度实时反馈
- ✅ 结果缓存

---

## 🔌 扩展 AI Service

### VisionAIService 接口 ✅

```java
public interface VisionAIService extends AIService {
    // 分析单张图片
    String analyzeImage(ImageInput imageInput, String prompt);
    
    // 分析多张图片
    String analyzeImages(List<ImageInput> images, String prompt);
    
    // 流式分析
    Flux<String> analyzeImageFlux(ImageInput imageInput, String prompt);
    
    // 多模态对话（文本 + 图片）
    AIResponse multimodalChat(MultimodalRequest request);
    Flux<String> multimodalChatFlux(MultimodalRequest request);
}
```

### 实现策略

#### 1. Ollama 实现
```java
@ConditionalOnProperty(prefix = "omni-agent.ai", name = "type", havingValue = "ollama")
public class OllamaVisionAIService implements VisionAIService {
    // 支持 llava, llava-phi3 等视觉模型
}
```

#### 2. Online API 实现
```java
@ConditionalOnProperty(prefix = "omni-agent.ai", name = "type", havingValue = "online-api")
public class OnlineVisionAIService implements VisionAIService {
    // 根据 provider 选择：
    // - qianwen: qwen-vl-plus
    // - openai: gpt-4-vision-preview
    // - claude: claude-3-opus
}
```

---

## 📂 文件结构

```
omni-agent-core/src/main/java/top/yumbo/ai/omni/core/document/
├── DocumentProcessor.java ✅
│   ├── ProcessingContext
│   ├── ProcessingResult
│   ├── ExtractedImage
│   ├── ValidationResult
│   ├── ProgressCallback
│   └── DocumentProcessingException
│
├── DocumentProcessorManager.java ✅
│   ├── 自动注入所有处理器
│   ├── findProcessor()
│   ├── processDocument()
│   └── processDocumentAsync()
│
└── processor/
    ├── PlainTextDocumentProcessor.java ✅
    ├── VisionLLMDocumentProcessor.java 🚧
    └── MediaFileProcessor.java 🔮

omni-agent-ai-api/src/main/java/top/yumbo/ai/ai/api/
└── VisionAIService.java ✅

omni-agent-ai-starter-ollama/
└── OllamaVisionAIServiceImpl.java 🔮

omni-agent-ai-starter-online-api/
└── OnlineVisionAIServiceImpl.java 🔮
```

---

## 🎯 使用示例

### 基础使用

```java
@Autowired
private DocumentProcessorManager processorManager;

// 处理文档
DocumentProcessor.ProcessingContext context = DocumentProcessor.ProcessingContext.builder()
    .filePath("/path/to/document.pdf")
    .fileExtension("pdf")
    .originalFileName("document.pdf")
    .fileSize(1024000)
    .build();

DocumentProcessor.ProcessingResult result = processorManager.processDocument(context);

System.out.println("提取的文本: " + result.getContent());
System.out.println("处理时间: " + result.getProcessingTimeMs() + "ms");
```

### 异步处理（大文件）

```java
String taskId = processorManager.processDocumentAsync(context, new DocumentProcessor.ProgressCallback() {
    @Override
    public void onProgress(String taskId, int progress, String message) {
        System.out.println("进度: " + progress + "% - " + message);
    }

    @Override
    public void onComplete(String taskId, ProcessingResult result) {
        System.out.println("处理完成: " + result.getContent().length() + " 字符");
    }

    @Override
    public void onError(String taskId, Exception error) {
        System.err.println("处理失败: " + error.getMessage());
    }
});

System.out.println("任务ID: " + taskId);
```

### 扩展自定义处理器

```java
@Component
public class MyCustomProcessor implements DocumentProcessor {
    
    @Override
    public boolean supports(String fileExtension) {
        return "xyz".equals(fileExtension);  // 支持 .xyz 文件
    }

    @Override
    public String getName() {
        return "MyCustomProcessor";
    }

    @Override
    public int getPriority() {
        return 30;  // 优先级
    }

    @Override
    public ProcessingResult process(ProcessingContext context) throws DocumentProcessingException {
        // 实现自定义处理逻辑
        String content = extractContentFromXYZ(context.getFilePath());
        
        return ProcessingResult.builder()
            .success(true)
            .content(content)
            .processorName(getName())
            .build();
    }
}
```

---

## 🚀 实施计划

### Phase 1: 基础架构 ✅
- [x] DocumentProcessor 接口
- [x] DocumentProcessorManager 服务
- [x] PlainTextDocumentProcessor 实现
- [x] 基础文档和示例

### Phase 2: Vision LLM 集成 🚧
- [ ] VisionAIService 实现
  - [ ] Ollama 实现 (llava)
  - [ ] Online API 实现 (qwen-vl, gpt-4v)
- [ ] 文档转图片功能
  - [ ] PDF → PNG (Apache PDFBox)
  - [ ] Word/Excel/PPT → PNG (Apache POI)
- [ ] VisionLLMDocumentProcessor 完善
- [ ] 测试和优化

### Phase 3: 媒体文件支持 🔮
- [ ] FFmpeg 集成
- [ ] Whisper 语音识别集成
- [ ] MediaFileProcessor 实现
- [ ] 异步处理和进度反馈
- [ ] 大文件分段处理

### Phase 4: 高级功能 🔮
- [ ] 压缩文件处理 (ZIP, RAR)
- [ ] 数据库文件 (SQLite, etc.)
- [ ] 特殊格式 (CAD, PSD, etc.)
- [ ] LLM 直接文件分析（API 支持）

---

## 🔧 技术依赖

### 当前依赖
```xml
<!-- Spring Boot -->
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter</artifactId>
</dependency>
```

### Phase 2 需要
```xml
<!-- Apache PDFBox (PDF处理) -->
<dependency>
    <groupId>org.apache.pdfbox</groupId>
    <artifactId>pdfbox</artifactId>
    <version>3.0.0</version>
</dependency>

<!-- Apache POI (Office文档) -->
<dependency>
    <groupId>org.apache.poi</groupId>
    <artifactId>poi-ooxml</artifactId>
    <version>5.2.5</version>
</dependency>
```

### Phase 3 需要
```xml
<!-- JAVE2 (FFmpeg Java Wrapper) -->
<dependency>
    <groupId>ws.schild</groupId>
    <artifactId>jave-core</artifactId>
    <version>3.4.0</version>
</dependency>

<!-- Whisper JNI (可选) -->
<!-- 或使用云服务API -->
```

---

## 📊 性能指标

| 文件类型 | 大小 | 处理方式 | 预估耗时 |
|---------|------|---------|----------|
| **纯文本** | <10MB | 同步 | <100ms |
| **PDF** | <50MB | 同步 | 5-10s |
| **Office** | <20MB | 同步 | 3-8s |
| **图片** | <5MB | 同步 | 1-3s |
| **视频** | >50MB | 异步 | 1-5分钟 |
| **音频** | >10MB | 异步 | 30s-2分钟 |

---

## ✅ 优势

1. **可扩展**: 新增文件类型只需实现 DocumentProcessor 接口
2. **可插拔**: 通过 Spring 自动发现和注入
3. **优先级**: 支持多个处理器竞争，优先级高的优先
4. **异步支持**: 大文件自动异步处理
5. **进度反馈**: 实时反馈处理进度
6. **统一管理**: DocumentProcessorManager 统一入口
7. **元数据**: 丰富的处理结果元数据
8. **错误处理**: 完善的异常处理和降级

---

## 🎉 总结

已完成：
- ✅ 可扩展的架构设计
- ✅ 核心接口定义
- ✅ 管理服务实现
- ✅ 纯文本处理器
- ✅ Vision LLM 处理器框架
- ✅ 媒体文件处理器规划

待实现：
- 🚧 Vision LLM API 集成
- 🚧 文档转图片功能
- 🔮 媒体文件处理

**架构已就绪，可以逐步实现各个处理器！** 🚀

