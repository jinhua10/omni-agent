# Vision LLM 文档处理正确实现方案

## 问题分析

之前的实现存在以下问题：
1. `DocumentProcessingController` 调用的是模拟方法而不是真实的文档处理器
2. `VisionLLMDocumentProcessor` 试图直接调用HTTP API，违反了架构设计原则
3. `AIService` 接口缺少Vision多模态支持

## 正确的架构设计

```
DocumentProcessingController
    ↓
DocumentProcessorManager
    ↓
VisionLLMDocumentProcessor
    ↓
AIService (接口层)
    ↓
OnlineAPIAIService (实现层)
    ↓
Vision API (千问VL/GPT-4V等)
```

## 实现的修改

### 1. 扩展 AIService 接口 ⭐

**文件**: `omni-agent-ai-api/src/main/java/top/yumbo/ai/ai/api/AIService.java`

添加了Vision多模态方法：

```java
// ========== Vision 多模态 (Vision Multi-Modal) ==========

/**
 * 分析单张图片（Vision LLM）
 */
default String analyzeImage(byte[] imageData, String prompt);

/**
 * 分析多张图片（Vision LLM）
 * 用于理解包含多张图片的复杂内容（如PPT幻灯片、流程图等）
 */
default String analyzeImages(List<byte[]> imagesData, String prompt);

/**
 * 多模态对话（支持文本+图片）
 */
default AIResponse chatWithVision(List<ChatMessage> messages);
```

### 2. 扩展 ChatMessage 支持多模态内容 ⭐

**文件**: `omni-agent-ai-api/src/main/java/top/yumbo/ai/ai/api/model/ChatMessage.java`

添加了多模态内容支持：

```java
/**
 * 多模态内容列表（文本+图片）
 * 如果此字段非空，则优先使用此字段而不是 content
 */
private List<ContentPart> contentParts;

/**
 * 内容部分（支持文本和图片）
 */
public static class ContentPart {
    private String type;        // "text" 或 "image_url"
    private String text;        // 文本内容
    private ImageUrl imageUrl;  // 图片URL
    
    public static ContentPart text(String text);
    public static ContentPart imageBase64(byte[] imageData);
    public static ContentPart imageUrl(String url);
}

/**
 * 创建用户消息（多模态：文本+图片）
 */
public static ChatMessage userWithImages(String text, List<byte[]> images);
```

### 3. OnlineAPIAIService 实现Vision方法 ⭐

**文件**: `omni-agent-ai-starter-online-api/src/main/java/top/yumbo/ai/ai/online/OnlineAPIAIService.java`

实现了Vision API调用：

```java
@Override
public String analyzeImage(byte[] imageData, String prompt) {
    List<byte[]> images = new ArrayList<>();
    images.add(imageData);
    return analyzeImages(images, prompt);
}

@Override
public String analyzeImages(List<byte[]> imagesData, String prompt) {
    // 创建多模态消息
    ChatMessage message = ChatMessage.userWithImages(prompt, imagesData);
    
    // 使用chatWithVision方法
    List<ChatMessage> messages = new ArrayList<>();
    messages.add(message);
    
    AIResponse response = chatWithVision(messages);
    return response.getText();
}

@Override
public AIResponse chatWithVision(List<ChatMessage> messages) {
    // 构建请求体（支持多模态内容）
    Map<String, Object> requestBody = new HashMap<>();
    requestBody.put("model", currentModel);
    
    // 转换消息格式（支持多模态）
    List<Map<String, Object>> formattedMessages = new ArrayList<>();
    for (ChatMessage msg : messages) {
        // 如果有多模态内容，使用contentParts
        if (msg.getContentParts() != null && !msg.getContentParts().isEmpty()) {
            List<Map<String, Object>> contentArray = new ArrayList<>();
            for (ChatMessage.ContentPart part : msg.getContentParts()) {
                Map<String, Object> partMap = new HashMap<>();
                partMap.put("type", part.getType());
                
                if ("text".equals(part.getType())) {
                    partMap.put("text", part.getText());
                } else if ("image_url".equals(part.getType())) {
                    Map<String, String> imageUrlMap = new HashMap<>();
                    imageUrlMap.put("url", part.getImageUrl().getUrl());
                    partMap.put("image_url", imageUrlMap);
                }
                
                contentArray.add(partMap);
            }
            formattedMsg.put("content", contentArray);
        }
    }
    
    // 发送请求到Vision API
    return restTemplate.exchange(...);
}
```

### 4. VisionLLMDocumentProcessor 调用AIService ⭐

**文件**: `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/document/processor/VisionLLMDocumentProcessor.java`

修改为通过AIService调用：

```java
private String recognizePageWithVisionLLM(DocumentPage page, String prompt) {
    // 检查 AIService 配置
    if (aiService == null) {
        log.warn("⚠️ [VisionLLM] AI Service 未配置");
        return "[AI Service 未配置]";
    }

    // 1. 提取所有图片数据
    List<byte[]> imagesData = new ArrayList<>();
    for (ExtractedImage image : page.getImages()) {
        imagesData.add(image.getData());
    }

    // 2. 构建 Vision 提示词（包含上下文信息）
    String visionPrompt = buildVisionPrompt(page, prompt);

    // 3. 调用 AIService 的 analyzeImages 方法 ⭐
    try {
        String result = aiService.analyzeImages(imagesData, visionPrompt);
        return result != null ? result : "";
        
    } catch (UnsupportedOperationException e) {
        // 当前AI服务不支持Vision功能
        return "[当前AI服务不支持Vision功能，请配置支持Vision的模型]";
    }
}
```

### 5. DocumentProcessingController 调用真实处理器 ⭐

**文件**: `omni-agent-web/src/main/java/top/yumbo/ai/omni/web/controller/DocumentProcessingController.java`

修改为调用真实的文档处理器：

```java
// 调用实际的文本提取服务
String extractedText = extractTextWithProcessor(documentId, content, request.getModel());

private String extractTextWithProcessor(String documentId, byte[] content, String model) {
    // 从documentId获取文件扩展名
    String fileExtension = getFileExtension(documentId);
    
    // 创建处理上下文
    DocumentProcessor.ProcessingContext context =
            DocumentProcessor.ProcessingContext.builder()
                    .fileBytes(content)
                    .fileExtension(fileExtension)
                    .originalFileName(documentId)
                    .fileSize(content.length)
                    .options(Map.of("model", model))
                    .build();

    // 调用文档处理器
    DocumentProcessor.ProcessingResult result =
            documentProcessorManager.processDocument(context);

    return result.getContent();
}
```

## VisionLLMDocumentProcessor 的完整流程

### PPT/PDF 处理流程

1. **提取页面图片**
   - 每页提取所有图片（包括位置信息）
   - 图片按空间位置排序（从上到下，从左到右）
   - 保留图片元数据（文件名、幻灯片文字、上下文等）

2. **智能批处理**
   - 根据上下文大小动态决定批次大小
   - 尽可能多页一起处理，减少API调用次数
   - 考虑模型的token限制

3. **并行处理**
   - 多个批次并行处理
   - 使用线程池提高处理速度

4. **Vision LLM 分析**
   - 调用 `aiService.analyzeImages()` 分析整页内容
   - 传入上下文感知的提示词
   - 理解流程图、架构图、部署图等复杂内容

### 提示词构建（buildVisionPrompt）

```java
private String buildVisionPrompt(DocumentPage page, String basePrompt) {
    StringBuilder prompt = new StringBuilder();
    
    // 1. 任务说明
    prompt.append("# 任务说明\n");
    prompt.append("请将这张 PPT 幻灯片的内容转换为文字描述。\n\n");
    
    // 2. 文档信息（文件名、总页数、当前页码）
    prompt.append("## 文档信息\n");
    prompt.append("- 文件名：").append(fileName).append("\n");
    prompt.append("- 当前页码：第 ").append(page.getPageNumber()).append(" 页\n\n");
    
    // 3. 幻灯片中的文字内容（最重要的上下文）
    if (slideText != null && !slideText.trim().isEmpty()) {
        prompt.append("## 幻灯片中的文字内容\n");
        prompt.append("```\n");
        prompt.append(slideText).append("\n");
        prompt.append("```\n\n");
    }
    
    // 4. 前几页的上下文（理解主题）
    if (documentContext != null && !documentContext.trim().isEmpty()) {
        prompt.append("## 文档主题参考\n");
        prompt.append(documentContext).append("\n\n");
    }
    
    // 5. 具体要求
    prompt.append("## 输出要求\n");
    prompt.append("- 完整准确地提取所有文字内容\n");
    prompt.append("- 理解图表、流程图、架构图的含义\n");
    prompt.append("- 如果有连线关系，说明其逻辑关系\n");
    
    return prompt.toString();
}
```

## 配置要求

### application.yml

```yaml
omni-agent:
  # AI 配置（使用支持Vision的在线API）
  ai:
    type: online-api
    online:
      provider: qianwen  # 或 openai
      api-key: ${AI_API_KEY:your-api-key-here}
      default-model: qwen-vl-plus  # 或 gpt-4o
      endpoint: https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions

  # Vision LLM 配置
  vision-llm:
    enabled: true
    model: qwen-vl-plus  # 必须是支持Vision的模型
    system-prompt: |
      请分析这张图片并提取其中的关键信息。
      如果图片包含文字，请完整准确地提取所有文字内容。
      如果是图表或示意图，请描述其主要内容和含义。
    
    # 智能批处理配置
    batch-processing:
      enabled: true
      max-context-tokens: 8000
      estimated-tokens-per-slide: 1500
      min-batch-size: 1
      max-batch-size: 5
```

### 环境变量

```bash
export AI_API_KEY=sk-your-actual-api-key
```

## 支持的文件格式

- **PDF**: 每页转为图片进行分析
- **PPT/PPTX**: 提取每张幻灯片的图片和文字
- **Word (DOC/DOCX)**: 提取文档中的图片
- **Excel (XLS/XLSX)**: 提取工作表中的图片
- **图片**: PNG, JPG, JPEG, BMP, TIFF, GIF

## 优势

1. **架构清晰**: 通过AIService接口层解耦，符合SOLID原则
2. **易于扩展**: 可以轻松切换不同的Vision模型（千问VL、GPT-4V等）
3. **保持上下文**: 提取幻灯片文字、文件名等信息辅助理解
4. **智能批处理**: 动态决定批次大小，减少API调用
5. **并行处理**: 多批次并行，大幅提升速度
6. **完整理解**: 能理解流程图、架构图等复杂内容

## 测试验证

```bash
# 启动应用
mvn spring-boot:run -pl omni-agent-example-basic

# 调用API
curl -X POST "http://localhost:3000/api/documents/processing/绿色环保能源灯泡——.ppt/extract" \
  -H "Content-Type: application/json" \
  -d '{"model": "vision-llm", "streaming": true}'
```

## 注意事项

1. **必须配置有效的API Key**: 否则会返回错误提示
2. **必须使用支持Vision的模型**: 如 qwen-vl-plus、gpt-4o 等
3. **网络连接**: 需要能够访问Vision API端点
4. **费用控制**: Vision API调用会产生费用，注意使用量
5. **处理时间**: 大文档处理可能需要较长时间

## 未来改进

1. **缓存机制**: 对相同文档的重复处理使用缓存
2. **流式响应**: 支持流式返回处理进度
3. **错误重试**: 添加自动重试机制
4. **模型适配**: 支持更多Vision模型（Claude、Gemini等）
5. **本地部署**: 支持本地部署的Vision模型（如LLaVA）

## 相关文件

- `omni-agent-ai-api/src/main/java/top/yumbo/ai/ai/api/AIService.java` - AI服务接口
- `omni-agent-ai-api/src/main/java/top/yumbo/ai/ai/api/model/ChatMessage.java` - 多模态消息模型
- `omni-agent-ai-starter-online-api/src/main/java/top/yumbo/ai/ai/online/OnlineAPIAIService.java` - 在线API实现
- `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/document/processor/VisionLLMDocumentProcessor.java` - Vision文档处理器
- `omni-agent-web/src/main/java/top/yumbo/ai/omni/web/controller/DocumentProcessingController.java` - 文档处理控制器

