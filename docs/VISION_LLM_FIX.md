# Vision LLM 文档提取功能修复说明

## 问题描述

调用 `/api/documents/processing/{documentId}/extract` 接口使用 `vision-llm` 模型提取文档内容时，返回的是模拟内容而不是真实的 Vision LLM 分析结果：

```
这是模拟提取的文本内容，使用模型: vision-llm\n文档大小: 1105408 字节
```

## 根本原因

系统存在两个问题：

### 1. DocumentProcessingController 使用模拟方法

`DocumentProcessingController.java` 中的 `extractText` 方法调用的是 `simulateTextExtraction` 而不是真实的文档处理器：

```java
// 错误的实现
String extractedText = simulateTextExtraction(content, request.getModel());
```

### 2. VisionLLMDocumentProcessor 无法调用 Vision API

`VisionLLMDocumentProcessor.java` 虽然准备了图片数据，但调用的是普通的 `aiService.chat()` 方法，该方法只接受文本输入，不支持图片：

```java
// 不支持图片的调用方式
String result = aiService.chat(visionPrompt);
```

## 解决方案

### 修改 1: DocumentProcessingController.java

**文件路径**: `omni-agent-web/src/main/java/top/yumbo/ai/omni/web/controller/DocumentProcessingController.java`

**修改内容**:

1. 替换模拟方法调用为真实的文档处理器调用：
```java
// 调用实际的文本提取服务
String extractedText = extractTextWithProcessor(documentId, content, request.getModel());
```

2. 新增 `extractTextWithProcessor` 方法，使用 `DocumentProcessorManager` 进行文档处理：
```java
private String extractTextWithProcessor(String documentId, byte[] content, String model) {
    try {
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

        if (result.isSuccess() && result.getContent() != null) {
            return result.getContent();
        } else {
            return "文档处理失败: " + (result.getError() != null ? result.getError() : "未知错误");
        }
    } catch (Exception e) {
        return "文档处理异常: " + e.getMessage();
    }
}
```

3. 新增 `getFileExtension` 辅助方法：
```java
private String getFileExtension(String filename) {
    if (filename == null || filename.isEmpty()) {
        return "";
    }
    int lastDot = filename.lastIndexOf('.');
    if (lastDot > 0 && lastDot < filename.length() - 1) {
        return filename.substring(lastDot + 1).toLowerCase();
    }
    return "";
}
```

### 修改 2: VisionLLMDocumentProcessor.java

**文件路径**: `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/document/processor/VisionLLMDocumentProcessor.java`

**修改内容**:

1. 添加 Vision API 配置字段：
```java
@Value("${omni-agent.vision-llm.api-key:}")
private String visionApiKey;

@Value("${omni-agent.vision-llm.endpoint:https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions}")
private String visionEndpoint;
```

2. 修改 `recognizePageWithVisionLLM` 方法，检查 API Key 并直接调用 Vision API：
```java
private String recognizePageWithVisionLLM(DocumentPage page, String prompt) {
    // 检查API Key配置
    if (visionApiKey == null || visionApiKey.trim().isEmpty() || "your-api-key-here".equals(visionApiKey)) {
        log.warn("⚠️ [VisionLLM] Vision API Key 未配置，返回占位内容");
        return String.format("[页面 %d 的内容 - Vision API Key 未配置]\n包含 %d 张图片",
                page.getPageNumber(), page.getImages().size());
    }

    // ... 准备图片数据 ...

    // 直接调用 Vision API（兼容 OpenAI Chat Completions 格式）
    String result = callVisionAPI(base64Images, visionPrompt);
    
    return result != null ? result : "";
}
```

3. 新增 `callVisionAPI` 方法，使用 HttpClient 直接调用 Vision API：
```java
private String callVisionAPI(List<String> base64Images, String textPrompt) throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();

    // 构建请求 JSON（OpenAI Chat Completions 兼容格式）
    ObjectNode requestJson = objectMapper.createObjectNode();
    requestJson.put("model", visionModel);
    requestJson.put("max_tokens", 2000);
    requestJson.put("temperature", 0.3);

    // 构建 messages（支持多模态：文本 + 图片）
    ArrayNode messages = objectMapper.createArrayNode();
    
    // System message
    ObjectNode systemMessage = objectMapper.createObjectNode();
    systemMessage.put("role", "system");
    systemMessage.put("content", systemPrompt);
    messages.add(systemMessage);

    // User message (包含图片和文本)
    ObjectNode userMessage = objectMapper.createObjectNode();
    userMessage.put("role", "user");

    // 创建content数组（多模态内容）
    ArrayNode content = objectMapper.createArrayNode();

    // 文本部分
    ObjectNode textPart = objectMapper.createObjectNode();
    textPart.put("type", "text");
    textPart.put("text", textPrompt);
    content.add(textPart);

    // 图片部分（支持多张）
    for (String base64Image : base64Images) {
        ObjectNode imagePart = objectMapper.createObjectNode();
        imagePart.put("type", "image_url");
        ObjectNode imageUrl = objectMapper.createObjectNode();
        imageUrl.put("url", "data:image/jpeg;base64," + base64Image);
        imagePart.set("image_url", imageUrl);
        content.add(imagePart);
    }

    userMessage.set("content", content);
    messages.add(userMessage);
    requestJson.set("messages", messages);

    // 使用 HttpClient 发送请求
    HttpClient client = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(30))
            .build();

    HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create(visionEndpoint))
            .header("Content-Type", "application/json")
            .header("Authorization", "Bearer " + visionApiKey)
            .POST(HttpRequest.BodyPublishers.ofString(objectMapper.writeValueAsString(requestJson)))
            .timeout(Duration.ofSeconds(60))
            .build();

    HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

    if (response.statusCode() != 200) {
        throw new RuntimeException("Vision API 返回错误: " + response.statusCode() + " - " + response.body());
    }

    // 解析响应
    JsonNode responseJson = objectMapper.readTree(response.body());
    JsonNode choices = responseJson.get("choices");

    if (choices != null && choices.isArray() && choices.size() > 0) {
        JsonNode firstChoice = choices.get(0);
        JsonNode message = firstChoice.get("message");
        if (message != null) {
            JsonNode contentNode = message.get("content");
            if (contentNode != null) {
                return contentNode.asText().trim();
            }
        }
    }

    log.warn("Vision API 响应格式异常: {}", response.body());
    return "[Vision API 响应格式异常]";
}
```

## 配置要求

确保 `application.yml` 中配置了有效的 Vision API Key：

```yaml
omni-agent:
  vision-llm:
    enabled: true
    api-key: ${QW_API_KEY:your-api-key-here}  # ⚠️ 必须配置有效的 API Key
    model: qwen-vl-plus
    endpoint: https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions
```

可以通过环境变量设置：
```bash
export QW_API_KEY=sk-your-actual-api-key
```

或者直接在配置文件中填写（不推荐提交到版本控制）：
```yaml
api-key: sk-your-actual-api-key
```

## 测试验证

### 1. 启动应用

```bash
mvn spring-boot:run -pl omni-agent-example-basic
```

### 2. 调用 API 测试

```bash
curl -X POST "http://localhost:3000/api/documents/processing/绿色环保能源灯泡——.ppt/extract" \
  -H "Content-Type: application/json" \
  -d '{"model": "vision-llm", "streaming": true}'
```

### 3. 预期结果

应该返回真实的 Vision LLM 分析结果，而不是模拟内容。例如：

```
event:message data:{"type":"progress","percent":10,"message":"正在读取文档..."}
event:message data:{"type":"progress","percent":30,"message":"正在解析文档格式..."}
event:message data:{"type":"progress","percent":80,"message":"文本提取完成"}
event:message data:{"type":"content","content":"# 绿色环保能源灯泡\n\n本PPT介绍了节能灯泡的优势...[真实的Vision LLM分析内容]"}
event:message data:{"type":"complete","message":"提取完成"}
```

## 注意事项

1. **API Key 必须配置**: 如果没有配置有效的 API Key，VisionLLMDocumentProcessor 会返回占位符内容。

2. **网络连接**: 需要能够访问 Vision API 端点（默认：阿里云千问 VL API）。

3. **模型选择**: 确保选择的模型支持视觉理解（如 `qwen-vl-plus`, `qwen-vl-max`, `gpt-4o` 等）。

4. **性能考虑**: Vision API 调用相对耗时，大文档处理可能需要较长时间。

5. **费用**: Vision API 调用会产生费用，请注意控制使用量。

## 相关文件

- `omni-agent-web/src/main/java/top/yumbo/ai/omni/web/controller/DocumentProcessingController.java`
- `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/document/processor/VisionLLMDocumentProcessor.java`
- `omni-agent-example-basic/src/main/resources/application.yml`

## 未来改进方向

1. **扩展 AIService 接口**: 添加原生的多模态支持，避免直接调用 HTTP API。

2. **实现 MultiModalAIService**: 使用已有的 `MultiModalAIService` 接口，统一管理多模态交互。

3. **添加缓存机制**: 对相同文档的重复处理使用缓存结果。

4. **错误重试**: 添加自动重试机制，提高鲁棒性。

5. **批量优化**: 优化大文档的批量处理策略，减少 API 调用次数。

