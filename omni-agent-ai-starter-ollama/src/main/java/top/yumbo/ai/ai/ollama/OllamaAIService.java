package top.yumbo.ai.ai.ollama;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;
import reactor.core.publisher.Flux;
import top.yumbo.ai.ai.api.AIService;
import top.yumbo.ai.ai.api.model.AIRequest;
import top.yumbo.ai.ai.api.model.AIResponse;
import top.yumbo.ai.ai.api.model.ChatMessage;
import top.yumbo.ai.ai.api.model.ModelInfo;

import java.util.*;

/**
 * Ollama AI 服务实现
 * (Ollama AI Service Implementation)
 *
 * <p>
 * 特点 (Features):
 * - 支持本地和远程 Ollama 部署
 * - 通过配置 baseUrl 切换本地/远程
 * - 本地部署：数据安全、离线可用
 * - 远程部署：集中管理、资源共享
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class OllamaAIService implements AIService {

    private final RestTemplate restTemplate;
    private final top.yumbo.ai.omni.common.http.HttpClientAdapter httpClientAdapter;
    private final OllamaProperties properties;
    private String currentModel;

    /**
     * 构造函数（使用 RestTemplate，向后兼容）
     */
    public OllamaAIService(RestTemplate restTemplate, OllamaProperties properties) {
        this(restTemplate, properties, null);
    }

    /**
     * 构造函数（支持自定义 HttpClientAdapter）
     */
    public OllamaAIService(RestTemplate restTemplate, OllamaProperties properties,
                          top.yumbo.ai.omni.common.http.HttpClientAdapter httpClientAdapter) {
        this.restTemplate = restTemplate;
        this.properties = properties;
        this.currentModel = properties.getDefaultModel();

        // 如果没有提供 httpClientAdapter，使用默认的 RestTemplateAdapter
        if (httpClientAdapter == null && restTemplate != null) {
            this.httpClientAdapter = new top.yumbo.ai.omni.common.http.RestTemplateAdapter(restTemplate);
        } else {
            this.httpClientAdapter = httpClientAdapter;
        }

        log.info("OllamaAIService initialized - baseUrl: {}, model: {}, HTTP Client: {}",
                properties.getBaseUrl(), currentModel,
                this.httpClientAdapter != null ? this.httpClientAdapter.getName() : "RestTemplate");
    }

    @Override
    public AIResponse generate(AIRequest request) {
        try {
            String url = properties.getBaseUrl() + "/api/generate";

            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("model", request.getModel() != null ? request.getModel() : currentModel);
            requestBody.put("prompt", request.getPrompt());
            requestBody.put("stream", false);

            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            HttpEntity<Map<String, Object>> entity = new HttpEntity<>(requestBody, headers);

            ResponseEntity<Map> response = restTemplate.postForEntity(url, entity, Map.class);

            if (response.getStatusCode() == HttpStatus.OK && response.getBody() != null) {
                Map<String, Object> body = response.getBody();
                String generatedText = (String) body.get("response");

                return AIResponse.builder()
                        .text(generatedText)
                        .model(currentModel)
                        .finishReason("stop")
                        .success(true)
                        .build();
            }

            return AIResponse.builder()
                    .text("")
                    .success(false)
                    .error("Failed to generate")
                    .build();
        } catch (Exception e) {
            log.error("Failed to generate text", e);
            return AIResponse.builder()
                    .text("")
                    .success(false)
                    .error(e.getMessage())
                    .build();
        }
    }

    @Override
    public String generate(String prompt) {
        AIRequest request = AIRequest.builder()
                .prompt(prompt)
                .build();
        AIResponse response = generate(request);
        return response.getText();
    }

    @Override
    @Deprecated
    public void generateStream(AIRequest request, java.util.function.Consumer<String> callback) {
        log.warn("Stream generation not fully implemented, falling back to sync");
        AIResponse response = generate(request);
        callback.accept(response.getText());
    }

    @Override
    public Flux<String> generateFlux(AIRequest request) {
        // 简化实现：返回完整文本作为单个元素
        return Flux.defer(() -> {
            AIResponse response = generate(request);
            return Flux.just(response.getText());
        });
    }

    @Override
    public Flux<AIResponse> generateFluxResponse(AIRequest request) {
        return Flux.defer(() -> {
            AIResponse response = generate(request);
            return Flux.just(response);
        });
    }

    // ========== Chat ==========
    public AIResponse chat(List<ChatMessage> messages) {
        return chat(null, messages);
    }

    @Override
    public AIResponse chat(String systemPrompt, List<ChatMessage> messages) {
        try {
            String url = properties.getBaseUrl() + "/api/chat";

            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("model", currentModel);
            requestBody.put("stream", false);

            List<Map<String, String>> ollamaMessages = new ArrayList<>();

            if (systemPrompt != null && !systemPrompt.isEmpty()) {
                Map<String, String> sysMsg = new HashMap<>();
                sysMsg.put("role", "system");
                sysMsg.put("content", systemPrompt);
                ollamaMessages.add(sysMsg);

                // ⭐ Debug 日志：系统提示
                log.debug("📤 [LLM Request] System Prompt:\n{}", systemPrompt);
            }

            for (ChatMessage message : messages) {
                Map<String, String> msg = new HashMap<>();
                msg.put("role", message.getRole());
                msg.put("content", message.getContent());
                ollamaMessages.add(msg);

                // ⭐ Debug 日志：消息完整内容（不截断）
                log.debug("📤 [LLM Request] Message [{}]:\n{}",
                    message.getRole(),
                    message.getContent()
                );
            }

            requestBody.put("messages", ollamaMessages);

            // ⭐ Debug 日志：完整请求元信息
            log.debug("📤 [LLM Request] URL: {}, Model: {}, Messages Count: {}",
                url, currentModel, ollamaMessages.size());

            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            HttpEntity<Map<String, Object>> entity = new HttpEntity<>(requestBody, headers);

            long startTime = System.currentTimeMillis();
            ResponseEntity<Map> response = restTemplate.postForEntity(url, entity, Map.class);
            long duration = System.currentTimeMillis() - startTime;

            if (response.getStatusCode() == HttpStatus.OK && response.getBody() != null) {
                Map<String, Object> body = response.getBody();
                Map<String, Object> message = (Map<String, Object>) body.get("message");
                String content = (String) message.get("content");

                // ⭐ Debug 日志：LLM 响应
                log.debug("📥 [LLM Response] Duration: {}ms, Content Length: {} chars",
                    duration, content != null ? content.length() : 0);
                log.debug("📥 [LLM Response] Content:\n{}", content);

                return AIResponse.builder()
                        .text(content)
                        .model(currentModel)
                        .finishReason("stop")
                        .success(true)
                        .build();
            }

            return AIResponse.builder()
                    .text("")
                    .success(false)
                    .error("Failed to chat")
                    .build();
        } catch (Exception e) {
            log.error("Failed to chat", e);
            return AIResponse.builder()
                    .text("")
                    .success(false)
                    .error(e.getMessage())
                    .build();
        }
    }

    @Override
    public String chat(String userMessage) {
        List<ChatMessage> messages = new ArrayList<>();
        messages.add(ChatMessage.builder()
                .role("user")
                .content(userMessage)
                .build());
        AIResponse response = chat(messages);
        return response.getText();
    }

    @Override
    @Deprecated
    public void chatStream(List<ChatMessage> messages, java.util.function.Consumer<String> callback) {
        log.warn("Stream chat not fully implemented, falling back to sync");
        AIResponse response = chat(messages);
        callback.accept(response.getText());
    }

    @Override
    public Flux<String> chatFlux(List<ChatMessage> messages) {
        return chatFlux(null, messages);
    }

    @Override
    public Flux<String> chatFlux(String systemPrompt, List<ChatMessage> messages) {
        // 简化实现：返回完整文本作为单个元素
        return Flux.defer(() -> {
            AIResponse response = chat(systemPrompt, messages);
            return Flux.just(response.getText());
        });
    }

    @Override
    public Flux<AIResponse> chatFluxResponse(List<ChatMessage> messages) {
        return Flux.defer(() -> {
            AIResponse response = chat(messages);
            return Flux.just(response);
        });
    }

    // ========== Model Management ==========
    public List<ModelInfo> listModels() {
        try {
            String url = properties.getBaseUrl() + "/api/tags";
            ResponseEntity<Map> response = restTemplate.getForEntity(url, Map.class);

            if (response.getStatusCode() == HttpStatus.OK && response.getBody() != null) {
                List<Map<String, Object>> models = (List<Map<String, Object>>) response.getBody().get("models");

                List<ModelInfo> modelInfoList = new ArrayList<>();
                if (models != null) {
                    for (Map<String, Object> model : models) {
                        ModelInfo info = ModelInfo.builder()
                                .name((String) model.get("name"))
                                .size(String.valueOf(model.get("size")))
                                .build();
                        modelInfoList.add(info);
                    }
                }
                return modelInfoList;
            }

            return new ArrayList<>();
        } catch (Exception e) {
            log.error("Failed to list models", e);
            return new ArrayList<>();
        }
    }

    @Override
    public String getCurrentModel() {
        return currentModel;
    }

    @Override
    public void setCurrentModel(String modelName) {
        this.currentModel = modelName;
        log.info("Switched to model: {}", modelName);
    }

    @Override
    public boolean isModelAvailable(String modelName) {
        List<ModelInfo> models = listModels();
        return models.stream().anyMatch(model -> model.getName().equals(modelName));
    }

    @Override
    public boolean isHealthy() {
        try {
            String url = properties.getBaseUrl() + "/api/tags";
            ResponseEntity<String> response = restTemplate.getForEntity(url, String.class);
            return response.getStatusCode() == HttpStatus.OK;
        } catch (Exception e) {
            log.error("Health check failed", e);
            return false;
        }
    }

    @Override
    public Map<String, Object> getStatus() {
        Map<String, Object> status = new HashMap<>();
        status.put("service", "local-ollama");
        status.put("baseUrl", properties.getBaseUrl());
        status.put("currentModel", currentModel);
        status.put("healthy", isHealthy());
        status.put("timestamp", System.currentTimeMillis());
        return status;
    }

    // ========== Vision Multi-Modal (Ollama 离线图像识别) ==========

    /**
     * 分析单张图片（Ollama Vision 模型）
     * 支持的模型：llava, bakllava, llava-phi3, llava-llama3
     */
    @Override
    public String analyzeImage(byte[] imageData, String prompt) {
        List<byte[]> images = new ArrayList<>();
        images.add(imageData);
        return analyzeImages(images, prompt);
    }

    /**
     * 分析多张图片（Ollama Vision 模型）
     * 使用离线的 LLaVA 等多模态模型进行图像理解
     */
    @Override
    public String analyzeImages(List<byte[]> imagesData, String prompt) {
        try {
            log.info("🔍 [Ollama Vision] 离线分析 {} 张图片", imagesData.size());

            // 创建多模态消息
            ChatMessage message = ChatMessage.userWithImages(prompt, imagesData);

            // 使用chatWithVision方法
            List<ChatMessage> messages = new ArrayList<>();
            messages.add(message);

            AIResponse response = chatWithVision(messages);

            if (response.isSuccess()) {
                log.info("✅ [Ollama Vision] 分析完成，内容长度: {} chars", response.getText().length());
                return response.getText();
            } else {
                log.error("❌ [Ollama Vision] 分析失败: {}", response.getError());
                return "[Ollama Vision分析失败: " + response.getError() + "]";
            }
        } catch (Exception e) {
            log.error("❌ [Ollama Vision] 分析异常", e);
            return "[Ollama Vision分析异常: " + e.getMessage() + "]";
        }
    }

    /**
     * 多模态对话（Ollama Vision 支持）
     *
     * Ollama API 格式：
     * {
     *   "model": "llava",
     *   "messages": [
     *     {
     *       "role": "user",
     *       "content": "What's in this image?",
     *       "images": ["base64_encoded_image"]
     *     }
     *   ]
     * }
     */
    @Override
    public AIResponse chatWithVision(List<ChatMessage> messages) {
        try {
            log.debug("🎨 [Ollama Vision] 发送多模态对话请求");

            String url = properties.getBaseUrl() + "/api/chat";

            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("model", currentModel);
            requestBody.put("stream", false);

            // 转换消息格式（Ollama Vision 格式）
            List<Map<String, Object>> ollamaMessages = new ArrayList<>();
            for (ChatMessage msg : messages) {
                Map<String, Object> ollamaMsg = new HashMap<>();
                ollamaMsg.put("role", msg.getRole());

                // 如果有多模态内容
                if (msg.getContentParts() != null && !msg.getContentParts().isEmpty()) {
                    // 提取文本内容
                    StringBuilder textContent = new StringBuilder();
                    List<String> base64Images = new ArrayList<>();

                    for (ChatMessage.ContentPart part : msg.getContentParts()) {
                        if ("text".equals(part.getType())) {
                            if (textContent.length() > 0) {
                                textContent.append(" ");
                            }
                            textContent.append(part.getText());
                        } else if ("image_url".equals(part.getType())) {
                            // 提取 base64 图片数据
                            String imageUrl = part.getImageUrl().getUrl();
                            if (imageUrl.startsWith("data:image/")) {
                                // 提取 base64 部分: data:image/jpeg;base64,xxx
                                int commaIndex = imageUrl.indexOf(',');
                                if (commaIndex > 0) {
                                    String base64Data = imageUrl.substring(commaIndex + 1);
                                    base64Images.add(base64Data);
                                }
                            }
                        }
                    }

                    ollamaMsg.put("content", textContent.toString());
                    if (!base64Images.isEmpty()) {
                        ollamaMsg.put("images", base64Images);
                    }
                } else {
                    // 普通文本消息
                    ollamaMsg.put("content", msg.getContent());
                }

                ollamaMessages.add(ollamaMsg);
            }

            requestBody.put("messages", ollamaMessages);

            // 发送请求
            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            HttpEntity<Map<String, Object>> entity = new HttpEntity<>(requestBody, headers);

            log.debug("🌐 [Ollama Vision] 发送到: {}", url);
            ResponseEntity<Map> responseEntity = restTemplate.exchange(
                    url,
                    HttpMethod.POST,
                    entity,
                    Map.class
            );

            Map<String, Object> body = responseEntity.getBody();
            if (body == null) {
                throw new RuntimeException("Empty response body");
            }

            // 解析 Ollama 响应格式
            Map<String, Object> messageObj = (Map<String, Object>) body.get("message");
            if (messageObj != null) {
                String content = (String) messageObj.get("content");

                return AIResponse.builder()
                        .text(content)
                        .model(currentModel)
                        .finishReason("stop")
                        .success(true)
                        .build();
            }

            throw new RuntimeException("Invalid response format");

        } catch (Exception e) {
            log.error("❌ [Ollama Vision] 失败", e);
            return AIResponse.builder()
                    .text("")
                    .success(false)
                    .error(e.getMessage())
                    .build();
        }
    }
}

