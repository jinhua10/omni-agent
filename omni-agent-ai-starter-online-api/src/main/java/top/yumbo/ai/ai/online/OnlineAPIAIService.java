package top.yumbo.ai.ai.online;

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
 * Online API AI 服务实现
 * (Online API AI Service Implementation)
 *
 * <p>
 * 特点 (Features):
 * - 支持多种在线 AI 服务
 * - OpenAI、Claude、通义千问等
 * - 最新模型支持
 * - 生产级可靠性
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - Online API Starter 实现
 */
@Slf4j
public class OnlineAPIAIService implements AIService {

    private final RestTemplate restTemplate;
    private final OnlineAPIProperties properties;
    private String currentModel;

    public OnlineAPIAIService(RestTemplate restTemplate, OnlineAPIProperties properties) {
        this.restTemplate = restTemplate;
        this.properties = properties;
        this.currentModel = properties.getDefaultModel();
        log.info("OnlineAPIAIService initialized with provider: {}, model: {}",
                properties.getProvider(), currentModel);
    }

    // ========== Text Generation ==========

    @Override
    public AIResponse generate(AIRequest request) {
        try {
            // 转换为 chat 格式（OpenAI 推荐使用 chat completion）
            List<ChatMessage> messages = new ArrayList<>();
            messages.add(ChatMessage.builder()
                    .role("user")
                    .content(request.getPrompt())
                    .build());

            return chat(messages);
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
        // 真实实现应该使用 WebClient 处理 SSE 流式响应
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

    @Override
    public AIResponse chat(List<ChatMessage> messages) {
        return chat(null, messages);
    }

    @Override
    public AIResponse chat(String systemPrompt, List<ChatMessage> messages) {
        try {
            String endpoint = getEndpoint();

            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("model", currentModel);

            // 构建消息列表
            List<Map<String, String>> apiMessages = new ArrayList<>();

            // 添加系统提示
            if (systemPrompt != null && !systemPrompt.isEmpty()) {
                Map<String, String> sysMsg = new HashMap<>();
                sysMsg.put("role", "system");
                sysMsg.put("content", systemPrompt);
                apiMessages.add(sysMsg);
            }

            // 添加对话历史
            for (ChatMessage message : messages) {
                Map<String, String> msg = new HashMap<>();
                msg.put("role", message.getRole());
                msg.put("content", message.getContent());
                apiMessages.add(msg);
            }

            requestBody.put("messages", apiMessages);
            requestBody.put("temperature", properties.getTemperature());
            requestBody.put("max_tokens", properties.getMaxTokens());
            requestBody.put("top_p", properties.getTopP());
            requestBody.put("stream", false);

            HttpHeaders headers = createHeaders();
            HttpEntity<Map<String, Object>> entity = new HttpEntity<>(requestBody, headers);

            ResponseEntity<Map> response = restTemplate.postForEntity(endpoint, entity, Map.class);

            if (response.getStatusCode() == HttpStatus.OK && response.getBody() != null) {
                return parseResponse(response.getBody());
            }

            return AIResponse.builder()
                    .text("")
                    .success(false)
                    .error("Failed to get response from API")
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
        // 真实实现应该使用 WebClient 处理 SSE 流式响应
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

    @Override
    public List<ModelInfo> listModels() {
        try {
            String url = getBaseEndpoint() + "/models";
            HttpHeaders headers = createHeaders();
            HttpEntity<Void> entity = new HttpEntity<>(headers);

            ResponseEntity<Map> response = restTemplate.exchange(url, HttpMethod.GET, entity, Map.class);

            if (response.getStatusCode() == HttpStatus.OK && response.getBody() != null) {
                List<Map<String, Object>> data = (List<Map<String, Object>>) response.getBody().get("data");

                List<ModelInfo> modelInfoList = new ArrayList<>();
                if (data != null) {
                    for (Map<String, Object> model : data) {
                        ModelInfo info = ModelInfo.builder()
                                .name((String) model.get("id"))
                                .size("unknown")
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

    // ========== Health Check ==========

    @Override
    public boolean isHealthy() {
        try {
            String url = getBaseEndpoint() + "/models";
            HttpHeaders headers = createHeaders();
            HttpEntity<Void> entity = new HttpEntity<>(headers);

            ResponseEntity<String> response = restTemplate.exchange(url, HttpMethod.GET, entity, String.class);
            return response.getStatusCode() == HttpStatus.OK;
        } catch (Exception e) {
            log.error("Health check failed", e);
            return false;
        }
    }

    @Override
    public Map<String, Object> getStatus() {
        Map<String, Object> status = new HashMap<>();
        status.put("service", "online-api");
        status.put("provider", properties.getProvider());
        status.put("endpoint", getBaseEndpoint());
        status.put("currentModel", currentModel);
        status.put("healthy", isHealthy());
        status.put("timestamp", System.currentTimeMillis());
        return status;
    }

    // ========== Helper Methods ==========

    /**
     * 获取API端点URL
     * 优先使用endpoint，如果没有配置则使用baseUrl（向后兼容）
     */
    private String getEndpoint() {
        String baseEndpoint = properties.getEndpoint();

        // 向后兼容：如果没有配置endpoint但配置了baseUrl，使用baseUrl
        if ((baseEndpoint == null || baseEndpoint.isEmpty()) &&
            properties.getBaseUrl() != null && !properties.getBaseUrl().isEmpty()) {
            log.warn("Using deprecated 'baseUrl' configuration. Please migrate to 'endpoint'");
            baseEndpoint = properties.getBaseUrl();
        }

        // 如果endpoint/baseUrl都没配置，使用默认值
        if (baseEndpoint == null || baseEndpoint.isEmpty()) {
            log.warn("No endpoint configured, using default qianwen endpoint");
            baseEndpoint = "https://dashscope.aliyuncs.com/api/v1";
        }

        return baseEndpoint + "/chat/completions";
    }

    /**
     * 获取基础端点URL（不含路径）
     */
    private String getBaseEndpoint() {
        String baseEndpoint = properties.getEndpoint();

        // 向后兼容
        if ((baseEndpoint == null || baseEndpoint.isEmpty()) &&
            properties.getBaseUrl() != null && !properties.getBaseUrl().isEmpty()) {
            baseEndpoint = properties.getBaseUrl();
        }

        if (baseEndpoint == null || baseEndpoint.isEmpty()) {
            baseEndpoint = "https://dashscope.aliyuncs.com/api/v1";
        }

        return baseEndpoint;
    }

    private HttpHeaders createHeaders() {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        String provider = properties.getProvider().toLowerCase();

        // 千问/通义 (Qianwen/Tongyi) 格式
        if ("qianwen".equals(provider) || "tongyi".equals(provider)) {
            headers.set("Authorization", "Bearer " + properties.getApiKey());
            headers.set("X-DashScope-SSE", "enable");  // 启用流式响应
        }
        // OpenAI 格式
        else if ("openai".equals(provider)) {
            headers.set("Authorization", "Bearer " + properties.getApiKey());
        }
        // Claude 格式
        else if ("claude".equals(provider)) {
            headers.set("x-api-key", properties.getApiKey());
            headers.set("anthropic-version", "2023-06-01");
        }
        // 智谱AI (Zhipu) 格式
        else if ("zhipu".equals(provider)) {
            headers.set("Authorization", "Bearer " + properties.getApiKey());
        }
        // 通用格式（默认）
        else {
            headers.set("Authorization", "Bearer " + properties.getApiKey());
        }

        return headers;
    }

    private AIResponse parseResponse(Map<String, Object> body) {
        try {
            List<Map<String, Object>> choices = (List<Map<String, Object>>) body.get("choices");
            if (choices == null || choices.isEmpty()) {
                return AIResponse.builder()
                        .text("")
                        .success(false)
                        .error("No choices in response")
                        .build();
            }

            Map<String, Object> firstChoice = choices.get(0);
            Map<String, Object> message = (Map<String, Object>) firstChoice.get("message");
            String content = (String) message.get("content");
            String finishReason = (String) firstChoice.get("finish_reason");

            // 提取 usage 信息
            Map<String, Object> usage = (Map<String, Object>) body.get("usage");
            Integer promptTokens = usage != null ? ((Number) usage.get("prompt_tokens")).intValue() : 0;
            Integer completionTokens = usage != null ? ((Number) usage.get("completion_tokens")).intValue() : 0;
            Integer totalTokens = usage != null ? ((Number) usage.get("total_tokens")).intValue() : 0;

            return AIResponse.builder()
                    .text(content)
                    .model(currentModel)
                    .finishReason(finishReason)
                    .promptTokens(promptTokens)
                    .completionTokens(completionTokens)
                    .totalTokens(totalTokens)
                    .success(true)
                    .build();
        } catch (Exception e) {
            log.error("Failed to parse response", e);
            return AIResponse.builder()
                    .text("")
                    .success(false)
                    .error("Failed to parse response: " + e.getMessage())
                    .build();
        }
    }
}

