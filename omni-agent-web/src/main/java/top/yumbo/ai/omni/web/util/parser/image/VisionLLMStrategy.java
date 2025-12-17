package top.yumbo.ai.omni.web.util.parser.image;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import lombok.extern.slf4j.Slf4j;
import okhttp3.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Base64;
import java.util.concurrent.TimeUnit;

/**
 * Vision LLM 策略
 * (Vision LLM Strategy)
 *
 * <p>通用的多模态视觉语言模型接口，支持：</p>
 * <ul>
 *   <li>千问 VL（qwen-vl-plus）</li>
 *   <li>DeepSeek VL（deepseek-vl）</li>
 *   <li>OpenAI GPT-4V</li>
 *   <li>其他兼容 OpenAI 格式的 Vision API</li>
 * </ul>
 *
 * <p>配置示例：</p>
 * <pre>
 * # 千问 VL
 * VisionLLMStrategy strategy = new VisionLLMStrategy(
 *     "your-api-key",
 *     "qwen-vl-plus",
 *     "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions"
 * );
 *
 * # DeepSeek VL
 * VisionLLMStrategy strategy = new VisionLLMStrategy(
 *     "your-api-key",
 *     "deepseek-vl",
 *     "https://api.deepseek.com/v1/chat/completions"
 * );
 * </pre>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class VisionLLMStrategy implements ImageContentExtractorStrategy {

    private final String apiKey;
    private final String model;
    private final String apiEndpoint;
    private final OkHttpClient httpClient;
    private final ObjectMapper objectMapper;
    private final String systemPrompt;
    private boolean available = false;

    // 默认配置
    private static final String DEFAULT_API_ENDPOINT = "https://api.openai.com/v1/chat/completions";
    private static final String DEFAULT_MODEL = "gpt-4o";
    private static final int DEFAULT_TIMEOUT = 120;
    private static final MediaType JSON = MediaType.parse("application/json; charset=utf-8");

    // 默认提示词：精简模式
    private static final String DEFAULT_SYSTEM_PROMPT =
            "请分析这张图片并提取其中的关键信息。" +
            "如果图片包含文字，请完整准确地提取所有文字内容。" +
            "如果是图表或示意图，请描述其主要内容和含义。" +
            "保持输出简洁，只提取核心信息。";

    /**
     * 构造函数
     *
     * @param apiKey      API密钥
     * @param model       模型名称（如 qwen-vl-plus, deepseek-vl, gpt-4o）
     * @param apiEndpoint API端点
     */
    public VisionLLMStrategy(String apiKey, String model, String apiEndpoint) {
        this(apiKey, model, apiEndpoint, DEFAULT_SYSTEM_PROMPT);
    }

    /**
     * 构造函数（带自定义提示词）
     *
     * @param apiKey       API密钥
     * @param model        模型名称
     * @param apiEndpoint  API端点
     * @param systemPrompt 系统提示词
     */
    public VisionLLMStrategy(String apiKey, String model, String apiEndpoint, String systemPrompt) {
        this.apiKey = apiKey;
        this.model = model != null && !model.isEmpty() ? model : DEFAULT_MODEL;
        this.apiEndpoint = apiEndpoint != null && !apiEndpoint.isEmpty() ? apiEndpoint : DEFAULT_API_ENDPOINT;
        this.systemPrompt = systemPrompt != null && !systemPrompt.isEmpty() ? systemPrompt : DEFAULT_SYSTEM_PROMPT;

        this.httpClient = new OkHttpClient.Builder()
                .connectTimeout(DEFAULT_TIMEOUT, TimeUnit.SECONDS)
                .readTimeout(DEFAULT_TIMEOUT, TimeUnit.SECONDS)
                .writeTimeout(DEFAULT_TIMEOUT, TimeUnit.SECONDS)
                .build();

        this.objectMapper = new ObjectMapper();

        checkAvailability();
    }

    /**
     * 从环境变量创建
     */
    public static VisionLLMStrategy fromEnv() {
        String apiKey = System.getenv("VISION_LLM_API_KEY");
        if (apiKey == null || apiKey.isEmpty()) {
            apiKey = System.getenv("AI_API_KEY");
        }

        String model = System.getenv("VISION_LLM_MODEL");
        if (model == null || model.isEmpty()) {
            model = "qwen-vl-plus";
        }

        String endpoint = System.getenv("VISION_LLM_ENDPOINT");
        if (endpoint == null || endpoint.isEmpty()) {
            endpoint = "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions";
        }

        return new VisionLLMStrategy(apiKey, model, endpoint);
    }

    /**
     * 从配置创建（读取 application.yml 中的 vision-llm 配置）
     *
     * @param apiKey   API密钥
     * @param model    模型名称
     * @param endpoint API端点
     * @return VisionLLMStrategy 实例
     */
    public static VisionLLMStrategy fromConfig(String apiKey, String model, String endpoint) {
        if (apiKey == null || apiKey.isEmpty()) {
            log.warn("Vision LLM API Key 未配置");
            return null;
        }
        return new VisionLLMStrategy(apiKey, model, endpoint);
    }

    private void checkAvailability() {
        if (apiKey == null || apiKey.isEmpty()) {
            available = false;
            log.warn("Vision LLM 不可用：未配置 API Key");
            log.info("提示：设置环境变量 AI_API_KEY 或 VISION_LLM_API_KEY 以启用图片内容提取");
            return;
        }

        available = true;
        log.info("Vision LLM 服务已启用");
        log.info("  模型: {}", model);
        log.info("  端点: {}", apiEndpoint);
    }

    @Override
    public String extractContent(InputStream imageStream, String imageName) {
        if (!available) {
            return String.format("[图片: %s - Vision LLM 未启用]", imageName);
        }

        // 检查图片格式
        if (!isSupportedImageFormat(imageName)) {
            log.warn("不支持的图片格式: {}", imageName);
            return String.format("[图片: %s - 不支持的格式]", imageName);
        }

        try {
            log.debug("使用 Vision LLM 处理图片: {}", imageName);

            // 1. 读取图片并转为 base64
            byte[] imageBytes = imageStream.readAllBytes();
            String base64Image = Base64.getEncoder().encodeToString(imageBytes);

            // 2. 调用 Vision API
            String result = callVisionAPI(base64Image, imageName);

            log.info("成功提取图片内容: {}, 长度: {} 字符", imageName, result.length());
            return result;

        } catch (Exception e) {
            log.error("Vision LLM 提取失败: {}", imageName, e);
            return String.format("[图片内容提取失败: %s]", imageName);
        }
    }

    @Override
    public String extractContent(File imageFile) {
        try (FileInputStream fis = new FileInputStream(imageFile)) {
            return extractContent(fis, imageFile.getName());
        } catch (IOException e) {
            log.error("读取图片文件失败: {}", imageFile.getName(), e);
            return String.format("[图片读取失败: %s]", imageFile.getName());
        }
    }

    /**
     * 调用 Vision API（OpenAI Chat Completions 兼容格式）
     */
    private String callVisionAPI(String base64Image, String imageName) throws Exception {
        // 构建请求 JSON
        ObjectNode requestJson = objectMapper.createObjectNode();
        requestJson.put("model", model);
        requestJson.put("max_tokens", 1000);
        requestJson.put("temperature", 0.3);

        // 构建 messages
        ArrayNode messages = objectMapper.createArrayNode();

        // System message
        ObjectNode systemMessage = objectMapper.createObjectNode();
        systemMessage.put("role", "system");
        systemMessage.put("content", systemPrompt);
        messages.add(systemMessage);

        // User message (包含图片)
        ObjectNode userMessage = objectMapper.createObjectNode();
        userMessage.put("role", "user");

        ArrayNode content = objectMapper.createArrayNode();

        // 文本部分
        ObjectNode textPart = objectMapper.createObjectNode();
        textPart.put("type", "text");
        textPart.put("text", "请分析这张图片");
        content.add(textPart);

        // 图片部分
        ObjectNode imagePart = objectMapper.createObjectNode();
        imagePart.put("type", "image_url");
        ObjectNode imageUrl = objectMapper.createObjectNode();
        imageUrl.put("url", "data:image/jpeg;base64," + base64Image);
        imagePart.set("image_url", imageUrl);
        content.add(imagePart);

        userMessage.set("content", content);
        messages.add(userMessage);

        requestJson.set("messages", messages);

        // 发送请求
        String requestBody = objectMapper.writeValueAsString(requestJson);
        log.debug("Vision API 请求: model={}, endpoint={}", model, apiEndpoint);

        Request request = new Request.Builder()
                .url(apiEndpoint)
                .post(RequestBody.create(requestBody, JSON))
                .addHeader("Authorization", "Bearer " + apiKey)
                .addHeader("Content-Type", "application/json")
                .build();

        try (Response response = httpClient.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                String errorBody = response.body() != null ? response.body().string() : "Unknown error";
                log.error("Vision API 调用失败: code={}, body={}", response.code(), errorBody);
                throw new IOException("Vision API 返回错误: " + response.code());
            }

            String responseBody = response.body().string();
            log.debug("Vision API 响应长度: {} bytes", responseBody.length());

            // 解析响应
            JsonNode responseJson = objectMapper.readTree(responseBody);
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

            log.warn("Vision API 响应格式异常: {}", responseBody);
            return String.format("[图片: %s - API 响应格式异常]", imageName);
        }
    }

    /**
     * 检查图片格式是否支持
     */
    private boolean isSupportedImageFormat(String filename) {
        String extension = getFileExtension(filename).toLowerCase();
        return extension.matches("jpg|jpeg|png|gif|bmp|webp");
    }

    /**
     * 获取文件扩展名
     */
    private String getFileExtension(String filename) {
        int lastDot = filename.lastIndexOf('.');
        if (lastDot > 0 && lastDot < filename.length() - 1) {
            return filename.substring(lastDot + 1);
        }
        return "";
    }

    @Override
    public String getStrategyName() {
        return "Vision LLM (" + model + ")";
    }

    @Override
    public boolean isAvailable() {
        return available;
    }
}

