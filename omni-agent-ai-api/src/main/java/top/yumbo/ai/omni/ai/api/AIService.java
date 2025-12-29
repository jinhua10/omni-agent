package top.yumbo.ai.omni.ai.api;

import reactor.core.publisher.Flux;
import top.yumbo.ai.omni.ai.api.model.AIRequest;
import top.yumbo.ai.omni.ai.api.model.AIResponse;
import top.yumbo.ai.omni.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.ai.api.model.ModelInfo;

import java.util.List;

/**
 * AI 服务核心接口
 * (AI Service Interface)
 *
 * <p>用于LLM推理、对话生成等AI功能</p>
 * <p>支持多种后端: Local Ollama, Remote Ollama, Online API (OpenAI, Claude, etc.)</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface AIService {

    // ========== 文本生成 (Text Generation) ==========

    /**
     * 生成文本（同步）
     * @param request AI请求
     * @return AI响应
     */
    AIResponse generate(AIRequest request);

    /**
     * 简单文本生成
     * @param prompt 提示词
     * @return 生成的文本
     */
    String generate(String prompt);

    /**
     * 流式文本生成（回调方式）
     * @param request AI请求
     * @param callback 回调函数，接收每个生成的token
     * @deprecated 推荐使用 generateFlux 方法
     */
    @Deprecated
    void generateStream(AIRequest request, java.util.function.Consumer<String> callback);

    /**
     * 流式文本生成（Flux 方式）
     * @param request AI请求
     * @return Flux 流，每个元素是生成的 token 片段
     */
    Flux<String> generateFlux(AIRequest request);

    /**
     * 流式文本生成（Flux 方式，返回完整响应）
     * @param request AI请求
     * @return Flux 流，每个元素是 AIResponse（包含累积的文本）
     */
    Flux<AIResponse> generateFluxResponse(AIRequest request);

    // ========== 对话 (Chat) ==========

    /**
     * 对话生成（支持多轮对话）
     * @param messages 对话历史
     * @return AI响应
     */
    AIResponse chat(List<ChatMessage> messages);

    /**
     * 对话生成（带系统提示）
     * @param systemPrompt 系统提示
     * @param messages 对话历史
     * @return AI响应
     */
    AIResponse chat(String systemPrompt, List<ChatMessage> messages);

    /**
     * 简单对话
     * @param userMessage 用户消息
     * @return AI回复
     */
    String chat(String userMessage);

    /**
     * 流式对话（回调方式）
     * @param messages 对话历史
     * @param callback 回调函数
     * @deprecated 推荐使用 chatFlux 方法
     */
    @Deprecated
    void chatStream(List<ChatMessage> messages, java.util.function.Consumer<String> callback);

    /**
     * 流式对话（Flux 方式）
     * @param messages 对话历史
     * @return Flux 流，每个元素是生成的 token 片段
     */
    Flux<String> chatFlux(List<ChatMessage> messages);

    /**
     * 流式对话（Flux 方式，带系统提示）
     * @param systemPrompt 系统提示
     * @param messages 对话历史
     * @return Flux 流，每个元素是生成的 token 片段
     */
    Flux<String> chatFlux(String systemPrompt, List<ChatMessage> messages);

    /**
     * 流式对话（Flux 方式，返回完整响应）
     * @param messages 对话历史
     * @return Flux 流，每个元素是 AIResponse（包含累积的文本）
     */
    Flux<AIResponse> chatFluxResponse(List<ChatMessage> messages);

    // ========== 模型管理 (Model Management) ==========

    /**
     * 列出可用的模型
     * @return 模型列表
     */
    List<ModelInfo> listModels();

    /**
     * 获取当前使用的模型
     * @return 模型名称
     */
    String getCurrentModel();

    /**
     * 设置当前使用的模型
     * @param modelName 模型名称
     */
    void setCurrentModel(String modelName);

    /**
     * 检查模型是否可用
     * @param modelName 模型名称
     * @return 是否可用
     */
    boolean isModelAvailable(String modelName);

    // ========== Vision 多模态 (Vision Multi-Modal) ==========

    /**
     * 分析单张图片（Vision LLM）
     * @param imageData 图片数据（Base64编码或字节数组）
     * @param prompt 提示词
     * @return AI分析结果
     */
    default String analyzeImage(byte[] imageData, String prompt) {
        throw new UnsupportedOperationException("当前AI服务不支持图片分析，请使用支持Vision的模型");
    }

    /**
     * 分析多张图片（Vision LLM）
     * 用于理解包含多张图片的复杂内容（如PPT幻灯片、流程图等）
     *
     * @param imagesData 多张图片数据列表
     * @param prompt 提示词
     * @return AI分析结果
     */
    default String analyzeImages(List<byte[]> imagesData, String prompt) {
        throw new UnsupportedOperationException("当前AI服务不支持多图片分析，请使用支持Vision的模型");
    }

    /**
     * 多模态对话（支持文本+图片）
     * 用于复杂的多模态理解场景
     *
     * @param messages 对话历史（可能包含图片）
     * @return AI响应
     */
    default AIResponse chatWithVision(List<ChatMessage> messages) {
        throw new UnsupportedOperationException("当前AI服务不支持Vision对话，请使用支持Vision的模型");
    }

    /**
     * 流式多模态对话（Flux 方式）
     *
     * <p>用于 UI 边生成边展示（例如：文档可视化解析、PPT/流程图理解）。</p>
     * <p>默认实现：不支持（保持向后兼容）。</p>
     *
     * @param messages 对话历史（可能包含图片）
     * @return Flux 流，每个元素是生成的 token 片段
     */
    default Flux<String> chatWithVisionFlux(List<ChatMessage> messages) {
        throw new UnsupportedOperationException("当前AI服务不支持Vision流式对话，请使用支持Vision的模型");
    }

    /**
     * 流式多模态对话（Flux 方式，返回累积响应）
     *
     * @param messages 对话历史（可能包含图片）
     * @return Flux 流，每个元素是 AIResponse（包含累积的文本）
     */
    default Flux<AIResponse> chatWithVisionFluxResponse(List<ChatMessage> messages) {
        // 默认实现：基于 chatWithVisionFlux 进行累积
        return chatWithVisionFlux(messages)
                .scan(
                        AIResponse.builder().text("").success(true).build(),
                        (acc, chunk) -> AIResponse.builder()
                                .text((acc.getText() == null ? "" : acc.getText()) + chunk)
                                .success(true)
                                .build()
                )
                // scan 的第一个元素是初始值，过滤掉
                .skip(1);
    }

    // ========== 健康检查 (Health Check) ==========

    /**
     * 检查服务是否健康
     * @return 是否健康
     */
    boolean isHealthy();

    /**
     * 获取服务状态
     * @return 状态信息
     */
    java.util.Map<String, Object> getStatus();
}

