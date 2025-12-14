package top.yumbo.ai.ai.api;

import reactor.core.publisher.Flux;
import top.yumbo.ai.ai.api.model.AIRequest;
import top.yumbo.ai.ai.api.model.AIResponse;
import top.yumbo.ai.ai.api.model.ChatMessage;
import top.yumbo.ai.ai.api.model.ModelInfo;

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

