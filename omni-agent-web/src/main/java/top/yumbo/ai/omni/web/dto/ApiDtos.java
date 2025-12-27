package top.yumbo.ai.omni.web.dto;

import lombok.Data;
import top.yumbo.ai.omni.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.rag.model.Document;

import java.util.List;

/**
 * Web API 请求和响应 DTOs
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
public class ApiDtos {

    /**
     * 简单对话请求
     */
    @Data
    public static class ChatRequest {
        private String message;
    }

    /**
     * 流式对话请求
     */
    @Data
    public static class StreamChatRequest {
        private String message;
        private String systemPrompt;
        private List<ChatMessage> history;
    }

    /**
     * 高级对话请求
     */
    @Data
    public static class AdvancedChatRequest {
        private String systemPrompt;
        private List<ChatMessage> messages;
    }

    /**
     * 文本生成请求
     */
    @Data
    public static class GenerateRequest {
        private String prompt;
        private Float temperature;
        private Integer maxTokens;
    }

    /**
     * RAG+AI组合查询请求
     */
    @Data
    public static class RagChatRequest {
        private String question;
        private Integer topK;
    }

    /**
     * 智能问答请求
     */
    @Data
    public static class QuestionRequest {
        private String question;
        private String knowledgeMode; // none, rag, role, intelligent
        private String roleName;
        private String hopeSessionId;  // 用于多轮对话
        private String userId;  // 用于智能问答模式
    }

    /**
     * HOPE查询请求
     */
    @Data
    public static class HOPEQueryRequest {
        private String question;
        private String sessionId;
    }

    /**
     * 批量索引请求
     */
    @Data
    public static class BatchIndexRequest {
        private List<Document> documents;
    }

    /**
     * 对话历史实体
     */
    @Data
    public static class ConversationHistory {
        private String userId;
        private String question;
        private String answer;
        private long timestamp;
    }
}






