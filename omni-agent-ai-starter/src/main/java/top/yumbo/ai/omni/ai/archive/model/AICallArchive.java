package top.yumbo.ai.omni.ai.archive.model;

import lombok.Builder;
import lombok.Data;
import top.yumbo.ai.omni.ai.api.model.ChatMessage;

import java.time.Instant;
import java.util.List;
import java.util.Map;

/**
 * AI调用归档记录
 *
 * <p>用于记录每次AI调用的完整信息，支持知识网络学习</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
public class AICallArchive {

    /**
     * 归档ID
     */
    private String archiveId;

    /**
     * 调用时间戳
     */
    private Long timestamp;

    /**
     * 调用时间（ISO格式）
     */
    private String callTime;

    /**
     * 服务类型（ollama, online-api, onnx等）
     */
    private String serviceType;

    /**
     * 模型名称
     */
    private String model;

    /**
     * 调用类型（chat, generate, vision等）
     */
    private CallType callType;

    /**
     * 是否流式调用
     */
    private Boolean isStream;

    /**
     * 系统提示词
     */
    private String systemPrompt;

    /**
     * 对话消息列表
     */
    private List<ChatMessage> messages;

    /**
     * 单独的用户输入（简单调用）
     */
    private String userInput;

    /**
     * AI响应文本
     */
    private String responseText;

    /**
     * 响应是否成功
     */
    private Boolean success;

    /**
     * 错误信息（如果失败）
     */
    private String error;

    /**
     * Token使用统计
     */
    private TokenUsage tokenUsage;

    /**
     * 调用耗时（毫秒）
     */
    private Long durationMs;

    /**
     * 额外元数据
     */
    private Map<String, Object> metadata;

    /**
     * 关联的文档ID（如果有）
     */
    private String relatedDocumentId;

    /**
     * 关联的域ID（知识网络）
     */
    private String relatedDomainId;

    /**
     * 调用来源（controller, service等）
     */
    private String source;

    /**
     * 调用类型枚举
     */
    public enum CallType {
        /** 对话 */
        CHAT,
        /** 文本生成 */
        GENERATE,
        /** 多模态对话 */
        VISION,
        /** Embedding */
        EMBEDDING,
        /** 其他 */
        OTHER
    }

    /**
     * Token使用统计
     */
    @Data
    @Builder
    public static class TokenUsage {
        /** 提示词Token数 */
        private Integer promptTokens;
        /** 完成Token数 */
        private Integer completionTokens;
        /** 总Token数 */
        private Integer totalTokens;
    }

    /**
     * 创建归档ID
     */
    public static String generateArchiveId() {
        return "archive_" + System.currentTimeMillis() + "_" +
               Integer.toHexString((int)(Math.random() * 0x10000));
    }

    /**
     * 获取ISO格式时间
     */
    public static String getCurrentTimeISO() {
        return Instant.now().toString();
    }
}

