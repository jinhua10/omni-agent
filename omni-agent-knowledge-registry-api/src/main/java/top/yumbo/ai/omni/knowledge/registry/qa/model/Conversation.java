package top.yumbo.ai.omni.knowledge.registry.qa.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 对话会话
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Conversation {

    /**
     * 对话ID
     */
    private String conversationId;

    /**
     * 用户ID
     */
    private String userId;

    /**
     * 消息列表
     */
    @Builder.Default
    private List<Message> messages = new ArrayList<>();

    /**
     * 收集的知识
     */
    @Builder.Default
    private Map<String, String> collectedKnowledge = new HashMap<>();

    /**
     * 缺失信息
     */
    @Builder.Default
    private List<String> missingInformation = new ArrayList<>();

    /**
     * 创建时间
     */
    @Builder.Default
    private LocalDateTime createdAt = LocalDateTime.now();

    /**
     * 最后更新时间
     */
    @Builder.Default
    private LocalDateTime lastUpdatedAt = LocalDateTime.now();

    /**
     * 添加消息
     */
    public void addMessage(Message message) {
        this.messages.add(message);
        this.lastUpdatedAt = LocalDateTime.now();
    }

    /**
     * 添加收集的知识
     */
    public void addKnowledge(String key, String value) {
        this.collectedKnowledge.put(key, value);
        this.lastUpdatedAt = LocalDateTime.now();
    }

    /**
     * 添加缺失信息
     */
    public void addMissingInfo(String info) {
        if (!this.missingInformation.contains(info)) {
            this.missingInformation.add(info);
        }
        this.lastUpdatedAt = LocalDateTime.now();
    }
}

