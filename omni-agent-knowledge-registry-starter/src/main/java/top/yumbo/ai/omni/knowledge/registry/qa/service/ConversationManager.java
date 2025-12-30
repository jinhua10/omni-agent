package top.yumbo.ai.omni.knowledge.registry.qa.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.qa.model.Conversation;
import top.yumbo.ai.omni.knowledge.registry.qa.model.Message;

import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 对话管理器
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class ConversationManager {

    private final Map<String, Conversation> conversations = new ConcurrentHashMap<>();

    /**
     * 创建新对话
     */
    public Conversation createConversation(String userId) {
        String conversationId = UUID.randomUUID().toString();
        Conversation conversation = Conversation.builder()
                .conversationId(conversationId)
                .userId(userId != null ? userId : "anonymous")
                .build();

        conversations.put(conversationId, conversation);
        log.info("创建新对话: conversationId={}, userId={}", conversationId, userId);

        return conversation;
    }

    /**
     * 获取对话
     */
    public Conversation getConversation(String conversationId) {
        return conversations.get(conversationId);
    }

    /**
     * 获取或创建对话
     */
    public Conversation getOrCreateConversation(String conversationId, String userId) {
        if (conversationId == null || conversationId.isEmpty()) {
            return createConversation(userId);
        }

        Conversation conversation = conversations.get(conversationId);
        if (conversation == null) {
            conversation = Conversation.builder()
                    .conversationId(conversationId)
                    .userId(userId != null ? userId : "anonymous")
                    .build();
            conversations.put(conversationId, conversation);
        }

        return conversation;
    }

    /**
     * 添加消息到对话
     */
    public void addMessage(String conversationId, Message message) {
        Conversation conversation = conversations.get(conversationId);
        if (conversation != null) {
            conversation.addMessage(message);
        }
    }

    /**
     * 获取对话历史
     */
    public List<Message> getConversationHistory(String conversationId) {
        Conversation conversation = conversations.get(conversationId);
        return conversation != null ? conversation.getMessages() : List.of();
    }

    /**
     * 格式化对话历史（用于AI上下文）
     */
    public String formatConversationHistory(String conversationId) {
        List<Message> messages = getConversationHistory(conversationId);
        if (messages.isEmpty()) {
            return "（无对话历史）";
        }

        StringBuilder sb = new StringBuilder();
        for (Message msg : messages) {
            sb.append(String.format("%s: %s\n",
                    msg.getRole().equals("user") ? "用户" : "助手",
                    msg.getContent()));
        }

        return sb.toString();
    }

    /**
     * 清理对话（可选）
     */
    public void removeConversation(String conversationId) {
        conversations.remove(conversationId);
        log.info("删除对话: conversationId={}", conversationId);
    }
}

