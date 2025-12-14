package top.yumbo.ai.ai.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;

import java.io.Serial;
import java.io.Serializable;

/**
 * 对话消息模型
 * (Chat Message Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ChatMessage implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 消息角色
     */
    @NotBlank(message = "角色不能为空")
    private String role;

    /**
     * 消息内容
     */
    @NotBlank(message = "内容不能为空")
    private String content;

    /**
     * 消息名称（可选）
     */
    private String name;

    /**
     * 时间戳
     */
    private Long timestamp;

    /**
     * 角色枚举
     */
    public static class Role {
        public static final String SYSTEM = "system";
        public static final String USER = "user";
        public static final String ASSISTANT = "assistant";
        public static final String FUNCTION = "function";
    }

    /**
     * 创建系统消息
     */
    public static ChatMessage system(String content) {
        return ChatMessage.builder()
                .role(Role.SYSTEM)
                .content(content)
                .timestamp(System.currentTimeMillis())
                .build();
    }

    /**
     * 创建用户消息
     */
    public static ChatMessage user(String content) {
        return ChatMessage.builder()
                .role(Role.USER)
                .content(content)
                .timestamp(System.currentTimeMillis())
                .build();
    }

    /**
     * 创建助手消息
     */
    public static ChatMessage assistant(String content) {
        return ChatMessage.builder()
                .role(Role.ASSISTANT)
                .content(content)
                .timestamp(System.currentTimeMillis())
                .build();
    }
}

