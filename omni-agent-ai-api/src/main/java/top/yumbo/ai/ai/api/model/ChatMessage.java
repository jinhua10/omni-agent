package top.yumbo.ai.ai.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 对话消息模型（支持多模态）
 * (Chat Message Model - Multi-Modal Support)
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
     * 消息内容（纯文本）
     */
    private String content;

    /**
     * 多模态内容列表（文本+图片）
     * 如果此字段非空，则优先使用此字段而不是 content
     */
    private List<ContentPart> contentParts;

    /**
     * 消息名称（可选）
     */
    private String name;

    /**
     * 时间戳
     */
    private Long timestamp;

    /**
     * 内容部分（支持文本和图片）
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ContentPart {
        /**
         * 内容类型：text 或 image_url
         */
        private String type;

        /**
         * 文本内容（当 type=text 时）
         */
        private String text;

        /**
         * 图片URL（当 type=image_url 时）
         */
        private ImageUrl imageUrl;

        /**
         * 创建文本内容部分
         */
        public static ContentPart text(String text) {
            return ContentPart.builder()
                    .type("text")
                    .text(text)
                    .build();
        }

        /**
         * 创建图片内容部分（Base64）
         */
        public static ContentPart imageBase64(byte[] imageData) {
            String base64 = java.util.Base64.getEncoder().encodeToString(imageData);
            return ContentPart.builder()
                    .type("image_url")
                    .imageUrl(new ImageUrl("data:image/jpeg;base64," + base64))
                    .build();
        }

        /**
         * 创建图片内容部分（URL）
         */
        public static ContentPart imageUrl(String url) {
            return ContentPart.builder()
                    .type("image_url")
                    .imageUrl(new ImageUrl(url))
                    .build();
        }
    }

    /**
     * 图片URL
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ImageUrl {
        private String url;
    }

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
     * 创建用户消息（多模态：文本+图片）
     */
    public static ChatMessage userWithImages(String text, List<byte[]> images) {
        List<ContentPart> parts = new ArrayList<>();

        // 添加文本部分
        if (text != null && !text.isEmpty()) {
            parts.add(ContentPart.text(text));
        }

        // 添加图片部分
        if (images != null) {
            for (byte[] imageData : images) {
                parts.add(ContentPart.imageBase64(imageData));
            }
        }

        return ChatMessage.builder()
                .role(Role.USER)
                .contentParts(parts)
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

