package top.yumbo.ai.omni.storage.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;

import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

/**
 * 文档分块模型
 * (Document Chunk Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Chunk implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 分块ID
     */
    private String id;

    /**
     * 文档ID
     */
    @NotBlank(message = "文档ID不能为空")
    private String documentId;

    /**
     * 分块内容
     */
    @NotBlank(message = "分块内容不能为空")
    private String content;

    /**
     * 分块序号（在文档中的位置）
     */
    @Builder.Default
    private int sequence = 0;

    /**
     * 开始位置
     */
    private Integer startPosition;

    /**
     * 结束位置
     */
    private Integer endPosition;

    /**
     * 元数据
     */
    private Map<String, Object> metadata;

    /**
     * 创建时间
     */
    private Long createdAt;

    /**
     * 分块大小（字节数）
     */
    public int getSize() {
        return content != null ? content.getBytes().length : 0;
    }
}

