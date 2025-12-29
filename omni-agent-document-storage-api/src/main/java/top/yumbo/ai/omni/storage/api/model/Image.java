package top.yumbo.ai.omni.storage.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Map;

/**
 * 图像模型
 * (Image Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Image implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 图像ID
     */
    private String id;

    /**
     * 文档ID
     */
    @NotBlank(message = "文档ID不能为空")
    private String documentId;

    /**
     * 图像数据
     */
    @NotNull(message = "图像数据不能为空")
    private byte[] data;

    /**
     * 图像格式 (PNG, JPG, GIF, etc.)
     */
    @NotBlank(message = "图像格式不能为空")
    private String format;

    /**
     * 图像宽度（像素）
     */
    private Integer width;

    /**
     * 图像高度（像素）
     */
    private Integer height;

    /**
     * 在文档中的页码
     */
    private Integer pageNumber;

    /**
     * 元数据
     */
    private Map<String, Object> metadata;

    /**
     * 创建时间
     */
    private Long createdAt;

    /**
     * 获取图像大小（字节数）
     */
    public int getSize() {
        return data != null ? data.length : 0;
    }
}

