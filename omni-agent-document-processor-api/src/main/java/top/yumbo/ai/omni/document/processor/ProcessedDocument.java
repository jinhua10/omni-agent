package top.yumbo.ai.omni.document.processor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 处理后的文档模型
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ProcessedDocument implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 文档ID
     */
    private String documentId;

    /**
     * 文档类型
     */
    private DocumentType documentType;

    /**
     * 提取的文本内容
     */
    private String text;

    /**
     * 文档标题
     */
    private String title;

    /**
     * 文档作者
     */
    private String author;

    /**
     * 页数（如适用）
     */
    private Integer pageCount;

    /**
     * 字符数
     */
    private Integer characterCount;

    /**
     * 提取的图片列表
     */
    @Builder.Default
    private List<ExtractedImage> images = new ArrayList<>();

    /**
     * 元数据
     */
    @Builder.Default
    private Map<String, Object> metadata = new HashMap<>();

    /**
     * 处理是否成功
     */
    private boolean success;

    /**
     * 错误消息（如果失败）
     */
    private String errorMessage;
}

