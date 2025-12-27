package top.yumbo.ai.omni.web.dto;

import lombok.Data;

/**
 * 文档索引请求
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Data
public class DocumentRequest {
    /**
     * 文档ID
     */
    private String id;

    /**
     * 文档标题
     */
    private String title;

    /**
     * 文档内容
     */
    private String content;

    /**
     * 文档摘要
     */
    private String summary;
}






