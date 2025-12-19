package top.yumbo.ai.storage.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

/**
 * 文档元数据
 * (Document Metadata)
 *
 * <p>用于存储文档的基本信息，不包含文档内容</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DocumentMetadata implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 文档ID
     */
    private String documentId;

    /**
     * 文件名
     */
    private String filename;

    /**
     * 相对路径（如果存储在子目录中）
     */
    private String relativePath;

    /**
     * 文件大小（字节）
     */
    private Long fileSize;

    /**
     * 文件类型/扩展名
     */
    private String fileType;

    /**
     * 上传时间
     */
    private Date uploadTime;

    /**
     * 最后修改时间
     */
    private Date lastModified;

    /**
     * 是否已索引
     */
    private Boolean indexed;

    /**
     * 分块数量
     */
    private Integer chunkCount;

    /**
     * 图片数量
     */
    private Integer imageCount;

    /**
     * MIME类型
     */
    private String mimeType;

    /**
     * 存储路径（用于FTP浏览）
     */
    private String storagePath;
}

