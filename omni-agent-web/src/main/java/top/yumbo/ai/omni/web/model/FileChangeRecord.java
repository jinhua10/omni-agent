package top.yumbo.ai.omni.web.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 文件变化记录
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FileChangeRecord {

    /**
     * 记录ID
     */
    private String id;

    /**
     * 文件路径
     */
    private String filePath;

    /**
     * 文件名
     */
    private String fileName;

    /**
     * 文档ID（如果已索引）
     */
    private String documentId;

    /**
     * 变化类型：CREATE, MODIFY, DELETE
     */
    private ChangeType changeType;

    /**
     * 文件大小（字节）
     */
    private Long fileSize;

    /**
     * 文件修改时间
     */
    private Long fileModifiedTime;

    /**
     * 文件哈希值（MD5）- 用于检测内容是否真正改变
     */
    private String fileHash;

    /**
     * 旧的文件哈希值（用于对比）
     */
    private String oldFileHash;

    /**
     * 变化时间
     */
    private Long changedAt;

    /**
     * 是否已处理（重新索引）
     */
    @Builder.Default
    private Boolean processed = false;

    /**
     * 处理时间
     */
    private Long processedAt;

    /**
     * 备注信息
     */
    private String note;

    /**
     * 变化类型枚举
     */
    public enum ChangeType {
        CREATE("新增"),
        MODIFY("修改"),
        DELETE("删除");

        private final String description;

        ChangeType(String description) {
            this.description = description;
        }

        public String getDescription() {
            return description;
        }
    }
}

