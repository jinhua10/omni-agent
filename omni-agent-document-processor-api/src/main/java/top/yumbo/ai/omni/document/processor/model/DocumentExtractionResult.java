package top.yumbo.ai.omni.document.processor.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 文档提取结果持久化实体
 * (Document Extraction Result Entity)
 *
 * <p>用于持久化存储文档的提取结果，避免重复提取</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DocumentExtractionResult implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 文档ID（唯一标识）
     */
    private String documentId;

    /**
     * 原始文件名
     */
    private String fileName;

    /**
     * 文件扩展名
     */
    private String fileExtension;

    /**
     * 文件大小（字节）
     */
    private Long fileSize;

    /**
     * 文件MD5哈希值（用于检测文件是否变化）
     */
    private String fileMd5;

    /**
     * 提取的文本内容
     */
    private String extractedText;

    /**
     * 使用的提取模型
     */
    private String extractionModel;

    /**
     * 提取方式（text-only, vision-llm等）
     */
    private String extractionMethod;

    /**
     * 提取状态（PENDING, EXTRACTING, COMPLETED, FAILED）
     */
    private String status;

    /**
     * 错误信息（如果提取失败）
     */
    private String errorMessage;

    /**
     * 提取开始时间
     */
    private Long startTime;

    /**
     * 提取完成时间
     */
    private Long completedTime;

    /**
     * 提取耗时（毫秒）
     */
    private Long duration;

    /**
     * 文档页数（如果适用）
     */
    private Integer pageCount;

    /**
     * 图片数量（如果适用）
     */
    private Integer imageCount;

    /**
     * 元数据（JSON格式存储额外信息）
     */
    private String metadata;

    /**
     * 创建时间
     */
    private Long createdAt;

    /**
     * 最后更新时间
     */
    private Long updatedAt;

    /**
     * 版本号（用于乐观锁）
     */
    private Integer version;

    /**
     * 是否需要重新提取
     */
    public boolean needsReExtraction() {
        // 如果状态是失败或者是旧版本，需要重新提取
        return "FAILED".equals(status) || extractedText == null || extractedText.isEmpty();
    }

    /**
     * 获取提取摘要信息
     */
    public String getSummary() {
        if (extractedText == null || extractedText.isEmpty()) {
            return "未提取";
        }
        int length = extractedText.length();
        String preview = extractedText.substring(0, Math.min(100, length));
        return String.format("%s... (共 %d 字符)", preview, length);
    }
}


