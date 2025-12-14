package top.yumbo.ai.storage.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 存储统计信息
 * (Storage Statistics)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class StorageStatistics implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 文档总数
     */
    @Builder.Default
    private long totalDocuments = 0;

    /**
     * 分块总数
     */
    @Builder.Default
    private long totalChunks = 0;

    /**
     * 图像总数
     */
    @Builder.Default
    private long totalImages = 0;

    /**
     * PPL数据总数
     */
    @Builder.Default
    private long totalPPLData = 0;

    /**
     * 总存储大小（字节）
     */
    @Builder.Default
    private long totalSize = 0;

    /**
     * 存储类型
     */
    private String storageType;

    /**
     * 健康状态
     */
    @Builder.Default
    private boolean healthy = true;

    /**
     * 统计时间
     */
    private Long timestamp;
}

