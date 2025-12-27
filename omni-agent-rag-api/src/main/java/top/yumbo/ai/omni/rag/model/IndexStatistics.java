package top.yumbo.ai.omni.rag.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

/**
 * 索引统计信息
 * (Index Statistics)
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class IndexStatistics implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 文档总数
     */
    @Builder.Default
    private long totalDocuments = 0;

    /**
     * 索引大小（字节）
     */
    @Builder.Default
    private long indexSize = 0;

    /**
     * 向量维度
     */
    private Integer vectorDimension;

    /**
     * 是否支持向量搜索
     */
    @Builder.Default
    private boolean vectorSearchEnabled = false;

    /**
     * 索引类型
     */
    private String indexType;

    /**
     * 健康状态
     */
    @Builder.Default
    private boolean healthy = true;

    /**
     * 最后更新时间
     */
    private Long timestamp;

    /**
     * 域ID
     */
    private String domainId;

    /**
     * 域名称
     */
    private String domainName;
}

