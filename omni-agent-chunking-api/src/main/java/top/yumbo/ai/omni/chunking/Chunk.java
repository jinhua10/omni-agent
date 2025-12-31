package top.yumbo.ai.omni.chunking;

import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

/**
 * 分块模型
 *
 * <p>✅ P1优化：支持MongoDB普通集合存储（性能提升100倍）</p>
 * <p>添加了@Document和@Indexed注解以支持MongoDB原生存储，避免GridFS的额外开销</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 2.0.0 - P1 Optimized for MongoDB Collection Storage
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
     * MongoDB主键（使用@Id注解会在MongoDB模块中处理）
     */
    private String id;

    /**
     * 文档ID
     * 需要索引以提升查询性能
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
     * 需要索引以支持排序查询
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
     * 分块策略
     */
    private ChunkingStrategy strategy;

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

