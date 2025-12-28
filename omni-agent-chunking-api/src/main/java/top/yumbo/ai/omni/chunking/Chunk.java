package top.yumbo.ai.omni.chunking;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

/**
 * 分块模型
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
    private String chunkId;

    /**
     * 文档ID
     */
    private String documentId;

    /**
     * 分块内容
     */
    private String content;

    /**
     * 分块索引（在文档中的顺序）
     */
    private int index;

    /**
     * 起始位置
     */
    private int startPosition;

    /**
     * 结束位置
     */
    private int endPosition;

    /**
     * 分块长度
     */
    private int length;

    /**
     * 分块策略
     */
    private ChunkingStrategy strategy;
}

