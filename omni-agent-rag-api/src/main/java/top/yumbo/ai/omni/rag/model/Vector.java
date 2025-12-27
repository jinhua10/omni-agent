package top.yumbo.ai.omni.rag.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 向量模型
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Vector {

    /**
     * 向量维度
     */
    private int dimension;

    /**
     * 向量数据
     */
    private float[] data;

    /**
     * 创建向量
     */
    public static Vector of(float[] data) {
        return Vector.builder()
                .dimension(data.length)
                .data(data)
                .build();
    }
}

