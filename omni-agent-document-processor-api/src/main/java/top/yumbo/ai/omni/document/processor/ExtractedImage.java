package top.yumbo.ai.omni.document.processor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 提取的图片
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ExtractedImage {

    /**
     * 图片ID
     */
    private String imageId;

    /**
     * 图片数据（字节数组）
     */
    private byte[] data;

    /**
     * 图片格式（png, jpg, gif等）
     */
    private String format;

    /**
     * 所在页码/幻灯片索引（从1开始）
     */
    private Integer pageNumber;

    /**
     * 图片宽度（像素）
     */
    private Integer width;

    /**
     * 图片高度（像素）
     */
    private Integer height;

    /**
     * 图片位置信息
     */
    private ImagePosition position;

    /**
     * 图片描述（Vision LLM 生成）
     */
    private String description;

    /**
     * 图片元数据
     */
    private java.util.Map<String, Object> metadata;

    /**
     * 创建时间
     */
    private Long createdAt;

    /**
     * 图片位置信息
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ImagePosition {
        /**
         * X 坐标（PDF、PPT）
         */
        private Double x;

        /**
         * Y 坐标（PDF、PPT）
         */
        private Double y;

        /**
         * 行号（Excel、Word）
         */
        private Integer row;

        /**
         * 列号（Excel、Word）
         */
        private Integer column;

        /**
         * 段落索引（Word）
         */
        private Integer paragraphIndex;

        /**
         * 位置描述
         */
        private String description;
    }
}

