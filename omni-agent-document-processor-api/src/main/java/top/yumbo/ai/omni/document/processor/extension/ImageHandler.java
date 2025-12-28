package top.yumbo.ai.omni.document.processor.extension;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ExtractedImage;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingContext;

/**
 * 图片处理器
 * (Image Handler)
 *
 * <p>
 * 对提取的图片进行自定义处理，可用于：
 * - 图片压缩和优化
 * - 格式转换
 * - 水印添加
 * - OCR 文字识别
 * - 图片分类
 * - 对象检测
 * - 自定义 Vision LLM 分析
 * </p>
 *
 * <p>示例实现：</p>
 * <pre>{@code
 * @Component
 * @Order(5)
 * public class ImageCompressionHandler implements ImageHandler {
 *     @Override
 *     public ProcessedImage handle(ProcessingContext context, ExtractedImage image) {
 *         byte[] compressedData = compressImage(image.getData());
 *         return ProcessedImage.builder()
 *                 .data(compressedData)
 *                 .format(image.getFormat())
 *                 .originalSize(image.getData().length)
 *                 .compressedSize(compressedData.length)
 *                 .build();
 *     }
 * }
 * }</pre>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface ImageHandler extends DocumentProcessorExtension {

    /**
     * 处理图片
     *
     * @param context 处理上下文
     * @param image 提取的图片
     * @return 处理后的图片
     * @throws Exception 处理失败时抛出异常
     */
    ProcessedImage handle(ProcessingContext context, ExtractedImage image) throws Exception;

    /**
     * 处理后的图片
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ProcessedImage {
        /** 处理后的图片数据 */
        private byte[] data;

        /** 图片格式 */
        private String format;

        /** 原始大小（字节） */
        private long originalSize;

        /** 压缩后大小（字节） */
        private long compressedSize;

        /** OCR 识别的文字 */
        private String ocrText;

        /** 图片描述（Vision LLM） */
        private String description;

        /** 图片分类 */
        private String category;

        /** 检测到的对象 */
        private java.util.List<DetectedObject> detectedObjects;

        /** 扩展元数据 */
        private java.util.Map<String, Object> metadata;
    }

    /**
     * 检测到的对象
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class DetectedObject {
        /** 对象类型（人、车、动物等） */
        private String type;

        /** 置信度 */
        private double confidence;

        /** 边界框（x, y, width, height） */
        private BoundingBox boundingBox;

        /** 对象标签 */
        private String label;
    }

    /**
     * 边界框
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class BoundingBox {
        private int x;
        private int y;
        private int width;
        private int height;
    }
}

