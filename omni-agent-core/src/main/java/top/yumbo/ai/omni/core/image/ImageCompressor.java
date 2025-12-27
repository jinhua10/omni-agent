package top.yumbo.ai.omni.core.image;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Iterator;

/**
 * 图片压缩器
 * (Image Compressor)
 *
 * <p>支持图片压缩和格式转换，优化存储空间</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class ImageCompressor {

    /**
     * 压缩配置
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CompressionConfig {
        /** 是否启用压缩 */
        private boolean enabled = true;

        /** 压缩质量（0.0 - 1.0，1.0 为最高质量） */
        private float quality = 0.85f;

        /** 最大宽度（像素），超过则缩放 */
        private int maxWidth = 2048;

        /** 最大高度（像素），超过则缩放 */
        private int maxHeight = 2048;

        /** 最小压缩大小（字节），小于此值不压缩 */
        private int minSizeToCompress = 100 * 1024; // 100KB

        /** 目标格式（jpg, png, webp） */
        private String targetFormat = "jpg";


        public void setQuality(float quality) {
            this.quality = Math.max(0.0f, Math.min(1.0f, quality));
        }
    }

    /**
     * 压缩结果
     */
    @Data
    @AllArgsConstructor
    public static class CompressionResult {
        private final byte[] data;
        private final String format;
        private final int originalSize;
        private final int compressedSize;
        private final boolean compressed;


        public float getCompressionRatio() {
            if (originalSize == 0) return 1.0f;
            return (float) compressedSize / originalSize;
        }

        public int getSavedBytes() {
            return originalSize - compressedSize;
        }
    }

    /**
     * 使用默认配置压缩图片
     *
     * @param imageData 原始图片数据
     * @param format 原始格式
     * @return 压缩结果
     */
    public static CompressionResult compress(byte[] imageData, String format) {
        return compress(imageData, format, new CompressionConfig());
    }

    /**
     * 使用指定配置压缩图片
     *
     * @param imageData 原始图片数据
     * @param originalFormat 原始格式
     * @param config 压缩配置
     * @return 压缩结果
     */
    public static CompressionResult compress(byte[] imageData, String originalFormat, CompressionConfig config) {
        if (imageData == null || imageData.length == 0) {
            throw new IllegalArgumentException("Image data cannot be null or empty");
        }

        int originalSize = imageData.length;

        // 如果未启用压缩或文件太小，直接返回
        if (!config.isEnabled() || originalSize < config.getMinSizeToCompress()) {
            log.debug("📊 图片不需要压缩: size={}KB, minSize={}KB",
                    originalSize / 1024, config.getMinSizeToCompress() / 1024);
            return new CompressionResult(imageData, originalFormat, originalSize, originalSize, false);
        }

        try {
            // 读取原始图片
            BufferedImage originalImage = ImageIO.read(new ByteArrayInputStream(imageData));
            if (originalImage == null) {
                log.warn("⚠️ 无法读取图片，返回原始数据");
                return new CompressionResult(imageData, originalFormat, originalSize, originalSize, false);
            }

            int width = originalImage.getWidth();
            int height = originalImage.getHeight();

            log.debug("📸 原始图片: {}x{}, {}KB, format={}",
                    width, height, originalSize / 1024, originalFormat);

            // 计算缩放尺寸
            BufferedImage processedImage = originalImage;
            if (width > config.getMaxWidth() || height > config.getMaxHeight()) {
                processedImage = scaleImage(originalImage, config.getMaxWidth(), config.getMaxHeight());
                log.debug("🔽 缩放图片: {}x{} -> {}x{}",
                        width, height, processedImage.getWidth(), processedImage.getHeight());
            }

            // 压缩图片
            String targetFormat = config.getTargetFormat();
            byte[] compressedData = compressImage(processedImage, targetFormat, config.getQuality());

            int compressedSize = compressedData.length;
            float ratio = (float) compressedSize / originalSize;

            log.info("✅ 图片压缩完成: {}KB -> {}KB (压缩率: {}%, 节省: {}KB)",
                    originalSize / 1024,
                    compressedSize / 1024,
                    String.format("%.1f", ratio * 100),
                    (originalSize - compressedSize) / 1024);

            return new CompressionResult(compressedData, targetFormat, originalSize, compressedSize, true);

        } catch (Exception e) {
            log.error("❌ 图片压缩失败，返回原始数据", e);
            return new CompressionResult(imageData, originalFormat, originalSize, originalSize, false);
        }
    }

    /**
     * 缩放图片（保持宽高比）
     */
    private static BufferedImage scaleImage(BufferedImage original, int maxWidth, int maxHeight) {
        int originalWidth = original.getWidth();
        int originalHeight = original.getHeight();

        // 计算缩放比例
        float scaleWidth = (float) maxWidth / originalWidth;
        float scaleHeight = (float) maxHeight / originalHeight;
        float scale = Math.min(scaleWidth, scaleHeight);

        int newWidth = (int) (originalWidth * scale);
        int newHeight = (int) (originalHeight * scale);

        // 创建缩放后的图片
        BufferedImage scaledImage = new BufferedImage(newWidth, newHeight, BufferedImage.TYPE_INT_RGB);
        Graphics2D g = scaledImage.createGraphics();

        // 设置高质量渲染
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

        g.drawImage(original, 0, 0, newWidth, newHeight, null);
        g.dispose();

        return scaledImage;
    }

    /**
     * 压缩图片到指定格式
     */
    private static byte[] compressImage(BufferedImage image, String format, float quality) throws IOException {
        // 转换为RGB格式（移除透明通道，适用于JPG）
        BufferedImage rgbImage = image;
        if ("jpg".equalsIgnoreCase(format) || "jpeg".equalsIgnoreCase(format)) {
            if (image.getType() != BufferedImage.TYPE_INT_RGB) {
                rgbImage = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_RGB);
                Graphics2D g = rgbImage.createGraphics();
                g.setColor(Color.WHITE);
                g.fillRect(0, 0, rgbImage.getWidth(), rgbImage.getHeight());
                g.drawImage(image, 0, 0, null);
                g.dispose();
            }
        }

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        // 获取对应格式的 ImageWriter
        Iterator<ImageWriter> writers = ImageIO.getImageWritersByFormatName(format);
        if (!writers.hasNext()) {
            throw new IOException("No writer found for format: " + format);
        }

        ImageWriter writer = writers.next();
        ImageWriteParam writeParam = writer.getDefaultWriteParam();

        // 设置压缩参数
        if (writeParam.canWriteCompressed()) {
            writeParam.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
            writeParam.setCompressionQuality(quality);
        }

        // 写入图片
        try (ImageOutputStream ios = ImageIO.createImageOutputStream(outputStream)) {
            writer.setOutput(ios);
            writer.write(null, new IIOImage(rgbImage, null, null), writeParam);
        } finally {
            writer.dispose();
        }

        return outputStream.toByteArray();
    }

    /**
     * 创建缩略图
     *
     * @param imageData 原始图片数据
     * @param width 缩略图宽度
     * @param height 缩略图高度
     * @return 缩略图数据（JPEG格式）
     */
    public static byte[] createThumbnail(byte[] imageData, int width, int height) throws IOException {
        BufferedImage originalImage = ImageIO.read(new ByteArrayInputStream(imageData));
        if (originalImage == null) {
            throw new IOException("无法读取图片数据");
        }

        BufferedImage thumbnail = scaleImage(originalImage, width, height);

        CompressionConfig config = new CompressionConfig();
        config.setQuality(0.75f);
        config.setTargetFormat("jpg");

        return compressImage(thumbnail, "jpg", 0.75f);
    }
}


