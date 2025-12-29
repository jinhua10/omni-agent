package top.yumbo.ai.omni.document.processor.extension.examples;

import lombok.extern.slf4j.Slf4j;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ExtractedImage;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingContext;
import top.yumbo.ai.omni.document.processor.extension.ImageHandler;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

/**
 * 图片压缩处理器示例
 * (Image Compression Handler Example)
 *
 * <p>
 * 这是一个示例，展示如何使用图片处理器来压缩图片。
 * 用户可以参考这个示例创建自己的图片处理器。
 * </p>
 *
 * <p>功能：</p>
 * <ul>
 *   <li>将大图片缩小到指定尺寸</li>
 *   <li>减少内存占用</li>
 *   <li>加快处理速度</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
@Order(5)  // 中等优先级
public class ImageCompressionHandler implements ImageHandler {

    private static final int MAX_WIDTH = 1024;
    private static final int MAX_HEIGHT = 1024;

    @Override
    public String getName() {
        return "ImageCompressionHandler";
    }

    @Override
    public int getOrder() {
        return 5;
    }

    @Override
    public ProcessedImage handle(ProcessingContext context, ExtractedImage image) throws Exception {
        log.debug("📋 [ImageCompression] 开始压缩图片");

        byte[] originalData = image.getData();
        long originalSize = originalData.length;

        // 读取图片
        BufferedImage bufferedImage = ImageIO.read(new ByteArrayInputStream(originalData));
        if (bufferedImage == null) {
            log.warn("⚠️ [ImageCompression] 无法读取图片，跳过压缩");
            return ProcessedImage.builder()
                    .data(originalData)
                    .format(image.getFormat())
                    .originalSize(originalSize)
                    .compressedSize(originalSize)
                    .build();
        }

        int width = bufferedImage.getWidth();
        int height = bufferedImage.getHeight();

        // 如果图片小于最大尺寸，不压缩
        if (width <= MAX_WIDTH && height <= MAX_HEIGHT) {
            log.debug("✅ [ImageCompression] 图片尺寸已符合要求，无需压缩");
            return ProcessedImage.builder()
                    .data(originalData)
                    .format(image.getFormat())
                    .originalSize(originalSize)
                    .compressedSize(originalSize)
                    .build();
        }

        // 计算缩放比例
        double scale = Math.min((double) MAX_WIDTH / width, (double) MAX_HEIGHT / height);
        int newWidth = (int) (width * scale);
        int newHeight = (int) (height * scale);

        // 压缩图片
        BufferedImage resizedImage = new BufferedImage(newWidth, newHeight, BufferedImage.TYPE_INT_RGB);
        Graphics2D g = resizedImage.createGraphics();
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g.drawImage(bufferedImage, 0, 0, newWidth, newHeight, null);
        g.dispose();

        // 转换为字节数组
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        String format = image.getFormat() != null ? image.getFormat() : "jpg";
        ImageIO.write(resizedImage, format, baos);
        byte[] compressedData = baos.toByteArray();

        long compressedSize = compressedData.length;
        double compressionRatio = (1 - (double) compressedSize / originalSize) * 100;

        log.debug("✅ [ImageCompression] 压缩完成: {}x{} -> {}x{}, 大小: {} -> {} bytes (压缩率: {}%)",
                width, height, newWidth, newHeight, originalSize, compressedSize, String.format("%.2f", compressionRatio));

        return ProcessedImage.builder()
                .data(compressedData)
                .format(format)
                .originalSize(originalSize)
                .compressedSize(compressedSize)
                .build();
    }
}

