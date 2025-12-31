package top.yumbo.ai.omni.storage.impl.redis.util;

import lombok.extern.slf4j.Slf4j;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

/**
 * Redis存储压缩工具类
 * 提供GZIP压缩/解压功能，优化大对象存储
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class CompressionUtils {

    /**
     * 压缩阈值：超过此大小才考虑压缩（1KB）
     */
    private static final int COMPRESSION_THRESHOLD = 1024;

    /**
     * 压缩比阈值：只有压缩比超过20%才使用压缩结果
     */
    private static final double COMPRESSION_RATIO_THRESHOLD = 0.2;

    /**
     * GZIP压缩字节数组
     *
     * @param data 原始数据
     * @return 压缩后的数据
     * @throws IOException 压缩失败
     */
    public static byte[] compress(byte[] data) throws IOException {
        if (data == null || data.length == 0) {
            return data;
        }

        try (ByteArrayOutputStream out = new ByteArrayOutputStream();
             GZIPOutputStream gzip = new GZIPOutputStream(out)) {
            gzip.write(data);
            gzip.finish();
            return out.toByteArray();
        }
    }

    /**
     * GZIP解压字节数组
     *
     * @param compressed 压缩的数据
     * @return 解压后的数据
     * @throws IOException 解压失败
     */
    public static byte[] decompress(byte[] compressed) throws IOException {
        if (compressed == null || compressed.length == 0) {
            return compressed;
        }

        try (ByteArrayInputStream in = new ByteArrayInputStream(compressed);
             GZIPInputStream gzip = new GZIPInputStream(in);
             ByteArrayOutputStream out = new ByteArrayOutputStream()) {

            byte[] buffer = new byte[8192];
            int len;
            while ((len = gzip.read(buffer)) > 0) {
                out.write(buffer, 0, len);
            }
            return out.toByteArray();
        }
    }

    /**
     * 压缩字符串
     *
     * @param text 原始文本
     * @return 压缩后的字节数组
     * @throws IOException 压缩失败
     */
    public static byte[] compressString(String text) throws IOException {
        if (text == null || text.isEmpty()) {
            return new byte[0];
        }
        return compress(text.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * 解压字符串
     *
     * @param compressed 压缩的数据
     * @return 解压后的文本
     * @throws IOException 解压失败
     */
    public static String decompressString(byte[] compressed) throws IOException {
        if (compressed == null || compressed.length == 0) {
            return "";
        }
        byte[] decompressed = decompress(compressed);
        return new String(decompressed, StandardCharsets.UTF_8);
    }

    /**
     * 智能压缩：只有在压缩效果好时才使用压缩
     *
     * @param data 原始数据
     * @return 压缩结果（包含是否压缩的标志）
     */
    public static CompressionResult smartCompress(byte[] data) {
        if (data == null || data.length < COMPRESSION_THRESHOLD) {
            // 数据太小，不压缩
            return new CompressionResult(data, false, data.length, data.length, 0);
        }

        try {
            byte[] compressed = compress(data);
            double ratio = 1.0 - ((double) compressed.length / data.length);

            if (ratio >= COMPRESSION_RATIO_THRESHOLD) {
                // 压缩效果好，使用压缩
                log.debug("✅ 压缩成功: {} bytes -> {} bytes (压缩率: {:.2f}%)",
                        data.length, compressed.length, ratio * 100);
                return new CompressionResult(compressed, true, data.length, compressed.length, ratio);
            } else {
                // 压缩效果不明显，不使用压缩
                log.debug("⚠️ 压缩效果不佳，使用原始数据: {} bytes -> {} bytes (压缩率: {:.2f}%)",
                        data.length, compressed.length, ratio * 100);
                return new CompressionResult(data, false, data.length, data.length, 0);
            }
        } catch (IOException e) {
            log.warn("❌ 压缩失败，使用原始数据: {}", e.getMessage());
            return new CompressionResult(data, false, data.length, data.length, 0);
        }
    }

    /**
     * 智能压缩字符串
     *
     * @param text 原始文本
     * @return 压缩结果
     */
    public static CompressionResult smartCompressString(String text) {
        if (text == null || text.isEmpty()) {
            return new CompressionResult(new byte[0], false, 0, 0, 0);
        }
        byte[] data = text.getBytes(StandardCharsets.UTF_8);
        return smartCompress(data);
    }

    /**
     * 压缩结果封装类
     */
    public static class CompressionResult {
        private final byte[] data;
        private final boolean compressed;
        private final int originalSize;
        private final int compressedSize;
        private final double compressionRatio;

        public CompressionResult(byte[] data, boolean compressed, int originalSize,
                                 int compressedSize, double compressionRatio) {
            this.data = data;
            this.compressed = compressed;
            this.originalSize = originalSize;
            this.compressedSize = compressedSize;
            this.compressionRatio = compressionRatio;
        }

        public byte[] getData() {
            return data;
        }

        public boolean isCompressed() {
            return compressed;
        }

        public int getOriginalSize() {
            return originalSize;
        }

        public int getCompressedSize() {
            return compressedSize;
        }

        public double getCompressionRatio() {
            return compressionRatio;
        }

        /**
         * 计算节省的空间（字节）
         */
        public int getSavedBytes() {
            return originalSize - compressedSize;
        }

        /**
         * 计算节省的空间百分比
         */
        public double getSavedPercentage() {
            return compressionRatio * 100;
        }
    }
}

