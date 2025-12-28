package top.yumbo.ai.omni.document.processor.starter.image;

import lombok.extern.slf4j.Slf4j;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

/**
 * 图片哈希计算器
 * (Image Hash Calculator)
 *
 * <p>用于计算图片的哈希值，支持图片去重</p>
 * <p>使用 MD5 算法，比 SHA-256 快 2-3 倍，对于图片去重场景完全够用</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class ImageHashCalculator {

    private static final String HASH_ALGORITHM = "MD5";

    /**
     * 计算图片的 MD5 哈希值
     *
     * @param imageData 图片二进制数据
     * @return 哈希值（十六进制字符串，32位）
     */
    public static String calculateHash(byte[] imageData) {
        if (imageData == null || imageData.length == 0) {
            throw new IllegalArgumentException("Image data cannot be null or empty");
        }

        try {
            MessageDigest digest = MessageDigest.getInstance(HASH_ALGORITHM);
            byte[] hashBytes = digest.digest(imageData);
            return HexFormat.of().formatHex(hashBytes);
        } catch (NoSuchAlgorithmException e) {
            log.error("❌ 哈希算法不可用: {}", HASH_ALGORITHM, e);
            throw new RuntimeException("Failed to calculate image hash", e);
        }
    }

    /**
     * 计算图片的短哈希值（用于快速比对或显示）
     *
     * @param imageData 图片二进制数据
     * @return 短哈希值（前16位十六进制字符）
     */
    public static String calculateShortHash(byte[] imageData) {
        String fullHash = calculateHash(imageData);
        return fullHash.substring(0, 16);
    }

    /**
     * 比较两个图片是否相同
     *
     * @param imageData1 图片1的二进制数据
     * @param imageData2 图片2的二进制数据
     * @return 是否相同
     */
    public static boolean areImagesEqual(byte[] imageData1, byte[] imageData2) {
        if (imageData1 == null || imageData2 == null) {
            return false;
        }

        // 先比较长度，快速过滤
        if (imageData1.length != imageData2.length) {
            return false;
        }

        // 再比较哈希值
        String hash1 = calculateHash(imageData1);
        String hash2 = calculateHash(imageData2);
        return hash1.equals(hash2);
    }
}


