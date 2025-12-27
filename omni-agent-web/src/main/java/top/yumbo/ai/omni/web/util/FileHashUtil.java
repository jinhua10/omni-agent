package top.yumbo.ai.omni.web.util;

import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/**
 * 文件哈希工具类
 *
 * 用于计算文件的 MD5、SHA256 等哈希值，判断文件内容是否真正改变
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public class FileHashUtil {

    /**
     * 哈希算法枚举
     */
    public enum HashAlgorithm {
        MD5("MD5"),           // 快速，适合大文件
        SHA256("SHA-256");    // 更安全，但稍慢

        private final String algorithm;

        HashAlgorithm(String algorithm) {
            this.algorithm = algorithm;
        }

        public String getAlgorithm() {
            return algorithm;
        }
    }

    /**
     * 计算文件哈希值（默认使用 MD5）
     *
     * @param filePath 文件路径
     * @return 哈希值（16进制字符串）
     */
    public static String calculateHash(Path filePath) {
        return calculateHash(filePath, HashAlgorithm.MD5);
    }

    /**
     * 计算文件哈希值
     *
     * @param filePath 文件路径
     * @param algorithm 哈希算法
     * @return 哈希值（16进制字符串）
     */
    public static String calculateHash(Path filePath, HashAlgorithm algorithm) {
        try {
            if (!Files.exists(filePath)) {
                log.warn("⚠️ 文件不存在: {}", filePath);
                return null;
            }

            // 读取文件内容
            byte[] fileBytes = Files.readAllBytes(filePath);

            // 计算哈希
            MessageDigest digest = MessageDigest.getInstance(algorithm.getAlgorithm());
            byte[] hashBytes = digest.digest(fileBytes);

            // 转换为16进制字符串
            return bytesToHex(hashBytes);

        } catch (IOException e) {
            log.error("❌ 读取文件失败: {}", filePath, e);
            return null;
        } catch (NoSuchAlgorithmException e) {
            log.error("❌ 不支持的哈希算法: {}", algorithm, e);
            return null;
        }
    }

    /**
     * 计算文件 MD5
     */
    public static String calculateMD5(Path filePath) {
        return calculateHash(filePath, HashAlgorithm.MD5);
    }

    /**
     * 计算文件 SHA256
     */
    public static String calculateSHA256(Path filePath) {
        return calculateHash(filePath, HashAlgorithm.SHA256);
    }

    /**
     * 比较两个文件的哈希值是否相同
     *
     * @param hash1 哈希值1
     * @param hash2 哈希值2
     * @return 是否相同
     */
    public static boolean isSameHash(String hash1, String hash2) {
        if (hash1 == null || hash2 == null) {
            return false;
        }
        return hash1.equalsIgnoreCase(hash2);
    }

    /**
     * 检查文件内容是否改变
     *
     * @param filePath 文件路径
     * @param oldHash 旧的哈希值
     * @return true=内容改变，false=内容未变
     */
    public static boolean hasFileChanged(Path filePath, String oldHash) {
        if (oldHash == null) {
            return true;  // 没有旧哈希，视为改变
        }

        String newHash = calculateHash(filePath);
        return !isSameHash(oldHash, newHash);
    }

    /**
     * 字节数组转16进制字符串
     */
    private static String bytesToHex(byte[] bytes) {
        StringBuilder hexString = new StringBuilder();
        for (byte b : bytes) {
            String hex = Integer.toHexString(0xff & b);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }

    /**
     * 快速比较文件大小和修改时间（避免计算大文件的哈希）
     *
     * @param filePath 文件路径
     * @param oldSize 旧的文件大小
     * @param oldModified 旧的修改时间
     * @return true=可能改变（需进一步校验），false=肯定没变
     */
    public static boolean maybeChanged(Path filePath, long oldSize, long oldModified) {
        try {
            if (!Files.exists(filePath)) {
                return true;
            }

            long newSize = Files.size(filePath);
            long newModified = Files.getLastModifiedTime(filePath).toMillis();

            // 大小或修改时间不同，说明可能改变
            return newSize != oldSize || newModified != oldModified;

        } catch (IOException e) {
            log.warn("⚠️ 检查文件属性失败: {}", filePath, e);
            return true;  // 出错时保守处理，视为可能改变
        }
    }
}



