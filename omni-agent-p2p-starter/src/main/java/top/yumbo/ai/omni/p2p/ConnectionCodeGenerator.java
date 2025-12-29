package top.yumbo.ai.omni.p2p;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 连接码生成器
 * (Connection Code Generator)
 *
 * 生成和管理一次性P2P连接码
 * (Generate and manage one-time P2P connection codes)
 *
 * 连接码格式: ABC-XYZ-123
 * - 6位字母 - 6位字母 - 3位数字
 * - 有效期: 10分钟
 * - 一次性使用
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class ConnectionCodeGenerator {

    /**
     * 连接码存储
     * Key: code, Value: CodeInfo
     */
    private final Map<String, CodeInfo> codeRegistry = new ConcurrentHashMap<>();

    /**
     * 随机数生成器
     */
    private final SecureRandom random = new SecureRandom();

    /**
     * 有效期（分钟）
     */
    private static final int VALIDITY_MINUTES = 10;

    /**
     * 生成连接码
     *
     * @param userId 用户ID
     * @param userName 用户昵称
     * @param publicKey 公钥
     * @return 连接码
     */
    public String generate(String userId, String userName, String publicKey) {
        // 生成唯一连接码
        String code = generateUniqueCode();

        // 创建连接码信息
        CodeInfo info = new CodeInfo();
        info.code = code;
        info.userId = userId;
        info.userName = userName;
        info.publicKey = publicKey;
        info.createTime = LocalDateTime.now();
        info.expiryTime = LocalDateTime.now().plusMinutes(VALIDITY_MINUTES);
        info.used = false;

        // 存储
        codeRegistry.put(code, info);

        log.info("Generated connection code: {} for user: {}", code, userId);

        // 异步清理过期码
        scheduleCleanup(code);

        return code;
    }

    /**
     * 验证连接码
     *
     * @param code 连接码
     * @return CodeInfo 或 null
     */
    public CodeInfo validate(String code) {
        CodeInfo info = codeRegistry.get(code);

        if (info == null) {
            log.warn("Connection code not found: {}", code);
            return null;
        }

        if (info.used) {
            log.warn("Connection code already used: {}", code);
            return null;
        }

        if (LocalDateTime.now().isAfter(info.expiryTime)) {
            log.warn("Connection code expired: {}", code);
            codeRegistry.remove(code);
            return null;
        }

        return info;
    }

    /**
     * 标记连接码为已使用
     *
     * @param code 连接码
     */
    public void markUsed(String code) {
        CodeInfo info = codeRegistry.get(code);
        if (info != null) {
            info.used = true;
            log.info("Connection code marked as used: {}", code);

            // 10秒后删除已使用的码
            new Thread(() -> {
                try {
                    Thread.sleep(10000);
                    codeRegistry.remove(code);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }).start();
        }
    }

    /**
     * 生成唯一连接码
     */
    private String generateUniqueCode() {
        String code;
        do {
            code = generateRandomCode();
        } while (codeRegistry.containsKey(code));
        return code;
    }

    /**
     * 生成随机连接码
     * 格式: ABC-DEF-123
     */
    private String generateRandomCode() {
        String part1 = generateRandomLetters(3);
        String part2 = generateRandomLetters(3);
        String part3 = generateRandomDigits(3);
        return String.format("%s-%s-%s", part1, part2, part3);
    }

    /**
     * 生成随机字母
     */
    private String generateRandomLetters(int length) {
        StringBuilder sb = new StringBuilder();
        String letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        for (int i = 0; i < length; i++) {
            sb.append(letters.charAt(random.nextInt(letters.length())));
        }
        return sb.toString();
    }

    /**
     * 生成随机数字
     */
    private String generateRandomDigits(int length) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < length; i++) {
            sb.append(random.nextInt(10));
        }
        return sb.toString();
    }

    /**
     * 计划清理任务
     */
    private void scheduleCleanup(String code) {
        new Thread(() -> {
            try {
                Thread.sleep((VALIDITY_MINUTES + 1) * 60 * 1000);
                CodeInfo info = codeRegistry.remove(code);
                if (info != null && !info.used) {
                    log.info("Connection code expired and removed: {}", code);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }).start();
    }

    /**
     * 连接码信息
     */
    public static class CodeInfo {
        public String code;
        public String userId;
        public String userName;
        public String publicKey;
        public LocalDateTime createTime;
        public LocalDateTime expiryTime;
        public boolean used;
    }
}


