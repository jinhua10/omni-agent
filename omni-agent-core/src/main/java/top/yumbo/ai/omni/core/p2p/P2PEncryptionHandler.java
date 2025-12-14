package top.yumbo.ai.omni.core.p2p;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.*;
import java.util.Base64;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * P2P加密处理器
 * (P2P Encryption Handler)
 *
 * 提供端到端加密服务
 * (Provides end-to-end encryption service)
 *
 * 加密方案:
 * - RSA 2048位 用于密钥交换
 * - AES 256位 用于内容加密
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class P2PEncryptionHandler {

    /**
     * 连接加密密钥存储
     * Key: connectionId, Value: AES密钥
     */
    private final Map<String, SecretKey> connectionKeys = new ConcurrentHashMap<>();

    /**
     * RSA密钥对存储
     * Key: userId, Value: KeyPair
     */
    private final Map<String, KeyPair> userKeyPairs = new ConcurrentHashMap<>();

    /**
     * 算法常量
     */
    private static final String AES_ALGORITHM = "AES";
    private static final String RSA_ALGORITHM = "RSA";
    private static final int AES_KEY_SIZE = 256;
    private static final int RSA_KEY_SIZE = 2048;

    /**
     * 生成RSA密钥对
     *
     * @param userId 用户ID
     * @return 公钥（Base64编码）
     */
    public String generateKeyPair(String userId) {
        try {
            KeyPairGenerator generator = KeyPairGenerator.getInstance(RSA_ALGORITHM);
            generator.initialize(RSA_KEY_SIZE);
            KeyPair keyPair = generator.generateKeyPair();

            userKeyPairs.put(userId, keyPair);

            String publicKey = Base64.getEncoder().encodeToString(
                keyPair.getPublic().getEncoded()
            );

            log.info("Generated RSA key pair for user: {}", userId);
            return publicKey;

        } catch (NoSuchAlgorithmException e) {
            log.error("Failed to generate RSA key pair", e);
            throw new RuntimeException("Encryption initialization failed", e);
        }
    }

    /**
     * 生成连接会话密钥（AES）
     *
     * @param connectionId 连接ID
     * @return 会话密钥（Base64编码）
     */
    public String generateSessionKey(String connectionId) {
        try {
            KeyGenerator generator = KeyGenerator.getInstance(AES_ALGORITHM);
            generator.init(AES_KEY_SIZE);
            SecretKey sessionKey = generator.generateKey();

            connectionKeys.put(connectionId, sessionKey);

            String encodedKey = Base64.getEncoder().encodeToString(
                sessionKey.getEncoded()
            );

            log.info("Generated AES session key for connection: {}", connectionId);
            return encodedKey;

        } catch (NoSuchAlgorithmException e) {
            log.error("Failed to generate session key", e);
            throw new RuntimeException("Session key generation failed", e);
        }
    }

    /**
     * 使用RSA加密会话密钥
     *
     * @param sessionKey 会话密钥（Base64）
     * @param publicKeyStr 对方公钥（Base64）
     * @return 加密后的会话密钥（Base64）
     */
    public String encryptSessionKey(String sessionKey, String publicKeyStr) {
        try {
            byte[] publicKeyBytes = Base64.getDecoder().decode(publicKeyStr);
            PublicKey publicKey = KeyFactory.getInstance(RSA_ALGORITHM)
                .generatePublic(new java.security.spec.X509EncodedKeySpec(publicKeyBytes));

            Cipher cipher = Cipher.getInstance(RSA_ALGORITHM);
            cipher.init(Cipher.ENCRYPT_MODE, publicKey);

            byte[] sessionKeyBytes = Base64.getDecoder().decode(sessionKey);
            byte[] encrypted = cipher.doFinal(sessionKeyBytes);

            return Base64.getEncoder().encodeToString(encrypted);

        } catch (Exception e) {
            log.error("Failed to encrypt session key", e);
            throw new RuntimeException("Session key encryption failed", e);
        }
    }

    /**
     * 使用RSA解密会话密钥
     *
     * @param encryptedSessionKey 加密的会话密钥（Base64）
     * @param userId 用户ID
     * @return 解密后的会话密钥（Base64）
     */
    public String decryptSessionKey(String encryptedSessionKey, String userId) {
        try {
            KeyPair keyPair = userKeyPairs.get(userId);
            if (keyPair == null) {
                throw new IllegalStateException("No key pair found for user: " + userId);
            }

            Cipher cipher = Cipher.getInstance(RSA_ALGORITHM);
            cipher.init(Cipher.DECRYPT_MODE, keyPair.getPrivate());

            byte[] encrypted = Base64.getDecoder().decode(encryptedSessionKey);
            byte[] decrypted = cipher.doFinal(encrypted);

            return Base64.getEncoder().encodeToString(decrypted);

        } catch (Exception e) {
            log.error("Failed to decrypt session key", e);
            throw new RuntimeException("Session key decryption failed", e);
        }
    }

    /**
     * 存储连接会话密钥
     *
     * @param connectionId 连接ID
     * @param sessionKeyStr 会话密钥（Base64）
     */
    public void storeSessionKey(String connectionId, String sessionKeyStr) {
        try {
            byte[] keyBytes = Base64.getDecoder().decode(sessionKeyStr);
            SecretKey sessionKey = new SecretKeySpec(keyBytes, AES_ALGORITHM);
            connectionKeys.put(connectionId, sessionKey);
            log.info("Stored session key for connection: {}", connectionId);
        } catch (Exception e) {
            log.error("Failed to store session key", e);
            throw new RuntimeException("Session key storage failed", e);
        }
    }

    /**
     * 使用AES加密内容
     *
     * @param content 原始内容
     * @param connectionId 连接ID
     * @return 加密后的内容（Base64）
     */
    public String encrypt(String content, String connectionId) {
        try {
            SecretKey sessionKey = connectionKeys.get(connectionId);
            if (sessionKey == null) {
                throw new IllegalStateException("No session key found for connection: " + connectionId);
            }

            Cipher cipher = Cipher.getInstance(AES_ALGORITHM);
            cipher.init(Cipher.ENCRYPT_MODE, sessionKey);

            byte[] encrypted = cipher.doFinal(content.getBytes(StandardCharsets.UTF_8));
            return Base64.getEncoder().encodeToString(encrypted);

        } catch (Exception e) {
            log.error("Failed to encrypt content", e);
            throw new RuntimeException("Content encryption failed", e);
        }
    }

    /**
     * 使用AES解密内容
     *
     * @param encryptedContent 加密内容（Base64）
     * @param connectionId 连接ID
     * @return 原始内容
     */
    public String decrypt(String encryptedContent, String connectionId) {
        try {
            SecretKey sessionKey = connectionKeys.get(connectionId);
            if (sessionKey == null) {
                throw new IllegalStateException("No session key found for connection: " + connectionId);
            }

            Cipher cipher = Cipher.getInstance(AES_ALGORITHM);
            cipher.init(Cipher.DECRYPT_MODE, sessionKey);

            byte[] encrypted = Base64.getDecoder().decode(encryptedContent);
            byte[] decrypted = cipher.doFinal(encrypted);

            return new String(decrypted, StandardCharsets.UTF_8);

        } catch (Exception e) {
            log.error("Failed to decrypt content", e);
            throw new RuntimeException("Content decryption failed", e);
        }
    }

    /**
     * 清理连接密钥
     *
     * @param connectionId 连接ID
     */
    public void clearConnectionKey(String connectionId) {
        connectionKeys.remove(connectionId);
        log.info("Cleared session key for connection: {}", connectionId);
    }

    /**
     * 清理用户密钥对
     *
     * @param userId 用户ID
     */
    public void clearUserKeyPair(String userId) {
        userKeyPairs.remove(userId);
        log.info("Cleared key pair for user: {}", userId);
    }
}

