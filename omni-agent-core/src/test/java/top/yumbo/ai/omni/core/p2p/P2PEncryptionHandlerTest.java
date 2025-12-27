package top.yumbo.ai.omni.core.p2p;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Base64;

import static org.assertj.core.api.Assertions.*;

/**
 * P2PEncryptionHandler 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("P2PEncryptionHandler Tests")
class P2PEncryptionHandlerTest {

    private P2PEncryptionHandler handler;

    @BeforeEach
    void setUp() {
        handler = new P2PEncryptionHandler();
    }

    @Test
    @DisplayName("应该生成RSA密钥对")
    void shouldGenerateRSAKeyPair() {
        // When
        String publicKey = handler.generateKeyPair("user-001");

        // Then
        assertThat(publicKey).isNotNull();
        assertThat(Base64.getDecoder().decode(publicKey)).isNotEmpty();
    }

    @Test
    @DisplayName("应该为不同用户生成不同的密钥对")
    void shouldGenerateDifferentKeyPairs() {
        // When
        String publicKey1 = handler.generateKeyPair("user-001");
        String publicKey2 = handler.generateKeyPair("user-002");

        // Then
        assertThat(publicKey1).isNotEqualTo(publicKey2);
    }

    @Test
    @DisplayName("应该生成AES会话密钥")
    void shouldGenerateSessionKey() {
        // When
        String sessionKey = handler.generateSessionKey("conn-001");

        // Then
        assertThat(sessionKey).isNotNull();
        byte[] keyBytes = Base64.getDecoder().decode(sessionKey);
        assertThat(keyBytes).hasSize(32); // AES-256 = 32 bytes
    }

    @Test
    @DisplayName("应该为不同连接生成不同的会话密钥")
    void shouldGenerateDifferentSessionKeys() {
        // When
        String sessionKey1 = handler.generateSessionKey("conn-001");
        String sessionKey2 = handler.generateSessionKey("conn-002");

        // Then
        assertThat(sessionKey1).isNotEqualTo(sessionKey2);
    }

    @Test
    @DisplayName("应该加密会话密钥")
    void shouldEncryptSessionKey() {
        // Given
        String publicKey = handler.generateKeyPair("user-001");
        String sessionKey = handler.generateSessionKey("conn-001");

        // When
        String encrypted = handler.encryptSessionKey(sessionKey, publicKey);

        // Then
        assertThat(encrypted).isNotNull();
        assertThat(encrypted).isNotEqualTo(sessionKey);
    }

    @Test
    @DisplayName("应该解密会话密钥")
    void shouldDecryptSessionKey() {
        // Given
        String userId = "user-001";
        String publicKey = handler.generateKeyPair(userId);
        String sessionKey = handler.generateSessionKey("conn-001");
        String encrypted = handler.encryptSessionKey(sessionKey, publicKey);

        // When
        String decrypted = handler.decryptSessionKey(encrypted, userId);

        // Then
        assertThat(decrypted).isEqualTo(sessionKey);
    }

    @Test
    @DisplayName("应该存储会话密钥")
    void shouldStoreSessionKey() {
        // Given
        String connectionId = "conn-001";
        String sessionKey = handler.generateSessionKey(connectionId);

        // When
        handler.storeSessionKey(connectionId, sessionKey);

        // Then
        // 验证可以使用该密钥加密
        String encrypted = handler.encrypt("test", connectionId);
        assertThat(encrypted).isNotNull();
    }

    @Test
    @DisplayName("应该加密内容")
    void shouldEncryptContent() {
        // Given
        String connectionId = "conn-001";
        String sessionKey = handler.generateSessionKey(connectionId);
        handler.storeSessionKey(connectionId, sessionKey);
        String content = "This is a secret message";

        // When
        String encrypted = handler.encrypt(content, connectionId);

        // Then
        assertThat(encrypted).isNotNull();
        assertThat(encrypted).isNotEqualTo(content);
    }

    @Test
    @DisplayName("应该解密内容")
    void shouldDecryptContent() {
        // Given
        String connectionId = "conn-001";
        String sessionKey = handler.generateSessionKey(connectionId);
        handler.storeSessionKey(connectionId, sessionKey);
        String content = "This is a secret message";
        String encrypted = handler.encrypt(content, connectionId);

        // When
        String decrypted = handler.decrypt(encrypted, connectionId);

        // Then
        assertThat(decrypted).isEqualTo(content);
    }

    @Test
    @DisplayName("应该加密和解密复杂内容")
    void shouldEncryptDecryptComplexContent() {
        // Given
        String connectionId = "conn-001";
        String sessionKey = handler.generateSessionKey(connectionId);
        handler.storeSessionKey(connectionId, sessionKey);
        String content = "特殊字符: @#$%^&*() 中文测试 Emoji: 🔐🔑";
        String encrypted = handler.encrypt(content, connectionId);

        // When
        String decrypted = handler.decrypt(encrypted, connectionId);

        // Then
        assertThat(decrypted).isEqualTo(content);
    }

    @Test
    @DisplayName("应该处理多次加密")
    void shouldHandleMultipleEncryptions() {
        // Given
        String connectionId = "conn-001";
        String sessionKey = handler.generateSessionKey(connectionId);
        handler.storeSessionKey(connectionId, sessionKey);

        // When
        String encrypted1 = handler.encrypt("message1", connectionId);
        String encrypted2 = handler.encrypt("message2", connectionId);

        // Then
        assertThat(handler.decrypt(encrypted1, connectionId)).isEqualTo("message1");
        assertThat(handler.decrypt(encrypted2, connectionId)).isEqualTo("message2");
    }

    @Test
    @DisplayName("应该清理连接密钥")
    void shouldClearConnectionKey() {
        // Given
        String connectionId = "conn-001";
        String sessionKey = handler.generateSessionKey(connectionId);
        handler.storeSessionKey(connectionId, sessionKey);

        // When
        handler.clearConnectionKey(connectionId);

        // Then
        assertThatThrownBy(() -> handler.encrypt("test", connectionId))
                .isInstanceOf(RuntimeException.class)
                .hasMessageContaining("encryption failed");
    }

    @Test
    @DisplayName("应该清理用户密钥对")
    void shouldClearUserKeyPair() {
        // Given
        String userId = "user-001";
        handler.generateKeyPair(userId);

        // When
        handler.clearUserKeyPair(userId);

        // Then
        // 尝试解密应该失败
        assertThatThrownBy(() -> handler.decryptSessionKey("encrypted", userId))
                .isInstanceOf(RuntimeException.class)
                .hasMessageContaining("decryption failed");
    }

    @Test
    @DisplayName("加密内容应该不同（即使内容相同）")
    void shouldProduceDifferentEncryptionsForSameContent() {
        // Given
        String connectionId1 = "conn-001";
        String connectionId2 = "conn-002";
        String sessionKey1 = handler.generateSessionKey(connectionId1);
        String sessionKey2 = handler.generateSessionKey(connectionId2);
        handler.storeSessionKey(connectionId1, sessionKey1);
        handler.storeSessionKey(connectionId2, sessionKey2);
        String content = "same message";

        // When
        String encrypted1 = handler.encrypt(content, connectionId1);
        String encrypted2 = handler.encrypt(content, connectionId2);

        // Then
        assertThat(encrypted1).isNotEqualTo(encrypted2);
    }

    @Test
    @DisplayName("解密错误的连接应该失败")
    void shouldFailToDecryptWithWrongConnection() {
        // Given
        String connectionId1 = "conn-001";
        String connectionId2 = "conn-002";
        handler.generateSessionKey(connectionId1);
        handler.generateSessionKey(connectionId2);
        String encrypted = handler.encrypt("message", connectionId1);

        // When/Then
        assertThatThrownBy(() -> handler.decrypt(encrypted, connectionId2))
                .isInstanceOf(Exception.class);
    }

    @Test
    @DisplayName("应该处理空内容")
    void shouldHandleEmptyContent() {
        // Given
        String connectionId = "conn-001";
        String sessionKey = handler.generateSessionKey(connectionId);
        handler.storeSessionKey(connectionId, sessionKey);

        // When
        String encrypted = handler.encrypt("", connectionId);
        String decrypted = handler.decrypt(encrypted, connectionId);

        // Then
        assertThat(decrypted).isEmpty();
    }
}

