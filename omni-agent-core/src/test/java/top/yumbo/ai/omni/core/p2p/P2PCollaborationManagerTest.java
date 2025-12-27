package top.yumbo.ai.omni.core.p2p;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import top.yumbo.ai.omni.p2p.api.model.ConnectionCode;
import top.yumbo.ai.omni.p2p.api.model.PeerConnection;
import top.yumbo.ai.omni.p2p.api.model.SharedKnowledge;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * P2PCollaborationManager 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("P2PCollaborationManager Tests")
class P2PCollaborationManagerTest {

    @Mock
    private ConnectionCodeGenerator codeGenerator;

    @Mock
    private P2PEncryptionHandler encryptionHandler;

    @InjectMocks
    private P2PCollaborationManager manager;

    @BeforeEach
    void setUp() {
        // Mock defaults
    }

    @Test
    @DisplayName("应该生成连接码")
    void shouldGenerateConnectionCode() {
        // Given
        String userId = "user-001";
        String userName = "Alice";
        String publicKey = "publicKey123";
        String code = "ABC-XYZ-123";

        when(encryptionHandler.generateKeyPair(userId)).thenReturn(publicKey);
        when(codeGenerator.generate(userId, userName, publicKey)).thenReturn(code);

        // When
        ConnectionCode result = manager.generateConnectionCode(userId, userName, 10);

        // Then
        assertThat(result).isNotNull();
        assertThat(result.getCode()).isEqualTo(code);
        assertThat(result.getInitiatorId()).isEqualTo(userId);
        assertThat(result.getInitiatorName()).isEqualTo(userName);
        assertThat(result.getPublicKey()).isEqualTo(publicKey);
        assertThat(result.isUsed()).isFalse();
        assertThat(result.getCreateTime()).isBeforeOrEqualTo(LocalDateTime.now());

        verify(encryptionHandler).generateKeyPair(userId);
        verify(codeGenerator).generate(userId, userName, publicKey);
    }

    @Test
    @DisplayName("应该通过连接码建立连接")
    void shouldConnectWithCode() {
        // Given
        String code = "ABC-XYZ-123";
        String userId = "user-002";
        String userName = "Bob";
        String remoteUserId = "user-001";
        String remoteUserName = "Alice";
        String publicKey = "publicKey123";
        String sessionKey = "sessionKey";

        ConnectionCodeGenerator.CodeInfo codeInfo = new ConnectionCodeGenerator.CodeInfo();
        codeInfo.userId = remoteUserId;
        codeInfo.userName = remoteUserName;
        codeInfo.publicKey = publicKey;

        when(codeGenerator.validate(code)).thenReturn(codeInfo);
        when(encryptionHandler.generateKeyPair(userId)).thenReturn("localPublicKey");
        when(encryptionHandler.generateSessionKey(anyString())).thenReturn(sessionKey);
        when(encryptionHandler.encryptSessionKey(sessionKey, publicKey)).thenReturn("encrypted");

        // When
        PeerConnection connection = manager.connectWithCode(code, userId, userName);

        // Then
        assertThat(connection).isNotNull();
        assertThat(connection.getConnectionId()).isNotNull();
        assertThat(connection.getLocalUserId()).isEqualTo(userId);
        assertThat(connection.getRemoteUserId()).isEqualTo(remoteUserId);
        assertThat(connection.getRemoteUserName()).isEqualTo(remoteUserName);
        assertThat(connection.getStatus()).isEqualTo(PeerConnection.ConnectionStatus.ESTABLISHED);

        verify(codeGenerator).validate(code);
        verify(codeGenerator).markUsed(code);
        verify(encryptionHandler).generateKeyPair(userId);
        verify(encryptionHandler).generateSessionKey(anyString());
        verify(encryptionHandler).storeSessionKey(anyString(), eq(sessionKey));
    }

    @Test
    @DisplayName("应该拒绝无效的连接码")
    void shouldRejectInvalidCode() {
        // Given
        when(codeGenerator.validate(anyString())).thenReturn(null);

        // When/Then
        assertThatThrownBy(() -> manager.connectWithCode("invalid", "user-002", "Bob"))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("Invalid or expired");
    }

    @Test
    @DisplayName("应该断开连接")
    void shouldDisconnect() {
        // Given
        String code = "ABC-XYZ-123";
        String userId = "user-002";

        ConnectionCodeGenerator.CodeInfo codeInfo = new ConnectionCodeGenerator.CodeInfo();
        codeInfo.userId = "user-001";
        codeInfo.userName = "Alice";
        codeInfo.publicKey = "publicKey";

        when(codeGenerator.validate(code)).thenReturn(codeInfo);
        when(encryptionHandler.generateKeyPair(anyString())).thenReturn("key");
        when(encryptionHandler.generateSessionKey(anyString())).thenReturn("sessionKey");
        when(encryptionHandler.encryptSessionKey(anyString(), anyString())).thenReturn("encrypted");

        PeerConnection connection = manager.connectWithCode(code, userId, "Bob");
        String connectionId = connection.getConnectionId();

        // When
        boolean result = manager.disconnect(connectionId);

        // Then
        assertThat(result).isTrue();
        verify(encryptionHandler).clearConnectionKey(connectionId);
    }

    @Test
    @DisplayName("断开不存在的连接应该返回false")
    void shouldReturnFalseForNonExistentConnection() {
        // When
        boolean result = manager.disconnect("non-existent");

        // Then
        assertThat(result).isFalse();
    }

    @Test
    @DisplayName("应该获取用户的连接列表")
    void shouldGetUserConnections() {
        // Given
        String code = "ABC-XYZ-123";
        String userId = "user-002";

        ConnectionCodeGenerator.CodeInfo codeInfo = new ConnectionCodeGenerator.CodeInfo();
        codeInfo.userId = "user-001";
        codeInfo.userName = "Alice";
        codeInfo.publicKey = "publicKey";

        when(codeGenerator.validate(code)).thenReturn(codeInfo);
        when(encryptionHandler.generateKeyPair(anyString())).thenReturn("key");
        when(encryptionHandler.generateSessionKey(anyString())).thenReturn("sessionKey");
        when(encryptionHandler.encryptSessionKey(anyString(), anyString())).thenReturn("encrypted");

        manager.connectWithCode(code, userId, "Bob");

        // When
        List<PeerConnection> connections = manager.getConnections(userId);

        // Then
        assertThat(connections).hasSize(1);
        assertThat(connections.get(0).getLocalUserId()).isEqualTo(userId);
    }

    @Test
    @DisplayName("应该通过ID获取连接")
    void shouldGetConnectionById() {
        // Given
        String code = "ABC-XYZ-123";
        String userId = "user-002";

        ConnectionCodeGenerator.CodeInfo codeInfo = new ConnectionCodeGenerator.CodeInfo();
        codeInfo.userId = "user-001";
        codeInfo.userName = "Alice";
        codeInfo.publicKey = "publicKey";

        when(codeGenerator.validate(code)).thenReturn(codeInfo);
        when(encryptionHandler.generateKeyPair(anyString())).thenReturn("key");
        when(encryptionHandler.generateSessionKey(anyString())).thenReturn("sessionKey");
        when(encryptionHandler.encryptSessionKey(anyString(), anyString())).thenReturn("encrypted");

        PeerConnection established = manager.connectWithCode(code, userId, "Bob");

        // When
        Optional<PeerConnection> result = manager.getConnection(established.getConnectionId());

        // Then
        assertThat(result).isPresent();
        assertThat(result.get().getConnectionId()).isEqualTo(established.getConnectionId());
    }

    @Test
    @DisplayName("应该分享知识")
    void shouldShareKnowledge() {
        // Given
        String code = "ABC-XYZ-123";
        String userId = "user-002";

        ConnectionCodeGenerator.CodeInfo codeInfo = new ConnectionCodeGenerator.CodeInfo();
        codeInfo.userId = "user-001";
        codeInfo.userName = "Alice";
        codeInfo.publicKey = "publicKey";

        when(codeGenerator.validate(code)).thenReturn(codeInfo);
        when(encryptionHandler.generateKeyPair(anyString())).thenReturn("key");
        when(encryptionHandler.generateSessionKey(anyString())).thenReturn("sessionKey");
        when(encryptionHandler.encryptSessionKey(anyString(), anyString())).thenReturn("encrypted");
        when(encryptionHandler.encrypt(anyString(), anyString())).thenReturn("encryptedContent");

        PeerConnection connection = manager.connectWithCode(code, userId, "Bob");

        SharedKnowledge knowledge = SharedKnowledge.builder()
                .sourceUserId(userId)
                .sourceUserName("Bob")
                .encryptedContent("test knowledge")
                .knowledgeType("SKILL")
                .qualityScore(0.8)
                .build();

        // When
        SharedKnowledge shared = manager.shareKnowledge(connection.getConnectionId(), knowledge);

        // Then
        assertThat(shared).isNotNull();
        assertThat(shared.getKnowledgeId()).isNotNull();
        assertThat(shared.getSourceUserId()).isEqualTo(userId);
        verify(encryptionHandler).encrypt(anyString(), eq(connection.getConnectionId()));
    }

    @Test
    @DisplayName("应该接收知识")
    void shouldReceiveKnowledge() {
        // Given
        String code = "ABC-XYZ-123";
        String userId = "user-002";

        ConnectionCodeGenerator.CodeInfo codeInfo = new ConnectionCodeGenerator.CodeInfo();
        codeInfo.userId = "user-001";
        codeInfo.userName = "Alice";
        codeInfo.publicKey = "publicKey";

        when(codeGenerator.validate(code)).thenReturn(codeInfo);
        when(encryptionHandler.generateKeyPair(anyString())).thenReturn("key");
        when(encryptionHandler.generateSessionKey(anyString())).thenReturn("sessionKey");
        when(encryptionHandler.encryptSessionKey(anyString(), anyString())).thenReturn("encrypted");
        when(encryptionHandler.encrypt(anyString(), anyString())).thenReturn("encryptedContent");
        when(encryptionHandler.decrypt(anyString(), anyString())).thenReturn("decrypted");

        PeerConnection connection = manager.connectWithCode(code, userId, "Bob");

        SharedKnowledge knowledge = SharedKnowledge.builder()
                .sourceUserId(userId)
                .sourceUserName("Bob")
                .encryptedContent("test")
                .knowledgeType("SKILL")
                .qualityScore(0.8)
                .build();

        manager.shareKnowledge(connection.getConnectionId(), knowledge);

        // When
        List<SharedKnowledge> received = manager.receiveKnowledge(connection.getConnectionId());

        // Then
        assertThat(received).isNotEmpty();
        verify(encryptionHandler, atLeastOnce()).decrypt(anyString(), eq(connection.getConnectionId()));
    }

    @Test
    @DisplayName("应该验证知识")
    void shouldVerifyKnowledge() {
        // Given
        String code = "ABC-XYZ-123";
        String userId = "user-002";

        ConnectionCodeGenerator.CodeInfo codeInfo = new ConnectionCodeGenerator.CodeInfo();
        codeInfo.userId = "user-001";
        codeInfo.userName = "Alice";
        codeInfo.publicKey = "publicKey";

        when(codeGenerator.validate(code)).thenReturn(codeInfo);
        when(encryptionHandler.generateKeyPair(anyString())).thenReturn("key");
        when(encryptionHandler.generateSessionKey(anyString())).thenReturn("sessionKey");
        when(encryptionHandler.encryptSessionKey(anyString(), anyString())).thenReturn("encrypted");
        when(encryptionHandler.encrypt(anyString(), anyString())).thenReturn("encryptedContent");

        PeerConnection connection = manager.connectWithCode(code, userId, "Bob");

        SharedKnowledge knowledge = SharedKnowledge.builder()
                .sourceUserId(userId)
                .sourceUserName("Bob")
                .encryptedContent("test")
                .knowledgeType("SKILL")
                .qualityScore(0.8)
                .build();

        SharedKnowledge shared = manager.shareKnowledge(connection.getConnectionId(), knowledge);

        // When
        boolean result = manager.verifyKnowledge(shared.getKnowledgeId(), 0.9);

        // Then
        assertThat(result).isTrue();
    }

    @Test
    @DisplayName("应该获取统计信息")
    void shouldGetStatistics() {
        // Given
        String code = "ABC-XYZ-123";
        String userId = "user-002";

        ConnectionCodeGenerator.CodeInfo codeInfo = new ConnectionCodeGenerator.CodeInfo();
        codeInfo.userId = "user-001";
        codeInfo.userName = "Alice";
        codeInfo.publicKey = "publicKey";

        when(codeGenerator.validate(code)).thenReturn(codeInfo);
        when(encryptionHandler.generateKeyPair(anyString())).thenReturn("key");
        when(encryptionHandler.generateSessionKey(anyString())).thenReturn("sessionKey");
        when(encryptionHandler.encryptSessionKey(anyString(), anyString())).thenReturn("encrypted");

        manager.connectWithCode(code, userId, "Bob");

        // When
        Map<String, Object> stats = manager.getSharingStatistics(userId);

        // Then
        assertThat(stats).isNotNull();
        assertThat(stats).containsKeys("connections_total", "connections_established");
        assertThat(stats.get("connections_total")).isEqualTo(1);
    }

    @Test
    @DisplayName("应该加密内容")
    void shouldEncryptContent() {
        // Given
        when(encryptionHandler.encrypt(anyString(), anyString())).thenReturn("encrypted");

        // When
        String result = manager.encrypt("test", "conn-001");

        // Then
        assertThat(result).isEqualTo("encrypted");
        verify(encryptionHandler).encrypt("test", "conn-001");
    }

    @Test
    @DisplayName("应该解密内容")
    void shouldDecryptContent() {
        // Given
        when(encryptionHandler.decrypt(anyString(), anyString())).thenReturn("decrypted");

        // When
        String result = manager.decrypt("encrypted", "conn-001");

        // Then
        assertThat(result).isEqualTo("decrypted");
        verify(encryptionHandler).decrypt("encrypted", "conn-001");
    }
}

