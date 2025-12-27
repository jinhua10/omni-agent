package top.yumbo.ai.p2p.core;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import top.yumbo.ai.omni.p2p.api.P2PConnection;
import top.yumbo.ai.omni.p2p.api.P2PSecureHandshake;

import static org.junit.jupiter.api.Assertions.*;

/**
 * DefaultP2PSecureHandshake 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
class DefaultP2PSecureHandshakeTest {

    private DefaultP2PSecureHandshake handshake;
    private DefaultP2PEndpointDiscovery discovery;
    private P2PConnection.EndpointInfo initiatorEndpoint;
    private P2PConnection.EndpointInfo acceptorEndpoint;

    @BeforeEach
    void setUp() {
        discovery = new DefaultP2PEndpointDiscovery();
        handshake = new DefaultP2PSecureHandshake();
        // 使用反射设置 @Autowired 字段
        ReflectionTestUtils.setField(handshake, "endpointDiscovery", discovery);
        initiatorEndpoint = new P2PConnection.EndpointInfo("initiator", "memory");
        acceptorEndpoint = new P2PConnection.EndpointInfo("acceptor", "memory");

        // 注册端点和连接码
        String code = discovery.generateConnectionCode("acceptor", 1440);
        discovery.registerEndpoint(acceptorEndpoint, "ABC123");
    }

    @Test
    void testInitiateHandshake() {
        // When
        P2PSecureHandshake.HandshakeSession session = handshake.initiateHandshake(
            initiatorEndpoint,
            acceptorEndpoint,
            "ABC123"
        );

        // Then
        assertNotNull(session);
        assertNotNull(session.getSessionId());
        assertNotNull(session.getChallengeToken());
        assertEquals(P2PSecureHandshake.HandshakeStatus.INITIATED, session.getStatus());
    }

    @Test
    void testAcceptHandshake_ValidConnectionCode() {
        // Given
        P2PSecureHandshake.HandshakeSession initiatedSession = handshake.initiateHandshake(
            initiatorEndpoint,
            acceptorEndpoint,
            "ABC123"
        );

        // When
        P2PSecureHandshake.HandshakeResult result = handshake.acceptHandshake(
            initiatedSession.getSessionId(),
            "ABC123"
        );

        // Then
        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertNotNull(result.getSharedSecret());
    }

    @Test
    void testAcceptHandshake_InvalidConnectionCode() {
        // Given
        P2PSecureHandshake.HandshakeSession initiatedSession = handshake.initiateHandshake(
            initiatorEndpoint,
            acceptorEndpoint,
            "ABC123"
        );

        // When
        P2PSecureHandshake.HandshakeResult result = handshake.acceptHandshake(
            initiatedSession.getSessionId(),
            "WRONG_CODE"
        );

        // Then
        assertNotNull(result);
        assertFalse(result.isSuccess());
        assertNotNull(result.getFailureReason());
    }

    @Test
    void testAcceptHandshake_InvalidSessionId() {
        // When
        P2PSecureHandshake.HandshakeResult result = handshake.acceptHandshake(
            "invalid-session-id",
            "ABC123"
        );

        // Then
        assertNotNull(result);
        assertFalse(result.isSuccess());
    }

    @Test
    void testVerifyHandshake() {
        // Given
        P2PSecureHandshake.HandshakeSession initiatedSession = handshake.initiateHandshake(
            initiatorEndpoint,
            acceptorEndpoint,
            "ABC123"
        );
        // Accept the handshake first
        handshake.acceptHandshake(initiatedSession.getSessionId(), "ABC123");

        // When
        boolean isValid = handshake.verifyHandshake(initiatedSession.getSessionId());

        // Then
        assertTrue(isValid);
    }

    @Test
    void testVerifyHandshake_InvalidSessionId() {
        // When
        boolean isValid = handshake.verifyHandshake("invalid-session-id");

        // Then
        assertFalse(isValid);
    }

    @Test
    void testFullHandshakeFlow() {
        // Given
        String connectionCode = "ABC123";

        // Step 1: Initiator starts handshake
        P2PSecureHandshake.HandshakeSession initiatedSession = handshake.initiateHandshake(
            initiatorEndpoint,
            acceptorEndpoint,
            connectionCode
        );
        assertNotNull(initiatedSession);
        assertEquals(P2PSecureHandshake.HandshakeStatus.INITIATED, initiatedSession.getStatus());

        // Step 2: Acceptor accepts handshake
        P2PSecureHandshake.HandshakeResult result = handshake.acceptHandshake(
            initiatedSession.getSessionId(),
            connectionCode
        );
        assertNotNull(result);
        assertTrue(result.isSuccess());
        assertNotNull(result.getSharedSecret());

        // Step 3: Verify handshake
        boolean isValid = handshake.verifyHandshake(initiatedSession.getSessionId());
        assertTrue(isValid);
    }

    @Test
    void testHandshakeSession_TimestampCreation() {
        // When
        P2PSecureHandshake.HandshakeSession session = handshake.initiateHandshake(
            initiatorEndpoint,
            acceptorEndpoint,
            "ABC123"
        );

        // Then
        assertNotNull(session.getInitiatedAt());
    }

    @Test
    void testSharedSecretGeneration_Consistency() {
        // Given
        String connectionCode = "ABC123";

        // When - Complete two handshakes with same parameters
        P2PSecureHandshake.HandshakeSession session1 = handshake.initiateHandshake(
            initiatorEndpoint, acceptorEndpoint, connectionCode
        );
        P2PSecureHandshake.HandshakeResult result1 = handshake.acceptHandshake(
            session1.getSessionId(), connectionCode
        );

        P2PSecureHandshake.HandshakeSession session2 = handshake.initiateHandshake(
            initiatorEndpoint, acceptorEndpoint, connectionCode
        );
        P2PSecureHandshake.HandshakeResult result2 = handshake.acceptHandshake(
            session2.getSessionId(), connectionCode
        );

        // Then - shared secrets should be different (due to different challenges)
        assertNotEquals(result1.getSharedSecret(), result2.getSharedSecret());
    }

    @Test
    void testChallengeGeneration_Uniqueness() {
        // When
        P2PSecureHandshake.HandshakeSession session1 = handshake.initiateHandshake(
            initiatorEndpoint, acceptorEndpoint, "ABC123"
        );
        P2PSecureHandshake.HandshakeSession session2 = handshake.initiateHandshake(
            initiatorEndpoint, acceptorEndpoint, "ABC123"
        );

        // Then
        assertNotEquals(session1.getChallengeToken(), session2.getChallengeToken());
        assertNotEquals(session1.getSessionId(), session2.getSessionId());
    }
}


