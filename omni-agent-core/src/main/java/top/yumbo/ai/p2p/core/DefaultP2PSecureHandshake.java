package top.yumbo.ai.p2p.core;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.p2p.api.P2PConnection;
import top.yumbo.ai.omni.p2p.api.P2PEndpointDiscovery;
import top.yumbo.ai.omni.p2p.api.P2PSecureHandshake;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * P2P安全握手协议实现
 * (P2P Secure Handshake Protocol Implementation)
 *
 * <p>实现端点之间的安全握手和连接建立</p>
 * <p>Implements secure handshake and connection establishment between endpoints</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class DefaultP2PSecureHandshake implements P2PSecureHandshake {

    /**
     * 握手会话存储
     */
    private final Map<String, HandshakeSessionImpl> sessions = new ConcurrentHashMap<>();

    /**
     * 端点发现服务
     */
    @Autowired
    private P2PEndpointDiscovery endpointDiscovery;

    /**
     * 安全随机数生成器
     */
    private final SecureRandom secureRandom = new SecureRandom();

    /**
     * 握手超时时间（分钟）
     */
    private static final int HANDSHAKE_TIMEOUT_MINUTES = 5;

    @Override
    public HandshakeSession initiateHandshake(P2PConnection.EndpointInfo sourceEndpoint,
                                             P2PConnection.EndpointInfo targetEndpoint,
                                             String connectionCode) {
        log.info("发起握手: {} → {}", 
                sourceEndpoint.getEndpointId(), 
                targetEndpoint.getEndpointId());

        // 生成会话ID
        String sessionId = UUID.randomUUID().toString();

        // 生成挑战令牌
        String challengeToken = generateChallengeToken();

        // 创建会话
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime expiresAt = now.plusMinutes(HANDSHAKE_TIMEOUT_MINUTES);

        Map<String, Object> securityContext = new HashMap<>();
        securityContext.put("protocol_version", "1.0");
        securityContext.put("encryption", "AES-256");
        securityContext.put("initiated_at", now);

        HandshakeSessionImpl session = new HandshakeSessionImpl(
                sessionId,
                sourceEndpoint,
                targetEndpoint,
                HandshakeStatus.INITIATED,
                now,
                expiresAt,
                challengeToken,
                securityContext,
                connectionCode
        );

        sessions.put(sessionId, session);

        log.info("✓ 握手会话创建: {} (有效期至: {})", sessionId, expiresAt);

        return session;
    }

    @Override
    public HandshakeResult acceptHandshake(String sessionId, String connectionCode) {
        log.info("接受握手: {}", sessionId);

        HandshakeSessionImpl session = sessions.get(sessionId);
        if (session == null) {
            log.warn("✗ 握手会话不存在: {}", sessionId);
            return new HandshakeResult(
                    false, sessionId, null, null, null,
                    "Session not found"
            );
        }

        // 检查是否过期
        if (LocalDateTime.now().isAfter(session.getExpiresAt())) {
            log.warn("✗ 握手会话已过期: {}", sessionId);
            session.status = HandshakeStatus.EXPIRED;
            return new HandshakeResult(
                    false, sessionId, null, null, null,
                    "Session expired"
            );
        }

        // 验证连接码
        if (!session.connectionCode.equals(connectionCode)) {
            log.warn("✗ 连接码验证失败: {}", sessionId);
            session.status = HandshakeStatus.REJECTED;
            return new HandshakeResult(
                    false, sessionId, null, null, null,
                    "Invalid connection code"
            );
        }

        // 使用端点发现服务验证连接码
        String targetEndpointId = session.getTargetEndpoint().getEndpointId();
        if (!endpointDiscovery.validateConnectionCode(targetEndpointId, connectionCode)) {
            log.warn("✗ 端点连接码验证失败: {}", targetEndpointId);
            session.status = HandshakeStatus.REJECTED;
            return new HandshakeResult(
                    false, sessionId, null, null, null,
                    "Endpoint connection code validation failed"
            );
        }

        // 生成共享密钥
        String sharedSecret = generateSharedSecret(session);

        // 生成连接ID
        String connectionId = UUID.randomUUID().toString();

        // 更新会话状态
        session.status = HandshakeStatus.COMPLETED;
        session.connectionId = connectionId;
        session.sharedSecret = sharedSecret;
        session.completedAt = LocalDateTime.now();

        log.info("✓ 握手完成: {} → Connection: {}", sessionId, connectionId);

        return new HandshakeResult(
                true,
                sessionId,
                connectionId,
                sharedSecret,
                session.completedAt,
                null
        );
    }

    @Override
    public void rejectHandshake(String sessionId, String reason) {
        log.info("拒绝握手: {} (原因: {})", sessionId, reason);

        HandshakeSessionImpl session = sessions.get(sessionId);
        if (session != null) {
            session.status = HandshakeStatus.REJECTED;
            session.rejectionReason = reason;
            log.info("✓ 握手已拒绝: {}", sessionId);
        }
    }

    @Override
    public boolean verifyHandshake(String sessionId) {
        log.debug("验证握手: {}", sessionId);

        HandshakeSessionImpl session = sessions.get(sessionId);
        if (session == null) {
            return false;
        }

        // 检查状态和过期时间
        boolean valid = session.getStatus() == HandshakeStatus.COMPLETED &&
                       LocalDateTime.now().isBefore(session.getExpiresAt());

        log.debug("握手验证结果: {} = {}", sessionId, valid);
        return valid;
    }

    @Override
    public HandshakeSession getSession(String sessionId) {
        return sessions.get(sessionId);
    }

    /**
     * 生成挑战令牌
     */
    private String generateChallengeToken() {
        byte[] randomBytes = new byte[32];
        secureRandom.nextBytes(randomBytes);
        return Base64.getEncoder().encodeToString(randomBytes);
    }

    /**
     * 生成共享密钥
     */
    private String generateSharedSecret(HandshakeSessionImpl session) {
        // 简化实现：实际应使用ECDH或RSA密钥交换
        String combined = session.getSessionId() + 
                         session.getChallengeToken() + 
                         session.connectionCode +
                         System.currentTimeMillis();
        
        byte[] hash = combined.getBytes();
        return Base64.getEncoder().encodeToString(hash);
    }

    /**
     * 清理过期会话（定期任务）
     */
    public void cleanupExpiredSessions() {
        LocalDateTime now = LocalDateTime.now();
        sessions.entrySet().removeIf(entry -> 
            now.isAfter(entry.getValue().getExpiresAt())
        );
        log.debug("清理过期握手会话完成");
    }

    /**
     * 握手会话实现类
     */
    private static class HandshakeSessionImpl extends HandshakeSession {
        HandshakeStatus status;
        String connectionCode;
        String connectionId;
        String sharedSecret;
        LocalDateTime completedAt;
        String rejectionReason;

        HandshakeSessionImpl(String sessionId,
                           P2PConnection.EndpointInfo sourceEndpoint,
                           P2PConnection.EndpointInfo targetEndpoint,
                           HandshakeStatus status,
                           LocalDateTime initiatedAt,
                           LocalDateTime expiresAt,
                           String challengeToken,
                           Map<String, Object> securityContext,
                           String connectionCode) {
            super(sessionId, sourceEndpoint, targetEndpoint, status,
                  initiatedAt, expiresAt, challengeToken, securityContext);
            this.status = status;
            this.connectionCode = connectionCode;
        }

        @Override
        public HandshakeStatus getStatus() {
            return status;
        }
    }
}

