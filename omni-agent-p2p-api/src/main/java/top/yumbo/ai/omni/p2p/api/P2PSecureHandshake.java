package top.yumbo.ai.omni.p2p.api;

import lombok.Data;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * P2P安全握手协议
 * (P2P Secure Handshake Protocol)
 *
 * <p>在两个端点之间建立安全连接的握手过程</p>
 * <p>Handshake process for establishing secure connections between endpoints</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface P2PSecureHandshake {

    /**
     * 发起握手请求
     * (Initiate handshake request)
     *
     * @param sourceEndpoint 源端点 (Source endpoint)
     * @param targetEndpoint 目标端点 (Target endpoint)
     * @param connectionCode 连接码 (Connection code)
     * @return 握手会话 (Handshake session)
     */
    HandshakeSession initiateHandshake(P2PConnection.EndpointInfo sourceEndpoint,
                                      P2PConnection.EndpointInfo targetEndpoint,
                                      String connectionCode);

    /**
     * 接受握手请求
     * (Accept handshake request)
     *
     * @param sessionId 会话ID (Session ID)
     * @param connectionCode 连接码 (Connection code)
     * @return 握手结果 (Handshake result)
     */
    HandshakeResult acceptHandshake(String sessionId, String connectionCode);

    /**
     * 拒绝握手请求
     * (Reject handshake request)
     *
     * @param sessionId 会话ID (Session ID)
     * @param reason 拒绝原因 (Rejection reason)
     */
    void rejectHandshake(String sessionId, String reason);

    /**
     * 验证握手完整性
     * (Verify handshake integrity)
     *
     * @param sessionId 会话ID (Session ID)
     * @return 是否有效 (Validity status)
     */
    boolean verifyHandshake(String sessionId);

    /**
     * 获取会话信息
     * (Get session information)
     *
     * @param sessionId 会话ID (Session ID)
     * @return 会话信息 (Session information)
     */
    HandshakeSession getSession(String sessionId);

    /**
     * 握手会话
     */
    @Data
    class HandshakeSession {
        private final String sessionId;
        private final P2PConnection.EndpointInfo sourceEndpoint;
        private final P2PConnection.EndpointInfo targetEndpoint;
        private final HandshakeStatus status;
        private final LocalDateTime initiatedAt;
        private final LocalDateTime expiresAt;
        private final String challengeToken;
        private final Map<String, Object> securityContext;

        public HandshakeSession(String sessionId,
                              P2PConnection.EndpointInfo sourceEndpoint,
                              P2PConnection.EndpointInfo targetEndpoint,
                              HandshakeStatus status,
                              LocalDateTime initiatedAt,
                              LocalDateTime expiresAt,
                              String challengeToken,
                              Map<String, Object> securityContext) {
            this.sessionId = sessionId;
            this.sourceEndpoint = sourceEndpoint;
            this.targetEndpoint = targetEndpoint;
            this.status = status;
            this.initiatedAt = initiatedAt;
            this.expiresAt = expiresAt;
            this.challengeToken = challengeToken;
            this.securityContext = securityContext;
        }

    }

    /**
     * 握手结果
     */
    @Data
    class HandshakeResult {
        private final boolean success;
        private final String sessionId;
        private final String connectionId;
        private final String sharedSecret;
        private final LocalDateTime establishedAt;
        private final String failureReason;

        public HandshakeResult(boolean success, String sessionId, String connectionId,
                             String sharedSecret, LocalDateTime establishedAt,
                             String failureReason) {
            this.success = success;
            this.sessionId = sessionId;
            this.connectionId = connectionId;
            this.sharedSecret = sharedSecret;
            this.establishedAt = establishedAt;
            this.failureReason = failureReason;
        }

    }

    /**
     * 握手状态
     */
    enum HandshakeStatus {
        INITIATED,      // 已发起
        PENDING,        // 等待响应
        ACCEPTED,       // 已接受
        REJECTED,       // 已拒绝
        COMPLETED,      // 已完成
        EXPIRED,        // 已过期
        FAILED          // 失败
    }
}
