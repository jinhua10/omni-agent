package top.yumbo.ai.omni.p2p.api.model;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * P2P连接模型
 * (P2P Connection Model)
 *
 * 表示两个用户之间的P2P连接
 * (Represents a P2P connection between two users)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
public class PeerConnection {

    /**
     * 连接ID
     * (Connection ID)
     */
    private String connectionId;

    /**
     * 本地用户ID
     * (Local user ID)
     */
    private String localUserId;

    /**
     * 远程用户ID
     * (Remote user ID)
     */
    private String remoteUserId;

    /**
     * 远程用户昵称
     * (Remote user nickname)
     */
    private String remoteUserName;

    /**
     * 连接状态
     * (Connection status)
     */
    private ConnectionStatus status;

    /**
     * 建立时间
     * (Established time)
     */
    private LocalDateTime establishedTime;

    /**
     * 最后活跃时间
     * (Last active time)
     */
    private LocalDateTime lastActiveTime;

    /**
     * 对称加密密钥
     * (Symmetric encryption key)
     */
    private String encryptionKey;

    /**
     * 连接状态枚举
     * (Connection Status Enum)
     */
    public enum ConnectionStatus {
        PENDING,      // 待建立
        ESTABLISHED,  // 已建立
        DISCONNECTED, // 已断开
        EXPIRED       // 已过期
    }
}

