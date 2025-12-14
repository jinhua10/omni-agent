package top.yumbo.ai.p2p.api.model;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * 连接码模型
 * (Connection Code Model)
 *
 * 用于P2P连接的一次性验证码
 * (One-time verification code for P2P connection)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
public class ConnectionCode {

    /**
     * 连接码（6位数字）
     * (Connection code - 6 digits)
     */
    private String code;

    /**
     * 发起者用户ID
     * (Initiator user ID)
     */
    private String initiatorId;

    /**
     * 发起者昵称
     * (Initiator nickname)
     */
    private String initiatorName;

    /**
     * 创建时间
     * (Create time)
     */
    private LocalDateTime createTime;

    /**
     * 过期时间
     * (Expiry time)
     */
    private LocalDateTime expiryTime;

    /**
     * 是否已使用
     * (Is used)
     */
    private boolean used;

    /**
     * 加密公钥
     * (Encryption public key)
     */
    private String publicKey;
}

