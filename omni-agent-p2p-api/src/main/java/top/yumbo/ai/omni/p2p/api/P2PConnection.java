package top.yumbo.ai.omni.p2p.api;

import lombok.Data;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * P2P端到端连接接口
 * (P2P Peer-to-Peer Connection Interface)
 *
 * <p>管理两个存储端点之间的持久化连接</p>
 * <p>Manages persistent connections between two storage endpoints</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface P2PConnection {

    /**
     * 连接ID
     */
    String getConnectionId();

    /**
     * 源端点信息
     */
    EndpointInfo getSourceEndpoint();

    /**
     * 目标端点信息
     */
    EndpointInfo getTargetEndpoint();

    /**
     * 连接状态
     */
    ConnectionStatus getStatus();

    /**
     * 建立时间
     */
    LocalDateTime getEstablishedTime();

    /**
     * 最后活跃时间
     */
    LocalDateTime getLastActiveTime();

    /**
     * 连接配置
     */
    Map<String, Object> getConfiguration();

    /**
     * 验证连接是否可用
     */
    boolean isAlive();

    /**
     * 关闭连接
     */
    void close();

    /**
     * 端点信息
     */
    @Data
    class EndpointInfo {
        private String endpointId;
        private String storageType;
        private String host;
        private int port;
        private String database;
        private Map<String, Object> metadata;

        public EndpointInfo(String endpointId, String storageType) {
            this.endpointId = endpointId;
            this.storageType = storageType;
        }

    }

    /**
     * 连接状态枚举
     */
    enum ConnectionStatus {
        CONNECTING,      // 连接中
        ESTABLISHED,     // 已建立
        TRANSFERRING,    // 传输中
        IDLE,           // 空闲
        ERROR,          // 错误
        CLOSED          // 已关闭
    }
}
