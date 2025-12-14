package top.yumbo.ai.p2p.api;

import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * P2P端点发现服务
 * (P2P Endpoint Discovery Service)
 *
 * <p>支持通过网络发现和注册可用的存储端点</p>
 * <p>Supports discovering and registering available storage endpoints over the network</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface P2PEndpointDiscovery {

    /**
     * 注册本地端点到网络
     * (Register local endpoint to network)
     *
     * @param endpoint 端点信息 (Endpoint information)
     * @param connectionCode 连接码 (Connection code for secure pairing)
     * @return 注册结果 (Registration result)
     */
    EndpointRegistration registerEndpoint(P2PConnection.EndpointInfo endpoint, String connectionCode);

    /**
     * 注销端点
     * (Unregister endpoint)
     *
     * @param endpointId 端点ID (Endpoint ID)
     * @return 是否成功 (Success status)
     */
    boolean unregisterEndpoint(String endpointId);

    /**
     * 扫描网络中的可用端点
     * (Scan available endpoints in the network)
     *
     * @param filter 过滤条件 (Filter conditions)
     * @return 发现的端点列表 (List of discovered endpoints)
     */
    List<DiscoveredEndpoint> scanEndpoints(EndpointFilter filter);

    /**
     * 根据端点ID查找端点
     * (Find endpoint by ID)
     *
     * @param endpointId 端点ID (Endpoint ID)
     * @return 端点信息 (Endpoint information)
     */
    Optional<DiscoveredEndpoint> findEndpoint(String endpointId);

    /**
     * 验证连接码
     * (Validate connection code)
     *
     * @param endpointId 端点ID (Endpoint ID)
     * @param connectionCode 连接码 (Connection code)
     * @return 是否验证通过 (Validation result)
     */
    boolean validateConnectionCode(String endpointId, String connectionCode);

    /**
     * 生成连接码
     * (Generate connection code)
     *
     * @param endpointId 端点ID (Endpoint ID)
     * @param validityMinutes 有效期（分钟） (Validity in minutes)
     * @return 连接码 (Connection code)
     */
    String generateConnectionCode(String endpointId, int validityMinutes);

    /**
     * 端点注册信息
     */
    @Data
    class EndpointRegistration {
        private final String endpointId;
        private final String connectionCode;
        private final LocalDateTime registeredAt;
        private final LocalDateTime expiresAt;
        private final String registryUrl;

        public EndpointRegistration(String endpointId, String connectionCode, 
                                   LocalDateTime registeredAt, LocalDateTime expiresAt,
                                   String registryUrl) {
            this.endpointId = endpointId;
            this.connectionCode = connectionCode;
            this.registeredAt = registeredAt;
            this.expiresAt = expiresAt;
            this.registryUrl = registryUrl;
        }

    }

    /**
     * 发现的端点信息
     */
    @Data
    class DiscoveredEndpoint {
        private final P2PConnection.EndpointInfo endpointInfo;
        private final EndpointStatus status;
        private final LocalDateTime discoveredAt;
        private final Map<String, Object> capabilities;
        private final boolean requiresAuthentication;

        public DiscoveredEndpoint(P2PConnection.EndpointInfo endpointInfo,
                                 EndpointStatus status,
                                 LocalDateTime discoveredAt,
                                 Map<String, Object> capabilities,
                                 boolean requiresAuthentication) {
            this.endpointInfo = endpointInfo;
            this.status = status;
            this.discoveredAt = discoveredAt;
            this.capabilities = capabilities;
            this.requiresAuthentication = requiresAuthentication;
        }

    }

    /**
     * 端点过滤条件
     */
    @Data
    class EndpointFilter {
        private String storageType;        // 存储类型过滤
        private String networkSegment;     // 网络段过滤（如 "192.168.1.0/24"）
        private Integer maxDistance;       // 最大跳数
        private Boolean requiresAuth;      // 是否需要认证
        private Map<String, Object> metadata;  // 元数据过滤

    }

    /**
     * 端点状态
     */
    enum EndpointStatus {
        ONLINE,         // 在线
        OFFLINE,        // 离线
        BUSY,          // 忙碌
        MAINTENANCE    // 维护中
    }
}
