package top.yumbo.ai.omni.p2p.api;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * P2P连接管理器接口
 * (P2P Connection Manager Interface)
 *
 * <p>管理端到端连接的生命周期</p>
 * <p>Manages the lifecycle of peer-to-peer connections</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface P2PConnectionManager {

    /**
     * 建立端到端连接
     * (Establish a peer-to-peer connection)
     *
     * @param sourceEndpoint 源端点信息
     * @param targetEndpoint 目标端点信息
     * @param config 连接配置
     * @return 连接对象
     */
    P2PConnection establish(
        P2PConnection.EndpointInfo sourceEndpoint,
        P2PConnection.EndpointInfo targetEndpoint,
        Map<String, Object> config
    );

    /**
     * 通过安全握手建立端到端连接
     * (Establish connection through secure handshake)
     *
     * @param sourceEndpoint 源端点信息
     * @param targetEndpoint 目标端点信息
     * @param connectionCode 连接码
     * @param config 连接配置
     * @return 连接对象
     */
    P2PConnection establishWithHandshake(
        P2PConnection.EndpointInfo sourceEndpoint,
        P2PConnection.EndpointInfo targetEndpoint,
        String connectionCode,
        Map<String, Object> config
    );

    /**
     * 通过连接ID获取连接
     * (Get connection by ID)
     *
     * @param connectionId 连接ID
     * @return 连接对象（如果存在）
     */
    Optional<P2PConnection> getConnection(String connectionId);

    /**
     * 获取所有活跃连接
     * (Get all active connections)
     *
     * @return 活跃连接列表
     */
    List<P2PConnection> getActiveConnections();

    /**
     * 关闭连接
     * (Close connection)
     *
     * @param connectionId 连接ID
     * @return 是否成功关闭
     */
    boolean closeConnection(String connectionId);

    /**
     * 验证连接健康状态
     * (Verify connection health)
     *
     * @param connectionId 连接ID
     * @return 连接是否健康
     */
    boolean isHealthy(String connectionId);

    /**
     * 通过连接进行数据传输
     * (Transfer data through connection)
     *
     * @param connectionId 连接ID
     * @param query 查询条件
     * @param batchSize 批次大小
     * @return 传输结果
     */
    P2PDataTransferService.TransferResult transferThroughConnection(
        String connectionId,
        Map<String, Object> query,
        int batchSize
    );

    /**
     * 通过 IP 地址直接连接（跨网络）
     * (Connect directly via IP address - cross-network)
     *
     * @param remoteIp 远程 IP 地址
     * @param remotePort 远程端口
     * @param connectionCode 连接码
     * @param config 连接配置
     * @return 连接对象
     */
    P2PConnection connectByIp(
        String remoteIp,
        int remotePort,
        String connectionCode,
        Map<String, Object> config
    );

    /**
     * 通过 IP 地址和端点 ID 连接
     * (Connect via IP address and endpoint ID)
     *
     * @param remoteIp 远程 IP 地址
     * @param remotePort 远程端口
     * @param endpointId 远程端点 ID
     * @param connectionCode 连接码
     * @param config 连接配置
     * @return 连接对象
     */
    P2PConnection connectByIpAndEndpoint(
        String remoteIp,
        int remotePort,
        String endpointId,
        String connectionCode,
        Map<String, Object> config
    );

    /**
     * 获取连接统计信息
     * (Get connection statistics)
     *
     * @param connectionId 连接ID
     * @return 统计信息
     */
    Map<String, Object> getConnectionStatistics(String connectionId);
}
