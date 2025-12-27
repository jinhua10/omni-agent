package top.yumbo.ai.example;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.omni.p2p.api.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * P2P安全连接示例
 * (P2P Secure Connection Example)
 *
 * <p>演示如何通过网络发现和连接码建立安全的端到端连接</p>
 * <p>Demonstrates how to establish secure P2P connections through network discovery and connection codes</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@SpringBootApplication
public class P2PSecureConnectionExample {

    public static void main(String[] args) {
        SpringApplication.run(P2PSecureConnectionExample.class, args);
    }

    @Bean
    public CommandLineRunner secureConnectionDemo(
            P2PConnectionManager connectionManager,
            P2PEndpointDiscovery endpointDiscovery,
            P2PSecureHandshake secureHandshake) {

        return args -> {
            log.info("=== P2P安全连接示例 (P2P Secure Connection Example) ===\n");

            // ========================================
            // 场景1: 端点注册和发现
            // ========================================
            log.info("--- 场景1: 端点注册和发现 ---");

            // 步骤1: 注册本地端点
            P2PConnection.EndpointInfo localEndpoint = new P2PConnection.EndpointInfo(
                    "sqlite-local-001",
                    "sqlite"
            );
            localEndpoint.setDatabase("./data/local.db");
            localEndpoint.setHost("192.168.1.100");
            localEndpoint.setPort(8080);

            // 生成连接码（8位随机码）
            String connectionCode = endpointDiscovery.generateConnectionCode(
                    localEndpoint.getEndpointId(),
                    30  // 30分钟有效期
            );

            log.info("✓ 生成连接码: {} (有效期: 30分钟)", connectionCode);

            // 注册端点到网络
            P2PEndpointDiscovery.EndpointRegistration registration = 
                    endpointDiscovery.registerEndpoint(localEndpoint, connectionCode);

            log.info("✓ 端点已注册到网络:");
            log.info("  - 端点ID: {}", registration.getEndpointId());
            log.info("  - 连接码: {}", registration.getConnectionCode());
            log.info("  - 注册时间: {}", registration.getRegisteredAt());
            log.info("  - 过期时间: {}", registration.getExpiresAt());

            // 步骤2: 注册远程端点（模拟另一个节点）
            P2PConnection.EndpointInfo remoteEndpoint = new P2PConnection.EndpointInfo(
                    "es-remote-001",
                    "elasticsearch"
            );
            remoteEndpoint.setHost("192.168.1.200");
            remoteEndpoint.setPort(9200);
            remoteEndpoint.setDatabase("knowledge_base");

            String remoteConnectionCode = endpointDiscovery.generateConnectionCode(
                    remoteEndpoint.getEndpointId(),
                    30
            );

            endpointDiscovery.registerEndpoint(remoteEndpoint, remoteConnectionCode);
            log.info("✓ 远程端点已注册: {} (连接码: {})", 
                    remoteEndpoint.getEndpointId(), 
                    remoteConnectionCode);

            // 步骤3: 扫描网络中的端点
            log.info("\n--- 扫描网络中的可用端点 ---");

            P2PEndpointDiscovery.EndpointFilter filter = new P2PEndpointDiscovery.EndpointFilter();
            // 不设置过滤条件，扫描所有端点

            List<P2PEndpointDiscovery.DiscoveredEndpoint> discovered = 
                    endpointDiscovery.scanEndpoints(filter);

            log.info("发现 {} 个端点:", discovered.size());
            for (P2PEndpointDiscovery.DiscoveredEndpoint endpoint : discovered) {
                log.info("  - {} ({}) @ {}:{}",
                        endpoint.getEndpointInfo().getEndpointId(),
                        endpoint.getEndpointInfo().getStorageType(),
                        endpoint.getEndpointInfo().getHost(),
                        endpoint.getEndpointInfo().getPort());
                log.info("    状态: {}, 需要认证: {}",
                        endpoint.getStatus(),
                        endpoint.isRequiresAuthentication() ? "是" : "否");
            }

            // 步骤4: 按存储类型过滤
            log.info("\n--- 查找Elasticsearch端点 ---");
            
            P2PEndpointDiscovery.EndpointFilter esFilter = new P2PEndpointDiscovery.EndpointFilter();
            esFilter.setStorageType("elasticsearch");

            List<P2PEndpointDiscovery.DiscoveredEndpoint> esEndpoints = 
                    endpointDiscovery.scanEndpoints(esFilter);

            log.info("找到 {} 个Elasticsearch端点", esEndpoints.size());
            for (P2PEndpointDiscovery.DiscoveredEndpoint endpoint : esEndpoints) {
                log.info("  ✓ {}", endpoint.getEndpointInfo().getEndpointId());
            }

            // ========================================
            // 场景2: 安全握手建立连接
            // ========================================
            log.info("\n--- 场景2: 安全握手建立连接 ---");

            // 步骤1: 发起握手
            log.info("从 {} 向 {} 发起握手...",
                    localEndpoint.getEndpointId(),
                    remoteEndpoint.getEndpointId());

            P2PSecureHandshake.HandshakeSession session = secureHandshake.initiateHandshake(
                    localEndpoint,
                    remoteEndpoint,
                    remoteConnectionCode
            );

            log.info("✓ 握手会话创建:");
            log.info("  - 会话ID: {}", session.getSessionId());
            log.info("  - 挑战令牌: {}", session.getChallengeToken().substring(0, 20) + "...");
            log.info("  - 状态: {}", session.getStatus());
            log.info("  - 过期时间: {}", session.getExpiresAt());

            // 步骤2: 接受握手
            log.info("\n目标端点验证连接码并接受握手...");

            P2PSecureHandshake.HandshakeResult handshakeResult = 
                    secureHandshake.acceptHandshake(
                            session.getSessionId(),
                            remoteConnectionCode
                    );

            if (handshakeResult.isSuccess()) {
                log.info("✓ 握手成功!");
                log.info("  - 连接ID: {}", handshakeResult.getConnectionId());
                log.info("  - 共享密钥: {}", 
                        handshakeResult.getSharedSecret().substring(0, 30) + "...");
                log.info("  - 建立时间: {}", handshakeResult.getEstablishedAt());
            } else {
                log.error("✗ 握手失败: {}", handshakeResult.getFailureReason());
            }

            // ========================================
            // 场景3: 通过连接码建立安全连接
            // ========================================
            log.info("\n--- 场景3: 使用连接码建立安全P2P连接 ---");

            // 配置连接参数
            Map<String, Object> config = new HashMap<>();
            config.put("batch_size", 100);
            config.put("encryption", "AES-256");
            config.put("timeout", 30000);

            // 通过安全握手建立连接
            P2PConnection secureConnection = connectionManager.establishWithHandshake(
                    localEndpoint,
                    remoteEndpoint,
                    remoteConnectionCode,
                    config
            );

            log.info("✓ 安全连接已建立:");
            log.info("  - 连接ID: {}", secureConnection.getConnectionId());
            log.info("  - 源端点: {} ({})",
                    secureConnection.getSourceEndpoint().getEndpointId(),
                    secureConnection.getSourceEndpoint().getStorageType());
            log.info("  - 目标端点: {} ({})",
                    secureConnection.getTargetEndpoint().getEndpointId(),
                    secureConnection.getTargetEndpoint().getStorageType());
            log.info("  - 状态: {}", secureConnection.getStatus());
            log.info("  - 安全配置: {}", 
                    secureConnection.getConfiguration().get("secure_connection"));

            // ========================================
            // 场景4: 连接码验证失败场景
            // ========================================
            log.info("\n--- 场景4: 连接码验证失败 ---");

            try {
                String wrongCode = "WRONG123";
                log.info("尝试使用错误的连接码: {}", wrongCode);

                connectionManager.establishWithHandshake(
                        localEndpoint,
                        remoteEndpoint,
                        wrongCode,
                        config
                );

                log.error("不应该执行到这里!");
            } catch (SecurityException e) {
                log.info("✓ 正确拒绝了错误的连接码: {}", e.getMessage());
            }

            // ========================================
            // 场景5: 通过安全连接传输数据
            // ========================================
            log.info("\n--- 场景5: 通过安全连接传输数据 ---");

            Map<String, Object> query = new HashMap<>();
            query.put("type", "knowledge");
            query.put("limit", 100);

            try {
                P2PDataTransferService.TransferResult result =
                        connectionManager.transferThroughConnection(
                                secureConnection.getConnectionId(),
                                query,
                                50
                        );

                log.info("✓ 安全传输完成:");
                log.info("  - 总记录数: {}", result.getTotalRecords());
                log.info("  - 成功: {}", result.getSuccessCount());
                log.info("  - 失败: {}", result.getFailureCount());
                log.info("  - 耗时: {}ms", result.getDurationMs());
            } catch (Exception e) {
                log.warn("传输失败（可能是服务未配置）: {}", e.getMessage());
            }

            // ========================================
            // 场景6: 查看连接统计
            // ========================================
            log.info("\n--- 场景6: 连接统计信息 ---");

            Map<String, Object> stats = connectionManager.getConnectionStatistics(
                    secureConnection.getConnectionId()
            );

            log.info("连接统计:");
            stats.forEach((key, value) -> log.info("  {}: {}", key, value));

            // ========================================
            // 场景7: 清理资源
            // ========================================
            log.info("\n--- 场景7: 清理资源 ---");

            // 关闭连接
            boolean closed = connectionManager.closeConnection(secureConnection.getConnectionId());
            log.info("连接关闭: {}", closed ? "成功" : "失败");

            // 注销端点
            boolean unregistered = endpointDiscovery.unregisterEndpoint(localEndpoint.getEndpointId());
            log.info("端点注销: {}", unregistered ? "成功" : "失败");

            log.info("\n=== P2P安全连接示例完成 ===");
        };
    }
}
