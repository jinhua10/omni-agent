package top.yumbo.ai.omni.example.basic.p2p;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.omni.p2p.api.P2PConnection;
import top.yumbo.ai.omni.p2p.api.P2PConnectionManager;
import top.yumbo.ai.omni.p2p.api.P2PEndpointDiscovery;

import java.util.HashMap;
import java.util.Map;

/**
 * P2P IP 直连示例
 * (P2P IP Direct Connection Example)
 *
 * <p>演示如何通过 IP 地址进行跨网络的 P2P 连接</p>
 * <p>Demonstrates how to establish cross-network P2P connections via IP address</p>
 *
 * 支持两种连接方式：
 * 1. 局域网连接码方式：扫描局域网内端点，使用连接码配对
 * 2. IP 直连方式：直接通过 IP:Port + 连接码跨网络连接
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@SpringBootApplication
public class P2PIPConnectionExample {

    public static void main(String[] args) {
        SpringApplication.run(P2PIPConnectionExample.class, args);
    }

    @Bean
    public CommandLineRunner ipConnectionDemo(
            P2PConnectionManager connectionManager,
            P2PEndpointDiscovery endpointDiscovery) {

        return args -> {
            log.info("=".repeat(80));
            log.info("P2P IP 直连示例 - 跨网络连接");
            log.info("=".repeat(80));

            // ========== 场景1: 注册本地端点 ==========
            log.info("\n【场景1】注册本地端点并生成连接码");

            // 创建端点信息（包含 IP 和端口）
            P2PConnection.EndpointInfo localEndpoint = new P2PConnection.EndpointInfo(
                    "storage-node-01",
                    "sqlite"
            );
            localEndpoint.setHost("192.168.1.100");  // 本地 IP
            localEndpoint.setPort(8081);              // 服务端口

            // 生成连接码（10分钟有效）
            String connectionCode = endpointDiscovery.generateConnectionCode(
                    localEndpoint.getEndpointId(),
                    10
            );

            // 注册端点到网络
            P2PEndpointDiscovery.EndpointRegistration registration =
                    endpointDiscovery.registerEndpoint(localEndpoint, connectionCode);

            log.info("✅ 端点已注册:");
            log.info("   端点 ID: {}", registration.getEndpointId());
            log.info("   连接码: {}", connectionCode);
            log.info("   本地地址: {}:{}", localEndpoint.getHost(), localEndpoint.getPort());
            log.info("   有效期至: {}", registration.getExpiresAt());

            // ========== 场景2: 局域网连接（传统方式） ==========
            log.info("\n【场景2】局域网内连接（扫描发现）");

            // 扫描局域网内的端点
            var discoveredEndpoints = endpointDiscovery.scanEndpoints(null);
            log.info("📡 扫描到 {} 个端点", discoveredEndpoints.size());

            if (!discoveredEndpoints.isEmpty()) {
                var discovered = discoveredEndpoints.get(0);
                log.info("   端点: {} ({})",
                        discovered.getEndpointInfo().getEndpointId(),
                        discovered.getEndpointInfo().getStorageType());
            }

            // ========== 场景3: IP 直连（跨网络） ==========
            log.info("\n【场景3】通过 IP 地址跨网络直连");

            // 远程服务器的信息（可以是公网 IP 或其他网段）
            String remoteIp = "203.0.113.50";      // 远程 IP（示例）
            int remotePort = 8081;                  // 远程端口
            String remoteConnectionCode = "ABC12345"; // 从远程获取的连接码

            log.info("🌐 连接到远程端点: {}:{}", remoteIp, remotePort);
            log.info("   使用连接码: {}", remoteConnectionCode);

            try {
                // 方法1: 仅通过 IP 和连接码连接
                Map<String, Object> config = new HashMap<>();
                config.put("local_storage_type", "sqlite");
                config.put("timeout_seconds", 30);

                // 注意：这个示例中远程端点需要实际存在才能成功连接
                // 实际使用时替换为真实的远程 IP 和连接码
                log.info("   尝试连接...");
                log.info("   ⚠️ 提示: 确保远程端点 {}:{} 已注册并生成连接码", remoteIp, remotePort);

                // P2PConnection connection = connectionManager.connectByIp(
                //         remoteIp,
                //         remotePort,
                //         remoteConnectionCode,
                //         config
                // );
                // log.info("✅ 连接成功: {}", connection.getConnectionId());

            } catch (Exception e) {
                log.warn("   ⚠️ 连接失败（演示模式）: {}", e.getMessage());
            }

            // ========== 场景4: IP + 端点ID 连接 ==========
            log.info("\n【场景4】通过 IP 地址 + 端点ID 连接");

            String remoteEndpointId = "storage-node-remote";

            log.info("🎯 连接到指定端点: {}", remoteEndpointId);
            log.info("   远程地址: {}:{}", remoteIp, remotePort);

            try {
                Map<String, Object> config = new HashMap<>();
                config.put("local_storage_type", "sqlite");

                // 方法2: 通过 IP + 端点 ID 连接（更精确）
                // P2PConnection connection = connectionManager.connectByIpAndEndpoint(
                //         remoteIp,
                //         remotePort,
                //         remoteEndpointId,
                //         remoteConnectionCode,
                //         config
                // );
                // log.info("✅ 连接成功: {}", connection.getConnectionId());

            } catch (Exception e) {
                log.warn("   ⚠️ 连接失败（演示模式）: {}", e.getMessage());
            }

            // ========== 使用说明 ==========
            log.info("\n" + "=".repeat(80));
            log.info("📖 跨网络 P2P 连接使用说明");
            log.info("=".repeat(80));
            log.info("\n1️⃣ 服务端（被连接方）操作:");
            log.info("   • 注册本地端点: endpointDiscovery.registerEndpoint(endpoint, connectionCode)");
            log.info("   • 生成连接码: endpointDiscovery.generateConnectionCode(endpointId, validMinutes)");
            log.info("   • 将连接码分享给客户端");
            log.info("   • 确保端口 {} 可访问（防火墙/NAT配置）", remotePort);

            log.info("\n2️⃣ 客户端（发起连接方）操作:");
            log.info("   • 获取服务端的 IP、端口和连接码");
            log.info("   • 方法A: connectionManager.connectByIp(ip, port, code, config)");
            log.info("   • 方法B: connectionManager.connectByIpAndEndpoint(ip, port, endpointId, code, config)");

            log.info("\n3️⃣ 网络要求:");
            log.info("   • 局域网: 端点可相互访问即可");
            log.info("   • 跨网络: 服务端需要公网 IP 或配置端口映射");
            log.info("   • 安全: 使用连接码验证，支持加密传输");

            log.info("\n4️⃣ 连接码安全:");
            log.info("   • 连接码具有时效性（默认10分钟）");
            log.info("   • 连接码一次性使用");
            log.info("   • 建议通过安全渠道传递连接码（加密消息、电话等）");

            log.info("\n" + "=".repeat(80));
            log.info("示例完成");
            log.info("=".repeat(80));
        };
    }
}

