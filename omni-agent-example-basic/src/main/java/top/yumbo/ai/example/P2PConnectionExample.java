package top.yumbo.ai.example;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.omni.p2p.api.P2PConnection;
import top.yumbo.ai.omni.p2p.api.P2PConnectionManager;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;
import top.yumbo.ai.omni.p2p.api.P2PTransferBridge;
import top.yumbo.ai.p2p.core.DefaultP2PConnectionManager;
import top.yumbo.ai.p2p.core.DefaultP2PTransferBridge;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * P2P端到端连接示例
 * (P2P Peer-to-Peer Connection Example)
 *
 * <p>演示如何建立和使用端到端连接进行数据传输</p>
 * <p>Demonstrates how to establish and use P2P connections for data transfer</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@SpringBootApplication
public class P2PConnectionExample {

    public static void main(String[] args) {
        SpringApplication.run(P2PConnectionExample.class, args);
    }

    @Bean
    public P2PTransferBridge transferBridge() {
        return new DefaultP2PTransferBridge();
    }

    @Bean
    public P2PConnectionManager connectionManager(P2PTransferBridge transferBridge) {
        return new DefaultP2PConnectionManager(transferBridge);
    }

    @Bean
    public CommandLineRunner connectionDemo(
            P2PConnectionManager connectionManager,
            Map<String, P2PDataTransferService> services) {

        return args -> {
            log.info("=== P2P端到端连接示例 (P2P Connection Example) ===\n");

            // 注册所有可用的数据传输服务
            DefaultP2PConnectionManager manager = (DefaultP2PConnectionManager) connectionManager;
            services.forEach((name, service) -> {
                String storageType = extractStorageType(name);
                manager.registerService(storageType, service);
                log.info("✓ 已注册服务: {} ({})", storageType, name);
            });

            log.info("\n--- 示例1: 建立SQLite → Elasticsearch连接 ---");
            
            // 配置源端点（SQLite）
            P2PConnection.EndpointInfo sqliteEndpoint = new P2PConnection.EndpointInfo(
                    "sqlite-local-db",
                    "sqlite"
            );
            sqliteEndpoint.setDatabase("./data/knowledge.db");
            sqliteEndpoint.setMetadata(Map.of("table", "knowledge_items"));

            // 配置目标端点（Elasticsearch）
            P2PConnection.EndpointInfo esEndpoint = new P2PConnection.EndpointInfo(
                    "es-search-cluster",
                    "elasticsearch"
            );
            esEndpoint.setHost("localhost");
            esEndpoint.setPort(9200);
            esEndpoint.setDatabase("knowledge_base");
            esEndpoint.setMetadata(Map.of("index", "knowledge_items"));

            // 连接配置
            Map<String, Object> config = new HashMap<>();
            config.put("batch_size", 100);
            config.put("auto_reconnect", true);
            config.put("max_retries", 3);

            // 建立连接
            P2PConnection connection = connectionManager.establish(
                    sqliteEndpoint,
                    esEndpoint,
                    config
            );

            log.info("✓ 连接已建立: {}", connection.getConnectionId());
            log.info("  源端点: {} ({})", 
                    connection.getSourceEndpoint().getStorageType(),
                    connection.getSourceEndpoint().getEndpointId());
            log.info("  目标端点: {} ({})",
                    connection.getTargetEndpoint().getStorageType(),
                    connection.getTargetEndpoint().getEndpointId());
            log.info("  状态: {}", connection.getStatus());

            log.info("\n--- 示例2: 通过连接传输数据 ---");

            // 定义查询条件
            Map<String, Object> query = new HashMap<>();
            query.put("type", "knowledge");
            query.put("limit", 1000);

            // 通过连接传输数据
            P2PDataTransferService.TransferResult result = 
                    connectionManager.transferThroughConnection(
                            connection.getConnectionId(),
                            query,
                            100
                    );

            log.info("✓ 传输完成:");
            log.info("  总记录数: {}", result.getTotalRecords());
            log.info("  成功: {}", result.getSuccessCount());
            log.info("  失败: {}", result.getFailureCount());
            log.info("  耗时: {}ms", result.getDurationMs());

            log.info("\n--- 示例3: 查看连接统计 ---");

            Map<String, Object> stats = connectionManager.getConnectionStatistics(
                    connection.getConnectionId()
            );

            log.info("连接统计信息:");
            stats.forEach((key, value) -> log.info("  {}: {}", key, value));

            log.info("\n--- 示例4: 多个连接同时工作 ---");

            // MongoDB → Redis 连接
            P2PConnection.EndpointInfo mongoEndpoint = new P2PConnection.EndpointInfo(
                    "mongo-primary",
                    "mongodb"
            );
            mongoEndpoint.setHost("localhost");
            mongoEndpoint.setPort(27017);
            mongoEndpoint.setDatabase("omni-agent");

            P2PConnection.EndpointInfo redisEndpoint = new P2PConnection.EndpointInfo(
                    "redis-cache",
                    "redis"
            );
            redisEndpoint.setHost("localhost");
            redisEndpoint.setPort(6379);

            P2PConnection mongoToRedis = connectionManager.establish(
                    mongoEndpoint,
                    redisEndpoint,
                    Map.of("batch_size", 50)
            );

            log.info("✓ 已建立第二个连接: {} ({} → {})",
                    mongoToRedis.getConnectionId(),
                    mongoToRedis.getSourceEndpoint().getStorageType(),
                    mongoToRedis.getTargetEndpoint().getStorageType());

            // 查看所有活跃连接
            List<P2PConnection> activeConnections = connectionManager.getActiveConnections();
            log.info("\n当前活跃连接数: {}", activeConnections.size());
            activeConnections.forEach(conn -> 
                log.info("  - {}: {} → {} ({})",
                        conn.getConnectionId(),
                        conn.getSourceEndpoint().getStorageType(),
                        conn.getTargetEndpoint().getStorageType(),
                        conn.getStatus())
            );

            log.info("\n--- 示例5: 连接健康检查 ---");

            boolean isHealthy = connectionManager.isHealthy(connection.getConnectionId());
            log.info("连接 {} 健康状态: {}", 
                    connection.getConnectionId(),
                    isHealthy ? "✓ 健康" : "✗ 异常");

            log.info("\n--- 示例6: 关闭连接 ---");

            boolean closed = connectionManager.closeConnection(connection.getConnectionId());
            log.info("连接 {} {}", 
                    connection.getConnectionId(),
                    closed ? "已关闭" : "关闭失败");

            log.info("\n=== 端到端连接示例完成 ===");
        };
    }

    private String extractStorageType(String serviceName) {
        // 从Bean名称提取存储类型
        // 例如: "sqliteP2PDataTransferService" -> "sqlite"
        if (serviceName.contains("sqlite")) return "sqlite";
        if (serviceName.contains("redis")) return "redis";
        if (serviceName.contains("mongo")) return "mongodb";
        if (serviceName.contains("elasticsearch") || serviceName.contains("es")) 
            return "elasticsearch";
        if (serviceName.contains("memory")) return "memory";
        return "unknown";
    }
}

