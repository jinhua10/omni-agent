package top.yumbo.ai.example;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;
import top.yumbo.ai.omni.p2p.api.P2PTransferBridge;
import top.yumbo.ai.p2p.core.DefaultP2PTransferBridge;

import java.util.HashMap;
import java.util.Map;

/**
 * P2P数据传输示例
 * (P2P Data Transfer Example)
 *
 * <p>演示如何在不同存储之间传输数据</p>
 * <p>Example: SQLite → Elasticsearch, MongoDB → Redis</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@SpringBootApplication
public class P2PTransferExample {

    public static void main(String[] args) {
        SpringApplication.run(P2PTransferExample.class, args);
    }

    @Bean
    public P2PTransferBridge transferBridge() {
        return new DefaultP2PTransferBridge();
    }

    @Bean
    public CommandLineRunner transferDemo(
            P2PTransferBridge transferBridge,
            P2PDataTransferService sourceService,
            P2PDataTransferService targetService) {

        return args -> {
            log.info("=== P2P Data Transfer Demo ===");

            // 示例1: 单向传输 SQLite → Elasticsearch
            log.info("\n--- Example 1: SQLite to Elasticsearch ---");
            
            Map<String, Object> query = new HashMap<>();
            query.put("type", "knowledge");
            query.put("limit", 100);

            P2PDataTransferService.TransferResult result = transferBridge.transfer(
                sourceService,
                targetService,
                query,
                data -> {
                    // 自定义数据转换
                    Map<String, Object> transformed = new HashMap<>(data);
                    transformed.put("transferred_at", System.currentTimeMillis());
                    transformed.put("source_type", "sqlite");
                    return transformed;
                },
                50  // 批次大小
            );

            log.info("Transfer Result: total={}, success={}, failure={}, duration={}ms",
                result.getTotalRecords(),
                result.getSuccessCount(),
                result.getFailureCount(),
                result.getDurationMs());

            // 示例2: 双向同步
            log.info("\n--- Example 2: Bidirectional Sync ---");
            
            P2PTransferBridge.SyncResult syncResult = transferBridge.bidirectionalSync(
                sourceService,
                targetService,
                P2PTransferBridge.SyncStrategy.MERGE
            );

            log.info("Sync Result: source→target={}, target→source={}, duration={}ms",
                syncResult.getService1ToService2Count(),
                syncResult.getService2ToService1Count(),
                syncResult.getDurationMs());

            // 示例3: 查看统计信息
            log.info("\n--- Example 3: Statistics ---");
            
            Map<String, Object> sourceStats = sourceService.getTransferStatistics();
            Map<String, Object> targetStats = targetService.getTransferStatistics();
            
            log.info("Source Stats: {}", sourceStats);
            log.info("Target Stats: {}", targetStats);
        };
    }
}

