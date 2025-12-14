package top.yumbo.ai.example;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.p2p.api.P2PDataTransferService;
import top.yumbo.ai.p2p.api.P2PTransferBridge;

import java.util.Map;
import java.util.function.Function;

/**
 * H2 P2P数据传输示例
 * (H2 P2P Data Transfer Example)
 *
 * <p>演示如何使用H2数据库进行P2P数据传输</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@SpringBootApplication
public class H2P2PTransferExample {

    public static void main(String[] args) {
        SpringApplication.run(H2P2PTransferExample.class, args);
    }

    @Bean
    public CommandLineRunner h2P2PDemo(
            P2PDataTransferService h2P2PDataTransferService,
            P2PTransferBridge transferBridge) {
        
        return args -> {
            System.out.println("\n========== H2 P2P数据传输示例 ==========\n");

            // 1. 测试连接
            System.out.println("1. 测试H2数据库连接...");
            boolean connected = h2P2PDataTransferService.testConnection();
            System.out.println("   连接状态: " + (connected ? "✅ 成功" : "❌ 失败"));

            if (!connected) {
                System.out.println("   H2数据库连接失败，请检查配置");
                return;
            }

            // 2. 查询统计信息
            System.out.println("\n2. 查询H2数据库统计信息...");
            Map<String, Object> stats = h2P2PDataTransferService.getTransferStatistics();
            stats.forEach((key, value) -> 
                System.out.println("   " + key + ": " + value)
            );

            // 3. 批量传输示例
            System.out.println("\n3. 执行批量数据传输...");
            Map<String, Object> query = Map.of(
                "type", "document",
                "limit", 100
            );

            P2PDataTransferService.TransferResult result = 
                h2P2PDataTransferService.batchTransfer(query, 50);

            System.out.println("   传输结果:");
            System.out.println("   - 总记录数: " + result.getTotalRecords());
            System.out.println("   - 成功数量: " + result.getSuccessCount());
            System.out.println("   - 失败数量: " + result.getFailureCount());
            System.out.println("   - 耗时: " + result.getDurationMs() + "ms");

            if (result.getErrors() != null && !result.getErrors().isEmpty()) {
                System.out.println("   - 错误信息:");
                result.getErrors().forEach(error -> 
                    System.out.println("     * " + error)
                );
            }

            // 4. 跨存储传输示例（如果配置了其他存储）
            System.out.println("\n4. H2跨存储传输示例");
            System.out.println("   提示: 可以通过P2PTransferBridge实现H2 → Elasticsearch等跨存储传输");
            System.out.println("   示例代码:");
            System.out.println("   ```java");
            System.out.println("   TransferResult result = transferBridge.transfer(");
            System.out.println("       h2Service,           // 源: H2");
            System.out.println("       esService,           // 目标: Elasticsearch");
            System.out.println("       query,               // 查询条件");
            System.out.println("       Function.identity(), // 数据转换");
            System.out.println("       100                  // 批次大小");
            System.out.println("   );");
            System.out.println("   ```");

            System.out.println("\n========== 示例完成 ==========\n");
        };
    }
}
