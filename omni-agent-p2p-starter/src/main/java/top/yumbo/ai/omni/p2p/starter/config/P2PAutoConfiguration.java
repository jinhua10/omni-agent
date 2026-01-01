package top.yumbo.ai.omni.p2p.starter.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.p2p.api.P2PCollaborationService;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;
import top.yumbo.ai.omni.p2p.api.P2PTransferBridge;
import top.yumbo.ai.omni.p2p.starter.core.DefaultP2PTransferBridge;
import top.yumbo.ai.omni.p2p.starter.memory.MemoryP2PCollaborationService;
import top.yumbo.ai.omni.p2p.starter.memory.MemoryP2PDataTransferService;

/**
 * P2P 统一自动配置
 * (P2P Unified Auto Configuration)
 *
 * <p>
 * 根据配置自动选择数据源实现：
 * - Memory（内存）- 默认
 * - H2（嵌入式数据库）
 * - SQLite（文件数据库）
 * - Redis（缓存数据库）
 * - MongoDB（文档数据库）
 * - Elasticsearch（搜索引擎）
 * </p>
 *
 * <p>配置示例：</p>
 * <pre>
 * # 使用内存实现（默认）
 * omni-agent.p2p.storage-type=memory
 *
 * # 使用 Redis
 * omni-agent.p2p.storage-type=redis
 * spring.redis.host=localhost
 * spring.redis.port=6379
 *
 * # 使用 MongoDB
 * omni-agent.p2p.storage-type=mongodb
 * spring.data.mongodb.uri=mongodb://localhost:27017/omni-agent
 * </pre>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(P2PProperties.class)
@ComponentScan(basePackages = {
        "top.yumbo.ai.omni.p2p.starter.memory",
        "top.yumbo.ai.omni.p2p.starter.h2",
        "top.yumbo.ai.omni.p2p.starter.sqlite",
        "top.yumbo.ai.omni.p2p.starter.redis",
        "top.yumbo.ai.omni.p2p.starter.mongodb",
        "top.yumbo.ai.omni.p2p.starter.elasticsearch"
})
@ConditionalOnProperty(
        prefix = "omni-agent.p2p",
        name = "enabled",
        havingValue = "true",
        matchIfMissing = true
)
public class P2PAutoConfiguration {

    /**
     * 创建 P2P 传输桥接服务
     * (Create P2P Transfer Bridge)
     */
    @Bean
    @ConditionalOnMissingBean
    public P2PTransferBridge p2pTransferBridge() {
        log.info("✅ [P2P] 创建 P2P Transfer Bridge");
        return new DefaultP2PTransferBridge();
    }

    /**
     * Memory 实现（默认）
     */
    @Bean
    @ConditionalOnMissingBean(P2PCollaborationService.class)
    @ConditionalOnProperty(
            prefix = "omni-agent.p2p",
            name = "storage-type",
            havingValue = "memory",
            matchIfMissing = true
    )
    public P2PCollaborationService memoryP2PCollaborationService() {
        log.info("✅ [P2P] 使用 Memory 实现");
        return new MemoryP2PCollaborationService();
    }

    @Bean
    @ConditionalOnMissingBean(P2PDataTransferService.class)
    @ConditionalOnProperty(
            prefix = "omni-agent.p2p",
            name = "storage-type",
            havingValue = "memory",
            matchIfMissing = true
    )
    public P2PDataTransferService memoryP2PDataTransferService() {
        log.info("✅ [P2P] 使用 Memory 数据传输服务");
        return new MemoryP2PDataTransferService();
    }
}

