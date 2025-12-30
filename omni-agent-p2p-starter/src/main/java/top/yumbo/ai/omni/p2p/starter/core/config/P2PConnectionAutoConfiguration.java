package top.yumbo.ai.omni.p2p.starter.core.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.omni.p2p.api.P2PConnectionManager;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;
import top.yumbo.ai.omni.p2p.api.P2PTransferBridge;
import top.yumbo.ai.omni.p2p.starter.core.DefaultP2PConnectionManager;

import java.util.List;

/**
 * P2P连接管理自动配置
 * (P2P Connection Manager Auto Configuration)
 *
 * <p>自动配置端到端连接管理器，并注册所有可用的数据传输服务</p>
 * <p>Automatically configures the P2P connection manager and registers all available data transfer services</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
public class P2PConnectionAutoConfiguration {

    /**
     * 创建P2P连接管理器Bean
     * (Creates P2P Connection Manager Bean)
     *
     * @param transferBridge 传输桥接服务 (Transfer Bridge Service)
     * @param services 所有可用的数据传输服务 (All available data transfer services)
     * @return 连接管理器实例 (Connection Manager instance)
     */
    @Bean
    @ConditionalOnMissingBean
    public P2PConnectionManager p2pConnectionManager(
            P2PTransferBridge transferBridge,
            List<P2PDataTransferService> services) {

        log.info("正在初始化P2P连接管理器 (Initializing P2P Connection Manager)");
        
        DefaultP2PConnectionManager manager = new DefaultP2PConnectionManager(transferBridge);

        // 自动注册所有数据传输服务
        // Auto-register all data transfer services
        if (services != null && !services.isEmpty()) {
            log.info("发现 {} 个数据传输服务 (Found {} data transfer services)", 
                    services.size(), services.size());
            
            for (P2PDataTransferService service : services) {
                String storageType = extractStorageType(service);
                manager.registerService(storageType, service);
                log.info("  ✓ 已注册服务: {} ({})", storageType, service.getClass().getSimpleName());
            }
        } else {
            log.warn("未发现任何数据传输服务 (No data transfer services found)");
        }

        log.info("P2P连接管理器初始化完成 (P2P Connection Manager initialized)");
        return manager;
    }

    /**
     * 从服务实例中提取存储类型
     * (Extracts storage type from service instance)
     *
     * @param service 数据传输服务 (Data transfer service)
     * @return 存储类型名称 (Storage type name)
     */
    private String extractStorageType(P2PDataTransferService service) {
        String className = service.getClass().getSimpleName().toLowerCase();
        
        if (className.contains("sqlite")) {
            return "sqlite";
        } else if (className.contains("redis")) {
            return "redis";
        } else if (className.contains("mongo")) {
            return "mongodb";
        } else if (className.contains("elasticsearch") || className.contains("es")) {
            return "elasticsearch";
        } else if (className.contains("memory")) {
            return "memory";
        } else if (className.contains("h2")) {
            return "h2";
        } else if (className.contains("file")) {
            return "file";
        }
        
        // 如果无法识别，使用类名
        log.warn("无法识别存储类型: {}, 使用类名作为存储类型", className);
        return className.replace("p2pdatatransferservice", "");
    }
}


