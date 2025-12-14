package top.yumbo.ai.p2p.starter.memory;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.p2p.api.P2PDataTransferService;

/**
 * P2P Memory Starter 自动配置
 * (P2P Memory Starter Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
public class P2PMemoryAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public P2PDataTransferService p2pDataTransferService() {
        log.info("Initializing Memory P2P Data Transfer Service");
        return new MemoryP2PDataTransferService();
    }
}

