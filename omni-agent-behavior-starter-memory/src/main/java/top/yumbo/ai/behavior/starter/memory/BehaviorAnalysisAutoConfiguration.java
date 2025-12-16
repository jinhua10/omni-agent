package top.yumbo.ai.behavior.starter.memory;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.behavior.api.BehaviorAnalysisService;

/**
 * 行为分析服务自动配置 (Behavior Analysis Service Auto Configuration)
 *
 * 当没有其他实现时，自动配置基于内存的行为分析服务
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@org.springframework.boot.autoconfigure.condition.ConditionalOnProperty(
    name = "omni-agent.behavior.type",
    havingValue = "memory",
    matchIfMissing = true
)
public class BehaviorAnalysisAutoConfiguration {

    /**
     * 配置基于内存的行为分析服务 (Configure Memory-based Behavior Analysis Service)
     *
     * @return BehaviorAnalysisService实例
     */
    @Bean
    @ConditionalOnMissingBean(BehaviorAnalysisService.class)
    public BehaviorAnalysisService behaviorAnalysisService() {
        log.info("🚀 Auto-configuring MemoryBehaviorAnalysisService");
        return new MemoryBehaviorAnalysisService();
    }
}

