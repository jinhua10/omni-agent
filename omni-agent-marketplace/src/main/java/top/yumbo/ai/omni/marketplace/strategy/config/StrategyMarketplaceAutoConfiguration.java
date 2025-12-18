package top.yumbo.ai.omni.marketplace.strategy.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.marketplace.strategy.MarketplaceStrategy;
import top.yumbo.ai.omni.marketplace.strategy.StrategyMarketplaceManager;

import java.util.List;

/**
 * 策略市场自动配置
 *
 * 在应用启动时自动注册所有策略
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Configuration
public class StrategyMarketplaceAutoConfiguration implements ApplicationRunner {

    @Autowired
    private StrategyMarketplaceManager marketplaceManager;

    @Autowired(required = false)
    private List<MarketplaceStrategy> strategies;

    @Override
    public void run(ApplicationArguments args) {
        log.info("========================================");
        log.info("🚀 开始自动注册策略市场策略");
        log.info("========================================");

        if (strategies == null || strategies.isEmpty()) {
            log.warn("⚠️ 未找到任何策略，跳过注册");
            return;
        }

        log.info("发现 {} 个策略，开始注册...", strategies.size());

        int successCount = 0;
        int failureCount = 0;

        for (MarketplaceStrategy strategy : strategies) {
            try {
                boolean success = marketplaceManager.registerStrategy(strategy);
                if (success) {
                    successCount++;
                    log.info("✅ 已注册: {} ({})",
                            strategy.getStrategyName(),
                            strategy.getCategory());
                } else {
                    failureCount++;
                    log.error("❌ 注册失败: {}", strategy.getStrategyName());
                }
            } catch (Exception e) {
                failureCount++;
                log.error("❌ 注册异常: {}", strategy.getStrategyName(), e);
            }
        }

        log.info("========================================");
        log.info("📊 策略注册完成");
        log.info("  成功: {} 个", successCount);
        log.info("  失败: {} 个", failureCount);
        log.info("  总计: {} 个", strategies.size());
        log.info("========================================");

        // 打印统计信息
        var stats = marketplaceManager.getStatistics();
        log.info("📈 策略市场统计: {}", stats);
    }
}

