package top.yumbo.ai.omni.storage.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.retry.annotation.EnableRetry;

/**
 * 文档存储重试配置
 *
 * <p>✅ P1优化4：启用Spring Retry机制</p>
 * <p>自动重试MongoDB连接失败、网络抖动等临时性错误</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 * @version P1 Optimized - Retry Mechanism
 */
@Configuration
@EnableRetry
public class StorageRetryConfig {
    // Spring Retry 会自动扫描@Retryable注解
    // 配置由application.yml提供
}

