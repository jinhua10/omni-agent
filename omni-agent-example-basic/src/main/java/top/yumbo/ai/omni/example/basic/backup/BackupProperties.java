package top.yumbo.ai.omni.example.basic.backup;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * 冗余备份配置属性
 * (Redundant Backup Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Component
@ConfigurationProperties(prefix = "omni-agent.backup")
public class BackupProperties {

    /**
     * 监控配置
     */
    private Monitoring monitoring = new Monitoring();

    /**
     * 健康检查配置
     */
    private HealthCheck healthCheck = new HealthCheck();

    /**
     * 自动恢复配置
     */
    private AutoRecovery autoRecovery = new AutoRecovery();

    /**
     * 同步策略配置
     */
    private SyncStrategy syncStrategy = new SyncStrategy();

    /**
     * 一致性检查配置
     */
    private ConsistencyCheck consistencyCheck = new ConsistencyCheck();

    @Data
    public static class Monitoring {
        private boolean enabled = true;
        private long checkInterval = 60000; // 1分钟
    }

    @Data
    public static class HealthCheck {
        private boolean enabled = true;
        private boolean alertOnFailure = true;
    }

    @Data
    public static class AutoRecovery {
        private boolean enabled = true;
        private int maxRetry = 3;
        private long retryInterval = 5000; // 5秒
    }

    @Data
    public static class SyncStrategy {
        private String mode = "async"; // async | sync
        private int batchSize = 100;
        private int threadPoolSize = 4;
    }

    @Data
    public static class ConsistencyCheck {
        private boolean enabled = true;
        private long checkInterval = 3600000; // 1小时
        private boolean autoRepair = true;
    }
}


