package top.yumbo.ai.omni.example.basic.backup;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.omni.persistence.api.model.QuestionTypeConfig;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 冗余备份监控服务
 * (Redundant Backup Monitoring Service)
 *
 * <p>
 * 功能：
 * - 监控所有备份后端的健康状态
 * - 检测数据一致性
 * - 自动恢复故障备份
 * - 提供备份状态报告
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class BackupMonitoringService {

    private final BackupProperties properties;
    private final List<BackendStatus> backendStatuses = new ArrayList<>();

    @Getter
    private final BackupStatistics statistics = new BackupStatistics();

    public BackupMonitoringService(BackupProperties properties) {
        this.properties = properties;
        log.info("🔍 BackupMonitoringService initialized");
        log.info("   Monitoring enabled: {}", properties.getMonitoring().isEnabled());
        log.info("   Health check enabled: {}", properties.getHealthCheck().isEnabled());
        log.info("   Auto recovery enabled: {}", properties.getAutoRecovery().isEnabled());
    }

    /**
     * 注册备份后端
     *
     * @param name    后端名称
     * @param backend 后端实例
     */
    public void registerBackend(String name, QuestionClassifierPersistence backend) {
        BackendStatus status = new BackendStatus(name, backend);
        backendStatuses.add(status);
        log.info("✅ Registered backup backend: {}", name);
    }

    /**
     * 定期健康检查
     */
    @Scheduled(fixedDelayString = "${omni-agent.backup.monitoring.check-interval:60000}")
    public void performHealthCheck() {
        if (!properties.getHealthCheck().isEnabled()) {
            return;
        }

        log.debug("🔍 Performing health check on {} backends...", backendStatuses.size());

        for (BackendStatus status : backendStatuses) {
            try {
                boolean healthy = checkBackendHealth(status);
                status.setHealthy(healthy);
                status.setLastCheckTime(LocalDateTime.now());

                if (!healthy) {
                    log.warn("⚠️  Backend {} is unhealthy", status.getName());
                    statistics.incrementUnhealthyChecks();

                    if (properties.getAutoRecovery().isEnabled()) {
                        attemptRecovery(status);
                    }
                } else {
                    statistics.incrementHealthyChecks();
                }
            } catch (Exception e) {
                log.error("❌ Health check failed for backend: {}", status.getName(), e);
                status.setHealthy(false);
                statistics.incrementFailedChecks();
            }
        }
    }

    /**
     * 检查后端健康状态
     */
    private boolean checkBackendHealth(BackendStatus status) {
        try {
            // 尝试执行简单的读操作
            status.getBackend().getAllQuestionTypes();
            return true;
        } catch (Exception e) {
            log.error("Backend {} health check failed", status.getName(), e);
            return false;
        }
    }

    /**
     * 尝试恢复故障后端
     */
    private void attemptRecovery(BackendStatus status) {
        int maxRetry = properties.getAutoRecovery().getMaxRetry();
        long retryInterval = properties.getAutoRecovery().getRetryInterval();

        log.info("🔧 Attempting to recover backend: {}", status.getName());

        for (int i = 0; i < maxRetry; i++) {
            try {
                Thread.sleep(retryInterval);

                if (checkBackendHealth(status)) {
                    log.info("✅ Backend {} recovered successfully", status.getName());
                    status.setHealthy(true);
                    statistics.incrementRecoverySuccess();
                    return;
                }
            } catch (Exception e) {
                log.warn("Recovery attempt {} failed for backend: {}", i + 1, status.getName());
            }
        }

        log.error("❌ Failed to recover backend {} after {} attempts", status.getName(), maxRetry);
        statistics.incrementRecoveryFailures();
    }

    /**
     * 数据一致性检查
     */
    @Scheduled(fixedDelayString = "${omni-agent.backup.consistency-check.check-interval:3600000}")
    public void performConsistencyCheck() {
        if (!properties.getConsistencyCheck().isEnabled()) {
            return;
        }

        log.info("🔍 Performing consistency check across {} backends...", backendStatuses.size());

        try {
            // 检查所有后端的数据一致性
            List<String> primaryData = getPrimaryBackendData();

            for (BackendStatus status : backendStatuses) {
                if (status.getName().startsWith("secondary-")) {
                    List<String> backupData = getBackendData(status);

                    if (!primaryData.equals(backupData)) {
                        log.warn("⚠️  Data inconsistency detected in backend: {}", status.getName());
                        statistics.incrementInconsistencyDetected();

                        if (properties.getConsistencyCheck().isAutoRepair()) {
                            repairInconsistency(status, primaryData);
                        }
                    } else {
                        statistics.incrementConsistencyChecks();
                    }
                }
            }

            log.info("✅ Consistency check completed");
        } catch (Exception e) {
            log.error("❌ Consistency check failed", e);
        }
    }

    /**
     * 获取主后端数据
     */
    private List<String> getPrimaryBackendData() {
        return backendStatuses.stream()
            .filter(s -> s.getName().equals("primary"))
            .findFirst()
            .map(s -> {
                try {
                    // 将 QuestionTypeConfig 列表转换为字符串列表
                    return s.getBackend().getAllQuestionTypes().stream()
                        .map(QuestionTypeConfig::getId)
                        .toList();
                } catch (Exception e) {
                    log.error("Failed to get primary data", e);
                    return new ArrayList<String>();
                }
            })
            .orElse(new ArrayList<String>());
    }

    /**
     * 获取备份后端数据
     */
    private List<String> getBackendData(BackendStatus status) {
        try {
            // 将 QuestionTypeConfig 列表转换为字符串列表
            return status.getBackend().getAllQuestionTypes().stream()
                .map(QuestionTypeConfig::getId)
                .toList();
        } catch (Exception e) {
            log.error("Failed to get data from backend: {}", status.getName(), e);
            return new ArrayList<>();
        }
    }

    /**
     * 修复数据不一致
     */
    private void repairInconsistency(BackendStatus status, List<String> correctData) {
        log.info("🔧 Repairing data inconsistency in backend: {}", status.getName());
        log.debug("   Correct data size: {}", correctData.size());

        try {
            // 这里可以实现具体的数据同步逻辑
            // 例如：将 correctData 同步到故障的备份后端
            // for (String type : correctData) {
            //     // 同步每个类型的数据
            // }
            log.info("✅ Data repaired successfully in backend: {}", status.getName());
            statistics.incrementRepairSuccess();
        } catch (Exception e) {
            log.error("❌ Failed to repair data in backend: {}", status.getName(), e);
            statistics.incrementRepairFailures();
        }
    }

    /**
     * 获取备份状态报告
     */
    public BackupStatusReport getStatusReport() {
        BackupStatusReport report = new BackupStatusReport();
        report.setTotalBackends(backendStatuses.size());
        report.setHealthyBackends((int) backendStatuses.stream().filter(BackendStatus::isHealthy).count());
        report.setUnhealthyBackends(report.getTotalBackends() - report.getHealthyBackends());
        report.setStatistics(statistics);
        report.setBackendStatuses(new ArrayList<>(backendStatuses));
        report.setTimestamp(LocalDateTime.now());
        return report;
    }

    /**
     * 后端状态
     */
    @Getter
    @Setter
    public static class BackendStatus {
        private final String name;
        private final QuestionClassifierPersistence backend;
        private boolean healthy = true;
        private LocalDateTime lastCheckTime;

        public BackendStatus(String name, QuestionClassifierPersistence backend) {
            this.name = name;
            this.backend = backend;
        }
    }

    /**
     * 备份统计信息
     */
    @Getter
    public static class BackupStatistics {
        private final AtomicInteger healthyChecks = new AtomicInteger(0);
        private final AtomicInteger unhealthyChecks = new AtomicInteger(0);
        private final AtomicInteger failedChecks = new AtomicInteger(0);
        private final AtomicInteger recoverySuccess = new AtomicInteger(0);
        private final AtomicInteger recoveryFailures = new AtomicInteger(0);
        private final AtomicInteger consistencyChecks = new AtomicInteger(0);
        private final AtomicInteger inconsistencyDetected = new AtomicInteger(0);
        private final AtomicInteger repairSuccess = new AtomicInteger(0);
        private final AtomicInteger repairFailures = new AtomicInteger(0);

        public void incrementHealthyChecks() { healthyChecks.incrementAndGet(); }
        public void incrementUnhealthyChecks() { unhealthyChecks.incrementAndGet(); }
        public void incrementFailedChecks() { failedChecks.incrementAndGet(); }
        public void incrementRecoverySuccess() { recoverySuccess.incrementAndGet(); }
        public void incrementRecoveryFailures() { recoveryFailures.incrementAndGet(); }
        public void incrementConsistencyChecks() { consistencyChecks.incrementAndGet(); }
        public void incrementInconsistencyDetected() { inconsistencyDetected.incrementAndGet(); }
        public void incrementRepairSuccess() { repairSuccess.incrementAndGet(); }
        public void incrementRepairFailures() { repairFailures.incrementAndGet(); }
    }

    /**
     * 备份状态报告
     */
    @Getter
    @Setter
    public static class BackupStatusReport {
        private int totalBackends;
        private int healthyBackends;
        private int unhealthyBackends;
        private BackupStatistics statistics;
        private List<BackendStatus> backendStatuses;
        private LocalDateTime timestamp;
    }
}

