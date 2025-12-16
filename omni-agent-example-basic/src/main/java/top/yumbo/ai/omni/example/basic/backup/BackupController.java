package top.yumbo.ai.omni.example.basic.backup;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.Map;

/**
 * 冗余备份监控控制器
 * (Redundant Backup Monitoring Controller)
 *
 * <p>提供备份状态查询和管理接口</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/backup")
public class BackupController {

    private final BackupMonitoringService monitoringService;

    public BackupController(BackupMonitoringService monitoringService) {
        this.monitoringService = monitoringService;
    }

    /**
     * 获取备份状态报告
     */
    @GetMapping("/status")
    public Map<String, Object> getBackupStatus() {
        log.debug("📊 Getting backup status report");

        BackupMonitoringService.BackupStatusReport report = monitoringService.getStatusReport();

        Map<String, Object> response = new HashMap<>();
        response.put("success", true);
        response.put("timestamp", report.getTimestamp());
        response.put("totalBackends", report.getTotalBackends());
        response.put("healthyBackends", report.getHealthyBackends());
        response.put("unhealthyBackends", report.getUnhealthyBackends());

        Map<String, Object> stats = new HashMap<>();
        stats.put("healthyChecks", report.getStatistics().getHealthyChecks().get());
        stats.put("unhealthyChecks", report.getStatistics().getUnhealthyChecks().get());
        stats.put("failedChecks", report.getStatistics().getFailedChecks().get());
        stats.put("recoverySuccess", report.getStatistics().getRecoverySuccess().get());
        stats.put("recoveryFailures", report.getStatistics().getRecoveryFailures().get());
        stats.put("consistencyChecks", report.getStatistics().getConsistencyChecks().get());
        stats.put("inconsistencyDetected", report.getStatistics().getInconsistencyDetected().get());
        stats.put("repairSuccess", report.getStatistics().getRepairSuccess().get());
        stats.put("repairFailures", report.getStatistics().getRepairFailures().get());
        response.put("statistics", stats);

        return response;
    }

    /**
     * 获取健康状态摘要
     */
    @GetMapping("/health")
    public Map<String, Object> getHealthSummary() {
        log.debug("🏥 Getting health summary");

        BackupMonitoringService.BackupStatusReport report = monitoringService.getStatusReport();

        Map<String, Object> response = new HashMap<>();
        response.put("success", true);
        response.put("healthy", report.getUnhealthyBackends() == 0);
        response.put("totalBackends", report.getTotalBackends());
        response.put("healthyBackends", report.getHealthyBackends());
        response.put("message", report.getUnhealthyBackends() == 0
            ? "All backup backends are healthy"
            : report.getUnhealthyBackends() + " backend(s) are unhealthy");

        return response;
    }

    /**
     * 触发手动健康检查
     */
    @PostMapping("/check")
    public Map<String, Object> triggerHealthCheck() {
        log.info("🔍 Manual health check triggered");

        try {
            monitoringService.performHealthCheck();

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "Health check completed successfully");
            return response;
        } catch (Exception e) {
            log.error("Manual health check failed", e);

            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("message", "Health check failed: " + e.getMessage());
            return response;
        }
    }

    /**
     * 触发一致性检查
     */
    @PostMapping("/consistency-check")
    public Map<String, Object> triggerConsistencyCheck() {
        log.info("🔍 Manual consistency check triggered");

        try {
            monitoringService.performConsistencyCheck();

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "Consistency check completed successfully");
            return response;
        } catch (Exception e) {
            log.error("Manual consistency check failed", e);

            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("message", "Consistency check failed: " + e.getMessage());
            return response;
        }
    }

    /**
     * 获取统计信息
     */
    @GetMapping("/statistics")
    public Map<String, Object> getStatistics() {
        log.debug("📈 Getting backup statistics");

        BackupMonitoringService.BackupStatistics stats = monitoringService.getStatistics();

        Map<String, Object> response = new HashMap<>();
        response.put("success", true);
        response.put("healthyChecks", stats.getHealthyChecks().get());
        response.put("unhealthyChecks", stats.getUnhealthyChecks().get());
        response.put("failedChecks", stats.getFailedChecks().get());
        response.put("recoverySuccess", stats.getRecoverySuccess().get());
        response.put("recoveryFailures", stats.getRecoveryFailures().get());
        response.put("consistencyChecks", stats.getConsistencyChecks().get());
        response.put("inconsistencyDetected", stats.getInconsistencyDetected().get());
        response.put("repairSuccess", stats.getRepairSuccess().get());
        response.put("repairFailures", stats.getRepairFailures().get());

        // 计算成功率
        int totalHealthChecks = stats.getHealthyChecks().get() + stats.getUnhealthyChecks().get();
        double healthRate = totalHealthChecks > 0
            ? (double) stats.getHealthyChecks().get() / totalHealthChecks * 100
            : 100.0;
        response.put("healthRate", String.format("%.2f%%", healthRate));

        int totalRecovery = stats.getRecoverySuccess().get() + stats.getRecoveryFailures().get();
        double recoveryRate = totalRecovery > 0
            ? (double) stats.getRecoverySuccess().get() / totalRecovery * 100
            : 100.0;
        response.put("recoveryRate", String.format("%.2f%%", recoveryRate));

        return response;
    }
}

