package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.web.config.FileWatcherConfig;
import top.yumbo.ai.omni.web.model.FileChangeRecord;
import top.yumbo.ai.omni.web.service.FileWatcherService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 文件监听管理 API
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/file-watcher")
@RequiredArgsConstructor
public class FileWatcherController {

    private final FileWatcherService fileWatcherService;

    /**
     * 获取当前配置
     * GET /api/file-watcher/config
     */
    @GetMapping("/config")
    public Map<String, Object> getConfig() {
        FileWatcherConfig config = fileWatcherService.getCurrentConfig();

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("config", config);

        return result;
    }

    /**
     * 更新配置
     * PUT /api/file-watcher/config
     */
    @PutMapping("/config")
    public Map<String, Object> updateConfig(@RequestBody ConfigUpdateRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            FileWatcherConfig currentConfig = fileWatcherService.getCurrentConfig();

            // 更新字段
            if (request.getEnabled() != null) {
                currentConfig.setEnabled(request.getEnabled());
            }
            if (request.getAutoIndex() != null) {
                currentConfig.setAutoIndex(request.getAutoIndex());
            }
            if (request.getWatchDirectory() != null) {
                currentConfig.setWatchDirectory(request.getWatchDirectory());
            }

            // 保存并应用配置
            boolean success = fileWatcherService.updateConfig(currentConfig);

            result.put("success", success);
            result.put("message", success ? "配置更新成功" : "配置更新失败");
            result.put("config", currentConfig);

            log.info("✅ 配置已更新: enabled={}, autoIndex={}",
                    currentConfig.getEnabled(), currentConfig.getAutoIndex());

        } catch (Exception e) {
            log.error("❌ 更新配置失败", e);
            result.put("success", false);
            result.put("message", "更新失败: " + e.getMessage());
        }

        return result;
    }

    /**
     * 获取未处理的文件变化
     * GET /api/file-watcher/changes/unprocessed
     */
    @GetMapping("/changes/unprocessed")
    public Map<String, Object> getUnprocessedChanges() {
        List<FileChangeRecord> changes = fileWatcherService.getUnprocessedChanges();

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("total", changes.size());
        result.put("changes", changes);

        return result;
    }

    /**
     * 获取所有文件变化
     * GET /api/file-watcher/changes
     */
    @GetMapping("/changes")
    public Map<String, Object> getAllChanges() {
        List<FileChangeRecord> changes = fileWatcherService.getAllChanges();

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("total", changes.size());
        result.put("changes", changes);

        return result;
    }

    /**
     * 手动处理单个文件变化
     * POST /api/file-watcher/changes/{recordId}/process
     */
    @PostMapping("/changes/{recordId}/process")
    public Map<String, Object> processChange(@PathVariable String recordId) {
        Map<String, Object> result = new HashMap<>();

        try {
            boolean success = fileWatcherService.processChange(recordId);

            result.put("success", success);
            result.put("message", success ? "处理成功" : "处理失败");
            result.put("recordId", recordId);

        } catch (Exception e) {
            log.error("❌ 处理文件变化失败: {}", recordId, e);
            result.put("success", false);
            result.put("message", "处理失败: " + e.getMessage());
        }

        return result;
    }

    /**
     * 批量处理所有未处理的变化
     * POST /api/file-watcher/changes/process-all
     */
    @PostMapping("/changes/process-all")
    public Map<String, Object> processAllUnprocessed() {
        Map<String, Object> result = new HashMap<>();

        try {
            int successCount = fileWatcherService.processAllUnprocessed();
            int totalCount = fileWatcherService.getUnprocessedChanges().size();

            result.put("success", true);
            result.put("message", String.format("批量处理完成: 成功 %d 个", successCount));
            result.put("successCount", successCount);
            result.put("totalCount", totalCount);

        } catch (Exception e) {
            log.error("❌ 批量处理失败", e);
            result.put("success", false);
            result.put("message", "批量处理失败: " + e.getMessage());
        }

        return result;
    }

    /**
     * 清除已处理的记录
     * DELETE /api/file-watcher/changes/processed
     */
    @DeleteMapping("/changes/processed")
    public Map<String, Object> clearProcessedRecords() {
        Map<String, Object> result = new HashMap<>();

        try {
            int count = fileWatcherService.clearProcessedRecords();

            result.put("success", true);
            result.put("message", String.format("已清除 %d 条记录", count));
            result.put("clearedCount", count);

        } catch (Exception e) {
            log.error("❌ 清除记录失败", e);
            result.put("success", false);
            result.put("message", "清除失败: " + e.getMessage());
        }

        return result;
    }

    /**
     * 配置更新请求
     */
    @Data
    public static class ConfigUpdateRequest {
        private Boolean enabled;
        private Boolean autoIndex;
        private String watchDirectory;
    }
}






