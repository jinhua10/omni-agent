package top.yumbo.ai.omni.web.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.ai.archive.model.AICallArchive;
import top.yumbo.ai.omni.ai.archive.AICallArchiveService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * AI调用归档查询API
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/ai/archives")
@RequiredArgsConstructor
@ConditionalOnBean(AICallArchiveService.class)
@Tag(name = "AI Archives", description = "AI调用归档查询API")
public class AIArchiveController {

    private final AICallArchiveService archiveService;

    /**
     * 获取单个归档记录
     */
    @GetMapping("/{archiveId}")
    @Operation(summary = "获取归档记录", description = "根据ID获取单个AI调用归档记录")
    public ResponseEntity<Map<String, Object>> getArchive(@PathVariable String archiveId) {
        Map<String, Object> result = new HashMap<>();

        return archiveService.getArchive(archiveId)
                .map(archive -> {
                    result.put("success", true);
                    result.put("archive", archive);
                    return ResponseEntity.ok(result);
                })
                .orElseGet(() -> {
                    result.put("success", false);
                    result.put("error", "Archive not found");
                    return ResponseEntity.notFound().build();
                });
    }

    /**
     * 按时间范围查询归档
     */
    @GetMapping("/range")
    @Operation(summary = "按时间范围查询", description = "查询指定时间范围内的AI调用归档")
    public ResponseEntity<Map<String, Object>> queryByTimeRange(
            @RequestParam Long startTime,
            @RequestParam Long endTime,
            @RequestParam(defaultValue = "100") Integer limit) {

        List<AICallArchive> archives = archiveService.queryByTimeRange(startTime, endTime, limit);

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("total", archives.size());
        result.put("archives", archives);

        return ResponseEntity.ok(result);
    }

    /**
     * 按模型查询归档
     */
    @GetMapping("/model/{model}")
    @Operation(summary = "按模型查询", description = "查询指定模型的AI调用归档")
    public ResponseEntity<Map<String, Object>> queryByModel(
            @PathVariable String model,
            @RequestParam(defaultValue = "100") Integer limit) {

        List<AICallArchive> archives = archiveService.queryByModel(model, limit);

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("total", archives.size());
        result.put("model", model);
        result.put("archives", archives);

        return ResponseEntity.ok(result);
    }

    /**
     * 按文档查询归档
     */
    @GetMapping("/document/{documentId}")
    @Operation(summary = "按文档查询", description = "查询与指定文档相关的AI调用归档")
    public ResponseEntity<Map<String, Object>> queryByDocument(@PathVariable String documentId) {
        List<AICallArchive> archives = archiveService.queryByDocument(documentId);

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("total", archives.size());
        result.put("documentId", documentId);
        result.put("archives", archives);

        return ResponseEntity.ok(result);
    }

    /**
     * 获取归档统计
     */
    @GetMapping("/statistics")
    @Operation(summary = "获取统计信息", description = "获取AI调用归档的统计信息")
    public ResponseEntity<Map<String, Object>> getStatistics() {
        AICallArchiveService.ArchiveStatistics stats = archiveService.getStatistics();

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("statistics", stats);

        return ResponseEntity.ok(result);
    }

    /**
     * 清理旧归档
     */
    @DeleteMapping("/cleanup")
    @Operation(summary = "清理旧归档", description = "删除指定时间之前的归档记录")
    public ResponseEntity<Map<String, Object>> cleanOldArchives(@RequestParam Long olderThan) {
        int deleted = archiveService.cleanOldArchives(olderThan);

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("deleted", deleted);
        result.put("message", "已清理 " + deleted + " 个旧归档目录");

        return ResponseEntity.ok(result);
    }
}

