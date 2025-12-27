package top.yumbo.ai.omni.web.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * 文档库浏览控制器（虚拟文件系统风格）
 *
 * 功能：
 * - 浏览虚拟文档目录（通过 DocumentStorageService 抽象层）
 * - 列出文件和文件夹
 * - 下载文件
 * - 删除文件/文件夹
 * - 创建文件夹
 *
 * 支持多种存储实现：
 * - 本地文件系统（File）
 * - MinIO / S3
 * - MongoDB
 * - Elasticsearch
 * - Redis
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/documents/browse")
@RequiredArgsConstructor
public class DocumentBrowseController {

    private final DocumentStorageService storageService;

    // 文档虚拟根路径（由存储服务实现决定实际存储位置）
    private static final String VIRTUAL_ROOT = "documents";

    /**
     * 列出指定路径下的文件和文件夹
     *
     * @param path 虚拟路径（为空则列出根目录，可以包含documents前缀）
     * @return 文件和文件夹列表
     */
    @GetMapping("/list")
    public ResponseEntity<Map<String, Object>> listFiles(
            @RequestParam(required = false, defaultValue = "") String path) {

        try {
            // 构建虚拟路径
            // 如果path已经以documents开头，直接使用；否则添加VIRTUAL_ROOT前缀
            String virtualPath;
            if (path.isEmpty()) {
                virtualPath = VIRTUAL_ROOT;
            } else if (path.startsWith(VIRTUAL_ROOT + "/") || path.equals(VIRTUAL_ROOT)) {
                virtualPath = path;  // 已经包含documents前缀
            } else {
                virtualPath = VIRTUAL_ROOT + "/" + path;  // 添加documents前缀
            }

            // 通过存储服务列出文件
            List<Map<String, Object>> items = storageService.listFiles(virtualPath);

            // 按类型和名称排序
            items.sort((a, b) -> {
                // 文件夹排在前面
                String typeA = (String) a.get("type");
                String typeB = (String) b.get("type");
                if (!typeA.equals(typeB)) {
                    return "directory".equals(typeA) ? -1 : 1;
                }
                // 同类型按名称排序
                return ((String) a.get("name")).compareTo((String) b.get("name"));
            });

            return ResponseEntity.ok(Map.of(
                    "success", true,
                    "path", path,
                    "items", items
            ));

        } catch (IllegalArgumentException e) {
            log.warn("非法路径: {}", path, e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "message", "非法路径"
            ));
        } catch (Exception e) {
            log.error("列出文件失败: {}", path, e);
            return ResponseEntity.status(500).body(Map.of(
                    "success", false,
                    "message", "列出文件失败: " + e.getMessage()
            ));
        }
    }

    /**
     * 下载文件
     *
     * @param path 文件虚拟路径（可以包含documents前缀）
     * @return 文件内容
     */
    @GetMapping("/download")
    public ResponseEntity<Resource> downloadFile(@RequestParam String path) {
        try {
            // 构建虚拟路径
            // 如果path已经以documents开头，直接使用；否则添加VIRTUAL_ROOT前缀
            String virtualPath;
            if (path.startsWith(VIRTUAL_ROOT + "/") || path.equals(VIRTUAL_ROOT)) {
                virtualPath = path;  // 已经包含documents前缀
            } else {
                virtualPath = VIRTUAL_ROOT + "/" + path;  // 添加documents前缀
            }

            // 通过存储服务读取文件
            byte[] data = storageService.readFile(virtualPath);
            if (data == null) {
                return ResponseEntity.notFound().build();
            }

            ByteArrayResource resource = new ByteArrayResource(data);

            // 获取文件名（从路径中提取）
            String filename = path.contains("/")
                ? path.substring(path.lastIndexOf("/") + 1)
                : path;
            String encodedFilename = URLEncoder.encode(filename, StandardCharsets.UTF_8)
                    .replace("+", "%20");

            return ResponseEntity.ok()
                    .header(HttpHeaders.CONTENT_DISPOSITION,
                            "attachment; filename=\"" + filename + "\"; filename*=UTF-8''" + encodedFilename)
                    .contentType(MediaType.APPLICATION_OCTET_STREAM)
                    .contentLength(data.length)
                    .body(resource);

        } catch (IllegalArgumentException e) {
            log.warn("非法路径: {}", path, e);
            return ResponseEntity.badRequest().build();
        } catch (Exception e) {
            log.error("下载文件失败: {}", path, e);
            return ResponseEntity.status(500).build();
        }
    }

    /**
     * 删除文件或文件夹
     *
     * @param path 虚拟路径（可以包含documents前缀）
     * @return 删除结果
     */
    @DeleteMapping("/delete")
    public ResponseEntity<Map<String, Object>> deleteFileOrFolder(@RequestParam String path) {
        try {
            // 构建虚拟路径
            // 如果path已经以documents开头，直接使用；否则添加VIRTUAL_ROOT前缀
            String virtualPath;
            if (path.startsWith(VIRTUAL_ROOT + "/") || path.equals(VIRTUAL_ROOT)) {
                virtualPath = path;  // 已经包含documents前缀
            } else {
                virtualPath = VIRTUAL_ROOT + "/" + path;  // 添加documents前缀
            }

            log.info("🗑️ 删除请求: path={}, virtualPath={}", path, virtualPath);

            // 通过存储服务删除
            boolean success = storageService.deleteFile(virtualPath);

            if (success) {
                log.info("✅ 删除成功: {}", path);
                return ResponseEntity.ok(Map.of(
                        "success", true,
                        "message", "删除成功"
                ));
            } else {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "message", "文件或文件夹不存在"
                ));
            }

        } catch (IllegalArgumentException e) {
            log.warn("非法路径: {}", path, e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "message", "非法路径"
            ));
        } catch (Exception e) {
            log.error("删除失败: {}", path, e);
            return ResponseEntity.status(500).body(Map.of(
                    "success", false,
                    "message", "删除失败: " + e.getMessage()
            ));
        }
    }

    /**
     * 创建文件夹
     *
     * @param path 虚拟路径（可以包含documents前缀）
     * @return 创建结果
     */
    @PostMapping("/mkdir")
    public ResponseEntity<Map<String, Object>> createFolder(@RequestParam String path) {
        try {
            // 构建虚拟路径
            // 如果path已经以documents开头，直接使用；否则添加VIRTUAL_ROOT前缀
            String virtualPath;
            if (path.startsWith(VIRTUAL_ROOT + "/") || path.equals(VIRTUAL_ROOT)) {
                virtualPath = path;  // 已经包含documents前缀
            } else {
                virtualPath = VIRTUAL_ROOT + "/" + path;  // 添加documents前缀
            }

            // 通过存储服务创建目录
            boolean success = storageService.createDirectory(virtualPath);

            if (success) {
                log.info("✅ 创建文件夹成功: {}", path);
                return ResponseEntity.ok(Map.of(
                        "success", true,
                        "message", "创建成功"
                ));
            } else {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "message", "文件夹已存在"
                ));
            }

        } catch (IllegalArgumentException e) {
            log.warn("非法路径: {}", path, e);
            return ResponseEntity.badRequest().body(Map.of(
                    "success", false,
                    "message", "非法路径"
            ));
        } catch (Exception e) {
            log.error("创建文件夹失败: {}", path, e);
            return ResponseEntity.status(500).body(Map.of(
                    "success", false,
                    "message", "创建失败: " + e.getMessage()
            ));
        }
    }

    /**
     * 获取文件统计信息
     *
     * @return 统计信息
     */
    @GetMapping("/stats")
    public ResponseEntity<Map<String, Object>> getStats() {
        try {
            // 通过存储服务获取统计信息
            Map<String, Object> stats = storageService.getStorageStats(VIRTUAL_ROOT);

            long totalFiles = ((Number) stats.getOrDefault("totalFiles", 0L)).longValue();
            long totalSize = ((Number) stats.getOrDefault("totalSize", 0L)).longValue();
            long totalFolders = ((Number) stats.getOrDefault("totalFolders", 0L)).longValue();

            return ResponseEntity.ok(Map.of(
                    "success", true,
                    "totalFiles", totalFiles,
                    "totalFolders", totalFolders,
                    "totalSize", totalSize,
                    "totalSizeFormatted", formatBytes(totalSize)
            ));

        } catch (Exception e) {
            log.error("获取统计信息失败", e);
            return ResponseEntity.status(500).body(Map.of(
                    "success", false,
                    "message", "获取统计信息失败: " + e.getMessage()
            ));
        }
    }

    /**
     * 格式化字节数
     */
    private String formatBytes(long bytes) {
        if (bytes < 1024) return bytes + " B";
        if (bytes < 1024 * 1024) return String.format("%.2f KB", bytes / 1024.0);
        if (bytes < 1024 * 1024 * 1024) return String.format("%.2f MB", bytes / (1024.0 * 1024));
        return String.format("%.2f GB", bytes / (1024.0 * 1024 * 1024));
    }
}






