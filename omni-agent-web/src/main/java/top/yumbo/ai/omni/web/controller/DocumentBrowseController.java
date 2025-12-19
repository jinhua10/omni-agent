package top.yumbo.ai.omni.web.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.storage.api.DocumentStorageService;

import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 文档库浏览控制器（FTP风格）
 *
 * 功能：
 * - 浏览 data/storage/documents 目录
 * - 列出文件和文件夹
 * - 下载文件
 * - 删除文件/文件夹
 * - 创建文件夹
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

    // 文档根目录
    private static final String DOCUMENT_ROOT = "./data/storage/documents";

    /**
     * 列出指定路径下的文件和文件夹
     *
     * @param path 相对路径（为空则列出根目录）
     * @return 文件和文件夹列表
     */
    @GetMapping("/list")
    public ResponseEntity<Map<String, Object>> listFiles(
            @RequestParam(required = false, defaultValue = "") String path) {

        try {
            Path fullPath = Paths.get(DOCUMENT_ROOT, path).normalize();

            // 安全检查：防止路径遍历攻击
            if (!fullPath.startsWith(Paths.get(DOCUMENT_ROOT).normalize())) {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "message", "非法路径"
                ));
            }

            if (!Files.exists(fullPath) || !Files.isDirectory(fullPath)) {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "message", "目录不存在"
                ));
            }

            // 列出文件和文件夹
            List<Map<String, Object>> items = Files.list(fullPath)
                    .map(p -> {
                        try {
                            Map<String, Object> item = new HashMap<>();
                            String fileName = p.getFileName().toString();
                            boolean isDirectory = Files.isDirectory(p);

                            item.put("name", fileName);
                            item.put("type", isDirectory ? "directory" : "file");
                            item.put("path", path.isEmpty() ? fileName : path + "/" + fileName);

                            if (!isDirectory) {
                                item.put("size", Files.size(p));
                                item.put("modified", Files.getLastModifiedTime(p).toMillis());
                            }

                            return item;
                        } catch (IOException e) {
                            log.error("获取文件信息失败: {}", p, e);
                            return null;
                        }
                    })
                    .filter(Objects::nonNull)
                    .sorted((a, b) -> {
                        // 文件夹排在前面
                        String typeA = (String) a.get("type");
                        String typeB = (String) b.get("type");
                        if (!typeA.equals(typeB)) {
                            return "directory".equals(typeA) ? -1 : 1;
                        }
                        // 同类型按名称排序
                        return ((String) a.get("name")).compareTo((String) b.get("name"));
                    })
                    .collect(Collectors.toList());

            return ResponseEntity.ok(Map.of(
                    "success", true,
                    "path", path,
                    "items", items
            ));

        } catch (IOException e) {
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
     * @param path 文件相对路径
     * @return 文件内容
     */
    @GetMapping("/download")
    public ResponseEntity<Resource> downloadFile(@RequestParam String path) {
        try {
            Path fullPath = Paths.get(DOCUMENT_ROOT, path).normalize();

            // 安全检查
            if (!fullPath.startsWith(Paths.get(DOCUMENT_ROOT).normalize())) {
                return ResponseEntity.badRequest().build();
            }

            if (!Files.exists(fullPath) || !Files.isRegularFile(fullPath)) {
                return ResponseEntity.notFound().build();
            }

            byte[] data = Files.readAllBytes(fullPath);
            ByteArrayResource resource = new ByteArrayResource(data);

            String filename = fullPath.getFileName().toString();
            String encodedFilename = URLEncoder.encode(filename, StandardCharsets.UTF_8)
                    .replace("+", "%20");

            return ResponseEntity.ok()
                    .header(HttpHeaders.CONTENT_DISPOSITION,
                            "attachment; filename=\"" + filename + "\"; filename*=UTF-8''" + encodedFilename)
                    .contentType(MediaType.APPLICATION_OCTET_STREAM)
                    .contentLength(data.length)
                    .body(resource);

        } catch (IOException e) {
            log.error("下载文件失败: {}", path, e);
            return ResponseEntity.status(500).build();
        }
    }

    /**
     * 删除文件或文件夹
     *
     * @param path 相对路径
     * @return 删除结果
     */
    @DeleteMapping("/delete")
    public ResponseEntity<Map<String, Object>> deleteFileOrFolder(@RequestParam String path) {
        try {
            Path fullPath = Paths.get(DOCUMENT_ROOT, path).normalize();

            // 安全检查
            if (!fullPath.startsWith(Paths.get(DOCUMENT_ROOT).normalize())) {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "message", "非法路径"
                ));
            }

            if (!Files.exists(fullPath)) {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "message", "文件或文件夹不存在"
                ));
            }

            // 递归删除
            if (Files.isDirectory(fullPath)) {
                Files.walk(fullPath)
                        .sorted(Comparator.reverseOrder())
                        .forEach(p -> {
                            try {
                                Files.delete(p);
                            } catch (IOException e) {
                                log.error("删除失败: {}", p, e);
                            }
                        });
            } else {
                Files.delete(fullPath);
            }

            log.info("✅ 删除成功: {}", path);

            return ResponseEntity.ok(Map.of(
                    "success", true,
                    "message", "删除成功"
            ));

        } catch (IOException e) {
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
     * @param path 相对路径
     * @return 创建结果
     */
    @PostMapping("/mkdir")
    public ResponseEntity<Map<String, Object>> createFolder(@RequestParam String path) {
        try {
            Path fullPath = Paths.get(DOCUMENT_ROOT, path).normalize();

            // 安全检查
            if (!fullPath.startsWith(Paths.get(DOCUMENT_ROOT).normalize())) {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "message", "非法路径"
                ));
            }

            if (Files.exists(fullPath)) {
                return ResponseEntity.badRequest().body(Map.of(
                        "success", false,
                        "message", "文件夹已存在"
                ));
            }

            Files.createDirectories(fullPath);

            log.info("✅ 创建文件夹成功: {}", path);

            return ResponseEntity.ok(Map.of(
                    "success", true,
                    "message", "创建成功"
            ));

        } catch (IOException e) {
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
            Path rootPath = Paths.get(DOCUMENT_ROOT);

            long totalFiles = Files.walk(rootPath)
                    .filter(Files::isRegularFile)
                    .count();

            long totalSize = Files.walk(rootPath)
                    .filter(Files::isRegularFile)
                    .mapToLong(p -> {
                        try {
                            return Files.size(p);
                        } catch (IOException e) {
                            return 0;
                        }
                    })
                    .sum();

            long totalFolders = Files.walk(rootPath)
                    .filter(Files::isDirectory)
                    .count() - 1; // 减去根目录

            return ResponseEntity.ok(Map.of(
                    "success", true,
                    "totalFiles", totalFiles,
                    "totalFolders", totalFolders,
                    "totalSize", totalSize,
                    "totalSizeFormatted", formatBytes(totalSize)
            ));

        } catch (IOException e) {
            log.error("获取统计信息失败", e);
            return ResponseEntity.status(500).body(Map.of(
                    "success", false,
                    "message", "获取统计信息失败"
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

