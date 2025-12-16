package top.yumbo.ai.omni.example.basic.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 主题管理控制器
 * (Theme Management Controller)
 *
 * <p>提供主题的上传、下载、列表、删除等功能</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/themes")
@CrossOrigin(origins = "*")
@RequiredArgsConstructor
public class ThemeController {

    // 主题存储路径
    private static final String THEME_BASE_PATH = "./data/themes";

    /**
     * 初始化主题目录
     */
    private void ensureThemeDirectory() {
        try {
            Path themePath = Paths.get(THEME_BASE_PATH);
            if (!Files.exists(themePath)) {
                Files.createDirectories(themePath);
                log.info("创建主题目录: {}", THEME_BASE_PATH);
            }
        } catch (IOException e) {
            log.error("创建主题目录失败", e);
        }
    }

    /**
     * 获取主题列表
     * GET /api/themes/list
     */
    @GetMapping("/list")
    public ResponseEntity<List<ThemeInfo>> getThemeList() {
        try {
            log.debug("获取主题列表");
            ensureThemeDirectory();

            Path themePath = Paths.get(THEME_BASE_PATH);
            List<ThemeInfo> themes = new ArrayList<>();

            if (Files.exists(themePath)) {
                Files.list(themePath)
                    .filter(Files::isDirectory)
                    .forEach(dir -> {
                        try {
                            String themeId = dir.getFileName().toString();
                            Path configFile = dir.resolve("theme.json");

                            ThemeInfo theme = new ThemeInfo();
                            theme.setId(themeId);
                            theme.setName(themeId);

                            if (Files.exists(configFile)) {
                                String config = Files.readString(configFile, StandardCharsets.UTF_8);
                                theme.setConfig(config);
                            }

                            theme.setCreatedAt(Files.getLastModifiedTime(dir).toMillis());
                            themes.add(theme);
                        } catch (IOException e) {
                            log.warn("读取主题失败: {}", dir.getFileName(), e);
                        }
                    });
            }

            log.debug("找到 {} 个主题", themes.size());
            return ResponseEntity.ok(themes);

        } catch (Exception e) {
            log.error("获取主题列表失败", e);
            return ResponseEntity.internalServerError().build();
        }
    }

    /**
     * 获取主题详情
     * GET /api/themes/{themeId}
     */
    @GetMapping("/{themeId}")
    public ResponseEntity<ThemeDetail> getTheme(@PathVariable String themeId) {
        try {
            log.debug("获取主题详情: {}", themeId);
            ensureThemeDirectory();

            Path themePath = Paths.get(THEME_BASE_PATH, themeId);

            if (!Files.exists(themePath)) {
                return ResponseEntity.notFound().build();
            }

            ThemeDetail detail = new ThemeDetail();
            detail.setId(themeId);
            detail.setName(themeId);

            // 读取配置文件
            Path configFile = themePath.resolve("theme.json");
            if (Files.exists(configFile)) {
                String config = Files.readString(configFile, StandardCharsets.UTF_8);
                detail.setConfig(config);
            }

            // 列出所有文件
            List<String> files = Files.walk(themePath)
                .filter(Files::isRegularFile)
                .map(p -> themePath.relativize(p).toString())
                .collect(Collectors.toList());
            detail.setFiles(files);

            detail.setCreatedAt(Files.getLastModifiedTime(themePath).toMillis());

            return ResponseEntity.ok(detail);

        } catch (Exception e) {
            log.error("获取主题详情失败: {}", themeId, e);
            return ResponseEntity.internalServerError().build();
        }
    }

    /**
     * 上传主题
     * POST /api/themes/upload
     */
    @PostMapping("/upload")
    public ResponseEntity<Map<String, Object>> uploadTheme(
            @RequestParam("themeId") String themeId,
            @RequestParam(value = "files", required = false) List<MultipartFile> files,
            @RequestParam(value = "config", required = false) String config) {

        Map<String, Object> result = new HashMap<>();

        try {
            log.info("上传主题: {}", themeId);
            ensureThemeDirectory();

            Path themePath = Paths.get(THEME_BASE_PATH, themeId);
            Files.createDirectories(themePath);

            // 保存配置文件
            if (config != null && !config.isEmpty()) {
                Path configFile = themePath.resolve("theme.json");
                Files.writeString(configFile, config, StandardCharsets.UTF_8);
            }

            // 保存上传的文件
            int savedCount = 0;
            if (files != null && !files.isEmpty()) {
                for (MultipartFile file : files) {
                    if (!file.isEmpty()) {
                        String filename = file.getOriginalFilename();
                        if (filename != null) {
                            Path targetPath = themePath.resolve(filename);
                            Files.createDirectories(targetPath.getParent());
                            file.transferTo(targetPath.toFile());
                            savedCount++;
                        }
                    }
                }
            }

            result.put("status", "success");
            result.put("message", "主题上传成功");
            result.put("themeId", themeId);
            result.put("filesCount", savedCount);

            log.info("主题上传成功: id={}, files={}", themeId, savedCount);

            return ResponseEntity.ok(result);

        } catch (Exception e) {
            log.error("上传主题失败: {}", themeId, e);
            result.put("status", "error");
            result.put("error", e.getMessage());
            return ResponseEntity.internalServerError().body(result);
        }
    }

    /**
     * 删除主题
     * DELETE /api/themes/{themeId}
     */
    @DeleteMapping("/{themeId}")
    public ResponseEntity<Map<String, Object>> deleteTheme(@PathVariable String themeId) {
        Map<String, Object> result = new HashMap<>();

        try {
            log.info("删除主题: {}", themeId);
            ensureThemeDirectory();

            Path themePath = Paths.get(THEME_BASE_PATH, themeId);

            if (!Files.exists(themePath)) {
                result.put("status", "error");
                result.put("message", "主题不存在");
                return ResponseEntity.notFound().build();
            }

            // 递归删除目录
            deleteDirectory(themePath.toFile());

            result.put("status", "success");
            result.put("message", "主题删除成功");
            result.put("themeId", themeId);

            log.info("主题删除成功: {}", themeId);

            return ResponseEntity.ok(result);

        } catch (Exception e) {
            log.error("删除主题失败: {}", themeId, e);
            result.put("status", "error");
            result.put("error", e.getMessage());
            return ResponseEntity.internalServerError().body(result);
        }
    }

    /**
     * 下载主题
     * GET /api/themes/{themeId}/download
     */
    @GetMapping("/{themeId}/download")
    public ResponseEntity<Resource> downloadTheme(@PathVariable String themeId) {
        try {
            log.info("下载主题: {}", themeId);
            ensureThemeDirectory();

            Path themePath = Paths.get(THEME_BASE_PATH, themeId);

            if (!Files.exists(themePath)) {
                return ResponseEntity.notFound().build();
            }

            // 读取配置文件作为下载内容
            Path configFile = themePath.resolve("theme.json");
            byte[] content;

            if (Files.exists(configFile)) {
                content = Files.readAllBytes(configFile);
            } else {
                // 如果没有配置文件，返回一个默认的JSON
                content = String.format("{\"id\":\"%s\",\"name\":\"%s\"}", themeId, themeId)
                    .getBytes(StandardCharsets.UTF_8);
            }

            ByteArrayResource resource = new ByteArrayResource(content);

            return ResponseEntity.ok()
                .contentType(MediaType.APPLICATION_JSON)
                .header(HttpHeaders.CONTENT_DISPOSITION,
                    "attachment; filename=\"" + themeId + ".json\"")
                .body(resource);

        } catch (Exception e) {
            log.error("下载主题失败: {}", themeId, e);
            return ResponseEntity.internalServerError().build();
        }
    }

    /**
     * 同步主题配置
     * PUT /api/themes/sync
     */
    @PutMapping("/sync")
    public ResponseEntity<Map<String, Object>> syncTheme(@RequestBody ThemeSyncRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            String themeId = request.getId();
            log.info("同步主题: {}", themeId);
            ensureThemeDirectory();

            Path themePath = Paths.get(THEME_BASE_PATH, themeId);
            Files.createDirectories(themePath);

            // 保存配置
            if (request.getConfig() != null) {
                Path configFile = themePath.resolve("theme.json");
                Files.writeString(configFile, request.getConfig(), StandardCharsets.UTF_8);
            }

            result.put("status", "success");
            result.put("message", "主题同步成功");
            result.put("themeId", themeId);

            log.info("主题同步成功: {}", themeId);

            return ResponseEntity.ok(result);

        } catch (Exception e) {
            log.error("同步主题失败", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
            return ResponseEntity.internalServerError().body(result);
        }
    }

    // ========== 辅助方法 ==========

    /**
     * 递归删除目录
     */
    private void deleteDirectory(File directory) throws IOException {
        if (directory.isDirectory()) {
            File[] files = directory.listFiles();
            if (files != null) {
                for (File file : files) {
                    deleteDirectory(file);
                }
            }
        }
        if (!directory.delete()) {
            throw new IOException("Failed to delete: " + directory);
        }
    }

    // ========== DTO 类 ==========

    @Data
    public static class ThemeInfo {
        private String id;
        private String name;
        private String config;
        private Long createdAt;
    }

    @Data
    public static class ThemeDetail {
        private String id;
        private String name;
        private String config;
        private List<String> files;
        private Long createdAt;
    }

    @Data
    public static class ThemeSyncRequest {
        private String id;
        private String name;
        private String config;
    }
}

