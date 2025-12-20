package top.yumbo.ai.omni.workflow;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 工作流注册表
 * (Workflow Registry)
 *
 * <p>负责工作流的注册、查询、版本管理和持久化</p>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@Service
public class WorkflowRegistry {

    private static final String WORKFLOWS_DIR = "./data/workflows/definitions";
    private static final String VERSIONS_DIR = "./data/workflows/versions";

    private final ObjectMapper yamlMapper = new ObjectMapper(new YAMLFactory());
    private final ObjectMapper jsonMapper = new ObjectMapper();

    // 工作流缓存（name -> version -> workflow）
    private final Map<String, Map<String, Workflow>> workflowCache = new ConcurrentHashMap<>();

    // 最新版本缓存（name -> latest version）
    private final Map<String, String> latestVersionCache = new ConcurrentHashMap<>();

    /**
     * 初始化：加载所有工作流定义
     */
    @jakarta.annotation.PostConstruct
    public void init() {
        try {
            loadAllWorkflows();
            log.info("✅ 工作流注册表初始化完成: 已加载 {} 个工作流", workflowCache.size());
        } catch (Exception e) {
            log.error("❌ 工作流注册表初始化失败", e);
        }
    }

    /**
     * 注册工作流
     *
     * @param workflow 工作流定义
     */
    public void register(Workflow workflow) {
        if (workflow.getName() == null || workflow.getName().isEmpty()) {
            throw new IllegalArgumentException("工作流名称不能为空");
        }

        if (workflow.getVersion() == null || workflow.getVersion().isEmpty()) {
            workflow.setVersion("1.0.0");
        }

        workflow.setUpdatedAt(System.currentTimeMillis());
        if (workflow.getCreatedAt() == null) {
            workflow.setCreatedAt(System.currentTimeMillis());
        }

        // 添加到缓存
        workflowCache
                .computeIfAbsent(workflow.getName(), k -> new ConcurrentHashMap<>())
                .put(workflow.getVersion(), workflow);

        // 更新最新版本
        updateLatestVersion(workflow.getName(), workflow.getVersion());

        // 持久化
        saveWorkflow(workflow);

        log.info("✅ 工作流已注册: name={}, version={}", workflow.getName(), workflow.getVersion());
    }

    /**
     * 获取工作流（最新版本）
     *
     * @param name 工作流名称
     * @return 工作流定义
     */
    public Workflow getLatestWorkflow(String name) {
        String latestVersion = latestVersionCache.get(name);
        if (latestVersion == null) {
            return null;
        }
        return getWorkflow(name, latestVersion);
    }

    /**
     * 获取工作流（指定版本）
     *
     * @param name 工作流名称
     * @param version 版本号
     * @return 工作流定义
     */
    public Workflow getWorkflow(String name, String version) {
        Map<String, Workflow> versions = workflowCache.get(name);
        if (versions == null) {
            return null;
        }
        return versions.get(version);
    }

    /**
     * 获取所有工作流（最新版本）
     *
     * @return 工作流列表
     */
    public List<Workflow> getAllWorkflows() {
        return latestVersionCache.entrySet().stream()
                .map(entry -> getWorkflow(entry.getKey(), entry.getValue()))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    /**
     * 按分类获取工作流
     *
     * @param category 分类
     * @return 工作流列表
     */
    public List<Workflow> getWorkflowsByCategory(String category) {
        return getAllWorkflows().stream()
                .filter(w -> category.equals(w.getTags() != null && w.getTags().contains(category)))
                .collect(Collectors.toList());
    }

    /**
     * 获取工作流的所有版本
     *
     * @param name 工作流名称
     * @return 版本列表
     */
    public List<String> getVersions(String name) {
        Map<String, Workflow> versions = workflowCache.get(name);
        if (versions == null) {
            return Collections.emptyList();
        }
        return new ArrayList<>(versions.keySet());
    }

    /**
     * 删除工作流（标记为已弃用）
     *
     * @param name 工作流名称
     */
    public void deactivate(String name) {
        Workflow workflow = getLatestWorkflow(name);
        if (workflow != null) {
            workflow.setStatus("deprecated");
            saveWorkflow(workflow);
            log.info("✅ 工作流已停用: name={}", name);
        }
    }

    /**
     * 保存工作流到文件
     */
    private void saveWorkflow(Workflow workflow) {
        try {
            // 确保目录存在
            Path workflowsDir = Paths.get(WORKFLOWS_DIR);
            if (!Files.exists(workflowsDir)) {
                Files.createDirectories(workflowsDir);
            }

            // 保存最新版本到 definitions 目录
            String category = workflow.getTags() != null && !workflow.getTags().isEmpty()
                    ? workflow.getTags().get(0) : "general";
            Path categoryDir = workflowsDir.resolve(category);
            if (!Files.exists(categoryDir)) {
                Files.createDirectories(categoryDir);
            }

            Path workflowFile = categoryDir.resolve(workflow.getName() + ".yml");
            yamlMapper.writerWithDefaultPrettyPrinter().writeValue(workflowFile.toFile(), workflow);

            // 归档版本到 versions 目录
            Path versionsDir = Paths.get(VERSIONS_DIR).resolve(workflow.getName());
            if (!Files.exists(versionsDir)) {
                Files.createDirectories(versionsDir);
            }

            Path versionFile = versionsDir.resolve("v" + workflow.getVersion() + ".yml");
            yamlMapper.writerWithDefaultPrettyPrinter().writeValue(versionFile.toFile(), workflow);

            log.debug("💾 工作流已保存: {}", workflowFile);

        } catch (IOException e) {
            log.error("❌ 保存工作流失败: name={}", workflow.getName(), e);
        }
    }

    /**
     * 加载所有工作流
     */
    private void loadAllWorkflows() throws IOException {
        Path workflowsDir = Paths.get(WORKFLOWS_DIR);
        if (!Files.exists(workflowsDir)) {
            Files.createDirectories(workflowsDir);
            log.info("📁 创建工作流目录: {}", workflowsDir.toAbsolutePath());
            return;
        }

        Files.walk(workflowsDir)
                .filter(Files::isRegularFile)
                .filter(p -> p.toString().endsWith(".yml") || p.toString().endsWith(".yaml"))
                .forEach(this::loadWorkflowFile);
    }

    /**
     * 加载单个工作流文件
     */
    private void loadWorkflowFile(Path file) {
        try {
            Workflow workflow = yamlMapper.readValue(file.toFile(), Workflow.class);

            workflowCache
                    .computeIfAbsent(workflow.getName(), k -> new ConcurrentHashMap<>())
                    .put(workflow.getVersion(), workflow);

            updateLatestVersion(workflow.getName(), workflow.getVersion());

            log.debug("📄 加载工作流: name={}, version={}", workflow.getName(), workflow.getVersion());

        } catch (IOException e) {
            log.error("❌ 加载工作流失败: file={}", file, e);
        }
    }

    /**
     * 更新最新版本
     */
    private void updateLatestVersion(String name, String version) {
        String currentLatest = latestVersionCache.get(name);
        if (currentLatest == null || compareVersions(version, currentLatest) > 0) {
            latestVersionCache.put(name, version);
        }
    }

    /**
     * 比较版本号（简单实现）
     *
     * @return 1 if v1 > v2, -1 if v1 < v2, 0 if equal
     */
    private int compareVersions(String v1, String v2) {
        String[] parts1 = v1.split("\\.");
        String[] parts2 = v2.split("\\.");

        int maxLen = Math.max(parts1.length, parts2.length);
        for (int i = 0; i < maxLen; i++) {
            int num1 = i < parts1.length ? Integer.parseInt(parts1[i]) : 0;
            int num2 = i < parts2.length ? Integer.parseInt(parts2[i]) : 0;

            if (num1 != num2) {
                return Integer.compare(num1, num2);
            }
        }

        return 0;
    }

    /**
     * 递增版本号（minor版本）
     */
    public String incrementVersion(String version) {
        String[] parts = version.split("\\.");
        if (parts.length >= 2) {
            int minor = Integer.parseInt(parts[1]);
            parts[1] = String.valueOf(minor + 1);
            return String.join(".", parts);
        }
        return version;
    }
}

