package top.yumbo.ai.omni.example.basic.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * AI服务控制器
 * (AI Service Controller)
 *
 * <p>处理AI服务市场相关的API请求，包括服务列表、安装、卸载、配置等</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/services")
@RequiredArgsConstructor
public class ServiceController {

    // 内存存储（用于演示，生产环境应使用数据库）
    private static final Map<String, ServiceDTO> serviceStore = new ConcurrentHashMap<>();
    private static final Map<String, ServiceConfigDTO> configStore = new ConcurrentHashMap<>();

    static {
        // 初始化示例服务数据
        initSampleServices();
    }

    /**
     * 获取服务列表
     * GET /api/services
     */
    @GetMapping
    public List<ServiceDTO> getServices(
            @RequestParam(required = false) String tab,
            @RequestParam(required = false) String category,
            @RequestParam(required = false) String keyword) {

        log.info("获取服务列表: tab={}, category={}, keyword={}", tab, category, keyword);

        List<ServiceDTO> filteredServices = serviceStore.values().stream()
                .filter(service -> {
                    // 标签页筛选
                    if ("installed".equals(tab)) {
                        return service.getInstalled();
                    } else if ("available".equals(tab)) {
                        return !service.getInstalled();
                    }
                    return true;
                })
                .filter(service -> category == null || service.getCategory().equals(category))
                .filter(service -> keyword == null ||
                        service.getName().toLowerCase().contains(keyword.toLowerCase()) ||
                        service.getDescription().toLowerCase().contains(keyword.toLowerCase()))
                .sorted(Comparator.comparing(ServiceDTO::getUsageCount).reversed())
                .collect(Collectors.toList());

        log.info("返回服务列表: count={}", filteredServices.size());
        return filteredServices;
    }

    /**
     * 获取服务详情
     * GET /api/services/{id}
     */
    @GetMapping("/{id}")
    public ServiceDTO getServiceDetail(@PathVariable String id) {
        log.info("获取服务详情: id={}", id);

        ServiceDTO service = serviceStore.get(id);
        if (service == null) {
            throw new RuntimeException("服务不存在: " + id);
        }

        log.info("返回服务详情: id={}, name={}", id, service.getName());
        return service;
    }

    /**
     * 安装服务
     * POST /api/services/{id}/install
     */
    @PostMapping("/{id}/install")
    public Map<String, Object> installService(@PathVariable String id) {
        log.info("安装服务: id={}", id);

        ServiceDTO service = serviceStore.get(id);
        if (service == null) {
            throw new RuntimeException("服务不存在: " + id);
        }

        service.setInstalled(true);
        service.setUsageCount(service.getUsageCount() + 1);

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("message", "服务安装成功");
        result.put("service", service);

        log.info("服务安装成功: id={}", id);
        return result;
    }

    /**
     * 卸载服务
     * POST /api/services/{id}/uninstall
     */
    @PostMapping("/{id}/uninstall")
    public Map<String, Object> uninstallService(@PathVariable String id) {
        log.info("卸载服务: id={}", id);

        ServiceDTO service = serviceStore.get(id);
        if (service == null) {
            throw new RuntimeException("服务不存在: " + id);
        }

        service.setInstalled(false);
        configStore.remove(id);

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("message", "服务卸载成功");

        log.info("服务卸载成功: id={}", id);
        return result;
    }

    /**
     * 更新服务配置
     * PUT /api/services/{id}/config
     */
    @PutMapping("/{id}/config")
    public Map<String, Object> updateServiceConfig(
            @PathVariable String id,
            @RequestBody ServiceConfigDTO config) {

        log.info("更新服务配置: id={}", id);

        ServiceDTO service = serviceStore.get(id);
        if (service == null) {
            throw new RuntimeException("服务不存在: " + id);
        }

        if (!service.getInstalled()) {
            throw new RuntimeException("服务未安装: " + id);
        }

        configStore.put(id, config);

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("message", "配置更新成功");
        result.put("config", config);

        log.info("配置更新成功: id={}", id);
        return result;
    }

    /**
     * 生成PPT
     * POST /api/services/ppt/generate
     */
    @PostMapping("/ppt/generate")
    public Map<String, Object> generatePPT(@RequestBody PPTGenerateRequest request) {
        log.info("生成PPT: topic={}", request.getTopic());

        // TODO: 实际的PPT生成逻辑
        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("message", "PPT生成成功");
        result.put("fileUrl", "/api/files/ppt/" + System.currentTimeMillis() + ".pptx");
        result.put("fileName", request.getTopic() + ".pptx");

        log.info("PPT生成成功");
        return result;
    }

    /**
     * 切换AI模型
     * POST /api/services/model/switch
     */
    @PostMapping("/model/switch")
    public Map<String, Object> switchModel(@RequestBody ModelSwitchRequest request) {
        log.info("切换模型: modelType={}", request.getModelType());

        Map<String, Object> result = new HashMap<>();
        result.put("success", true);
        result.put("message", "模型切换成功");
        result.put("modelType", request.getModelType());

        log.info("模型切换成功: modelType={}", request.getModelType());
        return result;
    }

    // ========== 辅助方法 ==========

    /**
     * 初始化示例服务数据
     */
    private static void initSampleServices() {
        // PPT生成器
        ServiceDTO pptGenerator = new ServiceDTO();
        pptGenerator.setId("ppt-generator");
        pptGenerator.setName("PPT生成器");
        pptGenerator.setDescription("根据主题和大纲自动生成精美的PPT演示文稿");
        pptGenerator.setCategory("generation");
        pptGenerator.setIcon("📊");
        pptGenerator.setInstalled(false);
        pptGenerator.setRating(4.8);
        pptGenerator.setUsageCount(1523);
        pptGenerator.setAuthor("OmniAgent Team");
        pptGenerator.setVersion("1.2.0");
        pptGenerator.setSize("2.3 MB");
        pptGenerator.setIsNew(true);
        pptGenerator.setIsPopular(true);
        pptGenerator.setFeatures(Arrays.asList(
                "支持多种PPT主题风格",
                "自动生成大纲结构",
                "智能配图和图表",
                "一键导出PPTX格式"
        ));
        pptGenerator.setChangelog(Arrays.asList(
                new ChangelogDTO("1.2.0", "2025-12-10", "增加更多主题模板"),
                new ChangelogDTO("1.1.0", "2025-11-15", "优化生成速度"),
                new ChangelogDTO("1.0.0", "2025-10-01", "初始版本发布")
        ));
        serviceStore.put(pptGenerator.getId(), pptGenerator);

        // 模型切换器
        ServiceDTO modelSwitcher = new ServiceDTO();
        modelSwitcher.setId("model-switcher");
        modelSwitcher.setName("模型切换器");
        modelSwitcher.setDescription("在本地模型和在线模型之间灵活切换，满足不同使用场景");
        modelSwitcher.setCategory("optimization");
        modelSwitcher.setIcon("🔄");
        modelSwitcher.setInstalled(true);
        modelSwitcher.setRating(4.9);
        modelSwitcher.setUsageCount(3245);
        modelSwitcher.setAuthor("OmniAgent Team");
        modelSwitcher.setVersion("2.0.0");
        modelSwitcher.setSize("1.5 MB");
        modelSwitcher.setIsNew(false);
        modelSwitcher.setIsPopular(true);
        modelSwitcher.setFeatures(Arrays.asList(
                "支持本地Ollama模型",
                "支持在线API模型",
                "一键快速切换",
                "自动保存配置"
        ));
        modelSwitcher.setChangelog(Arrays.asList(
                new ChangelogDTO("2.0.0", "2025-12-15", "重构切换逻辑"),
                new ChangelogDTO("1.5.0", "2025-11-20", "增加更多模型支持")
        ));
        serviceStore.put(modelSwitcher.getId(), modelSwitcher);

        // 数据分析助手
        ServiceDTO dataAnalyzer = new ServiceDTO();
        dataAnalyzer.setId("data-analyzer");
        dataAnalyzer.setName("数据分析助手");
        dataAnalyzer.setDescription("智能分析数据，生成可视化图表和分析报告");
        dataAnalyzer.setCategory("analysis");
        dataAnalyzer.setIcon("📈");
        dataAnalyzer.setInstalled(false);
        dataAnalyzer.setRating(4.6);
        dataAnalyzer.setUsageCount(892);
        dataAnalyzer.setAuthor("Data Team");
        dataAnalyzer.setVersion("1.0.0");
        dataAnalyzer.setSize("3.1 MB");
        dataAnalyzer.setIsNew(true);
        dataAnalyzer.setIsPopular(false);
        dataAnalyzer.setFeatures(Arrays.asList(
                "支持CSV、Excel等格式",
                "自动生成统计图表",
                "AI驱动的数据洞察",
                "导出分析报告"
        ));
        dataAnalyzer.setChangelog(Arrays.asList(
                new ChangelogDTO("1.0.0", "2025-12-01", "首次发布")
        ));
        serviceStore.put(dataAnalyzer.getId(), dataAnalyzer);

        // 文档转换器
        ServiceDTO docConverter = new ServiceDTO();
        docConverter.setId("doc-converter");
        docConverter.setName("文档转换器");
        docConverter.setDescription("支持多种文档格式之间的相互转换");
        docConverter.setCategory("conversion");
        docConverter.setIcon("🔄");
        docConverter.setInstalled(true);
        docConverter.setRating(4.7);
        docConverter.setUsageCount(2134);
        docConverter.setAuthor("Doc Team");
        docConverter.setVersion("1.3.0");
        docConverter.setSize("2.8 MB");
        docConverter.setIsNew(false);
        docConverter.setIsPopular(true);
        docConverter.setFeatures(Arrays.asList(
                "支持PDF、Word、Markdown等格式",
                "保持原文档格式",
                "批量转换",
                "高质量转换"
        ));
        docConverter.setChangelog(Arrays.asList(
                new ChangelogDTO("1.3.0", "2025-11-30", "增加Markdown支持"),
                new ChangelogDTO("1.2.0", "2025-10-15", "优化转换质量")
        ));
        serviceStore.put(docConverter.getId(), docConverter);

        log.info("初始化示例服务数据完成: count={}", serviceStore.size());
    }

    // ========== 请求/响应类 ==========

    @Data
    public static class ServiceDTO {
        private String id;
        private String name;
        private String description;
        private String category;
        private String icon;
        private Boolean installed;
        private Double rating;
        private Integer usageCount;
        private String author;
        private String version;
        private String size;
        private Boolean isNew;
        private Boolean isPopular;
        private List<String> features;
        private List<ChangelogDTO> changelog;
    }

    @Data
    public static class ChangelogDTO {
        private String version;
        private String date;
        private String changes;

        public ChangelogDTO() {}

        public ChangelogDTO(String version, String date, String changes) {
            this.version = version;
            this.date = date;
            this.changes = changes;
        }
    }

    @Data
    public static class ServiceConfigDTO {
        private Boolean enabled;
        private String model;
        private String apiKey;
        private Map<String, Object> customConfig;
    }

    @Data
    public static class PPTGenerateRequest {
        private String topic;
        private String outline;
        private String style;
    }

    @Data
    public static class ModelSwitchRequest {
        private String modelType; // local/online
    }
}

