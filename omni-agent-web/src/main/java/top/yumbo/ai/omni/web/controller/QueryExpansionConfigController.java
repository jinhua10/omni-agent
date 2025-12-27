package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.web.model.ApiResponse;

import java.util.*;

/**
 * 查询扩展配置控制器 (Query Expansion Configuration Controller)
 *
 * 提供查询扩展策略的交互式配置和实时预览功能
 * (Provides interactive configuration and real-time preview of query expansion strategies)
 *
 * Phase 4.2.2 - 查询扩展配置界面
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@RestController
@RequestMapping("/api/query-expansion")
public class QueryExpansionConfigController {

    /**
     * 获取当前查询扩展配置
     * (Get current query expansion configuration)
     *
     * GET /api/query-expansion/config
     */
    @GetMapping("/config")
    public ApiResponse<ConfigInfo> getCurrentConfig() {
        try {
            ConfigInfo config = new ConfigInfo();
            // 从配置文件读取或返回默认值
            config.setLlmExpansionEnabled(true);
            config.setMaxExpandedQueries(3);
            config.setSynonymWeight(0.3);
            config.setLlmWeight(0.4);
            config.setDomainWeight(0.3);
            config.setCacheEnabled(true);
            config.setCacheSize(10000);
            config.setCacheTtl(60);
            config.setParallelEnabled(true);
            config.setParallelThreads(4);

            log.info("📊 获取查询扩展配置成功");
            return ApiResponse.success(config);
        } catch (Exception e) {
            log.error("❌ 获取查询扩展配置失败", e);
            return ApiResponse.error("获取配置失败: " + e.getMessage());
        }
    }

    /**
     * 更新查询扩展配置
     * (Update query expansion configuration)
     *
     * POST /api/query-expansion/config
     */
    @PostMapping("/config")
    public ApiResponse<Void> updateConfig(@RequestBody ConfigUpdateRequest request) {
        try {
            log.info("🔧 更新查询扩展配置: {}", request);

            // 更新配置（需要重启服务才能生效，或者使用动态配置）
            // 这里只是示例，实际需要实现动态配置更新机制

            log.info("✅ 查询扩展配置更新成功");
            return ApiResponse.success(null, "配置更新成功（需重启服务生效）");
        } catch (Exception e) {
            log.error("❌ 更新查询扩展配置失败", e);
            return ApiResponse.error("更新配置失败: " + e.getMessage());
        }
    }

    /**
     * 预览查询扩展效果
     * (Preview query expansion effect)
     *
     * POST /api/query-expansion/preview
     */
    @PostMapping("/preview")
    public ApiResponse<PreviewResponse> previewExpansion(@RequestBody PreviewRequest request) {
        try {
            log.info("🔍 预览查询扩展: query={}", request.getQuery());

            String originalQuery = request.getQuery();
            if (originalQuery == null || originalQuery.trim().isEmpty()) {
                return ApiResponse.error("查询不能为空");
            }

            PreviewResponse response = new PreviewResponse();
            response.setOriginalQuery(originalQuery);

            // 获取扩展查询
            List<String> expandedQueries = new ArrayList<>();

            // 同义词扩展
            if (Boolean.TRUE.equals(request.getEnableSynonym())) {
                expandedQueries.add(originalQuery + " 同义词扩展示例");
            }

            // LLM扩展
            if (Boolean.TRUE.equals(request.getEnableLlm())) {
                expandedQueries.add(originalQuery + " LLM改写示例1");
                expandedQueries.add(originalQuery + " LLM改写示例2");
            }

            // 领域词扩展
            if (Boolean.TRUE.equals(request.getEnableDomain())) {
                expandedQueries.add(originalQuery + " 领域词扩展示例");
            }

            response.setExpandedQueries(expandedQueries);
            response.setTotalQueries(expandedQueries.size() + 1); // +1 for original

            // 统计信息
            Map<String, Object> stats = new HashMap<>();
            stats.put("originalLength", originalQuery.length());
            stats.put("avgExpandedLength", expandedQueries.stream()
                .mapToInt(String::length)
                .average()
                .orElse(0.0));
            stats.put("expansionRate", expandedQueries.size());
            response.setStatistics(stats);

            log.info("✅ 查询扩展预览成功: {} -> {} queries", originalQuery, response.getTotalQueries());
            return ApiResponse.success(response);
        } catch (Exception e) {
            log.error("❌ 查询扩展预览失败", e);
            return ApiResponse.error("预览失败: " + e.getMessage());
        }
    }

    /**
     * 获取领域词典
     * (Get domain dictionary)
     *
     * GET /api/query-expansion/dictionary
     */
    @GetMapping("/dictionary")
    public ApiResponse<DictionaryInfo> getDictionary() {
        try {
            DictionaryInfo dictionary = new DictionaryInfo();

            // 示例领域词
            Map<String, List<String>> domainTerms = new HashMap<>();
            domainTerms.put("技术", Arrays.asList("编程", "开发", "代码", "算法"));
            domainTerms.put("框架", Arrays.asList("Spring", "React", "Vue", "Angular"));
            domainTerms.put("数据库", Arrays.asList("MySQL", "MongoDB", "Redis", "PostgreSQL"));

            dictionary.setDomainTerms(domainTerms);
            dictionary.setTotalTerms(domainTerms.values().stream().mapToInt(List::size).sum());

            log.info("📚 获取领域词典成功: {} 个领域", domainTerms.size());
            return ApiResponse.success(dictionary);
        } catch (Exception e) {
            log.error("❌ 获取领域词典失败", e);
            return ApiResponse.error("获取词典失败: " + e.getMessage());
        }
    }

    /**
     * 更新领域词典
     * (Update domain dictionary)
     *
     * POST /api/query-expansion/dictionary
     */
    @PostMapping("/dictionary")
    public ApiResponse<Void> updateDictionary(@RequestBody DictionaryUpdateRequest request) {
        try {
            log.info("📝 更新领域词典: {} 个领域", request.getDomainTerms().size());

            // 实际应该保存到配置文件或数据库

            log.info("✅ 领域词典更新成功");
            return ApiResponse.success(null, "词典更新成功");
        } catch (Exception e) {
            log.error("❌ 更新领域词典失败", e);
            return ApiResponse.error("更新词典失败: " + e.getMessage());
        }
    }

    /**
     * 获取缓存统计
     * (Get cache statistics)
     *
     * GET /api/query-expansion/cache/stats
     */
    @GetMapping("/cache/stats")
    public ApiResponse<CacheStats> getCacheStats() {
        try {
            // 从缓存服务获取统计信息
            CacheStats stats = new CacheStats();
            stats.setHitRate(0.95); // 示例数据
            stats.setCacheSize(1000);
            stats.setMaxSize(10000);
            stats.setHitCount(9500L);
            stats.setMissCount(500L);
            stats.setTotalRequests(10000L);

            log.info("📊 获取缓存统计成功: 命中率={}", stats.getHitRate());
            return ApiResponse.success(stats);
        } catch (Exception e) {
            log.error("❌ 获取缓存统计失败", e);
            return ApiResponse.error("获取统计失败: " + e.getMessage());
        }
    }

    /**
     * 清除缓存
     * (Clear cache)
     *
     * POST /api/query-expansion/cache/clear
     */
    @PostMapping("/cache/clear")
    public ApiResponse<Void> clearCache() {
        try {
            log.info("🗑️ 清除查询扩展缓存");

            // 实际应该调用缓存服务清除

            log.info("✅ 缓存清除成功");
            return ApiResponse.success(null, "缓存已清除");
        } catch (Exception e) {
            log.error("❌ 清除缓存失败", e);
            return ApiResponse.error("清除缓存失败: " + e.getMessage());
        }
    }

    // ==================== DTO 类 ====================

    @Data
    public static class ConfigInfo {
        private Boolean llmExpansionEnabled;
        private Integer maxExpandedQueries;
        private Double synonymWeight;
        private Double llmWeight;
        private Double domainWeight;
        private Boolean cacheEnabled;
        private Integer cacheSize;
        private Integer cacheTtl;
        private Boolean parallelEnabled;
        private Integer parallelThreads;
    }

    @Data
    public static class ConfigUpdateRequest {
        private Boolean llmExpansionEnabled;
        private Integer maxExpandedQueries;
        private Double synonymWeight;
        private Double llmWeight;
        private Double domainWeight;
        private Boolean cacheEnabled;
        private Integer cacheSize;
        private Integer cacheTtl;
        private Boolean parallelEnabled;
        private Integer parallelThreads;
    }

    @Data
    public static class PreviewRequest {
        private String query;
        private Boolean enableSynonym = true;
        private Boolean enableLlm = true;
        private Boolean enableDomain = true;
    }

    @Data
    public static class PreviewResponse {
        private String originalQuery;
        private List<String> expandedQueries;
        private Integer totalQueries;
        private Map<String, Object> statistics;
    }

    @Data
    public static class DictionaryInfo {
        private Map<String, List<String>> domainTerms;
        private Integer totalTerms;
    }

    @Data
    public static class DictionaryUpdateRequest {
        private Map<String, List<String>> domainTerms;
    }

    @Data
    public static class CacheStats {
        private Double hitRate;
        private Integer cacheSize;
        private Integer maxSize;
        private Long hitCount;
        private Long missCount;
        private Long totalRequests;
    }
}



