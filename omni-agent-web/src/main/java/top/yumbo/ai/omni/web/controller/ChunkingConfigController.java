package top.yumbo.ai.omni.web.controller;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.chunking.ChunkingStrategy;
import top.yumbo.ai.omni.chunking.starter.ChunkingStrategyManager;
import top.yumbo.ai.omni.web.model.ApiResponse;
import top.yumbo.ai.omni.storage.api.model.Chunk;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 分块策略配置控制器 (Chunking Strategy Configuration Controller)
 *
 * 提供分块策略的交互式配置和实时预览功能
 * (Provides interactive configuration and real-time preview of chunking strategies)
 *
 * Phase 4.2.1 - 分块策略配置界面
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@RestController
@RequestMapping("/api/chunking")
@RequiredArgsConstructor
public class ChunkingConfigController {

    private final ChunkingStrategyManager strategyManager;

    /**
     * 获取所有可用的分块策略
     * (Get all available chunking strategies)
     *
     * GET /api/chunking/strategies
     */
    @GetMapping("/strategies")
    public ApiResponse<List<StrategyInfo>> getAvailableStrategies() {
        try {
            List<top.yumbo.ai.omni.chunking.ChunkingStrategy> strategyEnums = strategyManager.getAvailableStrategies();

            List<StrategyInfo> strategies = strategyEnums.stream()
                .map(strategy -> {
                    String name = strategy.name();
                    return new StrategyInfo(
                        name.toLowerCase().replace("_", "-"),
                        formatStrategyName(name),
                        getStrategyDescription(strategy),
                        getDefaultParams(strategy)
                    );
                })
                .collect(Collectors.toList());

            log.info("✅ 获取可用策略: {} 个", strategies.size());
            return ApiResponse.success(strategies);

        } catch (Exception e) {
            log.error("❌ 获取策略列表失败", e);
            return ApiResponse.error("获取策略列表失败: " + e.getMessage());
        }
    }

    /**
     * 获取策略的详细信息和默认参数
     * (Get strategy details and default parameters)
     *
     * GET /api/chunking/strategies/{strategyName}
     */
    @GetMapping("/strategies/{strategyName}")
    public ApiResponse<StrategyInfo> getStrategyInfo(@PathVariable String strategyName) {
        try {
            // 将 kebab-case 转换为 UPPER_SNAKE_CASE
            String enumName = strategyName.toUpperCase().replace("-", "_");
            top.yumbo.ai.omni.chunking.ChunkingStrategy strategy;

            try {
                strategy = top.yumbo.ai.omni.chunking.ChunkingStrategy.valueOf(enumName);
            } catch (IllegalArgumentException e) {
                return ApiResponse.error("策略不存在: " + strategyName);
            }

            StrategyInfo strategyInfo = new StrategyInfo(
                strategyName,
                formatStrategyName(strategy.name()),
                getStrategyDescription(strategy),
                getDefaultParams(strategy)
            );

            return ApiResponse.success(strategyInfo);

        } catch (Exception e) {
            log.error("❌ 获取策略信息失败: {}", strategyName, e);
            return ApiResponse.error("获取策略信息失败: " + e.getMessage());
        }
    }

    /**
     * 实时预览分块效果
     * (Real-time preview of chunking results)
     *
     * POST /api/chunking/preview
     *
     * @param request 预览请求（内容、策略、参数）
     * @return 分块预览结果
     */
    @PostMapping("/preview")
    public ApiResponse<ChunkingPreviewResponse> previewChunking(
            @RequestBody ChunkingPreviewRequest request) {
        try {
            log.info("🔍 预览分块: strategy={}, contentLength={}",
                request.getStrategy(), request.getContent().length());

            long startTime = System.currentTimeMillis();

            // 转换策略名称为枚举
            ChunkingStrategy strategy = parseStrategy(request.getStrategy());

            // 构建配置对象
            top.yumbo.ai.omni.chunking.ChunkingConfig config = buildChunkingConfig(request.getParams());

            // 执行分块
            List<top.yumbo.ai.omni.chunking.Chunk> chunkingChunks = strategyManager.chunkWithStrategy(
                "preview_" + System.currentTimeMillis(),
                request.getContent(),
                strategy,
                config
            );

            // 转换为存储模型
            List<Chunk> chunks = convertChunks(chunkingChunks);

            long elapsedTime = System.currentTimeMillis() - startTime;

            // 构建预览响应
            ChunkingPreviewResponse response = new ChunkingPreviewResponse();
            response.setChunks(chunks.stream()
                .map(this::toChunkPreview)
                .collect(Collectors.toList()));
            response.setTotalChunks(chunks.size());
            response.setStrategy(request.getStrategy());
            response.setElapsedTimeMs(elapsedTime);
            response.setStatistics(calculateStatistics(chunks));

            log.info("✅ 分块预览完成: {} 个块, 耗时 {}ms", chunks.size(), elapsedTime);
            return ApiResponse.success(response);

        } catch (Exception e) {
            log.error("❌ 分块预览失败", e);
            return ApiResponse.error("分块预览失败: " + e.getMessage());
        }
    }

    /**
     * 对比多个策略的分块效果
     * (Compare chunking results of multiple strategies)
     *
     * POST /api/chunking/compare
     */
    @PostMapping("/compare")
    public ApiResponse<List<StrategyComparisonResult>> compareStrategies(
            @RequestBody StrategyComparisonRequest request) {
        try {
            log.info("📊 对比分块策略: {} 个策略", request.getStrategies().size());

            List<StrategyComparisonResult> results = new ArrayList<>();

            for (StrategyComparison comparison : request.getStrategies()) {
                long startTime = System.currentTimeMillis();

                // 转换策略名称为枚举
                top.yumbo.ai.omni.chunking.ChunkingStrategy strategy = parseStrategy(comparison.getStrategy());

                // 构建配置对象
                top.yumbo.ai.omni.chunking.ChunkingConfig config = buildChunkingConfig(comparison.getParams());

                List<Chunk> chunkingChunks = strategyManager.chunkWithStrategy(
                    "compare_" + System.currentTimeMillis(),
                    request.getContent(),
                    strategy,
                    config
                );

                // 转换为存储模型
                List<Chunk> chunks = convertChunks(chunkingChunks);

                long elapsedTime = System.currentTimeMillis() - startTime;

                StrategyComparisonResult result = new StrategyComparisonResult();
                result.setStrategy(comparison.getStrategy());
                result.setChunkCount(chunks.size());
                result.setElapsedTimeMs(elapsedTime);
                result.setStatistics(calculateStatistics(chunks));
                result.setChunkPreviews(chunks.stream()
                    .limit(3) // 只返回前3个分块的预览
                    .map(this::toChunkPreview)
                    .collect(Collectors.toList()));

                results.add(result);
            }

            log.info("✅ 策略对比完成: {} 个策略", results.size());
            return ApiResponse.success(results);

        } catch (Exception e) {
            log.error("❌ 策略对比失败", e);
            return ApiResponse.error("策略对比失败: " + e.getMessage());
        }
    }

    // ========== 辅助方法 ==========

    /**
     * 将策略名称字符串转换为ChunkingStrategy枚举
     */
    private ChunkingStrategy parseStrategy(String strategyName) {
        try {
            String enumName = strategyName.toUpperCase().replace("-", "_");
            return ChunkingStrategy.valueOf(enumName);
        } catch (IllegalArgumentException e) {
            log.warn("⚠️ 未知策略: {}, 使用默认策略 FIXED_LENGTH", strategyName);
            return ChunkingStrategy.FIXED_LENGTH;
        }
    }

    /**
     * 构建ChunkingConfig对象
     */
    private top.yumbo.ai.omni.chunking.ChunkingConfig buildChunkingConfig(Map<String, Object> params) {
        if (params == null) {
            params = new HashMap<>();
        }

        return top.yumbo.ai.omni.chunking.ChunkingConfig.builder()
                .fixedLengthSize(getIntParam(params, "chunkSize", 500))
                .overlap(getIntParam(params, "overlapSize", 50))
                .minChunkSize(getIntParam(params, "minChunkSize", 100))
                .maxChunkSize(getIntParam(params, "maxChunkSize", 2000))
                .semanticThreshold(getDoubleParam(params, "semanticThreshold", 0.7))
                .build();
    }

    /**
     * 从参数Map中获取整数值
     */
    private int getIntParam(Map<String, Object> params, String key, int defaultValue) {
        if (params == null || !params.containsKey(key)) {
            return defaultValue;
        }
        Object value = params.get(key);
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        try {
            return Integer.parseInt(value.toString());
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    /**
     * 从参数Map中获取双精度浮点数值
     */
    private double getDoubleParam(Map<String, Object> params, String key, double defaultValue) {
        if (params == null || !params.containsKey(key)) {
            return defaultValue;
        }
        Object value = params.get(key);
        if (value instanceof Number) {
            return ((Number) value).doubleValue();
        }
        try {
            return Double.parseDouble(value.toString());
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    /**
     * 转换chunking.Chunk列表为storage.Chunk列表
     */
    private List<Chunk> convertChunks(List<top.yumbo.ai.omni.chunking.Chunk> chunkingChunks) {
        List<Chunk> chunks = new ArrayList<>();
        for (top.yumbo.ai.omni.chunking.Chunk chunkingChunk : chunkingChunks) {
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("strategy", chunkingChunk.getStrategy() != null ? chunkingChunk.getStrategy().name() : "UNKNOWN");
            metadata.put("startPosition", chunkingChunk.getStartPosition());
            metadata.put("endPosition", chunkingChunk.getEndPosition());
            metadata.put("length", chunkingChunk.getLength());

            Chunk chunk = Chunk.builder()
                    .id(chunkingChunk.getChunkId())
                    .documentId(chunkingChunk.getDocumentId())
                    .content(chunkingChunk.getContent())
                    .sequence(chunkingChunk.getIndex())
                    .startPosition(chunkingChunk.getStartPosition())
                    .endPosition(chunkingChunk.getEndPosition())
                    .metadata(metadata)
                    .createdAt(System.currentTimeMillis())
                    .build();
            chunks.add(chunk);
        }
        return chunks;
    }

    /**
     * 格式化策略名称
     */
    private String formatStrategyName(String enumName) {
        // 将 FIXED_LENGTH 转换为 "Fixed Length"
        return Arrays.stream(enumName.split("_"))
                .map(word -> word.substring(0, 1).toUpperCase() + word.substring(1).toLowerCase())
                .collect(Collectors.joining(" "));
    }

    /**
     * 获取策略描述
     */
    private String getStrategyDescription(ChunkingStrategy strategy) {
        return switch (strategy) {
            case FIXED_LENGTH -> "固定长度分块策略，按指定字符数分割文本";
            case SENTENCE -> "句子边界分块策略，在句子边界处分割";
            case PARAGRAPH -> "段落分块策略，按段落分割文本";
            case SEMANTIC -> "语义分块策略，基于语义相似度分割";
            case PPL -> "PPL分块策略，基于困惑度分割";
            case MARKDOWN -> "Markdown分块策略，按Markdown结构分割";
            default -> "未知策略";
        };
    }

    /**
     * 获取策略默认参数
     */
    private Map<String, Object> getDefaultParams(ChunkingStrategy strategy) {
        Map<String, Object> params = new HashMap<>();
        switch (strategy) {
            case FIXED_LENGTH:
                params.put("chunkSize", 500);
                params.put("overlapSize", 50);
                break;
            case SENTENCE:
                params.put("maxChunkSize", 1000);
                params.put("minChunkSize", 100);
                break;
            case PARAGRAPH:
                params.put("maxChunkSize", 2000);
                params.put("minChunkSize", 200);
                break;
            case SEMANTIC:
                params.put("semanticThreshold", 0.7);
                params.put("maxChunkSize", 1500);
                break;
            case PPL:
                params.put("perplexityThreshold", 10.0);
                params.put("maxChunkSize", 1000);
                break;
            case MARKDOWN:
                params.put("maxChunkSize", 2000);
                break;
            default:
                params.put("chunkSize", 500);
                break;
        }
        return params;
    }

    /**
     * 解析默认参数字符串
     */
    private Map<String, Object> parseDefaultParams(String paramsStr) {
        Map<String, Object> params = new HashMap<>();

        // 简单解析 "{key=value, key2=value2}" 格式
        paramsStr = paramsStr.trim();
        if (paramsStr.startsWith("{") && paramsStr.endsWith("}")) {
            paramsStr = paramsStr.substring(1, paramsStr.length() - 1);
            String[] pairs = paramsStr.split(",");

            for (String pair : pairs) {
                String[] kv = pair.trim().split("=");
                if (kv.length == 2) {
                    String key = kv[0].trim();
                    String value = kv[1].trim();

                    // 尝试转换为数字
                    try {
                        params.put(key, Integer.parseInt(value));
                    } catch (NumberFormatException e) {
                        params.put(key, value);
                    }
                }
            }
        }

        return params;
    }

    /**
     * 转换 Chunk 为预览格式
     */
    private ChunkPreview toChunkPreview(Chunk chunk) {
        ChunkPreview preview = new ChunkPreview();
        preview.setId(chunk.getId());
        preview.setSequence(chunk.getSequence());
        preview.setContent(chunk.getContent());
        preview.setContentLength(chunk.getContent().length());
        preview.setContentPreview(getContentPreview(chunk.getContent(), 200));
        preview.setMetadata(chunk.getMetadata());
        return preview;
    }

    /**
     * 获取内容预览（前N个字符）
     */
    private String getContentPreview(String content, int maxLength) {
        if (content.length() <= maxLength) {
            return content;
        }
        return content.substring(0, maxLength) + "...";
    }

    /**
     * 计算分块统计信息
     */
    private ChunkingStatistics calculateStatistics(List<Chunk> chunks) {
        if (chunks.isEmpty()) {
            return new ChunkingStatistics();
        }

        ChunkingStatistics stats = new ChunkingStatistics();
        stats.setTotalChunks(chunks.size());

        // 计算长度统计
        IntSummaryStatistics lengthStats = chunks.stream()
            .mapToInt(chunk -> chunk.getContent().length())
            .summaryStatistics();

        stats.setMinLength(lengthStats.getMin());
        stats.setMaxLength(lengthStats.getMax());
        stats.setAvgLength((int) lengthStats.getAverage());
        stats.setTotalLength((int) lengthStats.getSum());

        return stats;
    }

    // ========== 数据传输对象 (DTOs) ==========

    @Data
    public static class StrategyInfo {
        private String id;
        private String name;
        private String description;
        private Map<String, Object> defaultParams;

        public StrategyInfo(String id, String name, String description, Map<String, Object> defaultParams) {
            this.id = id;
            this.name = name;
            this.description = description;
            this.defaultParams = defaultParams;
        }
    }

    @Data
    public static class ChunkingPreviewRequest {
        private String content;           // 要分块的内容
        private String strategy;          // 策略名称
        private Map<String, Object> params;  // 策略参数
    }

    @Data
    public static class ChunkingPreviewResponse {
        private List<ChunkPreview> chunks;
        private int totalChunks;
        private String strategy;
        private long elapsedTimeMs;
        private ChunkingStatistics statistics;
    }

    @Data
    public static class ChunkPreview {
        private String id;
        private int sequence;
        private String content;
        private int contentLength;
        private String contentPreview;
        private Map<String, Object> metadata;
    }

    @Data
    public static class ChunkingStatistics {
        private int totalChunks;
        private int minLength;
        private int maxLength;
        private int avgLength;
        private int totalLength;
    }

    @Data
    public static class StrategyComparisonRequest {
        private String content;
        private List<StrategyComparison> strategies;
    }

    @Data
    public static class StrategyComparison {
        private String strategy;
        private Map<String, Object> params;
    }

    @Data
    public static class StrategyComparisonResult {
        private String strategy;
        private int chunkCount;
        private long elapsedTimeMs;
        private ChunkingStatistics statistics;
        private List<ChunkPreview> chunkPreviews;  // 前几个分块的预览
    }
}






