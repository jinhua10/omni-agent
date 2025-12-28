package top.yumbo.ai.omni.core.old.optimization;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.storage.api.model.OptimizationType;

import java.util.*;

/**
 * 自动算法选择引擎
 *
 * 基于查询特征、文档类型、性能要求等因素，
 * 自动推荐最佳的RAG优化算法组合
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
public class AutoOptimizationSelector {

    /**
     * 查询上下文
     */
    @Data
    public static class QueryContext {
        private String query;                    // 用户查询
        private int queryLength;                 // 查询长度
        private String documentType;             // 文档类型
        private int latencyRequirementMs;        // 延迟要求(ms)
        private double precisionRequirement;     // 精度要求(0-1)
        private int concurrentLevel;             // 并发级别
        private Map<String, Object> metadata;    // 额外元数据

        public static QueryContext fromQuery(String query) {
            QueryContext context = new QueryContext();
            context.setQuery(query);
            context.setQueryLength(query != null ? query.length() : 0);
            context.setDocumentType("general");
            context.setLatencyRequirementMs(200);
            context.setPrecisionRequirement(0.90);
            context.setConcurrentLevel(1);
            context.setMetadata(new HashMap<>());
            return context;
        }
    }

    /**
     * 算法推荐结果
     */
    @Data
    public static class OptimizationRecommendation {
        private List<String> primaryAlgorithms;      // 主要算法
        private List<String> secondaryAlgorithms;    // 次要算法
        private Map<String, Double> algorithmScores; // 算法评分
        private String reasoning;                     // 推荐理由
        private double expectedPrecisionGain;         // 预期精度提升
        private int expectedLatencyMs;                // 预期延迟
    }

    /**
     * 自动选择最佳算法组合
     */
    public OptimizationRecommendation selectOptimalAlgorithms(QueryContext context) {
        log.info("开始自动选择算法 - 查询长度: {}, 文档类型: {}, 延迟要求: {}ms, 精度要求: {}",
                context.getQueryLength(), context.getDocumentType(),
                context.getLatencyRequirementMs(), context.getPrecisionRequirement());

        OptimizationRecommendation recommendation = new OptimizationRecommendation();
        recommendation.setPrimaryAlgorithms(new ArrayList<>());
        recommendation.setSecondaryAlgorithms(new ArrayList<>());
        recommendation.setAlgorithmScores(new HashMap<>());

        // 第一层：根据查询长度选择
        selectByQueryLength(context, recommendation);

        // 第二层：根据文档类型优化
        selectByDocumentType(context, recommendation);

        // 第三层：根据性能要求调整
        adjustByPerformanceRequirement(context, recommendation);

        // 计算预期效果
        calculateExpectedResults(recommendation);

        // 生成推荐理由
        generateReasoning(context, recommendation);

        log.info("算法选择完成 - 主要算法: {}, 次要算法: {}",
                recommendation.getPrimaryAlgorithms(),
                recommendation.getSecondaryAlgorithms());

        return recommendation;
    }

    /**
     * 根据查询长度选择算法
     */
    private void selectByQueryLength(QueryContext context, OptimizationRecommendation recommendation) {
        int length = context.getQueryLength();

        if (length < 10) {
            // 极短查询 - 必须扩展
            addAlgorithm(recommendation, OptimizationType.QUERY_EXPANSION, 0.95, true);
            addAlgorithm(recommendation, OptimizationType.HYBRID_SEARCH, 0.85, false);
            log.debug("极短查询({})，添加Query Expansion和Hybrid Search", length);

        } else if (length < 20) {
            // 短查询
            addAlgorithm(recommendation, OptimizationType.QUERY_EXPANSION, 0.85, true);
            addAlgorithm(recommendation, OptimizationType.PPL, 0.90, true);
            log.debug("短查询({})，添加Query Expansion和PPL", length);

        } else if (length < 50) {
            // 中等查询
            addAlgorithm(recommendation, OptimizationType.PPL, 0.95, true);
            addAlgorithm(recommendation, OptimizationType.HYDE, 0.80, false);
            log.debug("中等查询({})，添加PPL和HyDE", length);

        } else {
            // 长查询 - 需要压缩
            addAlgorithm(recommendation, OptimizationType.CONTEXT_COMPRESSION, 0.90, true);
            addAlgorithm(recommendation, OptimizationType.HYDE, 0.85, true);
            log.debug("长查询({})，添加Context Compression和HyDE", length);
        }
    }

    /**
     * 根据文档类型选择算法
     */
    private void selectByDocumentType(QueryContext context, OptimizationRecommendation recommendation) {
        String docType = context.getDocumentType();

        switch (docType.toLowerCase()) {
            case "technical":
            case "code":
                addAlgorithm(recommendation, OptimizationType.SEMANTIC_CHUNKING, 0.90, true);
                addAlgorithm(recommendation, OptimizationType.METADATA_FILTER, 0.85, false);
                log.debug("技术文档，添加Semantic Chunking和Metadata Filter");
                break;

            case "faq":
            case "qa":
                addAlgorithm(recommendation, OptimizationType.HOPE_ROUTING, 0.95, true);
                addAlgorithm(recommendation, OptimizationType.HYBRID_SEARCH, 0.88, false);
                log.debug("FAQ文档，添加HOPE Routing和Hybrid Search");
                break;

            case "news":
            case "article":
                addAlgorithm(recommendation, OptimizationType.METADATA_FILTER, 0.90, true);
                addAlgorithm(recommendation, OptimizationType.CONTEXT_COMPRESSION, 0.75, false);
                log.debug("新闻文章，添加Metadata Filter和Context Compression");
                break;

            case "academic":
            case "paper":
                addAlgorithm(recommendation, OptimizationType.KNOWLEDGE_GRAPH, 0.88, true);
                addAlgorithm(recommendation, OptimizationType.RERANK, 0.92, true);
                log.debug("学术论文，添加Knowledge Graph和Rerank");
                break;

            case "ecommerce":
            case "product":
                addAlgorithm(recommendation, OptimizationType.BEHAVIOR_ANALYSIS, 0.85, false);
                addAlgorithm(recommendation, OptimizationType.METADATA_FILTER, 0.90, true);
                log.debug("电商产品，添加Behavior Analysis和Metadata Filter");
                break;

            default:
                addAlgorithm(recommendation, OptimizationType.HYBRID_SEARCH, 0.85, false);
                log.debug("通用文档，添加Hybrid Search");
        }
    }

    /**
     * 根据性能要求调整算法
     */
    private void adjustByPerformanceRequirement(QueryContext context, OptimizationRecommendation recommendation) {
        int latency = context.getLatencyRequirementMs();
        double precision = context.getPrecisionRequirement();

        // 低延迟要求 - 移除慢速算法
        if (latency < 100) {
            removeSlowAlgorithms(recommendation);
            // 添加快速算法
            if (!hasAlgorithm(recommendation, OptimizationType.HOPE_ROUTING)) {
                addAlgorithm(recommendation, OptimizationType.HOPE_ROUTING, 0.90, false);
            }
            log.debug("低延迟要求({}ms)，移除慢速算法，添加HOPE Routing", latency);

        } else if (latency > 300) {
            // 高延迟可接受 - 可以使用复杂算法
            if (precision > 0.93) {
                addAlgorithm(recommendation, OptimizationType.RERANK, 0.95, true);
                addAlgorithm(recommendation, OptimizationType.MULTI_MODEL_VOTING, 0.92, false);
                log.debug("高延迟可接受且高精度要求，添加Rerank和Multi-Model Voting");
            }
        }

        // 高精度要求
        if (precision > 0.95) {
            addAlgorithm(recommendation, OptimizationType.RERANK, 0.98, true);
            addAlgorithm(recommendation, OptimizationType.MULTI_MODEL_VOTING, 0.95, true);
            log.debug("高精度要求({})，添加Rerank和Multi-Model Voting", precision);

        } else if (precision > 0.90) {
            if (!hasAlgorithm(recommendation, OptimizationType.RERANK)) {
                addAlgorithm(recommendation, OptimizationType.RERANK, 0.88, false);
            }
            log.debug("中高精度要求({})，添加Rerank", precision);
        }
    }

    /**
     * 计算预期效果
     */
    private void calculateExpectedResults(OptimizationRecommendation recommendation) {
        double totalPrecisionGain = 0.0;
        int totalLatency = 0;

        // 计算所有主要算法的累计效果
        for (String algorithm : recommendation.getPrimaryAlgorithms()) {
            OptimizationType type = OptimizationType.fromCode(algorithm);

            // 精度提升（累加但有衰减）
            double gain = getAlgorithmPrecisionGain(type);
            totalPrecisionGain += gain * (1.0 - totalPrecisionGain / 100.0); // 边际递减

            // 延迟累加
            totalLatency += getAlgorithmLatency(type);
        }

        // 次要算法也贡献一部分
        for (String algorithm : recommendation.getSecondaryAlgorithms()) {
            OptimizationType type = OptimizationType.fromCode(algorithm);
            double gain = getAlgorithmPrecisionGain(type) * 0.5; // 次要算法只算50%
            totalPrecisionGain += gain * (1.0 - totalPrecisionGain / 100.0);
            totalLatency += getAlgorithmLatency(type);
        }

        recommendation.setExpectedPrecisionGain(totalPrecisionGain);
        recommendation.setExpectedLatencyMs(totalLatency);

        log.debug("预期精度提升: +{}%, 预期延迟: {}ms",
                String.format("%.1f", totalPrecisionGain), totalLatency);
    }

    /**
     * 生成推荐理由
     */
    private void generateReasoning(QueryContext context, OptimizationRecommendation recommendation) {
        StringBuilder reasoning = new StringBuilder();

        reasoning.append("基于以下因素选择算法组合：\n");
        reasoning.append(String.format("1. 查询长度: %d字符 ", context.getQueryLength()));

        if (context.getQueryLength() < 10) {
            reasoning.append("(极短查询，需要扩展)\n");
        } else if (context.getQueryLength() < 20) {
            reasoning.append("(短查询，需要优化)\n");
        } else if (context.getQueryLength() < 50) {
            reasoning.append("(中等查询，标准处理)\n");
        } else {
            reasoning.append("(长查询，需要压缩)\n");
        }

        reasoning.append(String.format("2. 文档类型: %s\n", context.getDocumentType()));
        reasoning.append(String.format("3. 延迟要求: %dms ", context.getLatencyRequirementMs()));

        if (context.getLatencyRequirementMs() < 100) {
            reasoning.append("(实时系统)\n");
        } else if (context.getLatencyRequirementMs() < 300) {
            reasoning.append("(一般要求)\n");
        } else {
            reasoning.append("(可接受较高延迟)\n");
        }

        reasoning.append(String.format("4. 精度要求: %.0f%% ", context.getPrecisionRequirement() * 100));

        if (context.getPrecisionRequirement() > 0.95) {
            reasoning.append("(高精度)\n");
        } else if (context.getPrecisionRequirement() > 0.90) {
            reasoning.append("(中高精度)\n");
        } else {
            reasoning.append("(标准精度)\n");
        }

        reasoning.append("\n推荐算法组合：\n");
        reasoning.append("主要算法: ").append(String.join(", ", recommendation.getPrimaryAlgorithms())).append("\n");
        reasoning.append("次要算法: ").append(String.join(", ", recommendation.getSecondaryAlgorithms())).append("\n");
        reasoning.append(String.format("\n预期效果：精度提升+%.1f%%, 延迟%dms",
                recommendation.getExpectedPrecisionGain(),
                recommendation.getExpectedLatencyMs()));

        recommendation.setReasoning(reasoning.toString());
    }

    // ========== 辅助方法 ==========

    private void addAlgorithm(OptimizationRecommendation recommendation,
                             OptimizationType type,
                             double score,
                             boolean isPrimary) {
        String code = type.getCode();

        if (isPrimary && !recommendation.getPrimaryAlgorithms().contains(code)) {
            recommendation.getPrimaryAlgorithms().add(code);
        } else if (!isPrimary && !recommendation.getSecondaryAlgorithms().contains(code)) {
            recommendation.getSecondaryAlgorithms().add(code);
        }

        recommendation.getAlgorithmScores().put(code, score);
    }

    private boolean hasAlgorithm(OptimizationRecommendation recommendation, OptimizationType type) {
        String code = type.getCode();
        return recommendation.getPrimaryAlgorithms().contains(code) ||
               recommendation.getSecondaryAlgorithms().contains(code);
    }

    private void removeSlowAlgorithms(OptimizationRecommendation recommendation) {
        // 移除慢速算法
        recommendation.getPrimaryAlgorithms().removeIf(alg ->
            alg.equals(OptimizationType.MULTI_MODEL_VOTING.getCode()) ||
            alg.equals(OptimizationType.KNOWLEDGE_GRAPH.getCode())
        );
        recommendation.getSecondaryAlgorithms().removeIf(alg ->
            alg.equals(OptimizationType.MULTI_MODEL_VOTING.getCode()) ||
            alg.equals(OptimizationType.KNOWLEDGE_GRAPH.getCode())
        );
    }

    private double getAlgorithmPrecisionGain(OptimizationType type) {
        // 返回算法的典型精度提升（%）
        return switch (type) {
            case PPL -> 22.5;
            case HYDE -> 12.5;
            case RERANK -> 10.0;
            case QUERY_EXPANSION -> 12.5;
            case METADATA_FILTER -> 17.5;
            case CONTEXT_COMPRESSION -> 12.5;
            case SEMANTIC_CHUNKING -> 17.5;
            case HYBRID_SEARCH -> 16.5;
            case KNOWLEDGE_GRAPH -> 21.5;
            case HOPE_ROUTING -> 27.5;
            case BEHAVIOR_ANALYSIS -> 13.5;
            case MULTI_MODEL_VOTING -> 25.0;
            default -> 10.0;
        };
    }

    private int getAlgorithmLatency(OptimizationType type) {
        // 返回算法的典型延迟（ms）
        return switch (type) {
            case PPL -> 10;
            case HYDE -> 50;
            case RERANK -> 80;
            case QUERY_EXPANSION -> 20;
            case METADATA_FILTER -> 5;
            case CONTEXT_COMPRESSION -> 60;
            case SEMANTIC_CHUNKING -> 30;
            case HYBRID_SEARCH -> 15;
            case KNOWLEDGE_GRAPH -> 120;
            case HOPE_ROUTING -> 5;
            case BEHAVIOR_ANALYSIS -> 10;
            case MULTI_MODEL_VOTING -> 200;
            default -> 50;
        };
    }

    /**
     * 批量评估多个场景
     */
    public Map<String, OptimizationRecommendation> evaluateScenarios(List<QueryContext> contexts) {
        Map<String, OptimizationRecommendation> results = new LinkedHashMap<>();

        for (QueryContext context : contexts) {
            String scenarioKey = String.format("%s_%s_%dms",
                    context.getDocumentType(),
                    context.getQueryLength() < 20 ? "short" : "long",
                    context.getLatencyRequirementMs());

            results.put(scenarioKey, selectOptimalAlgorithms(context));
        }

        return results;
    }
}


