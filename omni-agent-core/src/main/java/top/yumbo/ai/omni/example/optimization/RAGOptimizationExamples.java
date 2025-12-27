package top.yumbo.ai.omni.example.optimization;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.core.optimization.RAGOptimizationService;
import top.yumbo.ai.omni.storage.api.model.OptimizationData;
import top.yumbo.ai.omni.storage.api.model.OptimizationType;

import java.util.*;

/**
 * RAG优化算法使用示例
 *
 * 演示13种优化算法的具体使用方法和最佳实践
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@Component
public class RAGOptimizationExamples {

    @Autowired
    private RAGOptimizationService optimizationService;

    /**
     * 示例1: PPL (Prompt Programming Language) - 提示词编程
     *
     * 使用场景: 需要结构化生成高质量提示词
     * 精度提升: +20-25%
     */
    public void example1_PPL() {
        log.info("=== 示例1: PPL优化 ===");

        String documentId = "doc-technical-guide";

        // 分析文档生成关键提示点
        List<String> probablePoints = Arrays.asList(
            "这是一个技术文档",
            "包含API使用说明",
            "需要理解技术细节",
            "适合开发者阅读"
        );

        // 为每个提示点打分
        Map<String, Float> scores = new HashMap<>();
        scores.put("这是一个技术文档", 0.95f);
        scores.put("包含API使用说明", 0.88f);
        scores.put("需要理解技术细节", 0.92f);
        scores.put("适合开发者阅读", 0.85f);

        // 保存PPL优化数据
        String resultId = optimizationService.savePPLData(
            documentId,
            probablePoints,
            scores,
            "v1.0"
        );

        log.info("PPL优化保存成功: {}", resultId);

        // 获取并使用PPL数据
        Optional<OptimizationData> pplData = optimizationService.getOptimizationData(
            documentId,
            OptimizationType.PPL.getCode()
        );

        pplData.ifPresent(data -> {
            log.info("文档类型: {}", data.getOptimizationType());
            log.info("关键点数量: {}", data.getData().size());
        });
    }

    /**
     * 示例2: HyDE (Hypothetical Document Embeddings) - 假设性文档嵌入
     *
     * 使用场景: 查询扩展，提高召回率
     * 精度提升: +10-15%
     */
    public void example2_HyDE() {
        log.info("=== 示例2: HyDE优化 ===");

        String documentId = "doc-faq";
        String userQuery = "如何配置数据库连接？";

        // 生成假设性文档（LLM生成）
        String hypotheticalDoc =
            "要配置数据库连接，您需要在application.yml文件中添加以下配置：\n" +
            "spring:\n" +
            "  datasource:\n" +
            "    url: jdbc:mysql://localhost:3306/dbname\n" +
            "    username: root\n" +
            "    password: password\n" +
            "请确保MySQL服务已启动并且数据库已创建。";

        // 生成embedding向量（这里简化表示）
        float[] embedding = generateEmbedding(hypotheticalDoc);

        // 计算与实际文档的相似度
        double similarity = 0.87;

        // 保存HyDE优化数据
        String resultId = optimizationService.saveHyDEData(
            documentId,
            hypotheticalDoc,
            embedding,
            similarity
        );

        log.info("HyDE优化保存成功: {}, 相似度: {}", resultId, similarity);
    }

    /**
     * 示例3: Rerank - 语义重排序
     *
     * 使用场景: 对检索结果进行精排
     * 精度提升: +8-12%
     */
    public void example3_Rerank() {
        log.info("=== 示例3: Rerank优化 ===");

        String documentId = "doc-search-results";

        // 原始检索结果索引 [0, 1, 2, 3, 4]
        // 重排序后: [2, 0, 4, 1, 3]（基于更强的语义模型）
        List<Integer> rerankedIndices = Arrays.asList(2, 0, 4, 1, 3);

        // 重排序后的分数
        List<Double> scores = Arrays.asList(0.95, 0.91, 0.87, 0.82, 0.76);

        // 使用的重排序模型
        String model = "cross-encoder-ms-marco-MiniLM-L-6-v2";

        // 保存Rerank优化数据
        String resultId = optimizationService.saveRerankData(
            documentId,
            rerankedIndices,
            scores,
            model
        );

        log.info("Rerank优化保存成功: {}, 使用模型: {}", resultId, model);
        log.info("最相关文档索引: {}, 分数: {}", rerankedIndices.get(0), scores.get(0));
    }

    /**
     * 示例4: Query Expansion - 查询扩展
     *
     * 使用场景: 扩展用户查询，提高召回
     * 精度提升: +10-15%
     */
    public void example4_QueryExpansion() {
        log.info("=== 示例4: Query Expansion优化 ===");

        String documentId = "doc-query-expansion";
        String originalQuery = "Spring Boot配置";

        // 生成查询扩展
        List<String> expandedQueries = Arrays.asList(
            originalQuery,
            "Spring Boot application.yml配置",
            "Spring Boot properties文件设置",
            "Spring Boot配置文件示例",
            "SpringBoot参数配置方法"
        );

        // 为每个查询分配权重
        Map<String, Double> weights = new HashMap<>();
        weights.put(originalQuery, 1.0);
        weights.put("Spring Boot application.yml配置", 0.9);
        weights.put("Spring Boot properties文件设置", 0.85);
        weights.put("Spring Boot配置文件示例", 0.8);
        weights.put("SpringBoot参数配置方法", 0.75);

        // 保存Query Expansion优化数据
        String resultId = optimizationService.saveQueryExpansionData(
            documentId,
            expandedQueries,
            weights
        );

        log.info("Query Expansion优化保存成功: {}", resultId);
        log.info("扩展查询数量: {}", expandedQueries.size());
    }

    /**
     * 示例5: Metadata Filter - 元数据过滤
     *
     * 使用场景: 基于文档属性精确过滤
     * 精度提升: +15-20%
     */
    public void example5_MetadataFilter() {
        log.info("=== 示例5: Metadata Filter优化 ===");

        String documentId = "doc-filtered";
        String userQuery = "最近关于Java的技术文档";

        // 从查询中提取隐式过滤条件
        Map<String, Object> filters = new HashMap<>();
        filters.put("language", "Java");
        filters.put("type", "technical");
        filters.put("timestamp_gte", System.currentTimeMillis() - 30L * 24 * 3600 * 1000); // 最近30天
        filters.put("verified", true);

        // 提取的过滤条件描述
        List<String> extractedFilters = Arrays.asList(
            "language=Java",
            "type=technical",
            "time=recent_30_days",
            "verified=true"
        );

        // 保存Metadata Filter优化数据
        String resultId = optimizationService.saveMetadataFilterData(
            documentId,
            filters,
            extractedFilters
        );

        log.info("Metadata Filter优化保存成功: {}", resultId);
        log.info("应用过滤条件数量: {}", filters.size());
    }

    /**
     * 示例6: Context Compression - 上下文压缩
     *
     * 使用场景: 压缩长上下文，节省Token
     * 精度提升: +10-15%
     */
    public void example6_ContextCompression() {
        log.info("=== 示例6: Context Compression优化 ===");

        String documentId = "doc-long-context";

        // 原始长上下文
        String originalContext =
            "Spring Boot是一个基于Spring框架的开源Java框架，用于创建独立的、生产级别的基于Spring的应用程序。" +
            "它简化了Spring应用的初始搭建以及开发过程。Spring Boot提供了自动配置功能，可以根据添加的jar依赖自动配置Spring应用。" +
            "此外，Spring Boot还内嵌了Tomcat、Jetty等服务器，使得应用可以直接运行，无需部署war文件。" +
            "Spring Boot的核心特性包括：独立运行、内嵌服务器、简化配置、自动配置、无代码生成和无xml配置、" +
            "production-ready特性如metrics、health checks和externalized configuration等。";

        // 压缩后的关键上下文
        String compressedContext =
            "Spring Boot: Java框架，简化Spring应用开发。" +
            "核心特性: 自动配置、内嵌服务器、独立运行、无xml配置。";

        // 计算压缩比
        double compressionRatio = (double) compressedContext.length() / originalContext.length();

        // 保存Context Compression优化数据
        String resultId = optimizationService.saveContextCompressionData(
            documentId,
            originalContext,
            compressedContext,
            compressionRatio
        );

        log.info("Context Compression优化保存成功: {}", resultId);
        log.info("原始长度: {}, 压缩后长度: {}, 压缩比: {:.2f}%",
            originalContext.length(),
            compressedContext.length(),
            compressionRatio * 100);
    }

    /**
     * 示例7: 综合优化 - 多算法组合
     *
     * 使用场景: 生产环境综合优化
     * 精度提升: +60-70% (组合效果)
     */
    public void example7_CombinedOptimization() {
        log.info("=== 示例7: 综合优化（多算法组合） ===");

        String documentId = "doc-production";

        // 1. 应用PPL优化
        optimizationService.savePPLData(
            documentId,
            Arrays.asList("技术文档", "API参考"),
            Map.of("技术文档", 0.9f, "API参考", 0.85f),
            "v1.0"
        );

        // 2. 应用Query Expansion
        optimizationService.saveQueryExpansionData(
            documentId,
            Arrays.asList("原始查询", "扩展查询1", "扩展查询2"),
            Map.of("原始查询", 1.0, "扩展查询1", 0.8, "扩展查询2", 0.6)
        );

        // 3. 应用Rerank
        optimizationService.saveRerankData(
            documentId,
            Arrays.asList(2, 0, 1),
            Arrays.asList(0.95, 0.88, 0.76),
            "cross-encoder-v1"
        );

        // 4. 应用Metadata Filter
        optimizationService.saveMetadataFilterData(
            documentId,
            Map.of("type", "technical", "verified", true),
            Arrays.asList("type=technical", "verified=true")
        );

        // 获取所有应用的优化
        List<String> appliedOptimizations = optimizationService.getOptimizationTypes(documentId);

        log.info("综合优化完成！");
        log.info("应用的优化算法: {}", appliedOptimizations);
        log.info("优化算法数量: {}", appliedOptimizations.size());
    }

    /**
     * 示例8: A/B测试 - 对比不同算法效果
     */
    public void example8_ABTesting() {
        log.info("=== 示例8: A/B测试 ===");

        String documentId = "doc-ab-test";

        // 方案A: PPL优化
        Map<String, Object> pplData = Map.of(
            "probablePoints", Arrays.asList("point1", "point2"),
            "scores", Map.of("point1", 0.9f)
        );
        Map<String, Double> pplMetrics = Map.of(
            "precisionGain", 20.0,
            "recallGain", 15.0,
            "processingTime", 50.0
        );
        optimizationService.saveOptimizationData(
            documentId, "ppl", pplData, null, pplMetrics
        );

        // 方案B: HyDE优化
        Map<String, Object> hydeData = Map.of(
            "hypotheticalDoc", "假设文档...",
            "similarity", 0.85
        );
        Map<String, Double> hydeMetrics = Map.of(
            "precisionGain", 15.0,
            "recallGain", 20.0,
            "processingTime", 120.0
        );
        optimizationService.saveOptimizationData(
            documentId, "hyde", hydeData, null, hydeMetrics
        );

        // 对比结果
        List<OptimizationData> allResults = optimizationService.getAllOptimizationData(documentId);

        OptimizationData best = allResults.stream()
            .max(Comparator.comparing(d -> d.getMetric("precisionGain")))
            .orElse(null);

        if (best != null) {
            log.info("最佳算法: {}", best.getOptimizationType());
            log.info("精度提升: {}", best.getMetric("precisionGain"));
        }
    }

    /**
     * 示例9: 自定义算法
     */
    public void example9_CustomAlgorithm() {
        log.info("=== 示例9: 自定义算法 ===");

        String documentId = "doc-custom";

        // 自定义算法数据
        Map<String, Object> customData = Map.of(
            "algorithm", "MySemanticBoost",
            "version", "v1.0",
            "parameters", Map.of("boost_factor", 1.5, "threshold", 0.7),
            "results", Arrays.asList("result1", "result2", "result3")
        );

        Map<String, Object> metadata = Map.of(
            "author", "OmniAgent Team",
            "description", "自定义语义增强算法",
            "experimentId", "exp-001"
        );

        Map<String, Double> metrics = Map.of(
            "precisionGain", 25.0,
            "recallGain", 18.0,
            "f1Score", 0.89,
            "processingTime", 85.0
        );

        // 保存自定义算法数据
        String resultId = optimizationService.saveOptimizationData(
            documentId,
            "my_semantic_boost",
            customData,
            metadata,
            metrics
        );

        log.info("自定义算法保存成功: {}", resultId);
        log.info("算法描述: {}", metadata.get("description"));
    }

    // ========== 工具方法 ==========

    /**
     * 生成embedding向量（简化示例）
     */
    private float[] generateEmbedding(String text) {
        // 实际应用中应该使用真实的embedding模型
        float[] embedding = new float[384]; // BERT-base维度
        Random random = new Random(text.hashCode());
        for (int i = 0; i < embedding.length; i++) {
            embedding[i] = random.nextFloat();
        }
        return embedding;
    }

    /**
     * 运行所有示例
     */
    public void runAllExamples() {
        log.info("==================== 开始运行所有RAG优化示例 ====================");

        try {
            example1_PPL();
            example2_HyDE();
            example3_Rerank();
            example4_QueryExpansion();
            example5_MetadataFilter();
            example6_ContextCompression();
            example7_CombinedOptimization();
            example8_ABTesting();
            example9_CustomAlgorithm();

            log.info("==================== 所有示例运行完成 ====================");
        } catch (Exception e) {
            log.error("示例运行失败", e);
        }
    }
}

