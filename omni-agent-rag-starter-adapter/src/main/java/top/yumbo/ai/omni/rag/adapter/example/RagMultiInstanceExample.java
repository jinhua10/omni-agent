package top.yumbo.ai.omni.rag.adapter.example;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.adapter.impl.RagServiceRegistry;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;

import java.util.List;
import java.util.Map;

/**
 * RAG 多实例使用示例
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class RagMultiInstanceExample {

    /**
     * 主 RAG 服务（自动注入 primary 实例）
     */
    private final RagService primaryRagService;

    /**
     * RAG 服务注册表（管理所有实例）
     */
    private final RagServiceRegistry ragServiceRegistry;

    /**
     * 示例 1: 使用主实例进行语义搜索
     */
    public void example1_PrimaryInstance() {
        log.info("=== 示例 1: 使用主实例 ===");

        // 1. 索引文档（自动向量化）
        Document doc = Document.builder()
                .id("doc-001")
                .title("RAG 系统介绍")
                .content("RAG（Retrieval-Augmented Generation）是一种结合检索和生成的AI技术...")
                .metadata(Map.of("category", "技术文档"))
                .build();

        Vector vector = primaryRagService.embed(doc.getContent());
        primaryRagService.index(doc.getId(), vector, doc.getMetadata());

        // 2. 语义搜索
        List<Document> results = primaryRagService.semanticSearch("什么是RAG", 10);
        log.info("找到 {} 个相关文档", results.size());
    }

    /**
     * 示例 2: 使用指定实例
     */
    public void example2_SpecificInstance() {
        log.info("=== 示例 2: 使用指定实例 ===");

        // 获取 SQLite 实例
        RagService sqliteService = ragServiceRegistry.getServiceOrThrow("sqlite-ollama");

        // 使用 SQLite 实例进行搜索
        List<Document> results = sqliteService.semanticSearch("数据库查询", 5);
        log.info("SQLite 实例找到 {} 个文档", results.size());
    }

    /**
     * 示例 3: 批量向量化
     */
    public void example3_BatchEmbedding() {
        log.info("=== 示例 3: 批量向量化 ===");

        List<String> texts = List.of(
                "人工智能的发展历程",
                "机器学习的应用场景",
                "深度学习的技术原理"
        );

        // 批量向量化
        List<Vector> vectors = primaryRagService.batchEmbed(texts);
        log.info("批量向量化完成，共 {} 个向量", vectors.size());
    }

    /**
     * 示例 4: 多实例协同工作
     */
    public void example4_MultiInstanceCooperation() {
        log.info("=== 示例 4: 多实例协同 ===");

        // 使用 File 实例进行快速全文搜索
        RagService fileService = ragServiceRegistry.getServiceOrThrow("file-onnx");
        List<Document> fastResults = fileService.semanticSearch("快速查询", 5);

        // 使用 MongoDB 实例进行精确语义搜索
        RagService mongoService = ragServiceRegistry.getServiceOrThrow("mongodb-online");
        List<Document> accurateResults = mongoService.semanticSearch("精确查询", 5);

        log.info("File 实例: {} 个结果, MongoDB 实例: {} 个结果",
                fastResults.size(), accurateResults.size());
    }

    /**
     * 示例 5: 遍历所有实例
     */
    public void example5_IterateAllInstances() {
        log.info("=== 示例 5: 遍历所有实例 ===");

        log.info("当前活跃的 RAG 实例: {}", ragServiceRegistry.getInstanceIds());
        log.info("实例总数: {}", ragServiceRegistry.size());

        // 在每个实例中搜索
        for (String instanceId : ragServiceRegistry.getInstanceIds()) {
            RagService service = ragServiceRegistry.getServiceOrThrow(instanceId);
            long docCount = service.getDocumentCount();
            log.info("实例 [{}] 文档数: {}", instanceId, docCount);
        }
    }

    /**
     * 示例 6: 向量搜索
     */
    public void example6_VectorSearch() {
        log.info("=== 示例 6: 向量搜索 ===");

        // 1. 将查询文本向量化
        String query = "如何使用RAG系统";
        Vector queryVector = primaryRagService.embed(query);

        // 2. 使用向量进行搜索
        List<Document> results = primaryRagService.vectorSearch(queryVector, 10);
        log.info("向量搜索找到 {} 个文档", results.size());

        // 3. 查看结果
        for (Document doc : results) {
            log.info("文档: {}, 分数: {}", doc.getTitle(), doc.getScore());
        }
    }

    /**
     * 示例 7: 检查实例健康状态
     */
    public void example7_HealthCheck() {
        log.info("=== 示例 7: 健康检查 ===");

        for (String instanceId : ragServiceRegistry.getInstanceIds()) {
            RagService service = ragServiceRegistry.getServiceOrThrow(instanceId);
            boolean healthy = service.isHealthy();
            log.info("实例 [{}] 健康状态: {}", instanceId, healthy ? "✅" : "❌");
        }
    }
}

