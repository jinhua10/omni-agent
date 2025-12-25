package top.yumbo.ai.omni.example;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.ai.api.EmbeddingService;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.Query;
import top.yumbo.ai.rag.api.model.SearchResult;

import java.util.List;
import java.util.Map;

/**
 * RAG 检索示例
 * (RAG Retrieval Examples)
 * 
 * 演示 RAG 系统的各种检索方式：
 * 1. 全文检索（Text Search） - 基于 BM25/Lucene
 * 2. 向量检索（Vector Search） - 基于余弦相似度
 * 3. 语义检索（Semantic Search） - 自动向量化
 * 4. 混合检索（Hybrid Search） - 文本 + 向量
 *
 * @author OmniAgent Team
 * @since 2025-12-25
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class RAGExample {

    private final RAGService ragService;

    @Autowired(required = false)
    private EmbeddingService embeddingService;

    /**
     * 示例 1：索引文档
     */
    public void indexDocuments() {
        log.info("=== 示例 1：索引文档 ===");

        Document doc1 = Document.builder()
                .id("doc-001")
                .title("ONNX Runtime 入门指南")
                .content("ONNX Runtime 是一个高性能的推理引擎")
                .tags(List.of("ONNX", "机器学习"))
                .createdAt(System.currentTimeMillis())
                .build();

        ragService.indexDocument(doc1);
        log.info("✅ 已索引文档");
    }

    /**
     * 示例 2：文本检索
     */
    public void textSearchExample() {
        log.info("\n=== 示例 2：文本检索 ===");
        List<SearchResult> results = ragService.searchByText("ONNX Runtime", 5);
        results.forEach(r -> log.info("  - {}", r.getDocument().getTitle()));
    }

    /**
     * 示例 3：向量检索
     */
    public void vectorSearchExample() {
        log.info("\n=== 示例 3：向量检索 ===");
        if (embeddingService == null) {
            log.warn("⚠️ 未配置 EmbeddingService");
            return;
        }
        float[] embedding = embeddingService.embed("ONNX Runtime");
        List<SearchResult> results = ragService.vectorSearch(embedding, 5);
        results.forEach(r -> log.info("  - {}", r.getDocument().getTitle()));
    }

    /**
     * 运行所有示例
     */
    public void runAllExamples() {
        try {
            log.info("========================================");
            log.info("   RAG 检索示例集");
            log.info("========================================");

            indexDocuments();
            Thread.sleep(1000);

            textSearchExample();
            vectorSearchExample();

            log.info("\n✅ 所有示例运行完成！");
        } catch (Exception e) {
            log.error("示例运行失败", e);
        }
    }
}
