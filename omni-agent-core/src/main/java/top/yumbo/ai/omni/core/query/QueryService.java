package top.yumbo.ai.omni.core.query;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Query;
import top.yumbo.ai.rag.api.model.SearchResult;

import java.util.List;
import java.util.Map;

/**
 * 查询服务 (Query Service)
 *
 * 基于 RAGService 的查询处理服务
 * (Query processing service based on RAGService)
 *
 * 核心功能 (Core Features):
 * - 文本搜索 (Text search)
 * - 向量搜索 (Vector search)
 * - 混合检索 (Hybrid retrieval)
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@Slf4j
@Service
public class QueryService {

    private final RAGService ragService;

    /**
     * 查询统计 (Query statistics)
     */
    private long totalQueries = 0;

    @Autowired
    public QueryService(RAGService ragService) {
        this.ragService = ragService;
        log.info("QueryService initialized with RAGService");
    }

    /**
     * 执行文本搜索 (Execute text search)
     *
     * @param queryText 查询文本 (Query text)
     * @param limit 结果数量限制 (Result limit)
     * @return 搜索结果列表 (Search result list)
     */
    public List<SearchResult> search(String queryText, int limit) {
        long startTime = System.currentTimeMillis();
        totalQueries++;

        // 执行搜索 (Execute search)
        List<SearchResult> results = ragService.searchByText(queryText, limit);

        long duration = System.currentTimeMillis() - startTime;
        log.info("Search completed for query '{}': {} results in {}ms",
                queryText, results.size(), duration);

        return results;
    }

    /**
     * 执行向量搜索 (Execute vector search)
     *
     * @param embedding 查询向量 (Query embedding)
     * @param limit 结果数量限制 (Result limit)
     * @return 搜索结果列表 (Search result list)
     */
    public List<SearchResult> vectorSearch(float[] embedding, int limit) {
        long startTime = System.currentTimeMillis();

        List<SearchResult> results = ragService.vectorSearch(embedding, limit);

        long duration = System.currentTimeMillis() - startTime;
        log.info("Vector search completed: {} results in {}ms",
                results.size(), duration);

        return results;
    }

    /**
     * 执行混合检索 (Execute hybrid retrieval)
     *
     * @param queryText 查询文本 (Query text)
     * @param embedding 查询向量 (Query embedding)
     * @param limit 结果数量限制 (Result limit)
     * @return 搜索结果列表 (Search result list)
     */
    public List<SearchResult> hybridSearch(String queryText, float[] embedding, int limit) {
        long startTime = System.currentTimeMillis();

        Query query = Query.builder()
                .text(queryText)
                .embedding(embedding)
                .topK(limit)
                .build();

        List<SearchResult> results = ragService.hybridSearch(query);

        long duration = System.currentTimeMillis() - startTime;
        log.info("Hybrid search completed for query '{}': {} results in {}ms",
                queryText, results.size(), duration);

        return results;
    }

    /**
     * 获取查询统计 (Get query statistics)
     *
     * @return 统计信息 Map (Statistics map)
     */
    public Map<String, Long> getStatistics() {
        return Map.of("totalQueries", totalQueries);
    }

    /**
     * 重置统计 (Reset statistics)
     */
    public void resetStatistics() {
        totalQueries = 0;
        log.info("Query statistics reset");
    }
}

