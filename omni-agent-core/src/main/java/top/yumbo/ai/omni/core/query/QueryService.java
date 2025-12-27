package top.yumbo.ai.omni.core.query;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.SearchResult;

import java.util.List;
import java.util.Map;

/**
 * 查询服务 (Query Service)
 *
 * 基于 RagService 的查询处理服务
 * (Query processing service based on RagService)
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

    private final RagService ragService;

    /**
     * 查询统计 (Query statistics)
     */
    private long totalQueries = 0;

    @Autowired
    public QueryService(RagService ragService) {
        this.ragService = ragService;
        log.info("QueryService initialized with RagService");
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

        // ⭐ Debug 日志：查询开始
        log.debug("🔎 [Query] Text search - query: '{}', limit: {}", queryText, limit);

        // 执行搜索 (Execute search)
        var documents = ragService.semanticSearch(queryText, limit);
        List<SearchResult> results = documents.stream()
                .map(SearchResult::fromDocument)
                .toList();

        long duration = System.currentTimeMillis() - startTime;
        log.info("Search completed for query '{}': {} results in {}ms",
                queryText, results.size(), duration);

        // ⭐ Debug 日志：查询结果
        log.debug("🔎 [Query] Text search results ({} found):", results.size());
        for (int i = 0; i < Math.min(results.size(), 5); i++) {
            SearchResult r = results.get(i);
            String content = r.getContent() != null ? r.getContent() : "";
            log.debug("🔎 [Query] Result #{}: score={}, docId={}, content: {}",
                i + 1, r.getScore(), r.getDocumentId(),
                content.substring(0, Math.min(100, content.length())) + "...");
        }
        if (results.size() > 5) {
            log.debug("🔎 [Query] ... and {} more results", results.size() - 5);
        }

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

        // ⭐ Debug 日志：向量搜索开始
        log.debug("🔎 [Query] Vector search - embedding dim: {}, limit: {}", embedding.length, limit);

        var vector = top.yumbo.ai.omni.rag.model.Vector.of(embedding);
        var documents = ragService.vectorSearch(vector, limit);
        List<SearchResult> results = documents.stream()
                .map(SearchResult::fromDocument)
                .toList();

        long duration = System.currentTimeMillis() - startTime;
        log.info("Vector search completed: {} results in {}ms",
                results.size(), duration);

        // ⭐ Debug 日志：向量搜索结果
        log.debug("🔎 [Query] Vector search results ({} found):", results.size());
        for (int i = 0; i < Math.min(results.size(), 5); i++) {
            SearchResult r = results.get(i);
            String content = r.getContent() != null ? r.getContent() : "";
            log.debug("🔎 [Query] Result #{}: score={}, docId={}, content: {}",
                i + 1, r.getScore(), r.getDocumentId(),
                content.substring(0, Math.min(100, content.length())) + "...");
        }
        if (results.size() > 5) {
            log.debug("🔎 [Query] ... and {} more results", results.size() - 5);
        }

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

        // ⭐ Debug 日志：混合搜索开始
        log.debug("🔎 [Query] Hybrid search - query: '{}', embedding dim: {}, limit: {}",
            queryText, embedding.length, limit);

        // TODO: 实现真正的混合检索（文本+向量）
        // 当前使用语义搜索作为降级方案
        var documents = ragService.semanticSearch(queryText, limit);
        List<SearchResult> results = documents.stream()
                .map(SearchResult::fromDocument)
                .toList();

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


