package top.yumbo.ai.omni.marketplace;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.ai.api.AIService;
import top.yumbo.ai.omni.core.query.cache.QueryExpansionCacheService;
import top.yumbo.ai.omni.marketplace.config.QueryExpansionConfig;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.SearchResult;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

/**
 * 增强查询服务 - 整合算法市场优化
 * (Enhanced Query Service - Integrated with Algorithm Market)
 *
 * <p>
 * 核心功能 (Core Features):
 * - 查询扩展（Query Expansion）: 生成多个查询变体，提高召回率
 * - 语义分块（Semantic Chunking）: 智能文档分块
 * - 结果重排序（Rerank）: 优化检索结果顺序
 * - 多查询融合（Multi-Query Fusion）: 融合多个查询的结果
 * - LLM查询扩展: 使用LLM生成高质量查询变体
 * - 缓存优化: 缓存扩展结果和查询结果
 * - 并行执行: 并行执行多个查询提升性能
 * </p>
 *
 * <p>
 * 使用场景 (Use Cases):
 * - 双轨系统右轨的智能检索
 * - HOPE 知识系统的增强查询
 * - 角色知识库的精准检索
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class EnhancedQueryService {

    @Autowired
    private RAGService ragService;

    @Autowired(required = false)
    private AlgorithmMarketService algorithmMarketService;

    @Autowired(required = false)
    private AIService aiService;

    @Autowired(required = false)
    private QueryExpansionCacheService cacheService;

    @Autowired
    private QueryExpansionConfig config;

    private final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * 并行执行线程池
     */
    private ExecutorService executorService;

    /**
     * 初始化方法
     */
    @jakarta.annotation.PostConstruct
    public void init() {
        // 初始化线程池
        if (config.getParallel().isEnabled()) {
            int threadPoolSize = config.getParallel().getThreadPoolSize();
            this.executorService = Executors.newFixedThreadPool(
                    threadPoolSize,
                    r -> {
                        Thread thread = new Thread(r);
                        thread.setName("query-expansion-" + thread.threadId());
                        thread.setDaemon(true);
                        return thread;
                    }
            );
            log.info("✅ 查询扩展线程池初始化完成: poolSize={}", threadPoolSize);
        }
    }

    /**
     * 销毁方法
     */
    @jakarta.annotation.PreDestroy
    public void destroy() {
        if (executorService != null) {
            executorService.shutdown();
            try {
                if (!executorService.awaitTermination(5, TimeUnit.SECONDS)) {
                    executorService.shutdownNow();
                }
            } catch (InterruptedException e) {
                executorService.shutdownNow();
                Thread.currentThread().interrupt();
            }
            log.info("🔚 查询扩展线程池已关闭");
        }
    }

    /**
     * 增强查询 - 使用算法市场优化
     *
     * @param question 用户问题
     * @param topK 返回结果数量
     * @param useExpansion 是否使用查询扩展
     * @param useRerank 是否使用重排序
     * @return 优化后的搜索结果
     */
    public List<SearchResult> enhancedSearch(String question, int topK, boolean useExpansion, boolean useRerank) {
        log.info("🔍 增强查询: question={}, topK={}, expansion={}, rerank={}",
                question, topK, useExpansion, useRerank);

        try {
            // 生成缓存键
            String cacheKey = String.format("enhanced:%s:topK:%d:exp:%b:rerank:%b",
                    question, topK, useExpansion, useRerank);

            // 1. 尝试从缓存获取完整结果
            if (cacheService != null) {
                List<SearchResult> cached = cacheService.getResult(cacheKey);
                if (cached != null) {
                    log.info("🎯 增强查询缓存命中: 返回 {} 个结果", cached.size());
                    return cached;
                }
            }

            // 2. 查询扩展（如果启用）
            List<String> queries = new ArrayList<>();
            queries.add(question); // 原始查询

            if (useExpansion && config.isEnabled()) {
                List<String> expandedQueries = performQueryExpansion(question);
                queries.addAll(expandedQueries);
                log.info("📈 查询扩展: {} -> {} 个查询", question, queries.size());
            }

            // 3. 多查询检索（并行或串行）
            List<SearchResult> allResults;
            if (config.getParallel().isEnabled() && queries.size() > 1 && executorService != null) {
                allResults = parallelSearch(queries, topK);
            } else {
                allResults = serialSearch(queries, topK);
            }

            // 4. 去重和融合（基于文档ID）
            List<SearchResult> fusedResults = fuseResults(allResults);
            log.info("🔗 结果融合: {} -> {} 个结果", allResults.size(), fusedResults.size());

            // 5. 重排序（如果启用）
            if (useRerank && algorithmMarketService != null) {
                fusedResults = performRerank(question, fusedResults);
                log.info("🎯 重排序完成: {} 个结果", fusedResults.size());
            }

            // 6. 截取 topK
            if (fusedResults.size() > topK) {
                fusedResults = fusedResults.subList(0, topK);
            }

            // 7. 缓存结果
            if (cacheService != null && !fusedResults.isEmpty()) {
                cacheService.putResult(cacheKey, fusedResults);
            }

            log.info("✅ 增强查询完成: 返回 {} 个结果", fusedResults.size());
            return fusedResults;

        } catch (Exception e) {
            log.error("❌ 增强查询失败，降级到普通检索: {}", e.getMessage(), e);
            // 降级：使用普通 RAG 检索
            return ragService.searchByText(question, topK);
        }
    }

    /**
     * 并行执行多个查询
     *
     * @param queries 查询列表
     * @param topK 每个查询返回的结果数
     * @return 所有查询的结果
     */
    private List<SearchResult> parallelSearch(List<String> queries, int topK) {
        log.info("🚀 并行执行 {} 个查询", queries.size());
        long startTime = System.currentTimeMillis();

        List<CompletableFuture<List<SearchResult>>> futures = queries.stream()
                .map(query -> CompletableFuture.supplyAsync(
                        () -> {
                            try {
                                return ragService.searchByText(query, topK);
                            } catch (Exception e) {
                                log.error("查询失败: query={}, error={}", query, e.getMessage());
                                return Collections.<SearchResult>emptyList();
                            }
                        },
                        executorService
                ))
                .collect(Collectors.toList());

        // 等待所有查询完成（带超时）
        try {
            CompletableFuture<Void> allFutures = CompletableFuture.allOf(
                    futures.toArray(new CompletableFuture[0])
            );

            // 设置超时
            allFutures.get(config.getParallel().getTimeoutMs(), TimeUnit.MILLISECONDS);

        } catch (TimeoutException e) {
            log.warn("⚠️ 并行查询超时，使用已完成的结果");
        } catch (Exception e) {
            log.error("❌ 并行查询异常: {}", e.getMessage());
        }

        // 收集所有完成的结果
        List<SearchResult> allResults = futures.stream()
                .filter(CompletableFuture::isDone)
                .flatMap(future -> {
                    try {
                        return future.get().stream();
                    } catch (Exception e) {
                        return java.util.stream.Stream.empty();
                    }
                })
                .collect(Collectors.toList());

        long duration = System.currentTimeMillis() - startTime;
        log.info("✅ 并行查询完成: {} 个查询, 耗时 {}ms", queries.size(), duration);

        return allResults;
    }

    /**
     * 串行执行多个查询
     *
     * @param queries 查询列表
     * @param topK 每个查询返回的结果数
     * @return 所有查询的结果
     */
    private List<SearchResult> serialSearch(List<String> queries, int topK) {
        log.info("📝 串行执行 {} 个查询", queries.size());
        long startTime = System.currentTimeMillis();

        List<SearchResult> allResults = new ArrayList<>();
        for (String query : queries) {
            try {
                List<SearchResult> results = ragService.searchByText(query, topK);
                allResults.addAll(results);
            } catch (Exception e) {
                log.error("查询失败: query={}, error={}", query, e.getMessage());
            }
        }

        long duration = System.currentTimeMillis() - startTime;
        log.info("✅ 串行查询完成: {} 个查询, 耗时 {}ms", queries.size(), duration);

        return allResults;
    }

    /**
     * 简化版增强查询 - 仅查询扩展
     *
     * @param question 用户问题
     * @param topK 返回结果数量
     * @return 搜索结果
     */
    public List<SearchResult> enhancedSearchWithExpansion(String question, int topK) {
        return enhancedSearch(question, topK, true, false);
    }

    /**
     * 完整增强查询 - 查询扩展 + 重排序
     *
     * @param question 用户问题
     * @param topK 返回结果数量
     * @return 搜索结果
     */
    public List<SearchResult> fullyEnhancedSearch(String question, int topK) {
        return enhancedSearch(question, topK, true, true);
    }

    /**
     * 执行查询扩展
     *
     * @param question 原始问题
     * @return 扩展后的查询列表（不包含原始查询）
     */
    @SuppressWarnings("unchecked")
    private List<String> performQueryExpansion(String question) {
        if (!config.isEnabled()) {
            log.debug("⚠️ 查询扩展未启用");
            return Collections.emptyList();
        }

        try {
            // 1. 尝试从缓存获取
            if (cacheService != null) {
                List<String> cached = cacheService.getExpansion(question);
                if (cached != null) {
                    log.info("🎯 查询扩展缓存命中: query={}, count={}", question, cached.size());
                    return cached;
                }
            }

            List<String> allExpansions = new ArrayList<>();

            // 2. 使用 LLM 查询扩展（优先级最高）
            if (config.isLlmEnabled() && aiService != null) {
                List<String> llmExpansions = performLLMQueryExpansion(question);
                allExpansions.addAll(llmExpansions);
                log.info("🤖 LLM查询扩展: {} -> {} 个查询", question, llmExpansions.size());
            }

            // 3. 使用算法市场的查询扩展组件（作为补充）
            if (algorithmMarketService != null) {
                Map<String, Object> params = new HashMap<>();
                params.put("method", "synonym");
                params.put("maxExpansions", config.getMaxExpansions());

                var component = algorithmMarketService.getComponent("query_expansion");
                if (component != null) {
                    Object result = component.execute(question, params);
                    if (result instanceof Map) {
                        Map<String, Object> resultMap = (Map<String, Object>) result;
                        List<String> marketExpansions = (List<String>) resultMap.get("expandedQueries");

                        if (marketExpansions != null) {
                            // 移除原始查询和已有的扩展
                            final List<String> finalAllExpansions = allExpansions; // 创建final副本供lambda使用
                            marketExpansions = marketExpansions.stream()
                                    .filter(q -> !q.equals(question) && !finalAllExpansions.contains(q))
                                    .toList();
                            allExpansions.addAll(marketExpansions);
                            log.info("📈 算法市场查询扩展: 新增 {} 个查询", marketExpansions.size());
                        }
                    }
                }
            }

            // 4. 限制扩展数量
            if (allExpansions.size() > config.getMaxExpansions()) {
                allExpansions = allExpansions.subList(0, config.getMaxExpansions());
            }

            // 5. 缓存结果
            if (cacheService != null && !allExpansions.isEmpty()) {
                cacheService.putExpansion(question, allExpansions);
            }

            return allExpansions;

        } catch (Exception e) {
            log.error("❌ 查询扩展失败: {}", e.getMessage(), e);
            return Collections.emptyList();
        }
    }

    /**
     * 使用 LLM 执行查询扩展
     *
     * @param question 原始问题
     * @return 扩展后的查询列表
     */
    private List<String> performLLMQueryExpansion(String question) {
        try {
            String prompt = String.format("""
                你是一个查询扩展专家。请为以下用户问题生成3-5个语义相似但表达不同的查询变体。
                
                原始问题: %s
                
                要求:
                1. 保持原始问题的核心意图
                2. 使用不同的词汇和表达方式
                3. 覆盖可能的同义词和领域相关词
                4. 每个查询变体都应该是完整的问题
                
                输出格式（JSON）:
                {
                  "expandedQueries": ["查询1", "查询2", "查询3"]
                }
                
                只输出JSON，不要有其他内容。
                """, question);

            // 调用 LLM
            String response = aiService.chat(prompt);

            // 解析 JSON 响应
            Map<String, Object> resultMap = objectMapper.readValue(response, Map.class);
            @SuppressWarnings("unchecked")
            List<String> expandedQueries = (List<String>) resultMap.get("expandedQueries");

            if (expandedQueries != null && !expandedQueries.isEmpty()) {
                // 移除原始查询
                expandedQueries = expandedQueries.stream()
                        .filter(q -> !q.equals(question))
                        .toList();

                log.info("🤖 LLM生成了 {} 个查询变体", expandedQueries.size());
                return expandedQueries;
            }

            return Collections.emptyList();

        } catch (Exception e) {
            log.error("❌ LLM查询扩展失败: {}", e.getMessage());
            return Collections.emptyList();
        }
    }

    /**
     * 执行结果重排序
     *
     * @param question 原始问题
     * @param results 原始结果列表
     * @return 重排序后的结果列表
     */
    @SuppressWarnings("unchecked")
    private List<SearchResult> performRerank(String question, List<SearchResult> results) {
        try {
            if (results.isEmpty()) {
                return results;
            }

            // ⭐ 准备重排序输入：将 SearchResult 转换为 Map 列表
            List<Map<String, Object>> searchResults = results.stream()
                    .map(r -> {
                        Map<String, Object> map = new HashMap<>();
                        map.put("content", r.getDocument().getContent());
                        map.put("score", r.getScore());
                        map.put("documentId", r.getDocument().getId());
                        if (r.getDocument().getTitle() != null) {
                            map.put("title", r.getDocument().getTitle());
                        }
                        return map;
                    })
                    .collect(Collectors.toList());

            // ⭐ 参数中传递查询文本
            Map<String, Object> params = new HashMap<>();
            params.put("query", question);
            params.put("topK", results.size());

            // ⭐ Debug 日志
            log.debug("🔄 [Rerank] Input: {} results, query: '{}'", searchResults.size(), question);

            // 调用重排序组件
            var component = algorithmMarketService.getComponent("rerank");
            if (component == null) {
                log.warn("⚠️ 重排序组件未找到，跳过重排序");
                return results;
            }

            // ⭐ 直接传入 searchResults 列表作为 input
            Object result = component.execute(searchResults, params);

            // ⭐ Debug 日志
            log.debug("🔄 [Rerank] Result type: {}", result != null ? result.getClass().getSimpleName() : "null");

            if (result instanceof Map) {
                Map<String, Object> resultMap = (Map<String, Object>) result;

                // 尝试获取 rerankedIndices（新返回格式）
                List<Integer> rankedIndices = (List<Integer>) resultMap.get("rerankedIndices");

                if (rankedIndices != null && !rankedIndices.isEmpty()) {
                    // 根据排序后的索引重新排列结果
                    List<SearchResult> rerankedResults = new ArrayList<>();
                    for (Integer index : rankedIndices) {
                        if (index >= 0 && index < results.size()) {
                            rerankedResults.add(results.get(index));
                        }
                    }
                    log.debug("🔄 [Rerank] Reordered {} results using rerankedIndices", rerankedResults.size());
                    return rerankedResults;
                }
            }

            return results;

        } catch (Exception e) {
            log.error("❌ 重排序失败: {}", e.getMessage(), e);
            return results;
        }
    }

    /**
     * 融合多个查询的结果
     *
     * <p>算法：Reciprocal Rank Fusion (RRF)</p>
     * <p>公式：score(d) = Σ 1 / (k + rank(d))</p>
     * <p>其中 k=60 是常数，rank(d) 是文档在某个结果列表中的排名</p>
     *
     * @param allResults 所有查询的结果
     * @return 融合后的结果列表（按分数降序）
     */
    private List<SearchResult> fuseResults(List<SearchResult> allResults) {
        if (allResults.isEmpty()) {
            return Collections.emptyList();
        }

        // 统计每个文档的 RRF 分数
        Map<String, Double> docScores = new HashMap<>();
        Map<String, SearchResult> docMap = new HashMap<>();

        // RRF 常数
        final int k = 60;

        for (int i = 0; i < allResults.size(); i++) {
            SearchResult result = allResults.get(i);
            String docId = result.getDocument().getId();

            // 计算 RRF 分数：1 / (k + rank)
            double rrfScore = 1.0 / (k + i + 1);

            docScores.merge(docId, rrfScore, Double::sum);
            docMap.putIfAbsent(docId, result);
        }

        // 按 RRF 分数降序排序
        return docScores.entrySet().stream()
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .map(entry -> {
                    SearchResult result = docMap.get(entry.getKey());
                    // 更新分数为 RRF 分数
                    result.setScore(entry.getValue().floatValue());
                    return result;
                })
                .collect(Collectors.toList());
    }

    /**
     * 检查算法市场是否可用
     *
     * @return true 如果算法市场服务可用
     */
    public boolean isAlgorithmMarketAvailable() {
        return algorithmMarketService != null;
    }

    /**
     * 获取增强查询的统计信息
     *
     * @return 统计信息 Map
     */
    public Map<String, Object> getStatistics() {
        Map<String, Object> stats = new HashMap<>();
        stats.put("algorithmMarketAvailable", isAlgorithmMarketAvailable());
        stats.put("aiServiceAvailable", aiService != null);
        stats.put("cacheServiceAvailable", cacheService != null);
        stats.put("configEnabled", config.isEnabled());
        stats.put("llmEnabled", config.isLlmEnabled());
        stats.put("parallelEnabled", config.getParallel().isEnabled());

        if (algorithmMarketService != null) {
            stats.put("queryExpansionAvailable", algorithmMarketService.getComponent("query_expansion") != null);
            stats.put("rerankAvailable", algorithmMarketService.getComponent("rerank") != null);
            stats.put("semanticChunkingAvailable", algorithmMarketService.getComponent("semantic_chunking") != null);
        }

        // 添加缓存统计
        if (cacheService != null) {
            try {
                var cacheStats = cacheService.getStatistics();
                stats.put("cacheStatistics", Map.of(
                        "queryCacheSize", cacheStats.getQueryCacheSize(),
                        "queryCacheHits", cacheStats.getQueryCacheHits(),
                        "queryCacheMisses", cacheStats.getQueryCacheMisses(),
                        "queryCacheHitRate", String.format("%.2f%%", cacheStats.getQueryCacheHitRate() * 100),
                        "expansionCacheSize", cacheStats.getExpansionCacheSize(),
                        "expansionCacheHits", cacheStats.getExpansionCacheHits(),
                        "expansionCacheMisses", cacheStats.getExpansionCacheMisses(),
                        "expansionCacheHitRate", String.format("%.2f%%", cacheStats.getExpansionCacheHitRate() * 100),
                        "overallHitRate", String.format("%.2f%%", cacheStats.getOverallHitRate() * 100)
                ));
            } catch (Exception e) {
                log.error("获取缓存统计失败", e);
            }
        }

        return stats;
    }

    /**
     * 清除所有缓存
     */
    public void clearCache() {
        if (cacheService != null) {
            cacheService.clearAll();
            log.info("🧹 已清除所有查询扩展缓存");
        }
    }
}


