package top.yumbo.ai.omni.marketplace;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.SearchResult;

import java.util.*;
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
            // 1. 查询扩展（如果启用）
            List<String> queries = new ArrayList<>();
            queries.add(question); // 原始查询

            if (useExpansion && algorithmMarketService != null) {
                List<String> expandedQueries = performQueryExpansion(question);
                queries.addAll(expandedQueries);
                log.info("📈 查询扩展: {} -> {} 个查询", question, queries.size());
            }

            // 2. 多查询检索
            List<SearchResult> allResults = new ArrayList<>();
            for (String query : queries) {
                List<SearchResult> results = ragService.searchByText(query, topK);
                allResults.addAll(results);
            }

            // 3. 去重和融合（基于文档ID）
            List<SearchResult> fusedResults = fuseResults(allResults);
            log.info("🔗 结果融合: {} -> {} 个结果", allResults.size(), fusedResults.size());

            // 4. 重排序（如果启用）
            if (useRerank && algorithmMarketService != null) {
                fusedResults = performRerank(question, fusedResults);
                log.info("🎯 重排序完成: {} 个结果", fusedResults.size());
            }

            // 5. 截取 topK
            if (fusedResults.size() > topK) {
                fusedResults = fusedResults.subList(0, topK);
            }

            log.info("✅ 增强查询完成: 返回 {} 个结果", fusedResults.size());
            return fusedResults;

        } catch (Exception e) {
            log.error("❌ 增强查询失败，降级到普通检索: {}", e.getMessage());
            // 降级：使用普通 RAG 检索
            return ragService.searchByText(question, topK);
        }
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
        try {
            // 使用算法市场的查询扩展组件
            Map<String, Object> params = new HashMap<>();
            params.put("method", "synonym");
            params.put("maxExpansions", 5);

            // 直接调用组件（不需要发布算法）
            var component = algorithmMarketService.getComponent("query_expansion");
            if (component == null) {
                log.warn("⚠️ 查询扩展组件未找到，跳过扩展");
                return Collections.emptyList();
            }

            Object result = component.execute(question, params);
            if (result instanceof Map) {
                Map<String, Object> resultMap = (Map<String, Object>) result;
                List<String> expandedQueries = (List<String>) resultMap.get("expandedQueries");

                // 移除原始查询，只返回扩展的查询
                if (expandedQueries != null && expandedQueries.contains(question)) {
                    expandedQueries = new ArrayList<>(expandedQueries);
                    expandedQueries.remove(question);
                }

                return expandedQueries != null ? expandedQueries : Collections.emptyList();
            }

            return Collections.emptyList();

        } catch (Exception e) {
            log.error("❌ 查询扩展失败: {}", e.getMessage());
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

            // 准备重排序输入
            List<String> documents = results.stream()
                    .map(r -> r.getDocument().getContent())
                    .collect(Collectors.toList());

            Map<String, Object> input = new HashMap<>();
            input.put("query", question);
            input.put("documents", documents);

            Map<String, Object> params = new HashMap<>();
            params.put("topK", results.size());

            // 调用重排序组件
            var component = algorithmMarketService.getComponent("rerank");
            if (component == null) {
                log.warn("⚠️ 重排序组件未找到，跳过重排序");
                return results;
            }

            Object result = component.execute(input, params);
            if (result instanceof Map) {
                Map<String, Object> resultMap = (Map<String, Object>) result;
                List<Integer> rankedIndices = (List<Integer>) resultMap.get("rankedIndices");

                if (rankedIndices != null && !rankedIndices.isEmpty()) {
                    // 根据排序后的索引重新排列结果
                    List<SearchResult> rerankedResults = new ArrayList<>();
                    for (Integer index : rankedIndices) {
                        if (index >= 0 && index < results.size()) {
                            rerankedResults.add(results.get(index));
                        }
                    }
                    return rerankedResults;
                }
            }

            return results;

        } catch (Exception e) {
            log.error("❌ 重排序失败: {}", e.getMessage());
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
        List<SearchResult> fusedResults = docScores.entrySet().stream()
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .map(entry -> {
                    SearchResult result = docMap.get(entry.getKey());
                    // 更新分数为 RRF 分数
                    result.setScore(entry.getValue().floatValue());
                    return result;
                })
                .collect(Collectors.toList());

        return fusedResults;
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

        if (algorithmMarketService != null) {
            stats.put("queryExpansionAvailable", algorithmMarketService.getComponent("query_expansion") != null);
            stats.put("rerankAvailable", algorithmMarketService.getComponent("rerank") != null);
            stats.put("semanticChunkingAvailable", algorithmMarketService.getComponent("semantic_chunking") != null);
        }

        return stats;
    }
}


