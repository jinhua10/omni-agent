package top.yumbo.ai.omni.knowledge.registry.service.query;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.model.CrossDomainQueryConfig;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.router.DomainRouter;
import top.yumbo.ai.omni.knowledge.registry.service.cache.QueryResultCache;
import top.yumbo.ai.omni.knowledge.registry.service.preference.UserPreferenceLearner;
import top.yumbo.ai.omni.knowledge.registry.service.quality.DomainQualityScorer;
import top.yumbo.ai.omni.knowledge.registry.service.rag.RAGServiceFactory;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;

/**
 * 跨域查询服务（优化版）
 * (Cross-Domain Query Service - Optimized)
 *
 * <p>实现跨多个知识域的并发查询和智能结果合并</p>
 *
 * <p>核心优化：</p>
 * <ul>
 *     <li>并发查询 - 使用线程池并行查询多个域</li>
 *     <li>动态域权重 - 根据查询场景动态计算域权重</li>
 *     <li>智能重排 - 多维度综合排序算法</li>
 *     <li>多样性保证 - 避免结果过度集中</li>
 *     <li>超时控制 - 防止慢查询阻塞</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class CrossDomainQueryService {

    private final DomainRouter domainRouter;
    private final RAGServiceFactory ragServiceFactory;

    @Autowired(required = false)
    private KnowledgeRegistry knowledgeRegistry;

    private final DomainWeightStrategy weightStrategy;
    private final ResultReRanker resultReRanker;
    private final CrossDomainQueryConfig config;
    private final Executor executor;
    private final DomainQualityScorer qualityScorer;
    private final UserPreferenceLearner preferenceLearner;
    private final QueryResultCache resultCache;

    @Autowired
    public CrossDomainQueryService(
            DomainRouter domainRouter,
            RAGServiceFactory ragServiceFactory,
            DomainWeightStrategy weightStrategy,
            ResultReRanker resultReRanker,
            CrossDomainQueryConfig config,
            @Qualifier("crossDomainQueryExecutor") Executor executor,
            DomainQualityScorer qualityScorer,
            UserPreferenceLearner preferenceLearner,
            QueryResultCache resultCache) {
        this.domainRouter = domainRouter;
        this.ragServiceFactory = ragServiceFactory;
        // knowledgeRegistry 通过字段注入
        this.weightStrategy = weightStrategy;
        this.resultReRanker = resultReRanker;
        this.config = config;
        this.executor = executor;
        this.qualityScorer = qualityScorer;
        this.preferenceLearner = preferenceLearner;
        this.resultCache = resultCache;
    }

    /**
     * 初始化后检查依赖
     */
    @jakarta.annotation.PostConstruct
    public void init() {
        if (knowledgeRegistry == null) {
            log.warn("⚠️ KnowledgeRegistry not available - CrossDomainQueryService will use fallback mode");
        } else {
            log.info("✅ CrossDomainQueryService initialized with KnowledgeRegistry");
        }
    }

    /**
     * 跨域查询（并发优化版 + 缓存 + 质量评分）
     */
    public CrossDomainQueryResult crossDomainSearch(String query, int maxResults) {
        return crossDomainSearchWithUser(query, maxResults, null);
    }

    /**
     * 跨域查询（带用户ID，支持个性化）
     */
    public CrossDomainQueryResult crossDomainSearchWithUser(String query, int maxResults, String userId) {
        log.info("🔍 跨域查询: query='{}', maxResults={}, userId={}", query, maxResults, userId);

        long startTime = System.currentTimeMillis();

        // 1. 路由到相关的域
        var routeResult = domainRouter.route(query);
        List<String> domainIds = routeResult.getDomainIds();

        log.info("   路由到 {} 个域: {}", domainIds.size(), domainIds);

        if (domainIds.isEmpty()) {
            log.warn("   未找到匹配的域，返回空结果");
            return buildEmptyResult(query, startTime);
        }

        // 2. 尝试从缓存获取
        List<Document> cachedResults = resultCache.get(query, domainIds);
        if (cachedResults != null) {
            long queryTime = System.currentTimeMillis() - startTime;
            log.info("✅ 缓存命中，返回 {} 个结果，耗时 {}ms", cachedResults.size(), queryTime);
            return buildCachedResult(query, domainIds, cachedResults, maxResults, queryTime, routeResult.getConfidence());
        }

        // 3. 计算域权重（结合质量分数和用户偏好）
        Map<String, Double> domainWeights = calculateDomainWeightsWithQuality(domainIds, query, userId);

        // 4. 并发查询所有域
        Map<String, List<Document>> domainResults = queryAllDomainsWithMetrics(
                domainIds, query, maxResults, domainWeights, userId);

        // 5-7. 合并、重排、去重
        List<Document> mergedResults = mergeResults(domainResults);
        List<Document> rankedResults = resultReRanker.reRank(mergedResults, query, domainWeights);
        List<Document> dedupResults = deduplicateResults(rankedResults);
        List<Document> finalResults = dedupResults.stream().limit(maxResults).collect(Collectors.toList());

        // 8. 存入缓存
        resultCache.put(query, domainIds, finalResults);

        long queryTime = System.currentTimeMillis() - startTime;
        log.info("✅ 跨域查询完成: {} 个域, {} 个结果, {}ms", domainIds.size(), finalResults.size(), queryTime);

        // 9. 记录用户查询
        if (userId != null) {
            for (String domainId : domainIds) {
                int resultCount = domainResults.getOrDefault(domainId, Collections.emptyList()).size();
                preferenceLearner.recordQuery(userId, query, domainId, resultCount);
            }
        }

        return CrossDomainQueryResult.builder()
                .query(query)
                .totalDomains(domainIds.size())
                .queriedDomains(domainIds)
                .domainResults(domainResults)
                .domainWeights(domainWeights)
                .results(finalResults)
                .queryTime(queryTime)
                .routeConfidence(routeResult.getConfidence())
                .fromCache(false)
                .build();
    }

    /**
     * 计算域权重
     */
    private Map<String, Double> calculateDomainWeights(List<String> domainIds, String query) {
        Map<String, Double> weights = new HashMap<>();

        for (String domainId : domainIds) {
            try {
                var domain = knowledgeRegistry.findDomainById(domainId).orElse(null);
                if (domain != null) {
                    double weight = weightStrategy.calculateDomainWeight(
                            domainId,
                            domain.getDomainType(),
                            query,
                            null // 可传入查询上下文
                    );
                    weights.put(domainId, weight);
                } else {
                    weights.put(domainId, 1.0); // 默认权重
                }
            } catch (Exception e) {
                log.warn("   计算域 {} 权重失败: {}", domainId, e.getMessage());
                weights.put(domainId, 1.0);
            }
        }

        return weights;
    }

    /**
     * 计算域权重（结合质量分数和用户偏好）
     */
    private Map<String, Double> calculateDomainWeightsWithQuality(List<String> domainIds, String query, String userId) {
        Map<String, Double> weights = new HashMap<>();

        for (String domainId : domainIds) {
            try {
                var domain = knowledgeRegistry.findDomainById(domainId).orElse(null);
                if (domain != null) {
                    // 基础权重
                    double baseWeight = weightStrategy.calculateDomainWeight(
                            domainId, domain.getDomainType(), query, null);

                    // 质量分数
                    double qualityScore = qualityScorer.calculateQualityScore(domainId);

                    // 用户偏好权重
                    double preferenceWeight = 1.0;
                    if (userId != null) {
                        preferenceWeight = preferenceLearner.getDomainPreferenceWeight(userId, domainId);
                    }

                    // 综合权重
                    double finalWeight = baseWeight * qualityScore * preferenceWeight;
                    weights.put(domainId, finalWeight);

                    log.debug("   域 {} 综合权重: {:.2f} (基础:{:.2f}, 质量:{:.2f}, 偏好:{:.2f})",
                            domainId, finalWeight, baseWeight, qualityScore, preferenceWeight);
                } else {
                    weights.put(domainId, 1.0);
                }
            } catch (Exception e) {
                log.warn("   计算域 {} 权重失败: {}", domainId, e.getMessage());
                weights.put(domainId, 1.0);
            }
        }

        return weights;
    }

    /**
     * 并发查询所有域
     */
    private Map<String, List<Document>> queryAllDomainsConcurrently(
            List<String> domainIds,
            String query,
            int maxResults,
            Map<String, Double> domainWeights) {

        Map<String, List<Document>> results = new ConcurrentHashMap<>();
        List<CompletableFuture<Void>> futures = new ArrayList<>();

        // 为每个域创建异步查询任务
        for (String domainId : domainIds) {
            CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
                try {
                    log.debug("   [{}] 开始查询域: {}", Thread.currentThread().getName(), domainId);

                    RagService ragService = ragServiceFactory.getOrCreateRAGService(domainId);

                    // 根据域权重调整查询数量
                    double weight = domainWeights.getOrDefault(domainId, 1.0);
                    int adjustedLimit = (int) Math.ceil(maxResults * weight);
                    adjustedLimit = Math.min(adjustedLimit, maxResults * 2); // 最多查询2倍

                    List<Document> domainResults = ragService.semanticSearch(query, adjustedLimit);

                    // 标记文档来源域
                    domainResults.forEach(doc -> {
                        if (doc.getMetadata() == null) {
                            doc.setMetadata(new HashMap<>());
                        }
                        doc.getMetadata().put("sourceDomain", domainId);
                        doc.getMetadata().put("domainWeight", weight);
                    });

                    results.put(domainId, domainResults);

                    log.debug("   [{}] 域 {} 返回 {} 个结果",
                            Thread.currentThread().getName(), domainId, domainResults.size());

                } catch (Exception e) {
                    log.error("   域 {} 查询失败: {}", domainId, e.getMessage());
                    results.put(domainId, Collections.emptyList());
                }
            }, executor);

            futures.add(future);
        }

        // 等待所有查询完成，设置超时
        try {
            CompletableFuture<Void> allOf = CompletableFuture.allOf(
                    futures.toArray(new CompletableFuture[0]));

            allOf.get(config.getQueryTimeout(), TimeUnit.SECONDS);

        } catch (TimeoutException e) {
            log.warn("   部分域查询超时，使用已完成的结果");
            futures.forEach(f -> f.cancel(true));
        } catch (Exception e) {
            log.error("   等待查询完成时出错: {}", e.getMessage());
        }

        return results;
    }

    /**
     * 并发查询所有域（带性能指标记录）
     */
    private Map<String, List<Document>> queryAllDomainsWithMetrics(
            List<String> domainIds, String query, int maxResults,
            Map<String, Double> domainWeights, String userId) {

        Map<String, List<Document>> results = new ConcurrentHashMap<>();
        List<CompletableFuture<Void>> futures = new ArrayList<>();

        for (String domainId : domainIds) {
            CompletableFuture<Void> future = CompletableFuture.runAsync(() -> {
                long queryStart = System.currentTimeMillis();
                try {
                    RagService ragService = ragServiceFactory.getOrCreateRAGService(domainId);

                    double weight = domainWeights.getOrDefault(domainId, 1.0);
                    int adjustedLimit = (int) Math.ceil(maxResults * weight);
                    adjustedLimit = Math.min(adjustedLimit, maxResults * 2);

                    List<Document> domainResults = ragService.semanticSearch(query, adjustedLimit);

                    domainResults.forEach(doc -> {
                        if (doc.getMetadata() == null) {
                            doc.setMetadata(new HashMap<>());
                        }
                        doc.getMetadata().put("sourceDomain", domainId);
                        doc.getMetadata().put("domainWeight", weight);
                    });

                    results.put(domainId, domainResults);

                    // 记录性能指标
                    long responseTime = System.currentTimeMillis() - queryStart;
                    qualityScorer.recordQuery(domainId, domainResults.size(), responseTime);

                } catch (Exception e) {
                    log.error("   域 {} 查询失败: {}", domainId, e.getMessage());
                    results.put(domainId, Collections.emptyList());
                    qualityScorer.recordQuery(domainId, 0, System.currentTimeMillis() - queryStart);
                }
            }, executor);

            futures.add(future);
        }

        try {
            CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                    .get(config.getQueryTimeout(), TimeUnit.SECONDS);
        } catch (TimeoutException e) {
            log.warn("   部分域查询超时");
            futures.forEach(f -> f.cancel(true));
        } catch (Exception e) {
            log.error("   等待查询完成时出错: {}", e.getMessage());
        }

        return results;
    }

    /**
     * 合并多个域的结果
     */
    private List<Document> mergeResults(Map<String, List<Document>> domainResults) {
        List<Document> merged = new ArrayList<>();
        domainResults.values().forEach(merged::addAll);

        log.debug("   合并结果: {} 个文档", merged.size());

        return merged;
    }

    /**
     * 去重 - 基于文档ID或内容相似度
     */
    private List<Document> deduplicateResults(List<Document> documents) {
        // 基于文档ID去重
        Map<String, Document> uniqueDocs = new LinkedHashMap<>();

        for (Document doc : documents) {
            String key = doc.getId();
            if (!uniqueDocs.containsKey(key)) {
                uniqueDocs.put(key, doc);
            } else {
                // 如果ID相同，保留分数更高的
                Document existing = uniqueDocs.get(key);
                double existingScore = existing.getScore() != null ? existing.getScore() : 0.0;
                double newScore = doc.getScore() != null ? doc.getScore() : 0.0;

                if (newScore > existingScore) {
                    uniqueDocs.put(key, doc);
                }
            }
        }

        List<Document> deduped = new ArrayList<>(uniqueDocs.values());

        if (deduped.size() < documents.size()) {
            log.debug("   去重: {} -> {} 个文档", documents.size(), deduped.size());
        }

        return deduped;
    }

    /**
     * 构建空结果
     */
    private CrossDomainQueryResult buildEmptyResult(String query, long startTime) {
        return CrossDomainQueryResult.builder()
                .query(query)
                .totalDomains(0)
                .results(Collections.emptyList())
                .queryTime(System.currentTimeMillis() - startTime)
                .build();
    }

    /**
     * 构建缓存结果
     */
    private CrossDomainQueryResult buildCachedResult(
            String query, List<String> domainIds, List<Document> cachedResults,
            int maxResults, long queryTime, double confidence) {
        return CrossDomainQueryResult.builder()
                .query(query)
                .totalDomains(domainIds.size())
                .queriedDomains(domainIds)
                .results(cachedResults.stream().limit(maxResults).collect(Collectors.toList()))
                .queryTime(queryTime)
                .routeConfidence(confidence)
                .fromCache(true)
                .build();
    }

    /**
     * 跨域查询结果
     */
    @lombok.Data
    @lombok.Builder
    public static class CrossDomainQueryResult {
        /** 查询文本 */
        private String query;

        /** 查询的域总数 */
        private int totalDomains;

        /** 实际查询的域ID列表 */
        private List<String> queriedDomains;

        /** 每个域的查询结果 */
        private Map<String, List<Document>> domainResults;

        /** 每个域的权重 */
        private Map<String, Double> domainWeights;

        /** 合并后的最终结果 */
        private List<Document> results;

        /** 查询耗时（毫秒） */
        private long queryTime;

        /** 路由置信度 */
        private double routeConfidence;

        /** 是否来自缓存 */
        private boolean fromCache;

        /** 是否跨域查询 */
        public boolean isCrossDomain() {
            return totalDomains > 1;
        }
    }
}

