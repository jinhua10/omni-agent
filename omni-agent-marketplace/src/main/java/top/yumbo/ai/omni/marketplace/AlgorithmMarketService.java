package top.yumbo.ai.omni.marketplace;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.marketplace.security.SecureRemoteExecutor;
import top.yumbo.ai.omni.marketplace.security.SecureScriptExecutor;
import top.yumbo.ai.omni.storage.api.model.OptimizationData;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 算法市场服务
 *
 * 支持三种算法类型：
 * 1. Pipeline（配置化）- 最安全，推荐
 * 2. Script（脚本）- 需要沙箱隔离
 * 3. Remote（远程）- 需要网络鉴权
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class AlgorithmMarketService {

    @Autowired(required = false)
    private SecureScriptExecutor scriptExecutor;

    @Autowired(required = false)
    private SecureRemoteExecutor remoteExecutor;

    // 注册的算法组件
    private final Map<String, AlgorithmComponent> components = new ConcurrentHashMap<>();

    // 已发布的算法
    private final Map<String, MarketAlgorithm> publishedAlgorithms = new ConcurrentHashMap<>();

    public AlgorithmMarketService() {
        // 注册内置组件
        registerBuiltinComponents();
    }

    /**
     * 注册内置算法组件
     */
    private void registerBuiltinComponents() {
        // 示例：注册查询扩展组件
        registerComponent("query_expansion", new AlgorithmComponent() {
            @Override
            public Object execute(Object input, Map<String, Object> params) {
                log.info("执行查询扩展: input={}, params={}", input, params);

                // 查询扩展实现：生成查询变体
                String query = input.toString();
                List<String> expandedQueries = new ArrayList<>();
                expandedQueries.add(query); // 原始查询

                // 1. 同义词扩展
                String method = (String) params.getOrDefault("method", "synonym");
                if ("synonym".equals(method)) {
                    // 添加同义词变体（简化实现）
                    expandedQueries.add(query + " 相关");
                    expandedQueries.add(query.replace("配置", "设置"));
                    expandedQueries.add(query.replace("如何", "怎么"));
                }

                // 2. 添加领域相关词
                if (query.contains("Spring")) {
                    expandedQueries.add(query + " Boot");
                    expandedQueries.add(query + " Framework");
                }

                // 3. 限制扩展数量
                int maxExpansions = (int) params.getOrDefault("maxExpansions", 5);
                if (expandedQueries.size() > maxExpansions) {
                    expandedQueries = expandedQueries.subList(0, maxExpansions);
                }

                log.info("查询扩展完成: 原始={}, 扩展后={}", query, expandedQueries.size());

                // 返回扩展结果
                Map<String, Object> result = new HashMap<>();
                result.put("originalQuery", query);
                result.put("expandedQueries", expandedQueries);
                result.put("expansionCount", expandedQueries.size());

                return result;
            }

            @Override
            public Map<String, Double> getMetrics() {
                return Map.of("precisionGain", 12.5, "latency", 20.0, "recallGain", 15.0);
            }
        });

        // 注册其他组件
        registerComponent("semantic_chunking", createSemanticChunkingComponent());
        registerComponent("rerank", createRerankComponent());

        log.info("已注册 {} 个内置算法组件", components.size());
    }

    /**
     * 注册算法组件
     */
    public void registerComponent(String type, AlgorithmComponent component) {
        components.put(type, component);
        log.info("注册算法组件: {}", type);
    }

    /**
     * 获取已注册的算法组件
     *
     * @param type 组件类型
     * @return 算法组件，不存在则返回 null
     */
    public AlgorithmComponent getComponent(String type) {
        return components.get(type);
    }

    /**
     * 获取所有已注册的组件类型
     *
     * @return 组件类型列表
     */
    public Set<String> getRegisteredComponentTypes() {
        return new HashSet<>(components.keySet());
    }

    /**
     * 发布算法到市场（带安全审核）
     */
    public String publishAlgorithm(MarketAlgorithm algorithm) throws AlgorithmValidationException {
        // 1. 验证算法
        validateAlgorithm(algorithm);

        // 2. 生成ID
        String algorithmId = UUID.randomUUID().toString();
        algorithm.setAlgorithmId(algorithmId);
        algorithm.setCreatedAt(System.currentTimeMillis());
        algorithm.setUpdatedAt(System.currentTimeMillis());
        algorithm.setUsageCount(0L);
        algorithm.setApproved(false);  // 默认需要审核

        // 3. 保存算法
        publishedAlgorithms.put(algorithmId, algorithm);

        log.info("算法已发布（待审核）: id={}, name={}, type={}",
                algorithmId, algorithm.getName(), algorithm.getType());

        return algorithmId;
    }

    /**
     * 审核并批准算法
     */
    public void approveAlgorithm(String algorithmId) {
        MarketAlgorithm algorithm = publishedAlgorithms.get(algorithmId);
        if (algorithm != null) {
            algorithm.setApproved(true);
            log.info("算法已审核通过: id={}, name={}", algorithmId, algorithm.getName());
        }
    }

    /**
     * 执行市场算法（带安全检查）
     */
    public OptimizationData executeMarketAlgorithm(
            String algorithmId,
            String documentId,
            Map<String, Object> context) throws AlgorithmExecutionException {
        return executeMarketAlgorithm(algorithmId, (Object) documentId, context);
    }

    /**
     * 执行市场算法（带安全检查）- 接受任意输入类型
     */
    public OptimizationData executeMarketAlgorithm(
            String algorithmId,
            Object input,
            Map<String, Object> context) throws AlgorithmExecutionException {

        // 1. 获取算法
        MarketAlgorithm algorithm = publishedAlgorithms.get(algorithmId);
        if (algorithm == null) {
            throw new AlgorithmExecutionException("算法不存在: " + algorithmId);
        }

        // 2. 检查审核状态
        if (Boolean.FALSE.equals(algorithm.getApproved())) {
            throw new AlgorithmExecutionException("算法未审核通过: " + algorithmId);
        }

        // 3. 增加使用次数
        algorithm.setUsageCount(algorithm.getUsageCount() + 1);

        log.info("执行市场算法: id={}, name={}, type={}",
                algorithmId, algorithm.getName(), algorithm.getType());

        // 4. 根据类型执行
        return switch (algorithm.getType()) {
            case PIPELINE -> executePipelineWithInput(algorithm, input, context);
            case SCRIPT -> executeScriptWithInput(algorithm, input, context);
            case REMOTE -> executeRemoteWithInput(algorithm, input, context);
        };
    }

    /**
     * 执行配置化算法（Pipeline）- 最安全
     */
    private OptimizationData executePipeline(
            MarketAlgorithm algorithm,
            String documentId,
            Map<String, Object> context) throws AlgorithmExecutionException {
        return executePipelineWithInput(algorithm, documentId, context);
    }

    /**
     * 执行配置化算法（Pipeline）- 接受任意输入类型
     */
    private OptimizationData executePipelineWithInput(
            MarketAlgorithm algorithm,
            Object input,
            Map<String, Object> context) throws AlgorithmExecutionException {

        Object result = input;
        Map<String, Double> allMetrics = new HashMap<>();

        long startTime = System.currentTimeMillis();

        try {
            for (MarketAlgorithm.PipelineStep step : algorithm.getPipelineConfig().getSteps()) {
                // 获取组件
                AlgorithmComponent component = components.get(step.getType());
                if (component == null) {
                    log.warn("组件不存在: {}", step.getType());
                    continue;
                }

                // 合并 step 的 params 和 context
                Map<String, Object> mergedParams = new HashMap<>(step.getParams());
                mergedParams.putAll(context);

                // 执行组件
                result = component.execute(result, mergedParams);
                allMetrics.putAll(component.getMetrics());

                // 检查超时
                long elapsed = System.currentTimeMillis() - startTime;
                Integer maxTime = algorithm.getPipelineConfig().getMaxExecutionTimeMs();
                if (maxTime != null && elapsed > maxTime) {
                    throw new AlgorithmExecutionException("Pipeline execution timeout");
                }
            }

            // 提取 documentId（如果 input 是 String）
            String documentId = input instanceof String ? (String) input : "unknown";

            return OptimizationData.builder()
                    .documentId(documentId)
                    .optimizationType(algorithm.getName())
                    .algorithmVersion(algorithm.getVersion())
                    .data(Map.of("result", result))
                    .metadata(Map.of(
                            "algorithmId", algorithm.getAlgorithmId(),
                            "type", "pipeline"
                    ))
                    .metrics(allMetrics)
                    .processedAt(System.currentTimeMillis())
                    .build();

        } catch (Exception e) {
            log.error("Pipeline execution failed", e);
            throw new AlgorithmExecutionException("Pipeline execution failed: " + e.getMessage());
        }
    }

    /**
     * 执行脚本算法（Script）- 需要沙箱隔离
     */
    private OptimizationData executeScript(
            MarketAlgorithm algorithm,
            String documentId,
            Map<String, Object> context) throws AlgorithmExecutionException {

        if (scriptExecutor == null) {
            throw new AlgorithmExecutionException("Script executor not available");
        }

        try {
            // 安全执行脚本（沙箱隔离 + 超时控制）
            Map<String, Object> scriptResult = scriptExecutor.executeSecurely(
                    algorithm.getScript(),
                    context,
                    5000  // 5秒超时
            );

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) scriptResult.get("data");
            @SuppressWarnings("unchecked")
            Map<String, Double> metrics = (Map<String, Double>) scriptResult.get("metrics");

            return OptimizationData.builder()
                    .documentId(documentId)
                    .optimizationType(algorithm.getName())
                    .algorithmVersion(algorithm.getVersion())
                    .data(data)
                    .metrics(metrics)
                    .metadata(Map.of(
                            "algorithmId", algorithm.getAlgorithmId(),
                            "type", "script",
                            "language", algorithm.getScriptLanguage()
                    ))
                    .processedAt(System.currentTimeMillis())
                    .build();

        } catch (SecureScriptExecutor.ScriptExecutionException e) {
            log.error("Script execution failed: algorithmId={}", algorithm.getAlgorithmId(), e);
            throw new AlgorithmExecutionException("Script execution failed: " + e.getMessage());
        }
    }

    /**
     * 执行脚本算法（Script）- 接受任意输入类型
     */
    private OptimizationData executeScriptWithInput(
            MarketAlgorithm algorithm,
            Object input,
            Map<String, Object> context) throws AlgorithmExecutionException {

        // 将 input 添加到 context 中
        Map<String, Object> enhancedContext = new HashMap<>(context);
        enhancedContext.put("input", input);

        String documentId = input instanceof String ? (String) input : "unknown";
        return executeScript(algorithm, documentId, enhancedContext);
    }

    /**
     * 执行远程算法（Remote）- 需要网络鉴权
     */
    private OptimizationData executeRemote(
            MarketAlgorithm algorithm,
            String documentId,
            Map<String, Object> context) throws AlgorithmExecutionException {

        if (remoteExecutor == null) {
            throw new AlgorithmExecutionException("Remote executor not available");
        }

        try {
            // 安全调用远程服务
            Map<String, Object> remoteResult = remoteExecutor.executeRemote(
                    algorithm.getRemoteEndpoint(),
                    algorithm.getRemoteAuthToken(),
                    documentId,
                    context
            );

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) remoteResult.get("data");
            @SuppressWarnings("unchecked")
            Map<String, Double> metrics = (Map<String, Double>) remoteResult.get("metrics");

            return OptimizationData.builder()
                    .documentId(documentId)
                    .optimizationType(algorithm.getName())
                    .algorithmVersion(algorithm.getVersion())
                    .data(data)
                    .metrics(metrics)
                    .metadata(Map.of(
                            "algorithmId", algorithm.getAlgorithmId(),
                            "type", "remote",
                            "endpoint", algorithm.getRemoteEndpoint()
                    ))
                    .processedAt(System.currentTimeMillis())
                    .build();

        } catch (SecureRemoteExecutor.RemoteExecutionException e) {
            log.error("Remote execution failed: algorithmId={}", algorithm.getAlgorithmId(), e);
            throw new AlgorithmExecutionException("Remote execution failed: " + e.getMessage());
        }
    }

    /**
     * 执行远程算法（Remote）- 接受任意输入类型
     */
    private OptimizationData executeRemoteWithInput(
            MarketAlgorithm algorithm,
            Object input,
            Map<String, Object> context) throws AlgorithmExecutionException {

        // 将 input 添加到 context 中
        Map<String, Object> enhancedContext = new HashMap<>(context);
        enhancedContext.put("input", input);

        String documentId = input instanceof String ? (String) input : "unknown";
        return executeRemote(algorithm, documentId, enhancedContext);
    }

    /**
     * 获取所有已发布的算法
     */
    public List<MarketAlgorithm> listPublishedAlgorithms(boolean includeUnapproved) {
        return publishedAlgorithms.values().stream()
                .filter(alg -> includeUnapproved || Boolean.TRUE.equals(alg.getApproved()))
                .toList();
    }

    /**
     * 搜索算法
     */
    public List<MarketAlgorithm> searchAlgorithms(String keyword) {
        return publishedAlgorithms.values().stream()
                .filter(alg -> Boolean.TRUE.equals(alg.getApproved()))
                .filter(alg -> alg.getName().contains(keyword) ||
                              (alg.getDescription() != null && alg.getDescription().contains(keyword)) ||
                              (alg.getTags() != null && alg.getTags().contains(keyword)))
                .toList();
    }

    /**
     * 验证算法（安全检查）
     */
    private void validateAlgorithm(MarketAlgorithm algorithm) throws AlgorithmValidationException {
        if (algorithm.getName() == null || algorithm.getName().trim().isEmpty()) {
            throw new AlgorithmValidationException("算法名称不能为空");
        }

        switch (algorithm.getType()) {
            case PIPELINE -> validatePipeline(algorithm);
            case SCRIPT -> validateScript(algorithm);
            case REMOTE -> validateRemote(algorithm);
        }
    }

    private void validatePipeline(MarketAlgorithm algorithm) throws AlgorithmValidationException {
        if (algorithm.getPipelineConfig() == null ||
            algorithm.getPipelineConfig().getSteps() == null ||
            algorithm.getPipelineConfig().getSteps().isEmpty()) {
            throw new AlgorithmValidationException("Pipeline 配置不能为空");
        }

        // 验证所有步骤的组件都存在
        for (MarketAlgorithm.PipelineStep step : algorithm.getPipelineConfig().getSteps()) {
            if (!components.containsKey(step.getType())) {
                throw new AlgorithmValidationException("组件不存在: " + step.getType());
            }
        }
    }

    private void validateScript(MarketAlgorithm algorithm) throws AlgorithmValidationException {
        if (algorithm.getScript() == null || algorithm.getScript().trim().isEmpty()) {
            throw new AlgorithmValidationException("脚本内容不能为空");
        }

        if (algorithm.getScript().length() > 50000) {
            throw new AlgorithmValidationException("脚本过大（最多50KB）");
        }
    }

    private void validateRemote(MarketAlgorithm algorithm) throws AlgorithmValidationException {
        if (algorithm.getRemoteEndpoint() == null || algorithm.getRemoteEndpoint().trim().isEmpty()) {
            throw new AlgorithmValidationException("远程端点不能为空");
        }

        try {
            new java.net.URL(algorithm.getRemoteEndpoint());
        } catch (java.net.MalformedURLException e) {
            throw new AlgorithmValidationException("无效的URL: " + algorithm.getRemoteEndpoint());
        }
    }

    // ========== 辅助方法 ==========

    private AlgorithmComponent createSemanticChunkingComponent() {
        return new AlgorithmComponent() {
            @Override
            public Object execute(Object input, Map<String, Object> params) {
                log.info("执行语义分块: params={}", params);

                String content = input.toString();
                int chunkSize = (int) params.getOrDefault("chunkSize", 512);
                int overlap = (int) params.getOrDefault("overlap", 50);
                double similarityThreshold = (double) params.getOrDefault("similarityThreshold", 0.5);

                // 1. 按段落分割
                String[] paragraphs = content.split("\\n\\s*\\n");
                List<String> chunks = new ArrayList<>();

                // 2. 基于语义相似度合并段落成块
                StringBuilder currentChunk = new StringBuilder();
                int currentLength = 0;

                for (int i = 0; i < paragraphs.length; i++) {
                    String para = paragraphs[i].trim();
                    if (para.isEmpty()) continue;

                    // 计算是否需要开始新块
                    boolean shouldStartNewChunk = false;

                    if (currentLength == 0) {
                        // 第一段，直接添加
                        currentChunk.append(para);
                        currentLength = para.length();
                    } else if (currentLength + para.length() > chunkSize) {
                        // 超过大小限制，开始新块
                        shouldStartNewChunk = true;
                    } else {
                        // 简化的语义相似度判断：检查关键词重叠
                        double similarity = calculateSimpleSimilarity(
                            currentChunk.toString(), para);

                        if (similarity < similarityThreshold && currentLength > chunkSize / 2) {
                            // 语义不连贯且已有足够内容，开始新块
                            shouldStartNewChunk = true;
                        } else {
                            // 继续添加到当前块
                            currentChunk.append("\n\n").append(para);
                            currentLength += para.length();
                        }
                    }

                    if (shouldStartNewChunk) {
                        // 保存当前块
                        chunks.add(currentChunk.toString());

                        // 开始新块（包含overlap）
                        currentChunk = new StringBuilder();
                        if (overlap > 0 && chunks.size() > 0) {
                            String lastChunk = chunks.get(chunks.size() - 1);
                            int startPos = Math.max(0, lastChunk.length() - overlap);
                            currentChunk.append(lastChunk.substring(startPos)).append("\n\n");
                        }
                        currentChunk.append(para);
                        currentLength = para.length();
                    }
                }

                // 添加最后一块
                if (currentChunk.length() > 0) {
                    chunks.add(currentChunk.toString());
                }

                log.info("语义分块完成: 原始长度={}, 分块数={}, 平均块大小={}",
                    content.length(), chunks.size(),
                    chunks.isEmpty() ? 0 : content.length() / chunks.size());

                // 返回分块结果
                Map<String, Object> result = new HashMap<>();
                result.put("chunks", chunks);
                result.put("chunkCount", chunks.size());
                result.put("avgChunkSize", chunks.isEmpty() ? 0 :
                    chunks.stream().mapToInt(String::length).average().orElse(0));

                return result;
            }

            /**
             * 简化的相似度计算：基于词汇重叠率
             */
            private double calculateSimpleSimilarity(String text1, String text2) {
                String[] words1 = text1.toLowerCase().split("[\\s，。！？；、]+");
                String[] words2 = text2.toLowerCase().split("[\\s，。！？；、]+");

                java.util.Set<String> set1 = new java.util.HashSet<>(java.util.Arrays.asList(words1));
                java.util.Set<String> set2 = new java.util.HashSet<>(java.util.Arrays.asList(words2));

                // 计算交集
                java.util.Set<String> intersection = new java.util.HashSet<>(set1);
                intersection.retainAll(set2);

                // 计算并集
                java.util.Set<String> union = new java.util.HashSet<>(set1);
                union.addAll(set2);

                return union.isEmpty() ? 0.0 : (double) intersection.size() / union.size();
            }

            @Override
            public Map<String, Double> getMetrics() {
                return Map.of("precisionGain", 17.5, "latency", 30.0, "semanticCoherence", 0.85);
            }
        };
    }

    private AlgorithmComponent createRerankComponent() {
        return new AlgorithmComponent() {
            @Override
            public Object execute(Object input, Map<String, Object> params) {
                log.info("执行重排序: params={}", params);

                // input应该是搜索结果列表
                @SuppressWarnings("unchecked")
                List<Map<String, Object>> searchResults = (List<Map<String, Object>>) input;

                String query = (String) params.getOrDefault("query", "");
                String model = (String) params.getOrDefault("model", "simple");
                int topK = (int) params.getOrDefault("topK", 10);

                if (searchResults == null || searchResults.isEmpty()) {
                    log.warn("搜索结果为空，无需重排序");
                    return input;
                }

                // 1. 计算每个结果与查询的语义相关度
                List<ScoredResult> scoredResults = new ArrayList<>();
                for (int i = 0; i < searchResults.size(); i++) {
                    Map<String, Object> result = searchResults.get(i);
                    String content = result.getOrDefault("content", "").toString();

                    // 计算语义相关度分数
                    double semanticScore = calculateSemanticRelevance(query, content, model);

                    // 结合原始分数（如果有）
                    double originalScore = result.containsKey("score") ?
                        ((Number) result.get("score")).doubleValue() : 1.0;

                    // 加权组合：70%语义分数 + 30%原始分数
                    double finalScore = 0.7 * semanticScore + 0.3 * originalScore;

                    scoredResults.add(new ScoredResult(i, result, finalScore));
                }

                // 2. 按新分数排序
                scoredResults.sort((a, b) -> Double.compare(b.score, a.score));

                // 3. 取TopK
                List<Map<String, Object>> rerankedResults = scoredResults.stream()
                    .limit(topK)
                    .map(sr -> {
                        Map<String, Object> r = new HashMap<>(sr.result);
                        r.put("rerankScore", sr.score);
                        r.put("originalIndex", sr.originalIndex);
                        return r;
                    })
                    .collect(Collectors.toList());

                log.info("重排序完成: 原始数量={}, TopK={}, 模型={}",
                    searchResults.size(), rerankedResults.size(), model);

                // 返回重排序结果
                Map<String, Object> result = new HashMap<>();
                result.put("rerankedResults", rerankedResults);
                result.put("originalCount", searchResults.size());
                result.put("rerankModel", model);
                result.put("topK", topK);

                // 返回排序索引映射
                List<Integer> rerankedIndices = scoredResults.stream()
                    .limit(topK)
                    .map(sr -> sr.originalIndex)
                    .collect(Collectors.toList());
                result.put("rerankedIndices", rerankedIndices);

                return result;
            }

            /**
             * 计算语义相关度
             * 简化实现：基于关键词匹配和位置权重
             */
            private double calculateSemanticRelevance(String query, String content, String model) {
                if (query == null || query.isEmpty() || content == null || content.isEmpty()) {
                    return 0.0;
                }

                // 简化的语义相关度计算
                String[] queryWords = query.toLowerCase().split("[\\s，。！？；、]+");
                String contentLower = content.toLowerCase();

                double score = 0.0;
                int matchCount = 0;

                for (String word : queryWords) {
                    if (word.length() < 2) continue; // 忽略过短的词

                    if (contentLower.contains(word)) {
                        matchCount++;

                        // 计算词出现的位置权重（越靠前权重越高）
                        int firstIndex = contentLower.indexOf(word);
                        double positionWeight = 1.0 - (double) firstIndex / content.length();

                        // 计算词出现的次数权重
                        int occurrences = countOccurrences(contentLower, word);
                        double frequencyWeight = Math.min(occurrences * 0.2, 1.0);

                        score += (1.0 + positionWeight + frequencyWeight);
                    }
                }

                // 归一化分数
                if (queryWords.length > 0) {
                    score = score / (queryWords.length * 3.0); // 最大可能分数是3倍词数
                }

                // 添加覆盖率奖励
                double coverage = (double) matchCount / Math.max(queryWords.length, 1);
                score = score * 0.7 + coverage * 0.3;

                return Math.min(score, 1.0);
            }

            /**
             * 计算子串出现次数
             */
            private int countOccurrences(String text, String substring) {
                int count = 0;
                int index = 0;
                while ((index = text.indexOf(substring, index)) != -1) {
                    count++;
                    index += substring.length();
                }
                return count;
            }

            @Override
            public Map<String, Double> getMetrics() {
                return Map.of("precisionGain", 10.0, "latency", 80.0, "ndcg", 0.92);
            }
        };
    }

    /**
     * 内部类：带分数的搜索结果
     */
    private static class ScoredResult {
        int originalIndex;
        Map<String, Object> result;
        double score;

        ScoredResult(int originalIndex, Map<String, Object> result, double score) {
            this.originalIndex = originalIndex;
            this.result = result;
            this.score = score;
        }
    }

    // ========== 异常类 ==========

    public static class AlgorithmValidationException extends Exception {
        public AlgorithmValidationException(String message) {
            super(message);
        }
    }

    public static class AlgorithmExecutionException extends Exception {
        public AlgorithmExecutionException(String message) {
            super(message);
        }
    }
}

