package top.yumbo.ai.omni.core.hope;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.rag.RagService;

import java.util.*;

/**
 * HOPE 知识管理器
 * (Hierarchical Omni-Agent Persistent Engine - Knowledge Manager)
 *
 * <p>
 * HOPE 系统的核心协调器，管理三层知识结构：
 * - 持久层 (Permanent Layer): 长期稳定的核心知识
 * - 普通层 (Ordinary Layer): 一般性知识
 * - 高频层 (High Frequency Layer): 频繁访问的知识
 * </p>
 *
 * <p>
 * 基于知识网络架构重构，使用 Knowledge Registry 和 RAG 服务
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class HOPEKnowledgeManager {

    private final QuestionClassifier questionClassifier;
    private final RagService ragService;

    /**
     * 层级访问计数器
     */
    private final Map<String, LayerStats> layerStatsMap = new HashMap<>();

    @Autowired
    public HOPEKnowledgeManager(
            QuestionClassifier questionClassifier,
            RagService ragService) {
        this.questionClassifier = questionClassifier;
        this.ragService = ragService;

        // 初始化层级统计
        layerStatsMap.put("permanent", new LayerStats("permanent"));
        layerStatsMap.put("ordinary", new LayerStats("ordinary"));
        layerStatsMap.put("high_frequency", new LayerStats("high_frequency"));

        log.info("✅ HOPEKnowledgeManager initialized");
    }

    /**
     * 查询知识
     *
     * @param question 用户问题
     * @param maxResults 最大结果数
     * @return 查询结果
     */
    public QueryResult query(String question, int maxResults) {
        if (question == null || question.trim().isEmpty()) {
            return QueryResult.empty();
        }

        long startTime = System.currentTimeMillis();

        // 1. 分类问题
        String questionType = questionClassifier.classify(question);
        String suggestedLayer = questionClassifier.getSuggestedLayer(questionType);

        log.debug("🎯 Question classified as: {} (suggested layer: {})", questionType, suggestedLayer);

        // 2. 使用 RAG 进行语义搜索
        List<top.yumbo.ai.omni.rag.model.Document> documents =
                ragService.semanticSearch(question, maxResults);

        // 3. 更新统计信息
        LayerStats stats = layerStatsMap.get(suggestedLayer);
        if (stats != null) {
            stats.incrementQueryCount();
            stats.addQueryTime(System.currentTimeMillis() - startTime);
        }

        // 4. 构建结果
        QueryResult result = new QueryResult();
        result.setQuestion(question);
        result.setQuestionType(questionType);
        result.setSuggestedLayer(suggestedLayer);
        result.setDocuments(documents);
        result.setQueryTimeMs(System.currentTimeMillis() - startTime);
        result.setSuccess(!documents.isEmpty());

        log.debug("✅ Query completed in {}ms, found {} documents",
                result.getQueryTimeMs(), documents.size());

        return result;
    }

    /**
     * 获取层级统计信息
     */
    public Map<String, LayerStats> getLayerStats() {
        return new HashMap<>(layerStatsMap);
    }

    /**
     * 重置统计信息
     */
    public void resetStats() {
        layerStatsMap.values().forEach(LayerStats::reset);
        log.info("🔄 Layer statistics reset");
    }

    /**
     * 查询结果
     */
    @Data
    public static class QueryResult {
        private String question;
        private String questionType;
        private String suggestedLayer;
        private List<top.yumbo.ai.omni.rag.model.Document> documents;
        private long queryTimeMs;
        private boolean success;

        public static QueryResult empty() {
            QueryResult result = new QueryResult();
            result.setSuccess(false);
            result.setDocuments(new ArrayList<>());
            return result;
        }
    }

    /**
     * 层级统计信息
     */
    @Data
    public static class LayerStats {
        private final String layerName;
        private long queryCount = 0;
        private long totalQueryTimeMs = 0;
        private long lastQueryTime = 0;

        public LayerStats(String layerName) {
            this.layerName = layerName;
        }

        public void incrementQueryCount() {
            this.queryCount++;
            this.lastQueryTime = System.currentTimeMillis();
        }

        public void addQueryTime(long timeMs) {
            this.totalQueryTimeMs += timeMs;
        }

        public double getAverageQueryTimeMs() {
            return queryCount > 0 ? (double) totalQueryTimeMs / queryCount : 0;
        }

        public void reset() {
            this.queryCount = 0;
            this.totalQueryTimeMs = 0;
            this.lastQueryTime = 0;
        }
    }
}

