package top.yumbo.ai.omni.knowledge.registry.service.query;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.rag.model.Document;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 结果重排算法
 * (Result Re-ranking Algorithm)
 *
 * <p>基于多维度因素对跨域查询结果进行重新排序，确保最相关的内容排在前面</p>
 *
 * <p>排序因素：</p>
 * <ul>
 *     <li>相关性分数 - RAG 检索的原始分数</li>
 *     <li>域权重 - 域在当前查询场景下的权重</li>
 *     <li>内容质量 - 文档的完整性和结构性</li>
 *     <li>新鲜度 - 文档的时效性（如果有时间戳）</li>
 *     <li>多样性 - 避免结果过于集中在单一域</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class ResultReRanker {

    /**
     * 重排序结果
     *
     * @param documents 待排序的文档列表
     * @param query 查询文本
     * @param domainWeights 域权重映射
     * @return 重排序后的文档列表
     */
    public List<Document> reRank(
            List<Document> documents,
            String query,
            Map<String, Double> domainWeights) {

        if (documents == null || documents.isEmpty()) {
            return Collections.emptyList();
        }

        log.debug("🔄 开始重排序，文档数: {}", documents.size());

        // 1. 计算综合分数
        List<ScoredDocument> scoredDocs = documents.stream()
                .map(doc -> calculateComprehensiveScore(doc, query, domainWeights))
                .collect(Collectors.toList());

        // 2. 按综合分数排序
        scoredDocs.sort((a, b) -> Double.compare(b.getComprehensiveScore(), a.getComprehensiveScore()));

        // 3. 应用多样性调整（避免结果过于集中）
        List<ScoredDocument> diversified = applyDiversityBoost(scoredDocs);

        // 4. 提取最终文档列表
        List<Document> result = diversified.stream()
                .map(ScoredDocument::getDocument)
                .collect(Collectors.toList());

        log.debug("   重排序完成，Top 5 分数: {}",
                diversified.stream()
                        .limit(5)
                        .map(sd -> String.format("%.3f", sd.getComprehensiveScore()))
                        .collect(Collectors.joining(", ")));

        return result;
    }

    /**
     * 计算综合分数
     */
    private ScoredDocument calculateComprehensiveScore(
            Document doc,
            String query,
            Map<String, Double> domainWeights) {

        // 1. 获取原始相关性分数（归一化到 0-1）
        double relevanceScore = normalizeScore(doc.getScore());

        // 2. 获取域权重
        String sourceDomain = getSourceDomain(doc);
        double domainWeight = domainWeights.getOrDefault(sourceDomain, 1.0);

        // 3. 计算内容质量分数
        double qualityScore = calculateQualityScore(doc);

        // 4. 计算新鲜度分数
        double freshnessScore = calculateFreshnessScore(doc);

        // 5. 综合计算（加权平均）
        double comprehensiveScore =
                relevanceScore * 0.50 +      // 相关性权重 50%
                domainWeight * 0.25 +        // 域权重 25%
                qualityScore * 0.15 +        // 质量权重 15%
                freshnessScore * 0.10;       // 新鲜度权重 10%

        return ScoredDocument.builder()
                .document(doc)
                .relevanceScore(relevanceScore)
                .domainWeight(domainWeight)
                .qualityScore(qualityScore)
                .freshnessScore(freshnessScore)
                .comprehensiveScore(comprehensiveScore)
                .sourceDomain(sourceDomain)
                .build();
    }

    /**
     * 归一化分数到 0-1 范围
     */
    private double normalizeScore(Double score) {
        if (score == null) {
            return 0.5; // 默认中等分数
        }
        // 假设原始分数范围是 0-100，归一化到 0-1
        return Math.max(0.0, Math.min(1.0, score / 100.0));
    }

    /**
     * 计算内容质量分数
     * 基于文档的完整性、结构等因素
     */
    private double calculateQualityScore(Document doc) {
        double score = 0.5; // 基础分

        String content = doc.getContent();
        if (content != null && !content.isEmpty()) {
            // 内容长度合理性（避免过短或过长）
            int length = content.length();
            if (length >= 100 && length <= 2000) {
                score += 0.2; // 长度合适
            } else if (length < 50) {
                score -= 0.1; // 过短扣分
            }

            // 内容结构性（是否包含标点、换行等）
            if (content.contains("。") || content.contains(".")) {
                score += 0.1; // 有句子结构
            }

            // 是否包含代码块
            if (content.contains("```") || content.contains("public class")) {
                score += 0.1; // 包含代码示例
            }
        }

        // 是否有标题
        if (doc.getTitle() != null && !doc.getTitle().isEmpty()) {
            score += 0.1;
        }

        return Math.max(0.0, Math.min(1.0, score));
    }

    /**
     * 计算新鲜度分数
     * 基于文档的创建/更新时间
     */
    private double calculateFreshnessScore(Document doc) {
        // TODO: 如果文档有时间戳，计算新鲜度
        // 目前返回中等分数
        return 0.5;
    }

    /**
     * 应用多样性提升
     * 确保结果不会过度集中在单一域
     */
    private List<ScoredDocument> applyDiversityBoost(List<ScoredDocument> scoredDocs) {
        if (scoredDocs.size() <= 5) {
            return scoredDocs; // 结果太少，不需要调整
        }

        // 统计每个域已出现的次数
        Map<String, Integer> domainCounts = new HashMap<>();
        List<ScoredDocument> result = new ArrayList<>();

        for (ScoredDocument doc : scoredDocs) {
            String domain = doc.getSourceDomain();
            int count = domainCounts.getOrDefault(domain, 0);

            // 如果某个域已经出现过多次，略微降低后续文档的分数
            if (count >= 3) {
                double penalty = 0.95 - (count - 3) * 0.02; // 每多一次降低2%
                doc.setComprehensiveScore(doc.getComprehensiveScore() * penalty);
            }

            domainCounts.put(domain, count + 1);
            result.add(doc);
        }

        // 重新排序
        result.sort((a, b) -> Double.compare(b.getComprehensiveScore(), a.getComprehensiveScore()));

        return result;
    }

    /**
     * 获取文档的来源域
     */
    private String getSourceDomain(Document doc) {
        if (doc.getMetadata() != null) {
            Object domain = doc.getMetadata().get("sourceDomain");
            if (domain != null) {
                return domain.toString();
            }
        }
        return "unknown";
    }

    /**
     * 带分数的文档
     */
    @lombok.Data
    @lombok.Builder
    private static class ScoredDocument {
        private Document document;
        private double relevanceScore;      // 相关性分数
        private double domainWeight;        // 域权重
        private double qualityScore;        // 质量分数
        private double freshnessScore;      // 新鲜度分数
        private double comprehensiveScore;  // 综合分数
        private String sourceDomain;        // 来源域
    }
}

