package top.yumbo.ai.omni.knowledge.registry.network;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.knowledge.registry.model.RefinedKnowledge;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 默认知识关联服务实现
 *
 * <p>提供基础的知识关联功能</p>
 * <p>基于关键词提取和语义搜索实现相关知识推荐</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class DefaultKnowledgeAssociationService implements KnowledgeAssociationService {

    private final KnowledgeStorageService storageService;

    public DefaultKnowledgeAssociationService(KnowledgeStorageService storageService) {
        this.storageService = storageService;
        log.info("✅ DefaultKnowledgeAssociationService 已初始化（基于 KnowledgeStorageService）");
    }

    @Override
    public List<RefinedKnowledge> findRelatedKnowledge(String knowledgeId, String domainId, int maxResults) {
        log.debug("查找相关知识: knowledgeId={}, domainId={}, maxResults={}", knowledgeId, domainId, maxResults);

        try {
            // 1. 获取当前知识
            RefinedKnowledge currentKnowledge = storageService.getKnowledge(knowledgeId, domainId);
            if (currentKnowledge == null) {
                log.warn("⚠️ 知识不存在: knowledgeId={}, domainId={}", knowledgeId, domainId);
                return new ArrayList<>();
            }

            // 2. 提取关键词（从标题和内容）
            List<String> keywords = extractKeywords(currentKnowledge);
            if (keywords.isEmpty()) {
                log.debug("未提取到关键词，返回空结果");
                return new ArrayList<>();
            }

            log.debug("提取到 {} 个关键词: {}", keywords.size(), keywords);

            // 3. 使用关键词搜索相关知识
            Set<String> relatedKnowledgeIds = new HashSet<>();
            Map<String, RefinedKnowledge> knowledgeMap = new LinkedHashMap<>();
            Map<String, Double> scoreMap = new HashMap<>();

            for (String keyword : keywords) {
                List<RefinedKnowledge> searchResults = storageService.searchKnowledge(keyword, domainId, maxResults * 2);

                for (RefinedKnowledge knowledge : searchResults) {
                    // 跳过自己
                    if (knowledge.getKnowledgeId().equals(knowledgeId)) {
                        continue;
                    }

                    // 计算相似度分数
                    String kId = knowledge.getKnowledgeId();
                    if (!relatedKnowledgeIds.contains(kId)) {
                        relatedKnowledgeIds.add(kId);
                        knowledgeMap.put(kId, knowledge);
                        scoreMap.put(kId, calculateSimilarity(currentKnowledge, knowledge));
                    }
                }
            }

            // 4. 按相似度排序并限制结果数量
            List<RefinedKnowledge> relatedList = knowledgeMap.values().stream()
                    .sorted((k1, k2) -> {
                        double score1 = scoreMap.getOrDefault(k1.getKnowledgeId(), 0.0);
                        double score2 = scoreMap.getOrDefault(k2.getKnowledgeId(), 0.0);
                        return Double.compare(score2, score1); // 降序
                    })
                    .limit(maxResults)
                    .collect(Collectors.toList());

            log.debug("✅ 找到 {} 条相关知识", relatedList.size());
            return relatedList;

        } catch (Exception e) {
            log.error("❌ 查找相关知识失败: knowledgeId={}, domainId={}", knowledgeId, domainId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public List<RefinedKnowledge> findCrossDomainRelatedKnowledge(
            String knowledgeId,
            String sourceDomainId,
            List<String> targetDomainIds,
            int maxResults) {
        log.debug("跨域查找相关知识: knowledgeId={}, sourceDomainId={}, targetDomainIds={}, maxResults={}",
                knowledgeId, sourceDomainId, targetDomainIds, maxResults);

        try {
            // 1. 获取源知识
            RefinedKnowledge sourceKnowledge = storageService.getKnowledge(knowledgeId, sourceDomainId);
            if (sourceKnowledge == null) {
                return new ArrayList<>();
            }

            // 2. 提取关键词
            List<String> keywords = extractKeywords(sourceKnowledge);
            if (keywords.isEmpty()) {
                return new ArrayList<>();
            }

            // 3. 在所有目标域中搜索
            Map<String, RefinedKnowledge> knowledgeMap = new LinkedHashMap<>();
            Map<String, Double> scoreMap = new HashMap<>();

            for (String targetDomainId : targetDomainIds) {
                for (String keyword : keywords) {
                    List<RefinedKnowledge> searchResults = storageService.searchKnowledge(
                            keyword, targetDomainId, maxResults);

                    for (RefinedKnowledge knowledge : searchResults) {
                        String kId = knowledge.getKnowledgeId();
                        if (!knowledgeMap.containsKey(kId)) {
                            knowledgeMap.put(kId, knowledge);
                            scoreMap.put(kId, calculateSimilarity(sourceKnowledge, knowledge));
                        }
                    }
                }
            }

            // 4. 排序并限制结果
            return knowledgeMap.values().stream()
                    .sorted((k1, k2) -> {
                        double score1 = scoreMap.getOrDefault(k1.getKnowledgeId(), 0.0);
                        double score2 = scoreMap.getOrDefault(k2.getKnowledgeId(), 0.0);
                        return Double.compare(score2, score1);
                    })
                    .limit(maxResults)
                    .collect(Collectors.toList());

        } catch (Exception e) {
            log.error("❌ 跨域查找相关知识失败", e);
            return new ArrayList<>();
        }
    }

    @Override
    public boolean createAssociation(
            String sourceKnowledgeId,
            String targetKnowledgeId,
            String relationType,
            double strength) {
        log.debug("创建知识关联: source={}, target={}, type={}, strength={}",
                sourceKnowledgeId, targetKnowledgeId, relationType, strength);
        // TODO: 实现知识关联创建逻辑
        return true;
    }

    @Override
    public boolean removeAssociation(String sourceKnowledgeId, String targetKnowledgeId) {
        log.debug("删除知识关联: source={}, target={}", sourceKnowledgeId, targetKnowledgeId);
        // TODO: 实现知识关联删除逻辑
        return true;
    }

    @Override
    public List<DomainAssociation> findRelatedDomains(String domainId, int topK) {
        log.debug("查找相关域: domainId={}, topK={}", domainId, topK);
        // TODO: 实现相关域查找逻辑
        return new ArrayList<>();
    }

    @Override
    public List<DomainRecommendation> recommendDomains(String query, int topK) {
        log.debug("推荐知识域: query={}, topK={}", query, topK);
        // TODO: 实现域推荐逻辑
        return new ArrayList<>();
    }

    /**
     * 从知识中提取关键词
     */
    private List<String> extractKeywords(RefinedKnowledge knowledge) {
        List<String> keywords = new ArrayList<>();

        // 1. 从标题提取（标题通常包含核心概念）
        if (knowledge.getTitle() != null && !knowledge.getTitle().isEmpty()) {
            String[] titleWords = knowledge.getTitle().split("[\\s,，、]+");
            for (String word : titleWords) {
                String cleaned = word.trim();
                if (cleaned.length() >= 2) { // 忽略单字符
                    keywords.add(cleaned);
                }
            }
        }

        // 2. 从内容提取（简化版：提取前 100 个字符的关键词）
        if (knowledge.getRefinedContent() != null && !knowledge.getRefinedContent().isEmpty()) {
            String content = knowledge.getRefinedContent();
            String preview = content.length() > 100 ? content.substring(0, 100) : content;
            String[] contentWords = preview.split("[\\s,，、。.!！?？]+");

            for (String word : contentWords) {
                String cleaned = word.trim();
                if (cleaned.length() >= 2 && !keywords.contains(cleaned)) {
                    keywords.add(cleaned);
                }
            }
        }

        // 3. 限制关键词数量（取前 5 个最重要的）
        return keywords.stream()
                .limit(5)
                .collect(Collectors.toList());
    }

    /**
     * 计算两个知识之间的相似度
     *
     * @return 相似度分数（0.0 - 1.0）
     */
    private double calculateSimilarity(RefinedKnowledge k1, RefinedKnowledge k2) {
        double score = 0.0;

        // 1. 标题相似度（权重 0.4）
        if (k1.getTitle() != null && k2.getTitle() != null) {
            score += calculateTextSimilarity(k1.getTitle(), k2.getTitle()) * 0.4;
        }

        // 2. 内容相似度（权重 0.3）
        if (k1.getRefinedContent() != null && k2.getRefinedContent() != null) {
            String preview1 = getPreview(k1.getRefinedContent(), 200);
            String preview2 = getPreview(k2.getRefinedContent(), 200);
            score += calculateTextSimilarity(preview1, preview2) * 0.3;
        }

        // 3. 知识类型相同（权重 0.2）
        if (k1.getKnowledgeType() != null && k1.getKnowledgeType().equals(k2.getKnowledgeType())) {
            score += 0.2;
        }

        // 4. 重要性差异（权重 0.1）
        if (k1.getImportance() != null && k2.getImportance() != null) {
            double diff = Math.abs(k1.getImportance() - k2.getImportance());
            score += (1.0 - diff / 10.0) * 0.1;
        }

        return Math.min(1.0, score);
    }

    /**
     * 计算两段文本的相似度（基于共同词汇）
     */
    private double calculateTextSimilarity(String text1, String text2) {
        if (text1 == null || text2 == null || text1.isEmpty() || text2.isEmpty()) {
            return 0.0;
        }

        Set<String> words1 = new HashSet<>(Arrays.asList(text1.toLowerCase().split("\\s+")));
        Set<String> words2 = new HashSet<>(Arrays.asList(text2.toLowerCase().split("\\s+")));

        // 计算交集
        Set<String> intersection = new HashSet<>(words1);
        intersection.retainAll(words2);

        // 计算并集
        Set<String> union = new HashSet<>(words1);
        union.addAll(words2);

        // Jaccard 相似度
        return union.isEmpty() ? 0.0 : (double) intersection.size() / union.size();
    }

    /**
     * 获取文本预览（前N个字符）
     */
    private String getPreview(String text, int length) {
        if (text == null) {
            return "";
        }
        return text.length() > length ? text.substring(0, length) : text;
    }
}

