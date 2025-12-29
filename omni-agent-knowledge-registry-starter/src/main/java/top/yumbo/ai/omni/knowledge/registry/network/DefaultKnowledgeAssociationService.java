package top.yumbo.ai.omni.knowledge.registry.network;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.knowledge.registry.model.RefinedKnowledge;

import java.time.LocalDateTime;
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
    private final ObjectMapper objectMapper;
    private static final String ASSOCIATION_PREFIX = "knowledge-association";

    public DefaultKnowledgeAssociationService(KnowledgeStorageService storageService) {
        this.storageService = storageService;
        this.objectMapper = new ObjectMapper();
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

        try {
            // 1. 创建关联对象
            KnowledgeAssociation association = KnowledgeAssociation.builder()
                    .associationId(UUID.randomUUID().toString())
                    .sourceKnowledgeId(sourceKnowledgeId)
                    .targetKnowledgeId(targetKnowledgeId)
                    .relationType(relationType)
                    .strength(strength)
                    .createdAt(LocalDateTime.now())
                    .build();

            // 2. 序列化为 JSON
            byte[] jsonData = objectMapper.writeValueAsBytes(association);

            // 3. 构建存储路径：knowledge-association/{sourceId}/{targetId}.json
            String documentId = buildAssociationDocumentId(sourceKnowledgeId, targetKnowledgeId);
            String filename = targetKnowledgeId + ".json";

            // 4. 存储到 DocumentStorage
            // 注意：由于 DocumentStorage 是按域组织的，我们使用特殊的 "association" 域
            // 实际实现中可能需要调整存储策略
            storageService.storeKnowledge(
                    createAssociationAsKnowledge(association),
                    "association" // 特殊域用于存储关联
            );

            log.debug("✅ 创建知识关联成功: {} → {}", sourceKnowledgeId, targetKnowledgeId);
            return true;

        } catch (Exception e) {
            log.error("❌ 创建知识关联失败: source={}, target={}",
                    sourceKnowledgeId, targetKnowledgeId, e);
            return false;
        }
    }

    @Override
    public boolean removeAssociation(String sourceKnowledgeId, String targetKnowledgeId) {
        log.debug("删除知识关联: source={}, target={}", sourceKnowledgeId, targetKnowledgeId);

        try {
            // 构建关联的知识ID
            String associationKnowledgeId = buildAssociationKnowledgeId(sourceKnowledgeId, targetKnowledgeId);

            // 从 "association" 域中删除
            boolean deleted = storageService.deleteKnowledge(associationKnowledgeId, "association");

            if (deleted) {
                log.debug("✅ 删除知识关联成功: {} → {}", sourceKnowledgeId, targetKnowledgeId);
            } else {
                log.warn("⚠️ 知识关联不存在或删除失败: {} → {}", sourceKnowledgeId, targetKnowledgeId);
            }

            return deleted;

        } catch (Exception e) {
            log.error("❌ 删除知识关联失败: source={}, target={}",
                    sourceKnowledgeId, targetKnowledgeId, e);
            return false;
        }
    }

    @Override
    public List<DomainAssociation> findRelatedDomains(String domainId, int topK) {
        log.debug("查找相关域: domainId={}, topK={}", domainId, topK);

        try {
            // 1. 获取该域的所有知识（通过搜索空字符串获取所有）
            // 注意：这是一个简化实现，生产环境应该有专门的 listAllKnowledge 方法
            List<RefinedKnowledge> domainKnowledge = getAllKnowledgeInDomain(domainId);

            if (domainKnowledge.isEmpty()) {
                log.debug("域 {} 中没有知识", domainId);
                return new ArrayList<>();
            }

            // 2. 统计其他域的引用次数（基于知识内容中的域名称或关键词）
            Map<String, DomainReferenceInfo> domainReferences = new HashMap<>();

            for (RefinedKnowledge knowledge : domainKnowledge) {
                // 从知识内容中提取可能的域引用
                List<String> referencedDomains = extractDomainReferences(knowledge);

                for (String refDomainId : referencedDomains) {
                    if (!refDomainId.equals(domainId)) { // 排除自己
                        domainReferences.computeIfAbsent(refDomainId, k -> new DomainReferenceInfo())
                                .incrementCount();
                    }
                }
            }

            // 3. 转换为 DomainAssociation 并计算关联强度
            List<DomainAssociation> associations = new ArrayList<>();
            int totalKnowledge = domainKnowledge.size();

            for (Map.Entry<String, DomainReferenceInfo> entry : domainReferences.entrySet()) {
                String refDomainId = entry.getKey();
                int refCount = entry.getValue().getCount();

                // 计算关联强度：引用次数 / 总知识数
                double strength = (double) refCount / totalKnowledge;

                DomainAssociation association = DomainAssociation.builder()
                        .domainId(refDomainId)
                        .domainName(getDomainName(refDomainId))
                        .strength(Math.min(1.0, strength)) // 限制在 0-1 之间
                        .relationType("REFERENCE")
                        .sharedKnowledgeCount(refCount)
                        .build();

                associations.add(association);
            }

            // 4. 按关联强度排序并返回 Top K
            List<DomainAssociation> result = associations.stream()
                    .sorted((a, b) -> Double.compare(b.getStrength(), a.getStrength()))
                    .limit(topK)
                    .collect(Collectors.toList());

            log.debug("✅ 找到 {} 个相关域", result.size());
            return result;

        } catch (Exception e) {
            log.error("❌ 查找相关域失败: domainId={}", domainId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public List<DomainRecommendation> recommendDomains(String query, int topK) {
        log.debug("推荐知识域: query={}, topK={}", query, topK);

        try {
            // 1. 从查询中提取关键词
            List<String> queryKeywords = extractQueryKeywords(query);

            if (queryKeywords.isEmpty()) {
                log.debug("未能从查询中提取关键词");
                return new ArrayList<>();
            }

            log.debug("提取到查询关键词: {}", queryKeywords);

            // 2. 使用关键词在所有域中搜索，统计每个域的匹配度
            // 注意：这需要能够获取所有域的列表，这里简化实现
            Map<String, DomainMatchInfo> domainMatches = new HashMap<>();

            // 尝试搜索知识，根据搜索结果统计域分布
            for (String keyword : queryKeywords) {
                // 这里简化：假设我们通过某种方式能获取到跨域搜索结果
                // 实际实现需要配合 KnowledgeRegistry 获取所有域列表
                // 然后在每个域中搜索

                // 为了演示，我们使用一个固定的域列表进行搜索
                List<String> commonDomains = Arrays.asList(
                    "security", "authentication", "authorization",
                    "java", "spring", "database", "api", "frontend"
                );

                for (String domainId : commonDomains) {
                    try {
                        List<RefinedKnowledge> results = storageService.searchKnowledge(
                                keyword, domainId, 5);

                        if (!results.isEmpty()) {
                            DomainMatchInfo info = domainMatches.computeIfAbsent(
                                    domainId, k -> new DomainMatchInfo(domainId));
                            info.addMatches(results.size());
                            info.incrementKeywordMatches();
                        }
                    } catch (Exception e) {
                        // 域可能不存在，继续下一个
                        log.trace("域 {} 搜索失败或不存在", domainId);
                    }
                }
            }

            // 3. 计算推荐分数并构建推荐列表
            List<DomainRecommendation> recommendations = new ArrayList<>();

            for (DomainMatchInfo info : domainMatches.values()) {
                // 推荐分数 = (关键词匹配数 / 总关键词数) * 0.6 + (匹配知识数 / 10) * 0.4
                double keywordScore = (double) info.getKeywordMatches() / queryKeywords.size();
                double knowledgeScore = Math.min(1.0, info.getTotalMatches() / 10.0);
                double score = keywordScore * 0.6 + knowledgeScore * 0.4;

                DomainRecommendation recommendation = DomainRecommendation.builder()
                        .domainId(info.getDomainId())
                        .domainName(getDomainName(info.getDomainId()))
                        .score(score)
                        .reason(generateRecommendationReason(info, queryKeywords))
                        .matchedKeywords(queryKeywords)
                        .build();

                recommendations.add(recommendation);
            }

            // 4. 按推荐分数排序并返回 Top K
            List<DomainRecommendation> result = recommendations.stream()
                    .sorted((r1, r2) -> Double.compare(r2.getScore(), r1.getScore()))
                    .limit(topK)
                    .collect(Collectors.toList());

            log.debug("✅ 推荐 {} 个知识域", result.size());
            return result;

        } catch (Exception e) {
            log.error("❌ 推荐知识域失败: query={}", query, e);
            return new ArrayList<>();
        }
    }


    /**
     * 获取域中的所有知识（简化实现）
     */
    private List<RefinedKnowledge> getAllKnowledgeInDomain(String domainId) {
        // 简化实现：使用空字符串搜索尝试获取所有知识
        // 生产环境应该有专门的 API
        try {
            return storageService.searchKnowledge("", domainId, 1000);
        } catch (Exception e) {
            log.debug("无法获取域 {} 的所有知识", domainId);
            return new ArrayList<>();
        }
    }

    /**
     * 从知识内容中提取域引用
     */
    private List<String> extractDomainReferences(RefinedKnowledge knowledge) {
        List<String> references = new ArrayList<>();

        if (knowledge.getRefinedContent() == null) {
            return references;
        }

        String content = knowledge.getRefinedContent().toLowerCase();

        // 常见域名称模式（可以扩展）
        Map<String, List<String>> domainKeywords = Map.of(
            "security", Arrays.asList("security", "安全", "认证", "授权"),
            "authentication", Arrays.asList("authentication", "auth", "登录", "jwt"),
            "java", Arrays.asList("java", "spring", "springboot"),
            "database", Arrays.asList("database", "mysql", "redis", "数据库"),
            "api", Arrays.asList("api", "rest", "接口"),
            "frontend", Arrays.asList("frontend", "react", "vue", "前端")
        );

        for (Map.Entry<String, List<String>> entry : domainKeywords.entrySet()) {
            String domainId = entry.getKey();
            List<String> keywords = entry.getValue();

            for (String keyword : keywords) {
                if (content.contains(keyword)) {
                    if (!references.contains(domainId)) {
                        references.add(domainId);
                    }
                    break; // 找到一个关键词就够了
                }
            }
        }

        return references;
    }

    /**
     * 获取域名称（简化实现）
     */
    private String getDomainName(String domainId) {
        // 简化实现：直接返回 ID 的格式化版本
        // 生产环境应该从 KnowledgeRegistry 查询
        return domainId.substring(0, 1).toUpperCase() + domainId.substring(1);
    }

    /**
     * 从查询中提取关键词
     */
    private List<String> extractQueryKeywords(String query) {
        if (query == null || query.trim().isEmpty()) {
            return new ArrayList<>();
        }

        // 分词并过滤
        return Arrays.stream(query.split("[\\s,，、。.!！?？]+"))
                .map(String::trim)
                .filter(word -> word.length() >= 2) // 过滤单字符
                .filter(word -> !isStopWord(word))  // 过滤停用词
                .limit(10) // 限制关键词数量
                .collect(Collectors.toList());
    }

    /**
     * 判断是否为停用词
     */
    private boolean isStopWord(String word) {
        Set<String> stopWords = Set.of(
            "的", "了", "和", "是", "在", "有", "我", "个", "们", "这",
            "那", "与", "及", "等", "中", "可以", "什么", "如何", "为什么",
            "the", "a", "an", "and", "or", "but", "is", "are", "was", "were",
            "in", "on", "at", "to", "for", "of", "with", "by", "from"
        );
        return stopWords.contains(word.toLowerCase());
    }

    /**
     * 生成推荐原因
     */
    private String generateRecommendationReason(DomainMatchInfo info, List<String> queryKeywords) {
        return String.format("匹配 %d 个关键词，找到 %d 条相关知识",
                info.getKeywordMatches(), info.getTotalMatches());
    }

    /**
     * 构建关联的文档ID
     */
    private String buildAssociationDocumentId(String sourceId, String targetId) {
        return String.format("%s/%s/%s", ASSOCIATION_PREFIX, sourceId, targetId);
    }

    /**
     * 构建关联的知识ID
     */
    private String buildAssociationKnowledgeId(String sourceId, String targetId) {
        return String.format("assoc-%s-%s", sourceId, targetId);
    }

    /**
     * 将关联对象转换为 RefinedKnowledge（用于存储）
     */
    private RefinedKnowledge createAssociationAsKnowledge(KnowledgeAssociation association) {
        try {
            String jsonContent = objectMapper.writeValueAsString(association);

            return RefinedKnowledge.builder()
                    .knowledgeId(buildAssociationKnowledgeId(
                            association.getSourceKnowledgeId(),
                            association.getTargetKnowledgeId()))
                    .title(String.format("关联: %s → %s",
                            association.getSourceKnowledgeId(),
                            association.getTargetKnowledgeId()))
                    .refinedContent(jsonContent)
                    .knowledgeType("ASSOCIATION")
                    .importance(3.0)
                    .build();
        } catch (Exception e) {
            log.error("转换关联为知识对象失败", e);
            return null;
        }
    }

    /**
     * 域引用信息（内部类）
     */
    private static class DomainReferenceInfo {
        private int count = 0;

        public void incrementCount() {
            count++;
        }

        public int getCount() {
            return count;
        }
    }

    /**
     * 域匹配信息（内部类）
     */
    private static class DomainMatchInfo {
        private final String domainId;
        private int totalMatches = 0;
        private int keywordMatches = 0;

        public DomainMatchInfo(String domainId) {
            this.domainId = domainId;
        }

        public void addMatches(int count) {
            totalMatches += count;
        }

        public void incrementKeywordMatches() {
            keywordMatches++;
        }

        public String getDomainId() {
            return domainId;
        }

        public int getTotalMatches() {
            return totalMatches;
        }

        public int getKeywordMatches() {
            return keywordMatches;
        }
    }
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

    /**
     * 知识关联（内部类）
     *
     * <p>表示两个知识之间的关联关系</p>
     */
    private static class KnowledgeAssociation {
        private String associationId;
        private String sourceKnowledgeId;
        private String targetKnowledgeId;
        private String relationType;
        private double strength;
        private LocalDateTime createdAt;

        public static KnowledgeAssociationBuilder builder() {
            return new KnowledgeAssociationBuilder();
        }

        public String getAssociationId() {
            return associationId;
        }

        public String getSourceKnowledgeId() {
            return sourceKnowledgeId;
        }

        public String getTargetKnowledgeId() {
            return targetKnowledgeId;
        }

        public String getRelationType() {
            return relationType;
        }

        public double getStrength() {
            return strength;
        }

        public LocalDateTime getCreatedAt() {
            return createdAt;
        }

        /**
         * Builder for KnowledgeAssociation
         */
        private static class KnowledgeAssociationBuilder {
            private String associationId;
            private String sourceKnowledgeId;
            private String targetKnowledgeId;
            private String relationType;
            private double strength;
            private LocalDateTime createdAt;

            public KnowledgeAssociationBuilder associationId(String associationId) {
                this.associationId = associationId;
                return this;
            }

            public KnowledgeAssociationBuilder sourceKnowledgeId(String sourceKnowledgeId) {
                this.sourceKnowledgeId = sourceKnowledgeId;
                return this;
            }

            public KnowledgeAssociationBuilder targetKnowledgeId(String targetKnowledgeId) {
                this.targetKnowledgeId = targetKnowledgeId;
                return this;
            }

            public KnowledgeAssociationBuilder relationType(String relationType) {
                this.relationType = relationType;
                return this;
            }

            public KnowledgeAssociationBuilder strength(double strength) {
                this.strength = strength;
                return this;
            }

            public KnowledgeAssociationBuilder createdAt(LocalDateTime createdAt) {
                this.createdAt = createdAt;
                return this;
            }

            public KnowledgeAssociation build() {
                KnowledgeAssociation association = new KnowledgeAssociation();
                association.associationId = this.associationId;
                association.sourceKnowledgeId = this.sourceKnowledgeId;
                association.targetKnowledgeId = this.targetKnowledgeId;
                association.relationType = this.relationType;
                association.strength = this.strength;
                association.createdAt = this.createdAt;
                return association;
            }
        }
    }
}

