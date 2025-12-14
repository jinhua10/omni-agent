package top.yumbo.ai.omni.core.hope.layer;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.persistence.api.model.QuestionTypeConfig;

import jakarta.annotation.PostConstruct;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 中频层服务 - 管理常规知识
 * (Ordinary Layer Service - Manages regular knowledge)
 *
 * <p>
 * 重构说明 (Refactoring Notes):
 * - 使用 QuestionClassifierPersistence 接口存储常规知识
 * - 保留内存缓存提高查询性能
 * - 支持持久化到多种后端（Memory/H2/ES/MongoDB等）
 * </p>
 *
 * 特点 (Features):
 * - 存储常规的、通用的知识 (Store regular and general knowledge)
 * - 需要 LLM 辅助生成答案 (Need LLM to assist in generating answers)
 * - 定期更新，中等置信度 (Regular updates, medium confidence)
 * - 完全可插拔的持久化后端 (Fully pluggable persistence backend)
 *
 * @author OmniAgent Team
 * @since 1.0.0 (Refactored)
 * @version 3.0.0 - 重构为可插拔架构
 */
@Slf4j
@Service
public class OrdinaryLayerService {

    private final QuestionClassifierPersistence persistence;

    // 内存缓存 - 提高查询性能
    private final Map<String, QuestionTypeConfig> knowledgeCache = new ConcurrentHashMap<>();

    // 关键词索引
    private final Map<String, Set<String>> keywordIndex = new ConcurrentHashMap<>();

    /**
     * 构造函数 - Spring 自动注入持久化接口
     *
     * @param persistence 持久化接口
     */
    @Autowired
    public OrdinaryLayerService(QuestionClassifierPersistence persistence) {
        this.persistence = persistence;
        log.info("OrdinaryLayerService initialized with persistence: {}",
                 persistence.getClass().getSimpleName());
    }

    /**
     * 初始化 - 加载知识到缓存
     */
    @PostConstruct
    public void init() {
        log.info("Initializing OrdinaryLayerService...");
        try {
            loadKnowledgeToCache();
            log.info("OrdinaryLayerService initialized. Loaded {} knowledge items",
                    knowledgeCache.size());
        } catch (Exception e) {
            log.error("Failed to initialize OrdinaryLayerService", e);
        }
    }

    /**
     * 从持久化接口加载知识到缓存
     */
    private void loadKnowledgeToCache() {
        knowledgeCache.clear();
        keywordIndex.clear();

        // 加载所有中频层的问题类型
        List<QuestionTypeConfig> configs = persistence.getAllQuestionTypes();

        for (QuestionTypeConfig config : configs) {
            if (config.isEnabled() && "ORDINARY".equals(config.getSuggestedLayer())) {
                knowledgeCache.put(config.getId(), config);

                // 构建关键词索引
                List<String> keywords = persistence.getKeywords(config.getId());
                if (keywords != null && !keywords.isEmpty()) {
                    for (String keyword : keywords) {
                        keywordIndex.computeIfAbsent(keyword.toLowerCase(), k -> new HashSet<>())
                                   .add(config.getId());
                    }
                }
            }
        }
    }

    /**
     * 查询中频层知识
     *
     * @param question 问题
     * @return 查询结果
     */
    public QueryResult query(String question) {
        if (question == null || question.trim().isEmpty()) {
            return QueryResult.builder()
                .found(false)
                .needsLLM(true)
                .build();
        }

        String normalizedQuestion = question.toLowerCase().trim();

        // 通过关键词索引查找
        Set<String> matchedIds = new HashSet<>();
        for (Map.Entry<String, Set<String>> entry : keywordIndex.entrySet()) {
            if (normalizedQuestion.contains(entry.getKey())) {
                matchedIds.addAll(entry.getValue());
            }
        }

        if (!matchedIds.isEmpty()) {
            // 返回第一个匹配的知识
            String firstMatchId = matchedIds.iterator().next();
            QuestionTypeConfig config = knowledgeCache.get(firstMatchId);

            if (config != null) {
                return QueryResult.builder()
                    .found(true)
                    .knowledgeId(config.getId())
                    .knowledgeName(config.getName())
                    .context(config.getDescription())
                    .needsLLM(true) // 中频层需要 LLM 辅助
                    .confidence(0.7)
                    .build();
            }
        }

        return QueryResult.builder()
            .found(false)
            .needsLLM(true)
            .build();
    }

    /**
     * 添加新知识
     *
     * @param config 知识配置
     * @return 是否成功
     */
    public boolean addKnowledge(QuestionTypeConfig config) {
        try {
            // 设置为中频层
            config.setSuggestedLayer("ORDINARY");
            config.setEnabled(true);
            config.setCreatedAt(System.currentTimeMillis());

            // 保存到持久化接口
            boolean saved = persistence.saveQuestionType(config);

            if (saved) {
                // 更新缓存
                knowledgeCache.put(config.getId(), config);
                log.info("Added ordinary knowledge: {}", config.getId());
                return true;
            }

            return false;
        } catch (Exception e) {
            log.error("Failed to add ordinary knowledge: {}", config.getId(), e);
            return false;
        }
    }

    /**
     * 获取所有中频层知识
     *
     * @return 知识列表
     */
    public List<QuestionTypeConfig> getAllKnowledge() {
        return new ArrayList<>(knowledgeCache.values());
    }

    /**
     * 重新加载知识
     */
    public void reload() {
        log.info("Reloading ordinary layer knowledge...");
        loadKnowledgeToCache();
    }

    /**
     * 查询结果
     */
    @lombok.Data
    @lombok.Builder
    @lombok.NoArgsConstructor
    @lombok.AllArgsConstructor
    public static class QueryResult {
        /** 是否找到 */
        private boolean found;

        /** 知识ID */
        private String knowledgeId;

        /** 知识名称 */
        private String knowledgeName;

        /** 上下文（需要 LLM 处理） */
        private String context;

        /** 是否需要 LLM */
        private boolean needsLLM;

        /** 置信度 */
        private double confidence;
    }
}

