package top.yumbo.ai.omni.core.hope.layer;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.omni.persistence.api.model.QuestionTypeConfig;

import jakarta.annotation.PostConstruct;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 低频层服务 - 管理技能知识和确定性知识
 * (Permanent Layer Service - Manages skill templates and factual knowledge)
 *
 * <p>
 * 重构说明 (Refactoring Notes):
 * - 使用 QuestionClassifierPersistence 接口存储稳定知识
 * - 保留内存缓存提高查询性能
 * - 支持持久化到多种后端（Memory/H2/ES/MongoDB等）
 * </p>
 *
 * 特点 (Features):
 * - 存储稳定的、经过验证的知识 (Store stable and verified knowledge)
 * - 可直接回答，无需 LLM (Can answer directly without LLM)
 * - 极少更新，高置信度 (Rarely updated, high confidence)
 * - 完全可插拔的持久化后端 (Fully pluggable persistence backend)
 *
 * @author OmniAgent Team
 * @since 1.0.0 (Refactored)
 * @version 3.0.0 - 重构为可插拔架构
 */
@Slf4j
@Service
public class PermanentLayerService {

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
    public PermanentLayerService(QuestionClassifierPersistence persistence) {
        this.persistence = persistence;
        log.info("PermanentLayerService initialized with persistence: {}",
                 persistence.getClass().getSimpleName());
    }

    /**
     * 初始化 - 加载知识到缓存
     */
    @PostConstruct
    public void init() {
        log.info("Initializing PermanentLayerService...");
        try {
            loadKnowledgeToCache();
            log.info("PermanentLayerService initialized. Loaded {} knowledge items",
                    knowledgeCache.size());
        } catch (Exception e) {
            log.error("Failed to initialize PermanentLayerService", e);
        }
    }

    /**
     * 从持久化接口加载知识到缓存
     */
    private void loadKnowledgeToCache() {
        knowledgeCache.clear();
        keywordIndex.clear();

        // 加载所有问题类型作为知识库
        List<QuestionTypeConfig> configs = persistence.getAllQuestionTypes();

        for (QuestionTypeConfig config : configs) {
            if (config.isEnabled() && "PERMANENT".equals(config.getSuggestedLayer())) {
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
     * 查询低频层知识
     *
     * @param question 问题
     * @return 查询结果
     */
    public QueryResult query(String question) {
        if (question == null || question.trim().isEmpty()) {
            return QueryResult.builder()
                .found(false)
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
                    .answer(config.getDescription())
                    .confidence(0.9)
                    .build();
            }
        }

        return QueryResult.builder()
            .found(false)
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
            // 设置为低频层
            config.setSuggestedLayer("PERMANENT");
            config.setEnabled(true);
            config.setCreatedAt(System.currentTimeMillis());

            // 保存到持久化接口
            boolean saved = persistence.saveQuestionType(config);

            if (saved) {
                // 更新缓存
                knowledgeCache.put(config.getId(), config);
                log.info("Added knowledge: {}", config.getId());
                return true;
            }

            return false;
        } catch (Exception e) {
            log.error("Failed to add knowledge: {}", config.getId(), e);
            return false;
        }
    }

    /**
     * 获取所有低频层知识
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
        log.info("Reloading permanent layer knowledge...");
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

        /** 答案 */
        private String answer;

        /** 置信度 */
        private double confidence;
    }
}

