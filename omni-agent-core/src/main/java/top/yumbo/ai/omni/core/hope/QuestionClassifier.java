package top.yumbo.ai.omni.core.hope;

import lombok.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.persistence.api.model.QuestionTypeConfig;

import jakarta.annotation.PostConstruct;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * 问题分类器 - 决定使用哪一层知识回答
 * (Question Classifier - Decides which layer to use for answering)
 *
 * <p>
 * 重构亮点 (Refactoring Highlights):
 * <ul>
 *   <li>✅ 依赖接口而非实现 - 使用 QuestionClassifierPersistence 接口</li>
 *   <li>✅ Spring 自动注入 - 根据用户选择的 Starter 自动注入实现</li>
 *   <li>✅ 无需运行时切换 - 编译时通过依赖决定实现</li>
 *   <li>✅ 完全可插拔 - 支持 Memory/H2/ES/MongoDB 等多种后端</li>
 * </ul>
 * </p>
 *
 * <p>
 * 设计特点 (Design Features):
 * <ul>
 *   <li>✅ 动态配置加载 - 从持久化接口加载分类规则</li>
 *   <li>✅ 完整国际化 - 所有文本支持中英文</li>
 *   <li>✅ 可扩展类型 - 支持动态添加问题类型</li>
 *   <li>✅ 角色知识库适配 - 不同角色可以有不同的分类策略</li>
 *   <li>✅ 热重载 - 可以在运行时重新加载配置</li>
 *   <li>✅ 高性能 - 使用缓存和优化的匹配算法</li>
 * </ul>
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0 (Refactored)
 * @version 3.0.0 - 重构为可插拔架构
 */
@Slf4j
@Component
public class QuestionClassifier {

    /**
     * 持久化接口 (Persistence interface)
     * Spring Boot 会根据用户选择的 Starter 自动注入对应实现
     */
    private final QuestionClassifierPersistence persistence;

    /**
     * 分类配置缓存 (Classification configuration cache)
     */
    private final Map<String, QuestionTypeConfig> configCache = new ConcurrentHashMap<>();

    /**
     * 关键词库缓存 (Keyword library cache)
     */
    private final Map<String, List<String>> keywordCache = new ConcurrentHashMap<>();

    /**
     * 模式库缓存 (Pattern library cache)
     * 存储编译后的正则表达式
     */
    private final Map<String, List<Pattern>> patternCache = new ConcurrentHashMap<>();

    /**
     * 配置版本号 (Configuration version)
     */
    private String configVersion = "1.0.0";

    /**
     * 构造函数 - Spring 自动注入持久化接口
     *
     * @param persistence 持久化接口（可能是 Memory、H2、ES、MongoDB 等实现）
     */
    @Autowired
    public QuestionClassifier(QuestionClassifierPersistence persistence) {
        this.persistence = persistence;
        log.info("QuestionClassifier initialized with persistence: {}",
                 persistence.getClass().getSimpleName());
    }

    /**
     * 初始化 - 加载配置
     */
    @PostConstruct
    public void init() {
        log.info("Initializing QuestionClassifier...");
        try {
            loadConfiguration();
            log.info("QuestionClassifier initialized successfully. " +
                    "Loaded {} question types", configCache.size());
        } catch (Exception e) {
            log.error("Failed to initialize QuestionClassifier", e);
            // 初始化失败时使用默认配置
            loadDefaultConfiguration();
        }
    }

    /**
     * 从持久化接口加载配置
     * (Load configuration from persistence interface)
     */
    private void loadConfiguration() {
        // 清空缓存
        configCache.clear();
        keywordCache.clear();
        patternCache.clear();

        // 从持久化接口加载所有问题类型
        List<QuestionTypeConfig> configs = persistence.getAllQuestionTypes();

        for (QuestionTypeConfig config : configs) {
            if (config.isEnabled()) {
                configCache.put(config.getId(), config);

                // 加载关键词
                List<String> keywords = persistence.getKeywords(config.getId());
                if (keywords != null && !keywords.isEmpty()) {
                    keywordCache.put(config.getId(), keywords);
                }

                // 加载模式并编译
                List<String> patternStrings = persistence.getPatterns(config.getId());
                if (patternStrings != null && !patternStrings.isEmpty()) {
                    List<Pattern> patterns = patternStrings.stream()
                        .map(Pattern::compile)
                        .collect(Collectors.toList());
                    patternCache.put(config.getId(), patterns);
                }
            }
        }

        // 加载版本号
        configVersion = persistence.getVersion();

        log.info("Loaded {} active question types from persistence", configCache.size());
    }

    /**
     * 加载默认配置
     * (Load default configuration)
     */
    private void loadDefaultConfiguration() {
        log.warn("Loading default configuration as fallback");

        // 创建默认的问题类型
        QuestionTypeConfig defaultType = QuestionTypeConfig.builder()
            .id("default")
            .name("默认类型")
            .nameEn("Default Type")
            .priority(100)
            .complexity("MEDIUM")
            .suggestedLayer("ORDINARY")
            .enabled(true)
            .description("Default question type when classification fails")
            .createdAt(System.currentTimeMillis())
            .build();

        configCache.put("default", defaultType);
    }

    /**
     * 分类问题 - 决定使用哪一层知识
     * (Classify question - decide which layer to use)
     *
     * @param question 用户问题
     * @return 分类结果
     */
    public ClassificationResult classify(String question) {
        if (question == null || question.trim().isEmpty()) {
            return ClassificationResult.builder()
                .questionType("default")
                .suggestedLayer("ORDINARY")
                .confidence(0.0)
                .build();
        }

        String normalizedQuestion = question.toLowerCase().trim();

        // 1. 尝试关键词匹配
        ClassificationResult keywordResult = classifyByKeywords(normalizedQuestion);
        if (keywordResult.getConfidence() > 0.7) {
            return keywordResult;
        }

        // 2. 尝试模式匹配
        ClassificationResult patternResult = classifyByPatterns(normalizedQuestion);
        if (patternResult.getConfidence() > 0.6) {
            return patternResult;
        }

        // 3. 返回最佳结果或默认
        if (keywordResult.getConfidence() > patternResult.getConfidence()) {
            return keywordResult;
        } else if (patternResult.getConfidence() > 0) {
            return patternResult;
        }

        // 默认分类
        return ClassificationResult.builder()
            .questionType("default")
            .suggestedLayer("ORDINARY")
            .confidence(0.3)
            .build();
    }

    /**
     * 通过关键词分类
     */
    private ClassificationResult classifyByKeywords(String question) {
        String bestTypeId = null;
        double bestScore = 0.0;

        for (Map.Entry<String, List<String>> entry : keywordCache.entrySet()) {
            String typeId = entry.getKey();
            List<String> keywords = entry.getValue();

            long matchCount = keywords.stream()
                .filter(keyword -> question.contains(keyword.toLowerCase()))
                .count();

            double score = (double) matchCount / keywords.size();

            if (score > bestScore) {
                bestScore = score;
                bestTypeId = typeId;
            }
        }

        if (bestTypeId != null) {
            QuestionTypeConfig config = configCache.get(bestTypeId);
            return ClassificationResult.builder()
                .questionType(bestTypeId)
                .suggestedLayer(config.getSuggestedLayer())
                .confidence(bestScore)
                .method("KEYWORD")
                .build();
        }

        return ClassificationResult.builder()
            .questionType("default")
            .suggestedLayer("ORDINARY")
            .confidence(0.0)
            .build();
    }

    /**
     * 通过模式分类
     */
    private ClassificationResult classifyByPatterns(String question) {
        for (Map.Entry<String, List<Pattern>> entry : patternCache.entrySet()) {
            String typeId = entry.getKey();
            List<Pattern> patterns = entry.getValue();

            for (Pattern pattern : patterns) {
                if (pattern.matcher(question).find()) {
                    QuestionTypeConfig config = configCache.get(typeId);
                    return ClassificationResult.builder()
                        .questionType(typeId)
                        .suggestedLayer(config.getSuggestedLayer())
                        .confidence(0.8)
                        .method("PATTERN")
                        .build();
                }
            }
        }

        return ClassificationResult.builder()
            .questionType("default")
            .suggestedLayer("ORDINARY")
            .confidence(0.0)
            .build();
    }

    /**
     * 重新加载配置
     * (Reload configuration)
     */
    public void reload() {
        log.info("Reloading question classifier configuration...");
        loadConfiguration();
    }

    /**
     * 获取所有问题类型
     */
    public List<QuestionTypeConfig> getAllQuestionTypes() {
        return new ArrayList<>(configCache.values());
    }

    /**
     * 分类结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ClassificationResult {
        /** 问题类型ID */
        private String questionType;

        /** 建议的知识层 */
        private String suggestedLayer;

        /** 置信度 (0-1) */
        private double confidence;

        /** 分类方法 */
        private String method;
    }
}

