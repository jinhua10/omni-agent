package top.yumbo.ai.omni.hope.starter.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.hope.api.model.QuestionTypeConfig;
import top.yumbo.ai.omni.hope.api.persistence.HopePersistence;

import jakarta.annotation.PostConstruct;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

/**
 * 问题分类器 - 决定使用哪一层知识回答
 * (Question Classifier - Decides which layer to use for answering)
 *
 * <p>
 * HOPE 系统的核心组件，基于知识网络架构实现
 * 使用 Knowledge Registry 作为持久化后端
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class QuestionClassifier {

    private final HopePersistence persistence;

    /**
     * 分类配置缓存
     */
    private final Map<String, QuestionTypeConfig> configCache = new ConcurrentHashMap<>();

    /**
     * 关键词库缓存
     */
    private final Map<String, List<String>> keywordCache = new ConcurrentHashMap<>();

    /**
     * 模式库缓存（编译后的正则表达式）
     */
    private final Map<String, List<Pattern>> patternCache = new ConcurrentHashMap<>();

    @Autowired
    public QuestionClassifier(HopePersistence persistence) {
        this.persistence = persistence;
        log.info("✅ QuestionClassifier initialized with persistence: {}",
                persistence.getClass().getSimpleName());
    }

    /**
     * 初始化 - 加载配置
     */
    @PostConstruct
    public void init() {
        log.info("🔧 Initializing QuestionClassifier...");
        try {
            loadConfiguration();
            log.info("✅ QuestionClassifier initialized. Loaded {} question types", configCache.size());
        } catch (Exception e) {
            log.error("❌ Failed to initialize QuestionClassifier", e);
            loadDefaultConfiguration();
        }
    }

    /**
     * 从持久化加载配置
     */
    private void loadConfiguration() {
        // 加载问题类型
        List<QuestionTypeConfig> types = persistence.getAllQuestionTypes();
        for (QuestionTypeConfig type : types) {
            configCache.put(type.getId(), type);

            // 加载关键词
            List<String> keywords = persistence.getKeywords(type.getId());
            if (!keywords.isEmpty()) {
                keywordCache.put(type.getId(), keywords);
            }

            // 加载并编译模式
            List<String> patternStrings = persistence.getPatterns(type.getId());
            if (!patternStrings.isEmpty()) {
                List<Pattern> patterns = new ArrayList<>();
                for (String patternStr : patternStrings) {
                    try {
                        patterns.add(Pattern.compile(patternStr, Pattern.CASE_INSENSITIVE));
                    } catch (Exception e) {
                        log.warn("⚠️ Invalid pattern for type {}: {}", type.getId(), patternStr);
                    }
                }
                if (!patterns.isEmpty()) {
                    patternCache.put(type.getId(), patterns);
                }
            }
        }

        // 如果没有配置，加载默认配置
        if (configCache.isEmpty()) {
            loadDefaultConfiguration();
        }
    }

    /**
     * 加载默认配置
     */
    private void loadDefaultConfiguration() {
        log.info("📝 Loading default configuration...");

        List<QuestionTypeConfig> defaultTypes = Arrays.asList(
            QuestionTypeConfig.builder()
                .id("factual")
                .name("事实性问题")
                .nameEn("Factual Question")
                .priority(10)
                .complexity("simple")
                .suggestedLayer("permanent")
                .enabled(true)
                .keywords(Arrays.asList("是什么", "什么是", "定义", "含义"))
                .patterns(Arrays.asList(".*是什么.*", ".*什么是.*", ".*定义.*"))
                .description("关于事实、定义的问题")
                .build(),

            QuestionTypeConfig.builder()
                .id("procedural")
                .name("流程性问题")
                .nameEn("Procedural Question")
                .priority(8)
                .complexity("medium")
                .suggestedLayer("ordinary")
                .enabled(true)
                .keywords(Arrays.asList("如何", "怎么", "步骤", "流程"))
                .patterns(Arrays.asList(".*如何.*", ".*怎么.*", ".*步骤.*"))
                .description("关于操作流程、方法的问题")
                .build(),

            QuestionTypeConfig.builder()
                .id("analytical")
                .name("分析性问题")
                .nameEn("Analytical Question")
                .priority(6)
                .complexity("complex")
                .suggestedLayer("ordinary")
                .enabled(true)
                .keywords(Arrays.asList("为什么", "原因", "分析", "比较"))
                .patterns(Arrays.asList(".*为什么.*", ".*原因.*", ".*分析.*"))
                .description("需要分析、推理的问题")
                .build(),

            QuestionTypeConfig.builder()
                .id("conversational")
                .name("对话性问题")
                .nameEn("Conversational Question")
                .priority(5)
                .complexity("simple")
                .suggestedLayer("high_frequency")
                .enabled(true)
                .keywords(Arrays.asList("你好", "谢谢", "再见"))
                .patterns(Arrays.asList("^你好.*", "^谢谢.*"))
                .description("日常对话、寒暄")
                .build()
        );

        // 保存到持久化
        persistence.saveQuestionTypes(defaultTypes);

        // 保存关键词和模式
        for (QuestionTypeConfig type : defaultTypes) {
            if (!type.getKeywords().isEmpty()) {
                persistence.saveKeywords(type.getId(), type.getKeywords());
            }
            if (!type.getPatterns().isEmpty()) {
                persistence.savePatterns(type.getId(), type.getPatterns());
            }
        }

        // 重新加载
        loadConfiguration();

        log.info("✅ Default configuration loaded");
    }

    /**
     * 分类问题
     *
     * @param question 用户问题
     * @return 问题类型ID
     */
    public String classify(String question) {
        if (question == null || question.trim().isEmpty()) {
            return "unknown";
        }

        String normalizedQuestion = question.trim().toLowerCase();

        // 按优先级排序的类型列表
        List<QuestionTypeConfig> sortedTypes = configCache.values().stream()
                .filter(QuestionTypeConfig::getEnabled)
                .sorted((a, b) -> Integer.compare(b.getPriority(), a.getPriority()))
                .toList();

        // 1. 首先尝试正则表达式匹配
        for (QuestionTypeConfig type : sortedTypes) {
            List<Pattern> patterns = patternCache.get(type.getId());
            if (patterns != null) {
                for (Pattern pattern : patterns) {
                    if (pattern.matcher(normalizedQuestion).matches()) {
                        log.debug("🎯 Classified by pattern: {} -> {}", question, type.getId());
                        return type.getId();
                    }
                }
            }
        }

        // 2. 关键词匹配
        for (QuestionTypeConfig type : sortedTypes) {
            List<String> keywords = keywordCache.get(type.getId());
            if (keywords != null) {
                for (String keyword : keywords) {
                    if (normalizedQuestion.contains(keyword.toLowerCase())) {
                        log.debug("🎯 Classified by keyword: {} -> {}", question, type.getId());
                        return type.getId();
                    }
                }
            }
        }

        // 3. 默认返回 unknown
        log.debug("❓ Unknown question type: {}", question);
        return "unknown";
    }

    /**
     * 获取建议使用的层级
     *
     * @param questionTypeId 问题类型ID
     * @return 建议的层级（permanent/ordinary/high_frequency）
     */
    public String getSuggestedLayer(String questionTypeId) {
        QuestionTypeConfig config = configCache.get(questionTypeId);
        if (config != null) {
            return config.getSuggestedLayer();
        }
        return "ordinary"; // 默认使用普通层
    }

    /**
     * 重新加载配置
     */
    public void reload() {
        log.info("🔄 Reloading QuestionClassifier configuration...");
        configCache.clear();
        keywordCache.clear();
        patternCache.clear();
        loadConfiguration();
        log.info("✅ Configuration reloaded");
    }

    /**
     * 获取所有问题类型
     */
    public List<QuestionTypeConfig> getAllTypes() {
        return new ArrayList<>(configCache.values());
    }

    /**
     * 添加问题类型
     */
    public boolean addQuestionType(QuestionTypeConfig config) {
        if (config == null || config.getId() == null) {
            return false;
        }

        boolean saved = persistence.saveQuestionType(config);
        if (saved) {
            configCache.put(config.getId(), config);

            if (config.getKeywords() != null && !config.getKeywords().isEmpty()) {
                persistence.saveKeywords(config.getId(), config.getKeywords());
                keywordCache.put(config.getId(), config.getKeywords());
            }

            if (config.getPatterns() != null && !config.getPatterns().isEmpty()) {
                persistence.savePatterns(config.getId(), config.getPatterns());
                List<Pattern> patterns = config.getPatterns().stream()
                        .map(p -> Pattern.compile(p, Pattern.CASE_INSENSITIVE))
                        .toList();
                patternCache.put(config.getId(), patterns);
            }

            log.info("✅ Added question type: {}", config.getId());
        }

        return saved;
    }
}

