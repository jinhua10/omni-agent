package top.yumbo.ai.omni.hope.starter.persistence;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.core.hope.model.QuestionTypeConfig;
import top.yumbo.ai.omni.core.hope.persistence.HopePersistence;
import top.yumbo.ai.omni.knowledge.registry.model.domain.KnowledgeDomain;
import top.yumbo.ai.omni.knowledge.registry.model.domain.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.domain.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRegistry;

import java.util.*;

/**
 * 基于 Knowledge Registry 的 HOPE 持久化实现
 * 将问题类型配置存储在知识域的配置中
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class KnowledgeRegistryHopePersistence implements HopePersistence {

    private static final String HOPE_DOMAIN_ID = "hope-question-classifier";
    private static final String CONFIG_KEY_TYPES = "questionTypes";
    private static final String CONFIG_KEY_KEYWORDS = "keywords";
    private static final String CONFIG_KEY_PATTERNS = "patterns";

    private final KnowledgeRegistry knowledgeRegistry;
    private final ObjectMapper objectMapper;

    public KnowledgeRegistryHopePersistence(KnowledgeRegistry knowledgeRegistry) {
        this.knowledgeRegistry = knowledgeRegistry;
        this.objectMapper = new ObjectMapper();
        initializeHopeDomain();
    }

    /**
     * 初始化 HOPE 专用知识域
     */
    private void initializeHopeDomain() {
        try {
            Optional<KnowledgeDomain> existingDomain = knowledgeRegistry.findDomainById(HOPE_DOMAIN_ID);
            if (existingDomain.isEmpty()) {
                KnowledgeDomain hopeDomain = KnowledgeDomain.builder()
                        .domainId(HOPE_DOMAIN_ID)
                        .domainName("HOPE Question Classifier")
                        .domainType(DomainType.MIXED)
                        .description("HOPE 系统问题分类器配置存储域")
                        .status(DomainStatus.ACTIVE)
                        .config(new HashMap<>())
                        .build();

                knowledgeRegistry.saveDomain(hopeDomain);
                log.info("✅ Initialized HOPE domain for question classifier persistence");
            }
        } catch (Exception e) {
            log.error("❌ Failed to initialize HOPE domain", e);
        }
    }

    /**
     * 获取 HOPE 域的配置
     */
    private Map<String, Object> getHopeDomainConfig() {
        Optional<KnowledgeDomain> domainOpt = knowledgeRegistry.findDomainById(HOPE_DOMAIN_ID);
        if (domainOpt.isEmpty() || domainOpt.get().getConfig() == null) {
            return new HashMap<>();
        }
        return domainOpt.get().getConfig();
    }

    /**
     * 更新 HOPE 域的配置
     */
    private void updateHopeDomainConfig(Map<String, Object> config) {
        Optional<KnowledgeDomain> domainOpt = knowledgeRegistry.findDomainById(HOPE_DOMAIN_ID);
        if (domainOpt.isPresent()) {
            KnowledgeDomain domain = domainOpt.get();
            domain.setConfig(config);
            knowledgeRegistry.saveDomain(domain);
        }
    }

    /**
     * 获取所有问题类型配置的映射
     */
    @SuppressWarnings("unchecked")
    private Map<String, QuestionTypeConfig> getQuestionTypesMap() {
        Map<String, Object> config = getHopeDomainConfig();
        Object typesObj = config.get(CONFIG_KEY_TYPES);

        if (typesObj instanceof Map) {
            Map<String, Object> typesMap = (Map<String, Object>) typesObj;
            Map<String, QuestionTypeConfig> result = new HashMap<>();

            for (Map.Entry<String, Object> entry : typesMap.entrySet()) {
                try {
                    QuestionTypeConfig typeConfig = objectMapper.convertValue(
                            entry.getValue(), QuestionTypeConfig.class);
                    result.put(entry.getKey(), typeConfig);
                } catch (Exception e) {
                    log.warn("Failed to convert question type config: {}", entry.getKey(), e);
                }
            }
            return result;
        }

        return new HashMap<>();
    }

    /**
     * 保存问题类型配置映射
     */
    private void saveQuestionTypesMap(Map<String, QuestionTypeConfig> typesMap) {
        Map<String, Object> config = getHopeDomainConfig();
        config.put(CONFIG_KEY_TYPES, typesMap);
        updateHopeDomainConfig(config);
    }

    // ========== 问题类型管理 ==========

    @Override
    public boolean saveQuestionType(QuestionTypeConfig config) {
        if (config == null || config.getId() == null) {
            return false;
        }

        try {
            Map<String, QuestionTypeConfig> typesMap = getQuestionTypesMap();
            typesMap.put(config.getId(), config);
            saveQuestionTypesMap(typesMap);

            log.debug("📝 Saved question type: {}", config.getId());
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to save question type: {}", config.getId(), e);
            return false;
        }
    }

    @Override
    public int saveQuestionTypes(List<QuestionTypeConfig> configs) {
        if (configs == null) {
            return 0;
        }

        int count = 0;
        Map<String, QuestionTypeConfig> typesMap = getQuestionTypesMap();

        for (QuestionTypeConfig config : configs) {
            if (config != null && config.getId() != null) {
                typesMap.put(config.getId(), config);
                count++;
            }
        }

        if (count > 0) {
            saveQuestionTypesMap(typesMap);
            log.debug("📝 Batch saved {} question types", count);
        }

        return count;
    }

    @Override
    public Optional<QuestionTypeConfig> getQuestionType(String typeId) {
        if (typeId == null) {
            return Optional.empty();
        }

        Map<String, QuestionTypeConfig> typesMap = getQuestionTypesMap();
        return Optional.ofNullable(typesMap.get(typeId));
    }

    @Override
    public List<QuestionTypeConfig> getAllQuestionTypes() {
        Map<String, QuestionTypeConfig> typesMap = getQuestionTypesMap();
        return new ArrayList<>(typesMap.values());
    }

    @Override
    public boolean updateQuestionType(QuestionTypeConfig config) {
        if (config == null || config.getId() == null) {
            return false;
        }

        try {
            Map<String, QuestionTypeConfig> typesMap = getQuestionTypesMap();
            if (!typesMap.containsKey(config.getId())) {
                return false;
            }

            typesMap.put(config.getId(), config);
            saveQuestionTypesMap(typesMap);

            log.debug("✏️ Updated question type: {}", config.getId());
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to update question type: {}", config.getId(), e);
            return false;
        }
    }

    @Override
    public boolean deleteQuestionType(String typeId) {
        if (typeId == null) {
            return false;
        }

        try {
            Map<String, QuestionTypeConfig> typesMap = getQuestionTypesMap();
            boolean removed = typesMap.remove(typeId) != null;

            if (removed) {
                saveQuestionTypesMap(typesMap);
                log.debug("🗑️ Deleted question type: {}", typeId);
            }

            return removed;
        } catch (Exception e) {
            log.error("❌ Failed to delete question type: {}", typeId, e);
            return false;
        }
    }

    // ========== 关键词管理 ==========

    @SuppressWarnings("unchecked")
    private Map<String, List<String>> getKeywordsMap() {
        Map<String, Object> config = getHopeDomainConfig();
        Object keywordsObj = config.get(CONFIG_KEY_KEYWORDS);

        if (keywordsObj instanceof Map) {
            return (Map<String, List<String>>) keywordsObj;
        }

        return new HashMap<>();
    }

    private void saveKeywordsMap(Map<String, List<String>> keywordsMap) {
        Map<String, Object> config = getHopeDomainConfig();
        config.put(CONFIG_KEY_KEYWORDS, keywordsMap);
        updateHopeDomainConfig(config);
    }

    @Override
    public boolean saveKeywords(String typeId, List<String> keywords) {
        if (typeId == null || keywords == null) {
            return false;
        }

        try {
            Map<String, List<String>> keywordsMap = getKeywordsMap();
            keywordsMap.put(typeId, new ArrayList<>(keywords));
            saveKeywordsMap(keywordsMap);

            log.debug("📝 Saved {} keywords for type: {}", keywords.size(), typeId);
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to save keywords for type: {}", typeId, e);
            return false;
        }
    }

    @Override
    public boolean addKeywords(String typeId, List<String> keywords) {
        if (typeId == null || keywords == null) {
            return false;
        }

        try {
            Map<String, List<String>> keywordsMap = getKeywordsMap();
            List<String> existingKeywords = keywordsMap.getOrDefault(typeId, new ArrayList<>());

            for (String keyword : keywords) {
                if (!existingKeywords.contains(keyword)) {
                    existingKeywords.add(keyword);
                }
            }

            keywordsMap.put(typeId, existingKeywords);
            saveKeywordsMap(keywordsMap);

            log.debug("➕ Added keywords for type: {}", typeId);
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to add keywords for type: {}", typeId, e);
            return false;
        }
    }

    @Override
    public List<String> getKeywords(String typeId) {
        if (typeId == null) {
            return new ArrayList<>();
        }

        Map<String, List<String>> keywordsMap = getKeywordsMap();
        return new ArrayList<>(keywordsMap.getOrDefault(typeId, new ArrayList<>()));
    }

    @Override
    public boolean removeKeywords(String typeId, List<String> keywords) {
        if (typeId == null || keywords == null) {
            return false;
        }

        try {
            Map<String, List<String>> keywordsMap = getKeywordsMap();
            List<String> existingKeywords = keywordsMap.get(typeId);

            if (existingKeywords != null) {
                existingKeywords.removeAll(keywords);
                keywordsMap.put(typeId, existingKeywords);
                saveKeywordsMap(keywordsMap);

                log.debug("➖ Removed keywords for type: {}", typeId);
                return true;
            }

            return false;
        } catch (Exception e) {
            log.error("❌ Failed to remove keywords for type: {}", typeId, e);
            return false;
        }
    }

    // ========== 模式管理 ==========

    @SuppressWarnings("unchecked")
    private Map<String, List<String>> getPatternsMap() {
        Map<String, Object> config = getHopeDomainConfig();
        Object patternsObj = config.get(CONFIG_KEY_PATTERNS);

        if (patternsObj instanceof Map) {
            return (Map<String, List<String>>) patternsObj;
        }

        return new HashMap<>();
    }

    private void savePatternsMap(Map<String, List<String>> patternsMap) {
        Map<String, Object> config = getHopeDomainConfig();
        config.put(CONFIG_KEY_PATTERNS, patternsMap);
        updateHopeDomainConfig(config);
    }

    @Override
    public boolean savePatterns(String typeId, List<String> patterns) {
        if (typeId == null || patterns == null) {
            return false;
        }

        try {
            Map<String, List<String>> patternsMap = getPatternsMap();
            patternsMap.put(typeId, new ArrayList<>(patterns));
            savePatternsMap(patternsMap);

            log.debug("📝 Saved {} patterns for type: {}", patterns.size(), typeId);
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to save patterns for type: {}", typeId, e);
            return false;
        }
    }

    @Override
    public boolean addPatterns(String typeId, List<String> patterns) {
        if (typeId == null || patterns == null) {
            return false;
        }

        try {
            Map<String, List<String>> patternsMap = getPatternsMap();
            List<String> existingPatterns = patternsMap.getOrDefault(typeId, new ArrayList<>());

            for (String pattern : patterns) {
                if (!existingPatterns.contains(pattern)) {
                    existingPatterns.add(pattern);
                }
            }

            patternsMap.put(typeId, existingPatterns);
            savePatternsMap(patternsMap);

            log.debug("➕ Added patterns for type: {}", typeId);
            return true;
        } catch (Exception e) {
            log.error("❌ Failed to add patterns for type: {}", typeId, e);
            return false;
        }
    }

    @Override
    public List<String> getPatterns(String typeId) {
        if (typeId == null) {
            return new ArrayList<>();
        }

        Map<String, List<String>> patternsMap = getPatternsMap();
        return new ArrayList<>(patternsMap.getOrDefault(typeId, new ArrayList<>()));
    }

    @Override
    public boolean removePatterns(String typeId, List<String> patterns) {
        if (typeId == null || patterns == null) {
            return false;
        }

        try {
            Map<String, List<String>> patternsMap = getPatternsMap();
            List<String> existingPatterns = patternsMap.get(typeId);

            if (existingPatterns != null) {
                existingPatterns.removeAll(patterns);
                patternsMap.put(typeId, existingPatterns);
                savePatternsMap(patternsMap);

                log.debug("➖ Removed patterns for type: {}", typeId);
                return true;
            }

            return false;
        } catch (Exception e) {
            log.error("❌ Failed to remove patterns for type: {}", typeId, e);
            return false;
        }
    }
}

