package top.yumbo.ai.omni.web.service;

import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.web.model.RAGStrategyTemplate;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 系统RAG配置管理服务
 * (System RAG Configuration Management Service)
 *
 * 管理RAG流程的全局配置：
 * - 是否自动文本化
 * - 是否自动分块
 * - 默认策略
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@Service
public class SystemRAGConfigService {

    // 系统配置（可持久化到数据库）
    private final SystemRAGConfig config = new SystemRAGConfig();

    // 文档级别的配置覆盖
    private final Map<String, DocumentRAGConfig> documentConfigs = new ConcurrentHashMap<>();

    // ⭐ 策略模板存储
    private final Map<String, RAGStrategyTemplate> strategyTemplates = new ConcurrentHashMap<>();

    /**
     * 获取系统RAG配置
     */
    public SystemRAGConfig getSystemConfig() {
        return config;
    }

    /**
     * 是否自动文本化
     */
    public boolean isAutoTextExtraction() {
        return config.isAutoTextExtraction();
    }

    /**
     * 设置是否自动文本化
     */
    public void setAutoTextExtraction(boolean autoTextExtraction) {
        config.setAutoTextExtraction(autoTextExtraction);
        log.info("⚙️ 系统配置更新: 自动文本化={}", autoTextExtraction);
    }

    /**
     * 是否自动RAG
     */
    public boolean isAutoRAG() {
        return config.isAutoRAG();
    }

    /**
     * 设置是否自动RAG
     */
    public void setAutoRAG(boolean autoRAG) {
        config.setAutoRAG(autoRAG);
        log.info("⚙️ 系统配置更新: 自动RAG={}", autoRAG);
    }

    /**
     * 获取默认文本提取模型
     */
    public String getDefaultTextExtractionModel() {
        return config.getDefaultTextExtractionModel();
    }

    /**
     * 设置默认文本提取模型
     */
    public void setDefaultTextExtractionModel(String model) {
        config.setDefaultTextExtractionModel(model);
        log.info("⚙️ 系统配置更新: 默认文本提取模型={}", model);
    }

    /**
     * 获取默认分块策略
     */
    public String getDefaultChunkingStrategy() {
        return config.getDefaultChunkingStrategy();
    }

    /**
     * 设置默认分块策略
     */
    public void setDefaultChunkingStrategy(String strategy) {
        config.setDefaultChunkingStrategy(strategy);
        log.info("⚙️ 系统配置更新: 默认分块策略={}", strategy);
    }

    /**
     * 获取文档的RAG配置
     */
    public DocumentRAGConfig getDocumentConfig(String documentId) {
        return documentConfigs.computeIfAbsent(documentId, k -> {
            DocumentRAGConfig docConfig = new DocumentRAGConfig();
            docConfig.setDocumentId(documentId);
            docConfig.setTextExtractionModel(config.getDefaultTextExtractionModel());
            docConfig.setChunkingStrategy(config.getDefaultChunkingStrategy());
            docConfig.setStatus("PENDING");
            return docConfig;
        });
    }

    /**
     * 设置文档的RAG配置
     */
    public void setDocumentConfig(String documentId, DocumentRAGConfig docConfig) {
        docConfig.setDocumentId(documentId);
        documentConfigs.put(documentId, docConfig);
        log.info("📝 文档配置更新: documentId={}, config={}", documentId, docConfig);
    }

    /**
     * 更新文档状态
     */
    public void updateDocumentStatus(String documentId, String status) {
        DocumentRAGConfig docConfig = getDocumentConfig(documentId);
        docConfig.setStatus(status);
        log.info("📊 文档状态更新: documentId={}, status={}", documentId, status);
    }

    /**
     * 移除文档配置
     */
    public void removeDocumentConfig(String documentId) {
        documentConfigs.remove(documentId);
        log.info("🗑️ 文档配置已移除: documentId={}", documentId);
    }

    /**
     * 获取所有文档的配置状态
     */
    public Map<String, DocumentRAGConfig> getAllDocumentsStatus() {
        return new HashMap<>(documentConfigs);
    }

    /**
     * 保存策略模板
     */
    public RAGStrategyTemplate saveStrategyTemplate(RAGStrategyTemplate template) {
        if (template.getTemplateId() == null || template.getTemplateId().isEmpty()) {
            template.setTemplateId("template_" + System.currentTimeMillis());
        }
        template.setUpdatedAt(System.currentTimeMillis());
        if (template.getCreatedAt() == 0) {
            template.setCreatedAt(System.currentTimeMillis());
        }

        strategyTemplates.put(template.getTemplateId(), template);
        log.info("💾 保存策略模板: {} - {}", template.getTemplateId(), template.getTemplateName());
        return template;
    }

    /**
     * 获取所有策略模板
     */
    public List<RAGStrategyTemplate> getAllStrategyTemplates() {
        return new ArrayList<>(strategyTemplates.values());
    }

    /**
     * 获取指定策略模板
     */
    public RAGStrategyTemplate getStrategyTemplate(String templateId) {
        return strategyTemplates.get(templateId);
    }

    /**
     * 删除策略模板
     */
    public void deleteStrategyTemplate(String templateId) {
        strategyTemplates.remove(templateId);
        log.info("🗑️ 删除策略模板: {}", templateId);
    }

    /**
     * 应用策略模板到文档
     */
    public void applyTemplateToDocument(String documentId, String templateId) {
        RAGStrategyTemplate template = strategyTemplates.get(templateId);
        if (template == null) {
            throw new IllegalArgumentException("策略模板不存在: " + templateId);
        }

        DocumentRAGConfig docConfig = getDocumentConfig(documentId);
        docConfig.setTextExtractionModel(template.getTextExtractionModel());
        docConfig.setChunkingStrategy(template.getChunkingStrategy());
        docConfig.setChunkingParams(template.getChunkingParams());
        docConfig.setUpdatedAt(System.currentTimeMillis());

        // 增加模板使用次数
        template.setUseCount(template.getUseCount() + 1);

        log.info("📋 应用策略模板 {} 到文档 {}", template.getTemplateName(), documentId);
    }

    /**
     * 系统RAG配置
     */
    @Data
    public static class SystemRAGConfig {
        // 是否自动文本化
        private boolean autoTextExtraction = false;  // 默认不自动

        // 是否自动RAG（分块+向量化+索引）
        private boolean autoRAG = false;  // 默认不自动

        // 默认文本提取模型
        private String defaultTextExtractionModel = "standard";  // standard, vision-llm, ocr

        // 默认分块策略
        private String defaultChunkingStrategy = "fixed-size";  // fixed-size, semantic, ppl, paragraph

        // 默认分块参数
        private Map<String, Object> defaultChunkingParams = new HashMap<>();
    }

    /**
     * 文档级别的RAG配置
     */
    @Data
    public static class DocumentRAGConfig {
        private String documentId;

        // 文档当前状态
        // PENDING - 等待配置
        // EXTRACTING - 文本提取中
        // EXTRACTED - 文本提取完成
        // CHUNKING - 分块中
        // CHUNKED - 分块完成
        // VECTORIZING - 向量化中
        // INDEXING - 索引中
        // COMPLETED - 完成
        // FAILED - 失败
        private String status = "PENDING";

        // 文本提取模型
        private String textExtractionModel;

        // 分块策略
        private String chunkingStrategy;

        // 分块参数
        private Map<String, Object> chunkingParams = new HashMap<>();

        // 提取的文本内容（缓存）
        private String extractedText;

        // 错误信息
        private String errorMessage;

        // 创建时间
        private long createdAt = System.currentTimeMillis();

        // 更新时间
        private long updatedAt = System.currentTimeMillis();
    }
}
