package top.yumbo.ai.omni.web.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.type.MapType;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.web.model.RAGStrategyTemplate;
import top.yumbo.ai.storage.api.DocumentStorageService;

import java.io.File;
import java.io.IOException;
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
@RequiredArgsConstructor
public class SystemRAGConfigService {

    private final DocumentStorageService storageService;  // ⭐ 新增
    private final ObjectMapper objectMapper = new ObjectMapper();  // JSON序列化

    @Value("${omni.rag.config.persistence.path:./data/config/rag-configs.json}")
    private String configPersistencePath;

    // 系统配置（可持久化到数据库）
    private final SystemRAGConfig config = new SystemRAGConfig();

    // 文档级别的配置覆盖
    private final Map<String, DocumentRAGConfig> documentConfigs = new ConcurrentHashMap<>();

    // ⭐ 策略模板存储
    private final Map<String, RAGStrategyTemplate> strategyTemplates = new ConcurrentHashMap<>();

    /**
     * 应用启动时加载持久化配置
     */
    @PostConstruct
    public void loadPersistedConfigs() {
        try {
            File configFile = new File(configPersistencePath);
            if (configFile.exists()) {
                MapType mapType = objectMapper.getTypeFactory()
                    .constructMapType(HashMap.class, String.class, DocumentRAGConfig.class);
                Map<String, DocumentRAGConfig> loadedConfigs = objectMapper.readValue(configFile, mapType);
                documentConfigs.putAll(loadedConfigs);
                log.info("✅ 已加载 {} 个文档配置", loadedConfigs.size());
            } else {
                log.info("ℹ️ 配置文件不存在，将使用空配置: {}", configPersistencePath);
            }
        } catch (IOException e) {
            log.error("❌ 加载持久化配置失败: {}", configPersistencePath, e);
        }
    }

    /**
     * 应用关闭时保存配置
     */
    @PreDestroy
    public void savePersistedConfigs() {
        persistConfigs();
    }

    /**
     * 持久化配置到文件
     */
    private void persistConfigs() {
        try {
            File configFile = new File(configPersistencePath);
            // 确保父目录存在
            if (configFile.getParentFile() != null) {
                configFile.getParentFile().mkdirs();
            }
            objectMapper.writerWithDefaultPrettyPrinter().writeValue(configFile, documentConfigs);
            log.debug("💾 已保存 {} 个文档配置到: {}", documentConfigs.size(), configPersistencePath);
        } catch (IOException e) {
            log.error("❌ 持久化配置失败: {}", configPersistencePath, e);
        }
    }

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
        docConfig.setUpdatedAt(System.currentTimeMillis());  // 更新时间戳
        documentConfigs.put(documentId, docConfig);
        log.info("📝 文档配置更新: documentId={}, config={}", documentId, docConfig);
        // ⭐ 实时持久化（避免数据丢失）
        persistConfigs();
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
     * 获取提取的完整文本 ⭐
     * 优先从存储服务获取，fallback到配置中的缓存
     */
    public Optional<String> getExtractedText(String documentId) {
        DocumentRAGConfig config = getDocumentConfig(documentId);

        // 1. 优先从存储服务获取（新方式）
        if (config.getExtractedTextRef() != null) {
            Optional<String> text = storageService.getExtractedText(documentId);
            if (text.isPresent()) {
                log.debug("✅ 从存储服务获取提取文本: documentId={}, length={}",
                          documentId, text.get().length());
                return text;
            }
        }

        // 2. Fallback到配置中的缓存（旧方式，向后兼容）
        if (config.getExtractedText() != null) {
            log.debug("⚠️ 从配置缓存获取提取文本（旧方式）: documentId={}, length={}",
                      documentId, config.getExtractedText().length());
            return Optional.of(config.getExtractedText());
        }

        log.warn("❌ 未找到提取文本: documentId={}", documentId);
        return Optional.empty();
    }

    /**
     * 获取所有文档的配置状态（原方法）
     */
    public Map<String, DocumentRAGConfig> getAllDocumentConfigs() {
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

        // ⭐ 提取文本的摘要（前200字符，用于快速预览）
        private String textSummary;

        // ⭐ 提取文本的引用（documentId，用于从存储服务获取完整文本）
        private String extractedTextRef;

        // 提取的文本内容（缓存）⚠️ 保留用于向后兼容，新代码应使用存储服务
        private String extractedText;

        // 提取精度（0.0-1.0）
        private Double extractionAccuracy;

        // 错误信息
        private String errorMessage;

        // 创建时间
        private long createdAt = System.currentTimeMillis();

        // 更新时间
        private long updatedAt = System.currentTimeMillis();
    }
}
