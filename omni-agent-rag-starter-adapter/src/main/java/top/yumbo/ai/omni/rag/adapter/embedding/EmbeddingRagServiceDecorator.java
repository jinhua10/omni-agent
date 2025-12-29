package top.yumbo.ai.omni.rag.adapter.embedding;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.IndexStatistics;
import top.yumbo.ai.omni.rag.model.Vector;

import java.util.*;

/**
 * 带嵌入功能的 RAG 服务装饰器
 *
 * <p>将嵌入服务和存储服务组合在一起</p>
 * <p>存储服务只负责存储和检索，嵌入服务负责文本向量化</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class EmbeddingRagServiceDecorator implements RagService {

    /**
     * 底层存储服务（ES/MongoDB/H2 等）
     */
    private final RagService storageService;

    /**
     * 嵌入服务（ONNX/Ollama/Online）
     */
    private final RagService embeddingService;

    /**
     * 域 ID
     */
    private final String domainId;

    public EmbeddingRagServiceDecorator(
            RagService storageService,
            RagService embeddingService,
            String domainId) {
        this.storageService = storageService;
        this.embeddingService = embeddingService;
        this.domainId = domainId;

        log.info("✅ 创建带嵌入功能的 RAG 服务装饰器");
        log.info("  - 存储服务: {}", storageService.getClass().getSimpleName());
        log.info("  - 嵌入服务: {}", embeddingService.getClass().getSimpleName());
    }

    // ========== 嵌入功能委托给嵌入服务 ==========

    @Override
    public Vector embed(String text) {
        return embeddingService.embed(text);
    }

    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        return embeddingService.batchEmbed(texts);
    }

    // ========== 语义搜索：先向量化，再搜索 ==========

    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        log.debug("语义搜索: query={}", query);

        // 1. 使用嵌入服务将查询文本向量化
        Vector queryVector = embeddingService.embed(query);
        if (queryVector == null || queryVector.getData() == null) {
            log.warn("查询向量化失败，降级为文本搜索");
            // 降级为纯文本搜索（如果存储服务支持）
            return storageService.semanticSearch(query, maxResults);
        }

        // 2. 使用存储服务进行向量搜索
        return storageService.vectorSearch(queryVector, maxResults);
    }

    @Override
    public List<Document> vectorSearch(Vector vector, int maxResults) {
        return storageService.vectorSearch(vector, maxResults);
    }

    // ========== 文档索引：自动向量化 ==========

    @Override
    public void index(String id, Vector vector, Map<String, Object> metadata) {
        storageService.index(id, vector, metadata);
    }

    @Override
    public void batchIndex(List<Document> documents) {
        embedDocumentsIfNeeded(documents);
        storageService.batchIndex(documents);
    }

    // ========== 其他方法直接委托给存储服务 ==========


    @Override
    public void delete(String id) {
        storageService.delete(id);
    }

    @Override
    public void clearAll() {
        storageService.clearAll();
    }

    @Override
    public Optional<Document> getDocument(String documentId) {
        return storageService.getDocument(documentId);
    }

    @Override
    public boolean documentExists(String documentId) {
        return storageService.documentExists(documentId);
    }

    @Override
    public long getDocumentCount() {
        return storageService.getDocumentCount();
    }

    @Override
    public List<Document> getAllDocuments(int offset, int limit) {
        return storageService.getAllDocuments(offset, limit);
    }

    @Override
    public IndexStatistics getStatistics() {
        return storageService.getStatistics();
    }

    @Override
    public boolean isHealthy() {
        return storageService.isHealthy() && embeddingService.isHealthy();
    }

    @Override
    public void rebuildIndex() {
        storageService.rebuildIndex();
    }

    @Override
    public String getDomainId() {
        return domainId;
    }

    // ========== 辅助方法 ==========

    /**
     * 如果文档没有向量，自动向量化
     */
    private void embedDocumentIfNeeded(Document document) {
        if (document.getEmbedding() != null && document.getEmbedding().length > 0) {
            return; // 已有向量，跳过
        }

        String textToEmbed = buildTextForEmbedding(document);
        if (textToEmbed == null || textToEmbed.isEmpty()) {
            log.warn("文档 {} 没有可向量化的文本", document.getId());
            return;
        }

        Vector vector = embeddingService.embed(textToEmbed);
        if (vector != null && vector.getData() != null) {
            document.setEmbedding(vector.getData());
            log.debug("文档 {} 向量化完成，维度: {}", document.getId(), vector.getData().length);
        } else {
            log.warn("文档 {} 向量化失败", document.getId());
        }
    }

    /**
     * 批量向量化文档
     */
    private void embedDocumentsIfNeeded(List<Document> documents) {
        List<String> textsToEmbed = new ArrayList<>();
        List<Document> docsNeedEmbedding = new ArrayList<>();

        for (Document doc : documents) {
            if (doc.getEmbedding() == null || doc.getEmbedding().length == 0) {
                String text = buildTextForEmbedding(doc);
                if (text != null && !text.isEmpty()) {
                    textsToEmbed.add(text);
                    docsNeedEmbedding.add(doc);
                }
            }
        }

        if (textsToEmbed.isEmpty()) {
            return; // 所有文档都已有向量
        }

        log.debug("批量向量化 {} 个文档", textsToEmbed.size());

        // 批量向量化
        List<Vector> vectors = embeddingService.batchEmbed(textsToEmbed);

        if (vectors.size() != docsNeedEmbedding.size()) {
            log.warn("批量向量化结果数量不匹配: expected={}, actual={}",
                    docsNeedEmbedding.size(), vectors.size());
            return;
        }

        // 将向量设置到文档
        for (int i = 0; i < docsNeedEmbedding.size(); i++) {
            Document doc = docsNeedEmbedding.get(i);
            Vector vector = vectors.get(i);
            if (vector != null && vector.getData() != null) {
                doc.setEmbedding(vector.getData());
            }
        }

        log.debug("批量向量化完成");
    }

    /**
     * 构建用于向量化的文本
     *
     * <p>优先级: title + content > content > title > summary</p>
     */
    private String buildTextForEmbedding(Document document) {
        StringBuilder sb = new StringBuilder();

        if (document.getTitle() != null && !document.getTitle().isEmpty()) {
            sb.append(document.getTitle()).append("\n");
        }

        if (document.getContent() != null && !document.getContent().isEmpty()) {
            sb.append(document.getContent());
        } else if (document.getSummary() != null && !document.getSummary().isEmpty()) {
            sb.append(document.getSummary());
        }

        String result = sb.toString().trim();
        return result.isEmpty() ? null : result;
    }
}

