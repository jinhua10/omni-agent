package top.yumbo.ai.omni.rag.adapter.embedding;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.ai.api.EmbeddingService;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.IndexStatistics;
import top.yumbo.ai.omni.rag.model.Vector;

import java.util.*;

/**
 * 嵌入服务适配器
 *
 * <p>将 EmbeddingService 适配为 RagService，只提供 embed 功能</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class EmbeddingServiceAdapter implements RagService {

    private final EmbeddingService embeddingService;
    private final String domainId;

    public EmbeddingServiceAdapter(EmbeddingService embeddingService, String domainId) {
        this.embeddingService = embeddingService;
        this.domainId = domainId;
    }

    // ========== 嵌入功能（实现） ==========

    @Override
    public Vector embed(String text) {
        try {
            float[] embedding = embeddingService.embed(text);
            if (embedding == null || embedding.length == 0) {
                log.warn("嵌入服务返回空向量: text={}", text);
                return null;
            }
            return Vector.of(embedding);
        } catch (Exception e) {
            log.error("向量化失败: text={}", text, e);
            return null;
        }
    }

    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        try {
            // 逐个向量化（EmbeddingService 接口没有 batchEmbed 方法）
            List<Vector> vectors = new ArrayList<>();
            for (String text : texts) {
                Vector vector = embed(text);
                vectors.add(vector);
            }
            return vectors;
        } catch (Exception e) {
            log.error("批量向量化失败: count={}", texts.size(), e);
            return Collections.emptyList();
        }
    }

    // ========== 存储功能（不支持） ==========

    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        throw new UnsupportedOperationException("嵌入服务不支持搜索功能");
    }

    @Override
    public List<Document> vectorSearch(Vector vector, int maxResults) {
        throw new UnsupportedOperationException("嵌入服务不支持搜索功能");
    }

    @Override
    public void index(String id, Vector vector, Map<String, Object> metadata) {
        throw new UnsupportedOperationException("嵌入服务不支持索引功能");
    }

    @Override
    public void batchIndex(List<Document> documents) {
        throw new UnsupportedOperationException("嵌入服务不支持索引功能");
    }

    @Override
    public void delete(String id) {
        throw new UnsupportedOperationException("嵌入服务不支持删除功能");
    }

    @Override
    public void clearAll() {
        throw new UnsupportedOperationException("嵌入服务不支持清空功能");
    }

    @Override
    public Optional<Document> getDocument(String documentId) {
        return Optional.empty();
    }

    @Override
    public boolean documentExists(String documentId) {
        return false;
    }

    @Override
    public long getDocumentCount() {
        return 0;
    }

    @Override
    public List<Document> getAllDocuments(int offset, int limit) {
        return Collections.emptyList();
    }

    @Override
    public IndexStatistics getStatistics() {
        IndexStatistics stats = new IndexStatistics();
        stats.setTotalDocuments(0);
        stats.setIndexSize(0);
        return stats;
    }

    @Override
    public boolean isHealthy() {
        return true;
    }

    @Override
    public void rebuildIndex() {
        // 无需操作
    }

    @Override
    public String getDomainId() {
        return domainId;
    }
}

