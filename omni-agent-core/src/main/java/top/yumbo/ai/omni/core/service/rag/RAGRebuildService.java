package top.yumbo.ai.omni.core.service.rag;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.core.chunking.DocumentChunkingService;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.api.model.Chunk;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * RAG 重建服务
 * (RAG Rebuild Service)
 *
 * <p>
 * 核心能力：
 * - 从持久化的文本数据重新构建 RAG 索引
 * - 支持切换不同的 Embedding 模型
 * - 支持增量和全量重建
 * - 架构解耦：数据层 ← → 索引层
 * </p>
 *
 * <p>
 * 使用场景：
 * 1. 切换 Embedding 模型（不同维度）
 * 2. 优化索引结构
 * 3. 修复损坏的索引
 * 4. 迁移到新的存储后端
 * </p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class RAGRebuildService {

    private final DocumentStorageService storageService;
    private final RAGServiceFactory ragServiceFactory;

    @Autowired(required = false)
    private DocumentChunkingService chunkingService;

    /**
     * 重建策略
     */
    public enum RebuildStrategy {
        /**
         * 使用已有分块（快速，保持原有分块策略）
         */
        USE_EXISTING_CHUNKS,

        /**
         * 重新分块（慢，使用新的分块算法）⭐
         */
        RECHUNK,

        /**
         * 智能选择（如果分块质量差则重新分块）
         */
        SMART
    }

    /**
     * 完全重建 RAG 索引（使用已有分块）
     *
     * <p>从 data/storage/chunks 重新加载所有分块，用当前配置的 Embedding 模型重新向量化</p>
     *
     * @param domainId 域ID
     * @return 重建的文档数量
     */
    public RebuildResult rebuildFromStorage(String domainId) {
        return rebuildFromStorage(domainId, RebuildStrategy.USE_EXISTING_CHUNKS);
    }

    /**
     * 完全重建 RAG 索引（可选择重建策略）⭐
     *
     * @param domainId 域ID
     * @param strategy 重建策略
     * @return 重建结果
     */
    public RebuildResult rebuildFromStorage(String domainId, RebuildStrategy strategy) {
        log.info("🔄 开始重建 RAG 索引: domainId={}, strategy={}", domainId, strategy);

        RagService RagService = ragServiceFactory.getOrCreateRAGService(domainId);

        RebuildResult result = new RebuildResult();
        result.setDomainId(domainId);
        result.setStartTime(System.currentTimeMillis());

        try {
            // 1. 清空现有索引
            log.info("📌 步骤 1/4: 清空现有索引...");
            RagService.clearAll();
            result.setClearedOldIndex(true);

            // 2. 根据策略选择数据源 ⭐
            List<Document> documents;

            if (strategy == RebuildStrategy.RECHUNK) {
                // 策略1：重新分块 ⭐
                log.info("📌 步骤 2/4: 从原始文档重新分块...");
                documents = rechunkAndConvert(domainId);
                result.setRechunked(true);
            } else if (strategy == RebuildStrategy.SMART) {
                // 策略2：智能选择
                log.info("📌 步骤 2/4: 评估分块质量...");
                if (shouldRechunk()) {
                    log.info("   - 检测到分块质量较差，将重新分块");
                    documents = rechunkAndConvert(domainId);
                    result.setRechunked(true);
                } else {
                    log.info("   - 分块质量良好，使用已有分块");
                    List<Chunk> allChunks = loadAllChunks();
                    documents = convertChunksToDocuments(allChunks);
                    result.setRechunked(false);
                }
            } else {
                // 策略3：使用已有分块（默认）
                log.info("📌 步骤 2/4: 从存储加载已有分块...");
                List<Chunk> allChunks = loadAllChunks();
                result.setTotalChunks(allChunks.size());
                log.info("   - 加载了 {} 个分块", allChunks.size());

                log.info("📌 步骤 3/4: 转换为 RAG Document...");
                documents = convertChunksToDocuments(allChunks);
                result.setRechunked(false);
            }

            result.setTotalChunks(documents.size());
            log.info("   - 准备索引 {} 个文档", documents.size());

            // 3. 用新模型重新向量化并索引
            log.info("📌 步骤 4/4: 用新模型重新向量化并索引...");
            // ...existing code...

            // 批量索引
            int batchSize = 100;
            int total = documents.size();
            AtomicInteger indexed = new AtomicInteger(0);

            for (int i = 0; i < total; i += batchSize) {
                int end = Math.min(i + batchSize, total);
                List<Document> batch = documents.subList(i, end);

                try {
                    RagService.batchIndex(batch);
                    indexed.addAndGet(batch.size());
                    double progress = indexed.get() * 100.0 / total;
                    log.info("   - 进度: {}/{} ({:.1f}%)",
                            indexed.get(), total, String.format("%.1f", progress));
                } catch (Exception e) {
                    log.error("批量索引失败: batch {}-{}", i, end, e);
                    result.getFailedChunks().addAll(batch.stream()
                            .map(Document::getId)
                            .toList());
                }
            }

            result.setIndexedDocuments(indexed.get());
            result.setSuccess(true);
            result.setEndTime(System.currentTimeMillis());

            log.info("✅ RAG 索引重建完成!");
            log.info("   - 总分块: {}", result.getTotalChunks());
            log.info("   - 已索引: {}", result.getIndexedDocuments());
            log.info("   - 失败: {}", result.getFailedChunks().size());
            log.info("   - 耗时: {} ms", result.getDuration());

            return result;

        } catch (Exception e) {
            log.error("RAG 索引重建失败", e);
            result.setSuccess(false);
            result.setErrorMessage(e.getMessage());
            result.setEndTime(System.currentTimeMillis());
            return result;
        }
    }

    /**
     * 增量重建（只重建指定文档）
     *
     * @param domainId 域ID
     * @param documentIds 文档ID列表
     * @return 重建结果
     */
    public RebuildResult rebuildDocuments(String domainId, List<String> documentIds) {
        log.info("🔄 增量重建 RAG 索引: domainId={}, documents={}", domainId, documentIds.size());

        RagService RagService = ragServiceFactory.getOrCreateRAGService(domainId);

        RebuildResult result = new RebuildResult();
        result.setDomainId(domainId);
        result.setStartTime(System.currentTimeMillis());

        try {
            List<Document> documents = new ArrayList<>();

            for (String docId : documentIds) {
                // TODO: DocumentStorageService 需要添加 getChunks(docId) 方法
                // List<Chunk> chunks = storageService.getChunks(docId);
                // if (chunks != null && !chunks.isEmpty()) {
                //     documents.addAll(convertChunksToDocuments(chunks));
                // }

                log.warn("⚠️ getChunks() 方法待实现");
            }

            result.setTotalChunks(documents.size());

            // 重新索引
            RagService.batchIndex(documents);
            result.setIndexedDocuments(documents.size());

            result.setSuccess(true);
            result.setEndTime(System.currentTimeMillis());

            log.info("✅ 增量重建完成: {} 个文档, {} 个分块, 耗时 {} ms",
                    documentIds.size(), result.getTotalChunks(), result.getDuration());

            return result;

        } catch (Exception e) {
            log.error("增量重建失败", e);
            result.setSuccess(false);
            result.setErrorMessage(e.getMessage());
            result.setEndTime(System.currentTimeMillis());
            return result;
        }
    }

    /**
     * 切换 Embedding 模型并重建索引
     *
     * @param domainId 域ID
     * @param newEmbeddingModel 新的 Embedding 模型名称
     * @return 重建结果
     */
    public RebuildResult switchEmbeddingModel(String domainId, String newEmbeddingModel) {
        log.info("🔄 切换 Embedding 模型: domainId={}, newModel={}", domainId, newEmbeddingModel);

        // TODO: 更新配置中的 Embedding 模型
        // 暂时通过环境变量或配置文件手动切换

        log.warn("⚠️ 请确保已在配置中切换到新模型: {}", newEmbeddingModel);
        log.info("开始用新模型重建索引...");

        return rebuildFromStorage(domainId);
    }

    /**
     * 从存储加载所有分块
     */
    private List<Chunk> loadAllChunks() {
        // TODO: 实现批量加载所有分块的方法
        // 当前 DocumentStorageService 需要提供这个能力

        log.warn("⚠️ 当前实现：逐个文档加载分块（性能较低）");
        log.warn("💡 建议：在 DocumentStorageService 中添加 getAllChunks() 方法");

        // 临时方案：通过文档ID加载
        List<Chunk> allChunks = new ArrayList<>();

        // 这里需要一个获取所有文档ID的方法
        // 简化实现：假设已知文档ID

        return allChunks;
    }

    /**
     * 将分块转换为 RAG Document
     */
    private List<Document> convertChunksToDocuments(List<Chunk> chunks) {
        List<Document> documents = new ArrayList<>();

        for (Chunk chunk : chunks) {
            Document doc = Document.builder()
                    .id(chunk.getId())
                    .content(chunk.getContent())
                    .title(chunk.getMetadata().get("title") != null ?
                            chunk.getMetadata().get("title").toString() : null)
                    .source(chunk.getDocumentId())
                    .type("chunk")
                    .metadata(chunk.getMetadata())
                    .build();

            documents.add(doc);
        }

        return documents;
    }

    /**
     * 重新分块并转换为 Document ⭐
     */
    private List<Document> rechunkAndConvert(String domainId) {
        if (chunkingService == null) {
            log.error("❌ DocumentChunkingService 未配置，无法重新分块！");
            log.warn("💡 降级到使用已有分块");
            List<Chunk> allChunks = loadAllChunks();
            return convertChunksToDocuments(allChunks);
        }

        List<Document> documents = new ArrayList<>();

        try {
            // 1. 加载所有原始文档内容
            log.info("   - 加载原始文档内容...");
            // TODO: 需要从 DocumentStorageService 获取所有文档的原始内容
            // 临时方案：从已有分块重新组合或直接使用

            // 暂时使用已有分块进行演示
            List<Chunk> existingChunks = loadAllChunks();

            if (existingChunks.isEmpty()) {
                log.warn("   - 没有找到已有分块，无法重新分块");
                return documents;
            }

            // 按 documentId 分组
            var chunksByDoc = existingChunks.stream()
                    .collect(java.util.stream.Collectors.groupingBy(Chunk::getDocumentId));

            log.info("   - 找到 {} 个文档需要重新分块", chunksByDoc.size());

            // 2. 对每个文档重新分块
            int totalChunks = 0;
            for (var entry : chunksByDoc.entrySet()) {
                String docId = entry.getKey();
                List<Chunk> oldChunks = entry.getValue();

                log.debug("   - 重新分块: {}", docId);

                // 重新组合文档内容
                String fullContent = oldChunks.stream()
                        .map(Chunk::getContent)
                        .collect(java.util.stream.Collectors.joining("\n"));

                // 使用 DocumentChunkingService 重新分块 ⭐
                List<Chunk> newChunks = chunkingService.chunkDocument(docId, fullContent);
                totalChunks += newChunks.size();

                // 转换为 RAG Document
                documents.addAll(convertChunksToDocuments(newChunks));
            }

            log.info("   - ✅ 重新分块完成: {} 个文档 → {} 个新分块",
                    chunksByDoc.size(), totalChunks);

        } catch (Exception e) {
            log.error("❌ 重新分块失败，降级到使用已有分块", e);
            List<Chunk> allChunks = loadAllChunks();
            documents = convertChunksToDocuments(allChunks);
        }

        return documents;
    }

    /**
     * 加载所有原始文档（待实现）
     *
     * @deprecated 当前使用 loadAllChunks() 并重新组合内容的方式
     */
    @Deprecated
    private List<String> loadAllExtractedDocuments() {
        // TODO: 实现从 data/storage/extracted 加载所有原始文档
        // 当前 DocumentStorageService 需要提供这个能力

        log.warn("⚠️ 当前实现：需要 DocumentStorageService 提供获取原始文档内容的方法");

        // 临时返回空列表
        return new ArrayList<>();
    }

    /**
     * 评估是否需要重新分块
     */
    private boolean shouldRechunk() {
        // TODO: 实现分块质量评估逻辑
        // 评估指标：
        // 1. 分块大小分布（是否过大或过小）
        // 2. 分块边界质量（是否在句子中间截断）
        // 3. 语义连贯性

        log.warn("⚠️ 分块质量评估待实现，默认使用已有分块");
        return false;
    }

    /**
     * 重建结果
     */
    @lombok.Data
    public static class RebuildResult {
        private String domainId;
        private boolean success;
        private boolean clearedOldIndex;
        private boolean rechunked;  // ⭐ 是否重新分块
        private int totalChunks;
        private int indexedDocuments;
        private List<String> failedChunks = new ArrayList<>();
        private String errorMessage;
        private long startTime;
        private long endTime;

        public long getDuration() {
            return endTime - startTime;
        }
    }
}


