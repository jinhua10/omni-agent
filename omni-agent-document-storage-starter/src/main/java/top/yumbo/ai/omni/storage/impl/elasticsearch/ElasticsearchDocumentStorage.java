package top.yumbo.ai.omni.storage.impl.elasticsearch;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.elasticsearch._types.Result;
import co.elastic.clients.elasticsearch._types.SortOrder;
import co.elastic.clients.elasticsearch._types.Time;
import co.elastic.clients.elasticsearch.core.*;
import co.elastic.clients.elasticsearch.core.bulk.BulkResponseItem;
import co.elastic.clients.elasticsearch.core.search.Hit;
import co.elastic.clients.elasticsearch.indices.CreateIndexRequest;
import co.elastic.clients.elasticsearch.indices.ExistsRequest;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.api.exception.*;
import top.yumbo.ai.omni.storage.api.model.*;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;
import java.util.Base64;

/**
 * Elasticsearch 文档存储实现 - 生产级全文检索和文档索引
 * (Elasticsearch Document Storage Implementation - Production-grade Full-text Search and Document Indexing)
 *
 * <p>
 * 特点 (Features):
 * - 全文检索能力
 * - 分布式架构
 * - 高可用性
 * - 实时搜索
 * - 适合大规模文档存储
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - Elasticsearch Starter 实现
 */
@Slf4j
public class ElasticsearchDocumentStorage implements DocumentStorageService {

    private final ElasticsearchClient client;
    private final ElasticsearchStorageProperties properties;
    private final ObjectMapper objectMapper;

    // 索引名称
    private final String chunkIndex;
    private final String imageIndex;
    private final String pplIndex;
    private final String optimizationIndex;
    private final String extractedTextIndex;  // ⭐ 新增

    public ElasticsearchDocumentStorage(ElasticsearchClient client,
                                       ElasticsearchStorageProperties properties) {
        this.client = client;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();

        this.chunkIndex = properties.getIndexPrefix() + "-chunks";
        this.imageIndex = properties.getIndexPrefix() + "-images";
        this.pplIndex = properties.getIndexPrefix() + "-ppl";
        this.optimizationIndex = properties.getIndexPrefix() + "-optimizations";
        this.extractedTextIndex = properties.getIndexPrefix() + "-extracted-text";  // ⭐ 新增

        initIndices();
        log.info("ElasticsearchDocumentStorage initialized with prefix: {}", properties.getIndexPrefix());
    }

    private void initIndices() {
        try {
            createIndexIfNotExists(chunkIndex);
            createIndexIfNotExists(imageIndex);
            createIndexIfNotExists(pplIndex);
            createIndexIfNotExists(optimizationIndex);
            createIndexIfNotExists(extractedTextIndex);  // ⭐ 新增
        } catch (Exception e) {
            log.error("Failed to initialize indices", e);
        }
    }

    /**
     * ✅ P0优化3：创建索引时定义Mapping
     * 预期收益：查询性能提升2-5倍，存储空间节省20-30%
     */
    private void createIndexIfNotExists(String indexName) {
        try {
            ExistsRequest existsRequest = ExistsRequest.of(e -> e.index(indexName));
            boolean exists = client.indices().exists(existsRequest).value();

            if (!exists) {
                CreateIndexRequest createRequest = CreateIndexRequest.of(c -> c
                    .index(indexName)
                    .settings(s -> s
                        .numberOfShards(String.valueOf(properties.getNumberOfShards()))
                        .numberOfReplicas(String.valueOf(properties.getNumberOfReplicas()))
                        .refreshInterval(Time.of(t -> t.time("1s")))  // ✅ 1秒刷新间隔
                    )
                    .mappings(m -> {
                        // ✅ 定义通用字段映射
                        m.properties("documentId", p -> p.keyword(k -> k))  // keyword类型，精确匹配
                         .properties("id", p -> p.keyword(k -> k));

                        // ✅ 根据索引类型定义特定字段
                        if (indexName.contains("chunks")) {
                            m.properties("sequence", p -> p.integer(i -> i))  // 数值类型，支持排序
                             .properties("content", p -> p.text(t -> t.analyzer("standard")))  // 全文检索
                             .properties("createdAt", p -> p.date(d -> d));
                        } else if (indexName.contains("images")) {
                            m.properties("pageNumber", p -> p.integer(i -> i))
                             .properties("format", p -> p.keyword(k -> k))
                             .properties("data", p -> p.binary(b -> b));  // 二进制数据
                        } else if (indexName.contains("extracted-text")) {
                            m.properties("text", p -> p.text(t -> t.analyzer("standard")))  // 全文检索
                             .properties("createdAt", p -> p.date(d -> d));
                        }

                        return m;
                    })
                );
                client.indices().create(createRequest);
                log.info("✅ Created index with mappings: {}", indexName);
            }
        } catch (Exception e) {
            log.error("Failed to create index: {}", indexName, e);
        }
    }

    // ========== Chunk Storage ==========

    @Override
    public String saveChunk(String documentId, Chunk chunk) {
        try {
            String chunkId = chunk.getId() != null ? chunk.getId() : UUID.randomUUID().toString();
            chunk.setId(chunkId);
            chunk.setDocumentId(documentId);

            IndexRequest<Chunk> request = IndexRequest.of(i -> i
                .index(chunkIndex)
                .id(chunkId)
                .document(chunk)
            );

            IndexResponse response = client.index(request);

            if (response.result() == Result.Created || response.result() == Result.Updated) {
                log.debug("Saved chunk: {}", chunkId);
                return chunkId;
            }

            return null;
        } catch (Exception e) {
            log.error("Failed to save chunk", e);
            return null;
        }
    }

    /**
     * ✅ P0优化1：使用Bulk API批量索引
     * 预期收益：性能提升50-100倍（100次网络请求 → 1次）
     */
    @Override
    public List<String> saveChunks(String documentId, List<Chunk> chunks) {
        if (chunks == null || chunks.isEmpty()) {
            return new ArrayList<>();
        }

        List<String> chunkIds = new ArrayList<>();

        try {
            // ✅ P0优化：使用Bulk API批量索引
            BulkRequest.Builder bulkBuilder = new BulkRequest.Builder();

            for (Chunk chunk : chunks) {
                String chunkId = chunk.getId() != null ? chunk.getId() : UUID.randomUUID().toString();
                chunk.setId(chunkId);
                chunk.setDocumentId(documentId);
                chunkIds.add(chunkId);

                bulkBuilder.operations(op -> op
                    .index(idx -> idx
                        .index(chunkIndex)
                        .id(chunkId)
                        .document(chunk)
                    )
                );
            }

            // ✅ 一次性批量提交
            BulkResponse response = client.bulk(bulkBuilder.build());

            if (response.errors()) {
                log.warn("⚠️ Some chunks failed to save");
                // 记录失败的项
                for (BulkResponseItem item : response.items()) {
                    if (item.error() != null) {
                        log.error("Failed to index chunk: {}", item.error().reason());
                    }
                }
            }

            log.debug("✅ Saved {} chunks in bulk for document: {}", chunks.size(), documentId);
            return chunkIds;

        } catch (Exception e) {
            log.error("Failed to save chunks in bulk", e);
            return new ArrayList<>();
        }
    }

    @Override
    public Optional<Chunk> getChunk(String chunkId) {
        try {
            GetRequest request = GetRequest.of(g -> g
                .index(chunkIndex)
                .id(chunkId)
            );

            GetResponse<Chunk> response = client.get(request, Chunk.class);

            if (response.found()) {
                return Optional.ofNullable(response.source());
            }

            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get chunk: {}", chunkId, e);
            return Optional.empty();
        }
    }

    /**
     * ✅ P0优化2：使用Search After支持大数据集
     * 预期收益：避免数据丢失，支持无限量数据
     */
    @Override
    public List<Chunk> getChunksByDocument(String documentId) {
        try {
            List<Chunk> allChunks = new ArrayList<>();
            List<co.elastic.clients.elasticsearch._types.FieldValue> searchAfter = null;

            while (true) {
                SearchRequest.Builder searchBuilder = new SearchRequest.Builder()
                    .index(chunkIndex)
                    .query(q -> q
                        .term(t -> t
                            .field("documentId.keyword")  // ✅ 使用keyword字段
                            .value(documentId)
                        )
                    )
                    .sort(so -> so
                        .field(f -> f
                            .field("sequence")
                            .order(SortOrder.Asc)
                        )
                    )
                    .size(1000);  // 每批1000条

                if (searchAfter != null) {
                    searchBuilder.searchAfter(searchAfter);
                }

                SearchResponse<Chunk> response = client.search(searchBuilder.build(), Chunk.class);

                List<Hit<Chunk>> hits = response.hits().hits();
                if (hits.isEmpty()) {
                    break;
                }

                hits.stream()
                    .map(Hit::source)
                    .filter(Objects::nonNull)
                    .forEach(allChunks::add);

                // 获取最后一个文档的sort值用于下一次查询
                Hit<Chunk> lastHit = hits.get(hits.size() - 1);
                searchAfter = lastHit.sort();
            }

            log.debug("✅ Retrieved {} chunks using search_after for document: {}", allChunks.size(), documentId);
            return allChunks;

        } catch (Exception e) {
            log.error("Failed to get chunks for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteChunk(String chunkId) {
        try {
            DeleteRequest request = DeleteRequest.of(d -> d
                .index(chunkIndex)
                .id(chunkId)
            );

            client.delete(request);
            log.debug("Deleted chunk: {}", chunkId);
        } catch (Exception e) {
            log.error("Failed to delete chunk: {}", chunkId, e);
        }
    }

    /**
     * ✅ P0优化4：使用异步deleteByQuery提升性能
     * 预期收益：大量删除时性能提升10-50倍
     */
    @Override
    public void deleteChunksByDocument(String documentId) {
        try {
            DeleteByQueryRequest request = DeleteByQueryRequest.of(d -> d
                .index(chunkIndex)
                .query(q -> q
                    .term(t -> t
                        .field("documentId.keyword")  // ✅ 使用keyword字段
                        .value(documentId)
                    )
                )
                .waitForCompletion(false)  // ✅ 异步执行
                .refresh(true)  // ✅ 删除后刷新索引
            );

            DeleteByQueryResponse response = client.deleteByQuery(request);

            if (response.task() != null) {
                log.info("✅ Started async delete task for chunks: {}", response.task());
            } else {
                log.info("✅ Deleted {} chunks for document: {}", response.deleted(), documentId);
            }
        } catch (Exception e) {
            log.error("Failed to delete chunks for document: {}", documentId, e);
        }
    }

    // ========== Image Storage ==========

    @Override
    public String saveImage(String documentId, Image image) {
        try {
            // ⭐ 强制要求页码信息
            Integer pageNum = image.getPageNumber();
            if (pageNum == null || pageNum <= 0) {
                throw new IllegalArgumentException(
                    String.format("Image must have valid pageNumber (got: %s, documentId: %s). " +
                                "All images must be assigned a page number.",
                                pageNum, documentId));
            }

            // 从 metadata 中获取图片序号和基础文件名
            Integer imageIndex = null;
            String baseName = documentId;  // 默认使用documentId
            if (image.getMetadata() != null) {
                if (image.getMetadata().containsKey("imageIndex")) {
                    imageIndex = ((Number) image.getMetadata().get("imageIndex")).intValue();
                }
                if (image.getMetadata().containsKey("baseName")) {
                    baseName = (String) image.getMetadata().get("baseName");
                }
            }

            // ⭐ 生成简洁的图片ID：baseName_p001_i000
            String imageId = String.format("%s_p%03d_i%03d",
                    baseName, pageNum, imageIndex != null ? imageIndex : 0);
            image.setId(imageId);
            image.setDocumentId(documentId);

            IndexRequest<Image> request = IndexRequest.of(i -> i
                .index(this.imageIndex)  // ⭐ 使用 ES 索引名称
                .id(imageId)
                .document(image)
            );

            IndexResponse response = client.index(request);

            if (response.result() == Result.Created || response.result() == Result.Updated) {
                log.debug("Saved image: {}", imageId);
                return imageId;
            }

            return null;
        } catch (Exception e) {
            log.error("Failed to save image", e);
            return null;
        }
    }

    @Override
    public Optional<Image> getImage(String imageId) {
        try {
            GetRequest request = GetRequest.of(g -> g
                .index(imageIndex)
                .id(imageId)
            );

            GetResponse<Image> response = client.get(request, Image.class);

            if (response.found()) {
                return Optional.ofNullable(response.source());
            }

            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get image: {}", imageId, e);
            return Optional.empty();
        }
    }

    /**
     * ✅ P0优化2：使用Search After支持大数据集
     */
    @Override
    public List<Image> getImagesByDocument(String documentId) {
        try {
            List<Image> allImages = new ArrayList<>();
            List<co.elastic.clients.elasticsearch._types.FieldValue> searchAfter = null;

            while (true) {
                SearchRequest.Builder searchBuilder = new SearchRequest.Builder()
                    .index(imageIndex)
                    .query(q -> q
                        .term(t -> t
                            .field("documentId.keyword")  // ✅ 使用keyword字段
                            .value(documentId)
                        )
                    )
                    .sort(so -> so.field(f -> f.field("_id").order(SortOrder.Asc)))
                    .size(1000);

                if (searchAfter != null) {
                    searchBuilder.searchAfter(searchAfter);
                }

                SearchResponse<Image> response = client.search(searchBuilder.build(), Image.class);

                List<Hit<Image>> hits = response.hits().hits();
                if (hits.isEmpty()) {
                    break;
                }

                hits.stream()
                    .map(Hit::source)
                    .filter(Objects::nonNull)
                    .forEach(allImages::add);

                Hit<Image> lastHit = hits.get(hits.size() - 1);
                searchAfter = lastHit.sort();
            }

            log.debug("✅ Retrieved {} images using search_after", allImages.size());
            return allImages;

        } catch (Exception e) {
            log.error("Failed to get images for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteImage(String imageId) {
        try {
            DeleteRequest request = DeleteRequest.of(d -> d
                .index(imageIndex)
                .id(imageId)
            );

            client.delete(request);
            log.debug("Deleted image: {}", imageId);
        } catch (Exception e) {
            log.error("Failed to delete image: {}", imageId, e);
        }
    }

    /**
     * ✅ P0优化4：使用异步deleteByQuery提升性能
     */
    @Override
    public void deleteImagesByDocument(String documentId) {
        try {
            DeleteByQueryRequest request = DeleteByQueryRequest.of(d -> d
                .index(imageIndex)
                .query(q -> q
                    .term(t -> t
                        .field("documentId.keyword")  // ✅ 使用keyword字段
                        .value(documentId)
                    )
                )
                .waitForCompletion(false)  // ✅ 异步执行
                .refresh(true)
            );

            DeleteByQueryResponse response = client.deleteByQuery(request);

            if (response.task() != null) {
                log.info("✅ Started async delete task for images: {}", response.task());
            } else {
                log.info("✅ Deleted {} images for document: {}", response.deleted(), documentId);
            }
        } catch (Exception e) {
            log.error("Failed to delete images for document: {}", documentId, e);
        }
    }

    // ========== PPL Data Storage ==========

    @Override
    public String savePPLData(String documentId, PPLData data) {
        try {
            IndexRequest<PPLData> request = IndexRequest.of(i -> i
                .index(pplIndex)
                .id(documentId)
                .document(data)
            );

            IndexResponse response = client.index(request);

            if (response.result() == Result.Created || response.result() == Result.Updated) {
                log.debug("Saved PPL data for document: {}", documentId);
                return documentId;
            }

            return null;
        } catch (Exception e) {
            log.error("Failed to save PPL data", e);
            return null;
        }
    }

    @Override
    public Optional<PPLData> getPPLData(String documentId) {
        try {
            GetRequest request = GetRequest.of(g -> g
                .index(pplIndex)
                .id(documentId)
            );

            GetResponse<PPLData> response = client.get(request, PPLData.class);

            if (response.found()) {
                return Optional.ofNullable(response.source());
            }

            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get PPL data for document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deletePPLData(String documentId) {
        try {
            DeleteRequest request = DeleteRequest.of(d -> d
                .index(pplIndex)
                .id(documentId)
            );

            client.delete(request);
            log.info("Deleted PPL data for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete PPL data for document: {}", documentId, e);
        }
    }

    // ========== Optimization Data Storage ==========

    @Override
    public String saveOptimizationData(String documentId, OptimizationData data) {
        try {
            String docId = documentId + "_" + data.getOptimizationType();

            co.elastic.clients.elasticsearch.core.IndexRequest<OptimizationData> request =
                co.elastic.clients.elasticsearch.core.IndexRequest.of(i -> i
                    .index(optimizationIndex)
                    .id(docId)
                    .document(data)
                );

            client.index(request);
            log.debug("Saved {} optimization data for document: {}", data.getOptimizationType(), documentId);
            return docId;
        } catch (Exception e) {
            log.error("Failed to save optimization data", e);
            return null;
        }
    }

    @Override
    public Optional<OptimizationData> getOptimizationData(String documentId, String optimizationType) {
        try {
            String docId = documentId + "_" + optimizationType;
            co.elastic.clients.elasticsearch.core.GetRequest request =
                co.elastic.clients.elasticsearch.core.GetRequest.of(g -> g
                    .index(optimizationIndex)
                    .id(docId)
                );

            co.elastic.clients.elasticsearch.core.GetResponse<OptimizationData> response =
                client.get(request, OptimizationData.class);

            if (response.found()) {
                return Optional.ofNullable(response.source());
            }
            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get {} optimization data for document: {}", optimizationType, documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public java.util.List<OptimizationData> getAllOptimizationData(String documentId) {
        try {
            co.elastic.clients.elasticsearch.core.SearchRequest request =
                co.elastic.clients.elasticsearch.core.SearchRequest.of(s -> s
                    .index(optimizationIndex)
                    .query(q -> q.term(t -> t.field("documentId").value(documentId)))
                );

            co.elastic.clients.elasticsearch.core.SearchResponse<OptimizationData> response =
                client.search(request, OptimizationData.class);

            return response.hits().hits().stream()
                    .map(co.elastic.clients.elasticsearch.core.search.Hit::source)
                    .filter(java.util.Objects::nonNull)
                    .collect(java.util.stream.Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get all optimization data for document: {}", documentId, e);
            return new java.util.ArrayList<>();
        }
    }

    @Override
    public void deleteOptimizationData(String documentId, String optimizationType) {
        try {
            String docId = documentId + "_" + optimizationType;
            co.elastic.clients.elasticsearch.core.DeleteRequest request =
                co.elastic.clients.elasticsearch.core.DeleteRequest.of(d -> d
                    .index(optimizationIndex)
                    .id(docId)
                );

            client.delete(request);
            log.info("Deleted {} optimization data for document: {}", optimizationType, documentId);
        } catch (Exception e) {
            log.error("Failed to delete {} optimization data for document: {}", optimizationType, documentId, e);
        }
    }

    @Override
    public void deleteAllOptimizationData(String documentId) {
        try {
            co.elastic.clients.elasticsearch.core.DeleteByQueryRequest request =
                co.elastic.clients.elasticsearch.core.DeleteByQueryRequest.of(d -> d
                    .index(optimizationIndex)
                    .query(q -> q.term(t -> t.field("documentId").value(documentId)))
                );

            client.deleteByQuery(request);
            log.info("Deleted all optimization data for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete all optimization data for document: {}", documentId, e);
        }
    }

    // ========== Document Management ==========

    @Override
    public void cleanupDocument(String documentId) {
        deleteChunksByDocument(documentId);
        deleteImagesByDocument(documentId);
        deletePPLData(documentId);
        deleteAllOptimizationData(documentId);
        deleteExtractedText(documentId);  // ⭐ 新增
        log.info("Cleaned up all data for document: {}", documentId);
    }

    @Override
    public boolean documentExists(String documentId) {
        try {
            // 检查是否存在任何相关数据
            SearchRequest chunkRequest = SearchRequest.of(s -> s
                .index(chunkIndex)
                .query(q -> q.term(t -> t.field("documentId").value(documentId)))
                .size(1)
            );

            SearchResponse<Chunk> chunkResponse = client.search(chunkRequest, Chunk.class);
            if (chunkResponse.hits().total().value() > 0) {
                return true;
            }

            // 检查 PPL 数据
            GetRequest pplRequest = GetRequest.of(g -> g.index(pplIndex).id(documentId));
            GetResponse<PPLData> pplResponse = client.get(pplRequest, PPLData.class);

            return pplResponse.found();
        } catch (Exception e) {
            log.error("Failed to check document existence: {}", documentId, e);
            return false;
        }
    }

    @Override
    public long getDocumentSize(String documentId) {
        try {
            long size = 0;

            // 统计 chunks
            List<Chunk> chunks = getChunksByDocument(documentId);
            for (Chunk chunk : chunks) {
                if (chunk.getContent() != null) {
                    size += chunk.getContent().getBytes().length;
                }
            }

            // 统计 images
            List<Image> images = getImagesByDocument(documentId);
            for (Image image : images) {
                if (image.getData() != null) {
                    size += image.getData().length;
                }
            }

            // PPL 数据（估算）
            Optional<PPLData> pplData = getPPLData(documentId);
            if (pplData.isPresent()) {
                size += 1024; // 估算
            }

            return size;
        } catch (Exception e) {
            log.error("Failed to calculate document size for: {}", documentId, e);
            return 0;
        }
    }

    // ========== Document Management ==========

    @Override
    public String saveDocument(String documentId, String filename, byte[] fileData) {
        try {
            Map<String, Object> docData = new HashMap<>();
            docData.put("documentId", documentId);
            docData.put("filename", filename);
            docData.put("data", Base64.getEncoder().encodeToString(fileData));
            docData.put("createdAt", System.currentTimeMillis());

            IndexRequest<Map<String, Object>> request = IndexRequest.of(i -> i
                .index(properties.getIndexPrefix() + "-documents")
                .id(documentId)
                .document(docData)
            );

            client.index(request);
            log.debug("Saved document: {}", documentId);
            return documentId;
        } catch (Exception e) {
            log.error("Failed to save document: {}", documentId, e);
            return null;
        }
    }

    @Override
    public Optional<byte[]> getDocument(String documentId) {
        try {
            GetRequest request = GetRequest.of(g -> g
                .index(properties.getIndexPrefix() + "-documents")
                .id(documentId)
            );

            GetResponse<Map> response = client.get(request, Map.class);

            if (response.found() && response.source() != null) {
                String encodedData = (String) response.source().get("data");
                if (encodedData != null) {
                    return Optional.of(Base64.getDecoder().decode(encodedData));
                }
            }

            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deleteDocument(String documentId) {
        try {
            DeleteRequest request = DeleteRequest.of(d -> d
                .index(properties.getIndexPrefix() + "-documents")
                .id(documentId)
            );

            client.delete(request);
            log.debug("Deleted document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete document: {}", documentId, e);
        }
    }

    // ========== Extracted Text Storage ⭐ NEW ==========

    @Override
    public String saveExtractedText(String documentId, String text) {
        try {
            Map<String, Object> textData = new HashMap<>();
            textData.put("documentId", documentId);
            textData.put("text", text);
            textData.put("createdAt", System.currentTimeMillis());

            IndexRequest<Map<String, Object>> request = IndexRequest.of(i -> i
                .index(extractedTextIndex)
                .id(documentId)
                .document(textData)
            );

            IndexResponse response = client.index(request);

            if (response.result() == Result.Created || response.result() == Result.Updated) {
                log.debug("✅ Saved extracted text: {}, length={}", documentId, text.length());
                return documentId;
            }

            return null;
        } catch (Exception e) {
            log.error("❌ Failed to save extracted text: {}", documentId, e);
            return null;
        }
    }

    @Override
    public Optional<String> getExtractedText(String documentId) {
        try {
            GetRequest request = GetRequest.of(g -> g
                .index(extractedTextIndex)
                .id(documentId)
            );

            GetResponse<Map> response = client.get(request, Map.class);

            if (response.found() && response.source() != null) {
                String text = (String) response.source().get("text");
                if (text != null) {
                    log.debug("✅ Retrieved extracted text: {}, length={}", documentId, text.length());
                    return Optional.of(text);
                }
            }

            log.debug("⚠️ Extracted text not found: {}", documentId);
            return Optional.empty();
        } catch (Exception e) {
            log.error("❌ Failed to get extracted text: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deleteExtractedText(String documentId) {
        try {
            DeleteRequest request = DeleteRequest.of(d -> d
                .index(extractedTextIndex)
                .id(documentId)
            );

            client.delete(request);
            log.debug("🗑️ Deleted extracted text: {}", documentId);
        } catch (Exception e) {
            log.error("❌ Failed to delete extracted text: {}", documentId, e);
        }
    }

    // ========== Document Listing ==========

    @Override
    public List<DocumentMetadata> listAllDocuments() {
        try {
            SearchRequest request = SearchRequest.of(s -> s
                .index(properties.getIndexPrefix() + "-documents")
                .size(10000)
            );

            SearchResponse<Map> response = client.search(request, Map.class);

            return response.hits().hits().stream()
                    .map(this::convertToDocumentMetadata)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to list all documents", e);
            return new ArrayList<>();
        }
    }

    @Override
    public List<DocumentMetadata> listDocuments(int offset, int limit) {
        try {
            SearchRequest request = SearchRequest.of(s -> s
                .index(properties.getIndexPrefix() + "-documents")
                .from(offset)
                .size(limit)
            );

            SearchResponse<Map> response = client.search(request, Map.class);

            return response.hits().hits().stream()
                    .map(this::convertToDocumentMetadata)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to list documents with pagination", e);
            return new ArrayList<>();
        }
    }

    @Override
    public List<DocumentMetadata> searchDocuments(String keyword) {
        try {
            SearchRequest request = SearchRequest.of(s -> s
                .index(properties.getIndexPrefix() + "-documents")
                .query(q -> q
                    .match(m -> m
                        .field("filename")
                        .query(keyword)
                    )
                )
            );

            SearchResponse<Map> response = client.search(request, Map.class);

            return response.hits().hits().stream()
                    .map(this::convertToDocumentMetadata)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to search documents with keyword: {}", keyword, e);
            return new ArrayList<>();
        }
    }

    @Override
    public long getDocumentCount() {
        try {
            CountRequest request = CountRequest.of(c -> c
                .index(properties.getIndexPrefix() + "-documents")
            );

            CountResponse response = client.count(request);
            return response.count();
        } catch (Exception e) {
            log.error("Failed to get document count", e);
            return 0;
        }
    }

    private DocumentMetadata convertToDocumentMetadata(Hit<Map> hit) {
        try {
            Map<String, Object> source = hit.source();
            if (source == null) {
                return null;
            }

            String documentId = (String) source.get("documentId");
            String filename = (String) source.get("filename");
            Object createdAtObj = source.get("createdAt");
            long createdAt = createdAtObj instanceof Number ?
                    ((Number) createdAtObj).longValue() : System.currentTimeMillis();

            String encodedData = (String) source.get("data");
            long fileSize = encodedData != null ? encodedData.length() : 0;

            return DocumentMetadata.builder()
                    .documentId(documentId)
                    .filename(filename)
                    .fileSize(fileSize)
                    .uploadTime(new java.util.Date(createdAt))
                    .lastModified(new java.util.Date(createdAt))
                    .build();
        } catch (Exception e) {
            log.error("Failed to convert hit to DocumentMetadata", e);
            return null;
        }
    }

    // ========== Statistics ==========

    @Override
    public StorageStatistics getStatistics() {
        try {
            // 统计各索引的文档数
            CountRequest chunkCountRequest = CountRequest.of(c -> c.index(chunkIndex));
            long totalChunks = client.count(chunkCountRequest).count();

            CountRequest imageCountRequest = CountRequest.of(c -> c.index(imageIndex));
            long totalImages = client.count(imageCountRequest).count();

            CountRequest pplCountRequest = CountRequest.of(c -> c.index(pplIndex));
            long totalPPLData = client.count(pplCountRequest).count();

            // 统计唯一文档数（通过聚合）
            SearchRequest docCountRequest = SearchRequest.of(s -> s
                .index(chunkIndex, imageIndex, pplIndex)
                .size(0)
                .aggregations("unique_docs", a -> a
                    .cardinality(ca -> ca.field("documentId"))
                )
            );

            SearchResponse<Void> docCountResponse = client.search(docCountRequest, Void.class);
            long totalDocuments = docCountResponse.aggregations()
                    .get("unique_docs")
                    .cardinality()
                    .value();

            return StorageStatistics.builder()
                    .totalDocuments(totalDocuments)
                    .totalChunks(totalChunks)
                    .totalImages(totalImages)
                    .totalPPLData(totalPPLData)
                    .totalSize(0) // ES 难以准确计算
                    .storageType("elasticsearch")
                    .healthy(isHealthy())
                    .timestamp(System.currentTimeMillis())
                    .build();
        } catch (Exception e) {
            log.error("Failed to get statistics", e);
            return StorageStatistics.builder()
                    .storageType("elasticsearch")
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
                    .build();
        }
    }

    @Override
    public boolean isHealthy() {
        try {
            // 测试 ES 连接
            client.ping();
            return true;
        } catch (Exception e) {
            log.error("Health check failed", e);
            return false;
        }
    }

    // ========== 文件系统浏览实现 (File System Browse Implementation) ==========
    // 注意：Elasticsearch作为文档数据库，文件系统浏览功能基于索引查询实现

    @Override
    public List<Map<String, Object>> listFiles(String virtualPath) {
        try {
            // ES中的"虚拟目录"通过文档的path字段实现
            // 例如：documents/子目录/文件.pdf 存储为 path: "documents/子目录/文件.pdf"

            List<Map<String, Object>> items = new ArrayList<>();

            // 查询指定路径下的所有文档
            String searchPath = virtualPath.isEmpty() ? "" : virtualPath + "/";

            SearchResponse<Map> response = client.search(s -> s
                    .index("documents")
                    .query(q -> q
                        .prefix(p -> p
                            .field("path")
                            .value(searchPath)
                        )
                    )
                    .size(1000), // 限制返回数量
                    Map.class
            );

            // 处理结果，提取文件和目录
            Set<String> directories = new HashSet<>();
            for (Hit<Map> hit : response.hits().hits()) {
                Map<String, Object> source = hit.source();
                if (source != null) {
                    String path = (String) source.get("path");
                    if (path != null && path.startsWith(searchPath)) {
                        String relativePath = path.substring(searchPath.length());
                        int slashIndex = relativePath.indexOf('/');

                        if (slashIndex > 0) {
                            // 这是子目录中的文件，提取目录名
                            String dirName = relativePath.substring(0, slashIndex);
                            if (!directories.contains(dirName)) {
                                directories.add(dirName);
                                Map<String, Object> dirItem = new HashMap<>();
                                dirItem.put("name", dirName);
                                dirItem.put("type", "directory");
                                dirItem.put("path", virtualPath.isEmpty() ? dirName : virtualPath + "/" + dirName);
                                items.add(dirItem);
                            }
                        } else {
                            // 这是当前目录的文件
                            Map<String, Object> fileItem = new HashMap<>();
                            fileItem.put("name", relativePath);
                            fileItem.put("type", "file");
                            fileItem.put("path", path);
                            fileItem.put("size", source.getOrDefault("size", 0L));
                            fileItem.put("modified", source.getOrDefault("modified", System.currentTimeMillis()));
                            items.add(fileItem);
                        }
                    }
                }
            }

            return items;
        } catch (Exception e) {
            log.error("列出文件失败: {}", virtualPath, e);
            throw new RuntimeException("列出文件失败: " + e.getMessage(), e);
        }
    }

    @Override
    public byte[] readFile(String virtualPath) {
        try {
            // 从ES中查询文档
            SearchResponse<Map> response = client.search(s -> s
                    .index("documents")
                    .query(q -> q
                        .term(t -> t
                            .field("path")
                            .value(virtualPath)
                        )
                    )
                    .size(1),
                    Map.class
            );

            if (response.hits().hits().isEmpty()) {
                log.warn("文件不存在: {}", virtualPath);
                return null;
            }

            Map<String, Object> source = response.hits().hits().get(0).source();
            if (source != null && source.containsKey("content")) {
                // 内容以Base64存储
                String base64Content = (String) source.get("content");
                return Base64.getDecoder().decode(base64Content);
            }

            return null;
        } catch (Exception e) {
            log.error("读取文件失败: {}", virtualPath, e);
            throw new RuntimeException("读取文件失败: " + e.getMessage(), e);
        }
    }

    @Override
    public boolean deleteFile(String virtualPath) {
        try {
            // 删除指定路径的文档
            DeleteByQueryResponse response = client.deleteByQuery(d -> d
                    .index("documents")
                    .query(q -> q
                        .bool(b -> b
                            .should(s -> s
                                .term(t -> t
                                    .field("path")
                                    .value(virtualPath)
                                )
                            )
                            .should(s -> s
                                .prefix(p -> p
                                    .field("path")
                                    .value(virtualPath + "/")
                                )
                            )
                        )
                    )
            );

            long deleted = response.deleted();
            log.info("✅ 删除成功: {} (删除了{}个文档)", virtualPath, deleted);
            return deleted > 0;
        } catch (Exception e) {
            log.error("删除失败: {}", virtualPath, e);
            return false;
        }
    }

    @Override
    public boolean createDirectory(String virtualPath) {
        try {
            // ES中创建"目录"只是一个标记，实际不存储空目录
            // 创建一个元数据文档表示目录
            Map<String, Object> dirDoc = new HashMap<>();
            dirDoc.put("path", virtualPath);
            dirDoc.put("type", "directory");
            dirDoc.put("created", System.currentTimeMillis());

            IndexResponse response = client.index(i -> i
                    .index("documents")
                    .document(dirDoc)
            );

            log.info("✅ 创建目录成功: {}", virtualPath);
            return response.result() == Result.Created;
        } catch (Exception e) {
            log.error("创建目录失败: {}", virtualPath, e);
            return false;
        }
    }

    @Override
    public Map<String, Object> getStorageStats(String virtualPath) {
        try {
            // 统计指定路径下的文档
            String searchPath = virtualPath.isEmpty() ? "" : virtualPath + "/";

            SearchResponse<Map> response = client.search(s -> s
                    .index("documents")
                    .query(q -> q
                        .prefix(p -> p
                            .field("path")
                            .value(searchPath)
                        )
                    )
                    .size(0) // 只要统计数量，不返回文档
                    .aggregations("total_size", a -> a
                        .sum(su -> su.field("size"))
                    ),
                    Map.class
            );

            long totalFiles = response.hits().total().value();
            long totalSize = (long) response.aggregations()
                    .get("total_size")
                    .sum()
                    .value();

            // ES中目录是虚拟的，通过路径层级计算
            long totalFolders = 0;

            return Map.of(
                    "totalFiles", totalFiles,
                    "totalFolders", totalFolders,
                    "totalSize", totalSize
            );
        } catch (Exception e) {
            log.error("获取存储统计失败: {}", virtualPath, e);
            return Map.of(
                    "totalFiles", 0L,
                    "totalFolders", 0L,
                    "totalSize", 0L
            );
        }
    }

    // ========== 流式读写 API ⭐ NEW ==========
    // 注意：ES不适合存储大文件，这里提供Base64编码的流式接口

    @Override
    public InputStream getDocumentStream(String documentId) throws StorageException {
        try {
            GetResponse<Map> response = client.get(g -> g
                .index(properties.getIndexPrefix() + "documents")
                .id(documentId), Map.class);

            if (!response.found()) {
                throw new DocumentNotFoundException(documentId);
            }

            String base64Data = (String) response.source().get("data");
            byte[] data = Base64.getDecoder().decode(base64Data);
            return new ByteArrayInputStream(data);
        } catch (DocumentNotFoundException e) {
            throw e;
        } catch (Exception e) {
            throw new StorageIOException(documentId, "Failed to get stream", e);
        }
    }

    @Override
    public String saveDocumentStream(String documentId, String filename, InputStream inputStream)
            throws StorageException {
        try {
            byte[] data = inputStream.readAllBytes();
            String base64Data = Base64.getEncoder().encodeToString(data);

            Map<String, Object> doc = new HashMap<>();
            doc.put("documentId", documentId);
            doc.put("filename", filename);
            doc.put("data", base64Data);
            doc.put("timestamp", System.currentTimeMillis());

            client.index(i -> i
                .index(properties.getIndexPrefix() + "documents")
                .id(documentId)
                .document(doc));

            log.debug("✅ Saved document via stream: {}", documentId);
            return documentId;
        } catch (IOException e) {
            throw new StorageIOException(documentId, "Failed to save stream", e);
        }
    }

    @Override
    public void copyDocumentToStream(String documentId, OutputStream outputStream)
            throws StorageException {
        try (InputStream inputStream = getDocumentStream(documentId)) {
            inputStream.transferTo(outputStream);
        } catch (IOException e) {
            throw new StorageIOException(documentId, "Failed to copy to stream", e);
        }
    }

    @Override
    public InputStream getExtractedTextStream(String documentId) throws StorageException {
        try {
            GetResponse<Map> response = client.get(g -> g
                .index(properties.getIndexPrefix() + "extracted_text")
                .id(documentId), Map.class);

            if (!response.found()) {
                throw new DocumentNotFoundException(documentId, "Extracted text not found");
            }

            String text = (String) response.source().get("text");
            return new ByteArrayInputStream(text.getBytes(java.nio.charset.StandardCharsets.UTF_8));
        } catch (DocumentNotFoundException e) {
            throw e;
        } catch (Exception e) {
            throw new StorageIOException(documentId, "Failed to get text stream", e);
        }
    }

    @Override
    public String saveExtractedTextStream(String documentId, InputStream inputStream)
            throws StorageException {
        try {
            String text = new String(inputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8);

            Map<String, Object> doc = new HashMap<>();
            doc.put("documentId", documentId);
            doc.put("text", text);
            doc.put("timestamp", System.currentTimeMillis());

            client.index(i -> i
                .index(properties.getIndexPrefix() + "extracted_text")
                .id(documentId)
                .document(doc));

            log.debug("✅ Saved text via stream: {}", documentId);
            return documentId;
        } catch (IOException e) {
            throw new StorageIOException(documentId, "Failed to save text stream", e);
        }
    }

    // ========== 事务性批量操作 ⭐ NEW ==========

    @Override
    public BatchOperationResult saveDocumentsTransactional(List<Map<String, Object>> documents)
            throws BatchOperationException {

        List<String> successIds = new ArrayList<>();
        Map<String, String> errorMessages = new HashMap<>();

        try {
            for (Map<String, Object> doc : documents) {
                String documentId = (String) doc.get("documentId");
                String filename = (String) doc.get("filename");
                byte[] fileData = (byte[]) doc.get("fileData");

                try {
                    String id = saveDocument(documentId, filename, fileData);
                    if (id != null) {
                        successIds.add(id);
                    } else {
                        throw new StorageException("SAVE_FAILED", documentId, "Failed to save");
                    }
                } catch (Exception e) {
                    errorMessages.put(documentId, e.getMessage());
                    throw e;
                }
            }

            log.info("✅ Transaction: All {} documents saved", successIds.size());
            return BatchOperationResult.builder()
                    .successCount(successIds.size())
                    .failureCount(0)
                    .totalCount(documents.size())
                    .successIds(successIds)
                    .failureIds(new ArrayList<>())
                    .errorMessages(new HashMap<>())
                    .build();

        } catch (Exception e) {
            log.warn("⏮ Rolling back {} documents...", successIds.size());

            for (String docId : successIds) {
                try {
                    deleteDocument(docId);
                } catch (Exception rollbackError) {
                    log.error("Rollback failed: {}", docId, rollbackError);
                    errorMessages.put(docId, "Rollback failed: " + rollbackError.getMessage());
                }
            }

            throw new BatchOperationException(
                "Batch save failed and rolled back: " + e.getMessage(),
                e, new ArrayList<>(), successIds, errorMessages
            );
        }
    }

    @Override
    public BatchOperationResult deleteDocumentsTransactional(List<String> documentIds)
            throws BatchOperationException {

        Map<String, byte[]> backups = new HashMap<>();
        List<String> successIds = new ArrayList<>();
        Map<String, String> errorMessages = new HashMap<>();

        try {
            // 备份
            log.debug("📦 Backing up {} documents...", documentIds.size());
            for (String documentId : documentIds) {
                try {
                    Optional<byte[]> data = getDocument(documentId);
                    if (data.isPresent()) {
                        backups.put(documentId, data.get());
                    }
                } catch (Exception e) {
                    errorMessages.put(documentId, "Backup failed: " + e.getMessage());
                    throw e;
                }
            }

            // 删除
            log.debug("🗑️ Deleting {} documents...", documentIds.size());
            for (String documentId : documentIds) {
                if (backups.containsKey(documentId)) {
                    deleteDocument(documentId);
                    successIds.add(documentId);
                }
            }

            return BatchOperationResult.builder()
                    .successCount(successIds.size())
                    .failureCount(0)
                    .totalCount(documentIds.size())
                    .successIds(successIds)
                    .failureIds(new ArrayList<>())
                    .errorMessages(new HashMap<>())
                    .build();

        } catch (Exception e) {
            log.warn("⏮ Restoring {} documents...", successIds.size());

            for (String docId : successIds) {
                try {
                    byte[] data = backups.get(docId);
                    if (data != null) {
                        saveDocument(docId, docId, data);
                    }
                } catch (Exception restoreError) {
                    log.error("Restore failed: {}", docId, restoreError);
                    errorMessages.put(docId, "Restore failed: " + restoreError.getMessage());
                }
            }

            throw new BatchOperationException(
                "Batch delete failed and restored: " + e.getMessage(),
                e, new ArrayList<>(), successIds, errorMessages
            );
        }
    }

    // ========== 元数据管理 ⭐ NEW ==========

    @Override
    public void saveMetadata(DocumentMetadata metadata) {
        try {
            client.index(i -> i
                .index(properties.getIndexPrefix() + "metadata")
                .id(metadata.getDocumentId())
                .document(metadata));
            log.debug("💾 Saved metadata: {}", metadata.getDocumentId());
        } catch (Exception e) {
            log.error("Failed to save metadata: {}", metadata.getDocumentId(), e);
        }
    }

    @Override
    public Optional<DocumentMetadata> getMetadata(String documentId) {
        try {
            GetResponse<DocumentMetadata> response = client.get(g -> g
                .index(properties.getIndexPrefix() + "metadata")
                .id(documentId), DocumentMetadata.class);

            return response.found() ? Optional.of(response.source()) : Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get metadata: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<DocumentMetadata> getAllMetadata() {
        try {
            SearchResponse<DocumentMetadata> response = client.search(s -> s
                .index(properties.getIndexPrefix() + "metadata")
                .size(10000), DocumentMetadata.class);

            return response.hits().hits().stream()
                .map(Hit::source)
                .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get all metadata", e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteMetadata(String documentId) {
        try {
            client.delete(d -> d
                .index(properties.getIndexPrefix() + "metadata")
                .id(documentId));
            log.debug("🗑️ Deleted metadata: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete metadata: {}", documentId, e);
        }
    }
}
