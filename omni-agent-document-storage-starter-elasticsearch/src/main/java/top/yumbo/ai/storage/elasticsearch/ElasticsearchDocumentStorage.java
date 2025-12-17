package top.yumbo.ai.storage.elasticsearch;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.elasticsearch._types.Result;
import co.elastic.clients.elasticsearch.core.*;
import co.elastic.clients.elasticsearch.core.search.Hit;
import co.elastic.clients.elasticsearch.indices.CreateIndexRequest;
import co.elastic.clients.elasticsearch.indices.ExistsRequest;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Chunk;
import top.yumbo.ai.storage.api.model.Image;
import top.yumbo.ai.storage.api.model.PPLData;
import top.yumbo.ai.storage.api.model.StorageStatistics;

import java.util.*;
import java.util.stream.Collectors;

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

    public ElasticsearchDocumentStorage(ElasticsearchClient client,
                                       ElasticsearchStorageProperties properties) {
        this.client = client;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();

        this.chunkIndex = properties.getIndexPrefix() + "-chunks";
        this.imageIndex = properties.getIndexPrefix() + "-images";
        this.pplIndex = properties.getIndexPrefix() + "-ppl";

        initIndices();
        log.info("ElasticsearchDocumentStorage initialized with prefix: {}", properties.getIndexPrefix());
    }

    private void initIndices() {
        try {
            createIndexIfNotExists(chunkIndex);
            createIndexIfNotExists(imageIndex);
            createIndexIfNotExists(pplIndex);
        } catch (Exception e) {
            log.error("Failed to initialize indices", e);
        }
    }

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
                    )
                );
                client.indices().create(createRequest);
                log.info("Created index: {}", indexName);
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

    @Override
    public List<String> saveChunks(String documentId, List<Chunk> chunks) {
        List<String> chunkIds = new ArrayList<>();
        for (Chunk chunk : chunks) {
            String chunkId = saveChunk(documentId, chunk);
            if (chunkId != null) {
                chunkIds.add(chunkId);
            }
        }
        return chunkIds;
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

    @Override
    public List<Chunk> getChunksByDocument(String documentId) {
        try {
            SearchRequest request = SearchRequest.of(s -> s
                .index(chunkIndex)
                .query(q -> q
                    .term(t -> t
                        .field("documentId")
                        .value(documentId)
                    )
                )
                .size(1000) // 最多返回1000个分块
            );

            SearchResponse<Chunk> response = client.search(request, Chunk.class);

            return response.hits().hits().stream()
                    .map(Hit::source)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
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

    @Override
    public void deleteChunksByDocument(String documentId) {
        try {
            DeleteByQueryRequest request = DeleteByQueryRequest.of(d -> d
                .index(chunkIndex)
                .query(q -> q
                    .term(t -> t
                        .field("documentId")
                        .value(documentId)
                    )
                )
            );

            client.deleteByQuery(request);
            log.info("Deleted all chunks for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete chunks for document: {}", documentId, e);
        }
    }

    // ========== Image Storage ==========

    @Override
    public String saveImage(String documentId, Image image) {
        try {
            String imageId = image.getId() != null ? image.getId() : UUID.randomUUID().toString();
            image.setId(imageId);
            image.setDocumentId(documentId);

            IndexRequest<Image> request = IndexRequest.of(i -> i
                .index(imageIndex)
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

    @Override
    public List<Image> getImagesByDocument(String documentId) {
        try {
            SearchRequest request = SearchRequest.of(s -> s
                .index(imageIndex)
                .query(q -> q
                    .term(t -> t
                        .field("documentId")
                        .value(documentId)
                    )
                )
                .size(1000)
            );

            SearchResponse<Image> response = client.search(request, Image.class);

            return response.hits().hits().stream()
                    .map(Hit::source)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
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

    @Override
    public void deleteImagesByDocument(String documentId) {
        try {
            DeleteByQueryRequest request = DeleteByQueryRequest.of(d -> d
                .index(imageIndex)
                .query(q -> q
                    .term(t -> t
                        .field("documentId")
                        .value(documentId)
                    )
                )
            );

            client.deleteByQuery(request);
            log.info("Deleted all images for document: {}", documentId);
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

    // ========== Optimization Data Storage (TODO: 待实现) ==========

    @Override
    public String saveOptimizationData(String documentId, top.yumbo.ai.storage.api.model.OptimizationData data) {
        // TODO: 待实现Elasticsearch优化数据存储
        log.warn("saveOptimizationData not implemented for Elasticsearch yet");
        return null;
    }

    @Override
    public Optional<top.yumbo.ai.storage.api.model.OptimizationData> getOptimizationData(String documentId, String optimizationType) {
        // TODO: 待实现Elasticsearch优化数据获取
        log.warn("getOptimizationData not implemented for Elasticsearch yet");
        return Optional.empty();
    }

    @Override
    public java.util.List<top.yumbo.ai.storage.api.model.OptimizationData> getAllOptimizationData(String documentId) {
        // TODO: 待实现Elasticsearch所有优化数据获取
        log.warn("getAllOptimizationData not implemented for Elasticsearch yet");
        return new java.util.ArrayList<>();
    }

    @Override
    public void deleteOptimizationData(String documentId, String optimizationType) {
        // TODO: 待实现Elasticsearch优化数据删除
        log.warn("deleteOptimizationData not implemented for Elasticsearch yet");
    }

    @Override
    public void deleteAllOptimizationData(String documentId) {
        // TODO: 待实现Elasticsearch所有优化数据删除
        log.warn("deleteAllOptimizationData not implemented for Elasticsearch yet");
    }

    // ========== Document Management ==========

    @Override
    public void cleanupDocument(String documentId) {
        deleteChunksByDocument(documentId);
        deleteImagesByDocument(documentId);
        deletePPLData(documentId);
        deleteAllOptimizationData(documentId);
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
}

