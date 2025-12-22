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

    public ElasticsearchDocumentStorage(ElasticsearchClient client,
                                       ElasticsearchStorageProperties properties) {
        this.client = client;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();

        this.chunkIndex = properties.getIndexPrefix() + "-chunks";
        this.imageIndex = properties.getIndexPrefix() + "-images";
        this.pplIndex = properties.getIndexPrefix() + "-ppl";
        this.optimizationIndex = properties.getIndexPrefix() + "-optimizations";

        initIndices();
        log.info("ElasticsearchDocumentStorage initialized with prefix: {}", properties.getIndexPrefix());
    }

    private void initIndices() {
        try {
            createIndexIfNotExists(chunkIndex);
            createIndexIfNotExists(imageIndex);
            createIndexIfNotExists(pplIndex);
            createIndexIfNotExists(optimizationIndex);
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

    // ========== Optimization Data Storage ==========

    @Override
    public String saveOptimizationData(String documentId, top.yumbo.ai.storage.api.model.OptimizationData data) {
        try {
            String docId = documentId + "_" + data.getOptimizationType();

            co.elastic.clients.elasticsearch.core.IndexRequest<top.yumbo.ai.storage.api.model.OptimizationData> request =
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
    public Optional<top.yumbo.ai.storage.api.model.OptimizationData> getOptimizationData(String documentId, String optimizationType) {
        try {
            String docId = documentId + "_" + optimizationType;
            co.elastic.clients.elasticsearch.core.GetRequest request =
                co.elastic.clients.elasticsearch.core.GetRequest.of(g -> g
                    .index(optimizationIndex)
                    .id(docId)
                );

            co.elastic.clients.elasticsearch.core.GetResponse<top.yumbo.ai.storage.api.model.OptimizationData> response =
                client.get(request, top.yumbo.ai.storage.api.model.OptimizationData.class);

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
    public java.util.List<top.yumbo.ai.storage.api.model.OptimizationData> getAllOptimizationData(String documentId) {
        try {
            co.elastic.clients.elasticsearch.core.SearchRequest request =
                co.elastic.clients.elasticsearch.core.SearchRequest.of(s -> s
                    .index(optimizationIndex)
                    .query(q -> q.term(t -> t.field("documentId").value(documentId)))
                );

            co.elastic.clients.elasticsearch.core.SearchResponse<top.yumbo.ai.storage.api.model.OptimizationData> response =
                client.search(request, top.yumbo.ai.storage.api.model.OptimizationData.class);

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

    @Override
    public List<top.yumbo.ai.storage.api.model.DocumentMetadata> listAllDocuments() {
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
    public List<top.yumbo.ai.storage.api.model.DocumentMetadata> listDocuments(int offset, int limit) {
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
    public List<top.yumbo.ai.storage.api.model.DocumentMetadata> searchDocuments(String keyword) {
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

    private top.yumbo.ai.storage.api.model.DocumentMetadata convertToDocumentMetadata(Hit<Map> hit) {
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

            return top.yumbo.ai.storage.api.model.DocumentMetadata.builder()
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
}
