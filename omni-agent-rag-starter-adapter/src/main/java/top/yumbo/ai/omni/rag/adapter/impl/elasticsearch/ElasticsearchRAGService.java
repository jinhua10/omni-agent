package top.yumbo.ai.omni.rag.adapter.impl.elasticsearch;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.elasticsearch._types.SortOrder;
import co.elastic.clients.elasticsearch._types.query_dsl.*;
import co.elastic.clients.elasticsearch.core.*;
import co.elastic.clients.elasticsearch.indices.CreateIndexRequest;
import co.elastic.clients.elasticsearch.indices.DeleteIndexRequest;
import co.elastic.clients.elasticsearch.indices.ExistsRequest;
import co.elastic.clients.elasticsearch.indices.GetIndexRequest;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;
import top.yumbo.ai.omni.rag.model.IndexStatistics;

import jakarta.annotation.PostConstruct;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 基于 Elasticsearch 的 RAG 实现
 * (Elasticsearch-based RAG Implementation)
 * 
 * <p>特性:
 * <ul>
 *   <li>分布式全文搜索（Elasticsearch BM25）</li>
 *   <li>高性能向量搜索（kNN/HNSW）</li>
 *   <li>混合检索（文本 + 向量）</li>
 *   <li>高可用性（分片 + 副本）</li>
 *   <li>水平扩展（集群模式）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class ElasticsearchRAGService implements RagService {

    private final ElasticsearchClient client;
    private final ElasticsearchRAGProperties properties;
    private final ObjectMapper objectMapper;
    private final String domainId;

    public ElasticsearchRAGService(ElasticsearchClient client,
                                   ElasticsearchRAGProperties properties) {
        this(client, properties, "elasticsearch-domain");
    }

    public ElasticsearchRAGService(ElasticsearchClient client,
                                   ElasticsearchRAGProperties properties,
                                   String domainId) {
        this.client = client;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();
        this.domainId = domainId;
    }

    @PostConstruct
    public void init() {
        try {
            // 检查索引是否存在
            ExistsRequest existsRequest = ExistsRequest.of(e -> e.index(properties.getIndexName()));
            boolean exists = client.indices().exists(existsRequest).value();

            if (!exists) {
                createIndex();
                log.info("Elasticsearch RAG 索引创建成功: {}", properties.getIndexName());
            } else {
                log.info("Elasticsearch RAG 索引已存在: {}", properties.getIndexName());
            }

        } catch (IOException e) {
            log.error("初始化 Elasticsearch RAG 失败", e);
            throw new RuntimeException("初始化失败", e);
        }
    }

    // ========== 文档索引 ==========

    public String indexDocument(Document document) {
        try {
            if (document.getId() == null || document.getId().isEmpty()) {
                document.setId(UUID.randomUUID().toString());
            }

            IndexRequest<Document> request = IndexRequest.of(i -> i
                .index(properties.getIndexName())
                .id(document.getId())
                .document(document)
                .refresh(properties.isRefreshAfterWrite() ? 
                    co.elastic.clients.elasticsearch._types.Refresh.True : 
                    co.elastic.clients.elasticsearch._types.Refresh.False)
            );

            client.index(request);

            log.debug("文档索引成功: {}", document.getId());
            return document.getId();

        } catch (IOException e) {
            log.error("索引文档失败", e);
            throw new RuntimeException("索引文档失败", e);
        }
    }

    public List<String> indexDocuments(List<Document> documents) {
        try {
            List<String> docIds = new ArrayList<>();
            BulkRequest.Builder bulkBuilder = new BulkRequest.Builder();

            for (Document document : documents) {
                if (document.getId() == null || document.getId().isEmpty()) {
                    document.setId(UUID.randomUUID().toString());
                }

                bulkBuilder.operations(op -> op
                    .index(idx -> idx
                        .index(properties.getIndexName())
                        .id(document.getId())
                        .document(document)
                    )
                );

                docIds.add(document.getId());
            }

            BulkResponse response = client.bulk(bulkBuilder.build());

            if (response.errors()) {
                log.warn("批量索引部分失败");
            }

            log.debug("批量索引完成: {} 文档", docIds.size());
            return docIds;

        } catch (IOException e) {
            log.error("批量索引文档失败", e);
            throw new RuntimeException("批量索引文档失败", e);
        }
    }

    public boolean updateDocument(Document document) {
        try {
            if (!documentExists(document.getId())) {
                log.warn("文档不存在: {}", document.getId());
                return false;
            }

            UpdateRequest<Document, Document> request = UpdateRequest.of(u -> u
                .index(properties.getIndexName())
                .id(document.getId())
                .doc(document)
                .refresh(properties.isRefreshAfterWrite() ? 
                    co.elastic.clients.elasticsearch._types.Refresh.True : 
                    co.elastic.clients.elasticsearch._types.Refresh.False)
            );

            client.update(request, Document.class);

            log.debug("文档更新成功: {}", document.getId());
            return true;

        } catch (IOException e) {
            log.error("更新文档失败: {}", document.getId(), e);
            return false;
        }
    }

    public boolean deleteDocument(String documentId) {
        try {
            DeleteRequest request = DeleteRequest.of(d -> d
                .index(properties.getIndexName())
                .id(documentId)
                .refresh(properties.isRefreshAfterWrite() ? 
                    co.elastic.clients.elasticsearch._types.Refresh.True : 
                    co.elastic.clients.elasticsearch._types.Refresh.False)
            );

            DeleteResponse response = client.delete(request);

            boolean success = response.result() == co.elastic.clients.elasticsearch._types.Result.Deleted;
            if (success) {
                log.debug("文档删除成功: {}", documentId);
            } else {
                log.warn("文档不存在: {}", documentId);
            }

            return success;

        } catch (IOException e) {
            log.error("删除文档失败: {}", documentId, e);
            return false;
        }
    }

    @Override
    public void clearAll() {
        try {
            // 删除并重建索引
            DeleteIndexRequest deleteRequest = DeleteIndexRequest.of(d -> d
                .index(properties.getIndexName())
            );

            client.indices().delete(deleteRequest);
            createIndex();

            log.info("所有索引已清空并重建");

        } catch (IOException e) {
            log.error("清空索引失败", e);
            throw new RuntimeException("清空索引失败", e);
        }
    }

    // ========== 文档管理 ==========


    @Override
    public Optional<Document> getDocument(String documentId) {
        try {
            GetRequest request = GetRequest.of(g -> g
                .index(properties.getIndexName())
                .id(documentId)
            );

            GetResponse<Document> response = client.get(request, Document.class);

            if (response.found()) {
                return Optional.ofNullable(response.source());
            }

            return Optional.empty();

        } catch (IOException e) {
            log.error("获取文档失败: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public boolean documentExists(String documentId) {
        try {
            co.elastic.clients.elasticsearch.core.ExistsRequest request = 
                co.elastic.clients.elasticsearch.core.ExistsRequest.of(e -> e
                    .index(properties.getIndexName())
                    .id(documentId)
                );

            return client.exists(request).value();

        } catch (IOException e) {
            log.error("检查文档存在失败: {}", documentId, e);
            return false;
        }
    }

    @Override
    public long getDocumentCount() {
        try {
            CountRequest request = CountRequest.of(c -> c
                .index(properties.getIndexName())
            );

            CountResponse response = client.count(request);
            return response.count();

        } catch (IOException e) {
            log.error("获取文档总数失败", e);
            return 0;
        }
    }

    @Override
    public List<Document> getAllDocuments(int offset, int limit) {
        try {
            SearchRequest request = SearchRequest.of(s -> s
                .index(properties.getIndexName())
                .query(q -> q.matchAll(m -> m))
                .from(offset)
                .size(limit)
                .sort(so -> so.field(f -> f.field("createdAt").order(SortOrder.Desc)))
            );

            SearchResponse<Document> response = client.search(request, Document.class);

            List<Document> documents = new ArrayList<>();
            for (var hit : response.hits().hits()) {
                if (hit.source() != null) {
                    documents.add(hit.source());
                }
            }

            log.debug("获取文档列表: offset={}, limit={}, count={}", offset, limit, documents.size());
            return documents;

        } catch (IOException e) {
            log.error("获取所有文档失败", e);
            return Collections.emptyList();
        }
    }

    // ========== 统计与健康 ==========

    @Override
    public IndexStatistics getStatistics() {
        try {
            long totalDocs = getDocumentCount();

            // 获取索引大小
            GetIndexRequest getIndexRequest = GetIndexRequest.of(g -> g
                .index(properties.getIndexName())
            );

            var indexResponse = client.indices().get(getIndexRequest);
            long indexSize = 0; // Elasticsearch需要通过stats API获取

            return IndexStatistics.builder()
                .totalDocuments(totalDocs)
                .indexSize(indexSize)
                .indexType("Elasticsearch")
                .healthy(isHealthy())
                .timestamp(System.currentTimeMillis())
                .domainId(domainId)
                .build();

        } catch (IOException e) {
            log.error("获取统计信息失败", e);
            return IndexStatistics.builder()
                .totalDocuments(0L)
                .indexSize(0L)
                .healthy(false)
                .timestamp(System.currentTimeMillis())
                .domainId(domainId)
                .build();
        }
    }

    @Override
    public boolean isHealthy() {
        try {
            var response = client.ping();
            return response.value();

        } catch (IOException e) {
            log.error("健康检查失败", e);
            return false;
        }
    }

    @Override
    public void rebuildIndex() {
        try {
            // 重建索引
            clearAll();
            log.info("索引重建完成");

        } catch (Exception e) {
            log.error("重建索引失败", e);
            throw new RuntimeException("重建索引失败", e);
        }
    }

    // ========== 工具方法 ==========

    private void createIndex() throws IOException {
        CreateIndexRequest request = CreateIndexRequest.of(c -> c
            .index(properties.getIndexName())
            .settings(s -> s
                .numberOfShards(String.valueOf(properties.getNumberOfShards()))
                .numberOfReplicas(String.valueOf(properties.getNumberOfReplicas()))
            )
            .mappings(m -> m
                .properties("id", p -> p.keyword(k -> k))
                .properties("title", p -> p.text(t -> t.analyzer("standard")))
                .properties("content", p -> p.text(t -> t.analyzer("standard")))
                .properties("summary", p -> p.text(t -> t.analyzer("standard")))
                .properties("tags", p -> p.keyword(k -> k))
                .properties("type", p -> p.keyword(k -> k))
                .properties("source", p -> p.keyword(k -> k))
                .properties("author", p -> p.keyword(k -> k))
                .properties("embedding", p -> p
                    .denseVector(d -> d
                        .dims(properties.getVectorDimension())
                        .index(true)
                        .similarity("cosine")
                    )
                )
                .properties("createdAt", p -> p.date(d -> d))
                .properties("updatedAt", p -> p.date(d -> d))
            )
        );

        client.indices().create(request);
    }

    private List<Query> buildFilterQueries(Map<String, Object> filters) {
        List<Query> queries = new ArrayList<>();

        for (Map.Entry<String, Object> entry : filters.entrySet()) {
            String field = entry.getKey();
            Object value = entry.getValue();

            queries.add(Query.of(q -> q
                .term(t -> t
                    .field(field)
                    .value(v -> v.stringValue(value.toString()))
                )
            ));
        }

        return queries;
    }

    private List<Float> arrayToList(float[] array) {
        List<Float> list = new ArrayList<>(array.length);
        for (float value : array) {
            list.add(value);
        }
        return list;
    }

    // ========== 新接口实现 ==========

    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        log.debug("语义搜索: query={}, maxResults={}", query, maxResults);
        return searchByTextInternal(query, maxResults);
    }

    @Override
    public List<Document> vectorSearch(Vector vector, int maxResults) {
        if (vector == null || vector.getData() == null) {
            return Collections.emptyList();
        }
        return vectorSearchInternal(vector.getData(), maxResults);
    }

    @Override
    public Vector embed(String text) {
        log.warn("ElasticsearchRAGService 不提供嵌入功能");
        return null;
    }

    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        log.warn("ElasticsearchRAGService 不提供批量嵌入功能");
        return Collections.emptyList();
    }

    @Override
    public void index(String id, Vector vector, Map<String, Object> metadata) {
        Document.DocumentBuilder builder = Document.builder().id(id);

        if (metadata != null) {
            if (metadata.containsKey("title")) builder.title((String) metadata.get("title"));
            if (metadata.containsKey("content")) builder.content((String) metadata.get("content"));
            if (metadata.containsKey("source")) builder.source((String) metadata.get("source"));
        }

        if (vector != null && vector.getData() != null) {
            builder.embedding(vector.getData());
        }

        indexDocument(builder.build());
    }

    @Override
    public void batchIndex(List<Document> documents) {
        indexDocuments(documents);
    }

    @Override
    public void delete(String id) {
        deleteDocument(id);
    }

    @Override
    public String getDomainId() {
        return domainId;
    }

    // ========== 内部辅助方法 ==========

    private List<Document> searchByTextInternal(String text, int maxResults) {
        try {
            SearchRequest request = SearchRequest.of(s -> s
                    .index(properties.getIndexName())
                    .query(q -> q.multiMatch(m -> m
                            .query(text)
                            .fields("title^2", "content", "summary")
                    ))
                    .size(maxResults)
            );

            SearchResponse<Document> response = client.search(request, Document.class);
            return response.hits().hits().stream()
                    .map(hit -> {
                        Document doc = hit.source();
                        if (doc != null && hit.score() != null) {
                            doc.setScore(hit.score());
                        }
                        return doc;
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("文本搜索失败: {}", text, e);
            return Collections.emptyList();
        }
    }

    private List<Document> vectorSearchInternal(float[] embedding, int maxResults) {
        try {
            SearchRequest request = SearchRequest.of(s -> s
                    .index(properties.getIndexName())
                    .knn(k -> k
                            .field("embedding")
                            .queryVector(arrayToList(embedding))
                            .k(maxResults)
                            .numCandidates(maxResults * 10)
                    )
            );

            SearchResponse<Document> response = client.search(request, Document.class);
            return response.hits().hits().stream()
                    .map(hit -> {
                        Document doc = hit.source();
                        if (doc != null && hit.score() != null) {
                            doc.setScore(hit.score());
                        }
                        return doc;
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("向量搜索失败", e);
            return Collections.emptyList();
        }
    }
}
