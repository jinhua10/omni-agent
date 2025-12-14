package top.yumbo.ai.p2p.starter.elasticsearch;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.Criteria;
import org.springframework.data.elasticsearch.core.query.CriteriaQuery;
import org.springframework.data.elasticsearch.core.query.Query;
import org.springframework.data.elasticsearch.core.mapping.IndexCoordinates;
import top.yumbo.ai.p2p.api.P2PDataTransferService;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Elasticsearch P2P数据传输服务实现
 * (Elasticsearch P2P Data Transfer Service Implementation)
 *
 * <p>基于Elasticsearch的搜索引擎数据传输实现</p>
 * <p>Elasticsearch-based search engine data transfer implementation</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class ElasticsearchP2PDataTransferService implements P2PDataTransferService {

    private final ElasticsearchOperations elasticsearchOperations;
    private final ElasticsearchP2PProperties properties;

    public ElasticsearchP2PDataTransferService(ElasticsearchOperations elasticsearchOperations,
                                              ElasticsearchP2PProperties properties) {
        this.elasticsearchOperations = elasticsearchOperations;
        this.properties = properties;
        log.info("ElasticsearchP2PDataTransferService initialized");
    }

    @Override
    public List<Map<String, Object>> readFromSource(Map<String, Object> query) {
        log.debug("Reading from Elasticsearch with query: {}", query);

        Query esQuery;

        // 构建查询条件
        if (query.containsKey("type")) {
            Criteria criteria = Criteria.where("type").is(query.get("type"));
            esQuery = new CriteriaQuery(criteria);
        } else if (query.containsKey("query_string")) {
            // 支持全文搜索
            esQuery = Query.findAll();
            // 注意：这里简化处理，实际应使用NativeSearchQuery进行复杂查询
        } else {
            esQuery = Query.findAll();
        }

        // 应用分页
        int offset = (int) query.getOrDefault("offset", 0);
        int limit = (int) query.getOrDefault("limit", 1000);
        esQuery.setPageable(org.springframework.data.domain.PageRequest.of(
                offset / limit,
                limit
        ));

        try {
            IndexCoordinates index = IndexCoordinates.of(properties.getIndexName());
            SearchHits<Map> searchHits = elasticsearchOperations.search(
                    esQuery,
                    Map.class,
                    index
            );

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> results = searchHits.stream()
                    .map(SearchHit::getContent)
                    .map(content -> (Map<String, Object>) content)
                    .collect(Collectors.toList());

            log.debug("Read {} records from Elasticsearch", results.size());
            return results;

        } catch (Exception e) {
            log.error("Failed to read from Elasticsearch: {}", e.getMessage(), e);
            return Collections.emptyList();
        }
    }

    @Override
    public int writeToTarget(List<Map<String, Object>> data) {
        log.debug("Writing {} records to Elasticsearch", data.size());

        int written = 0;
        List<Map<String, Object>> enrichedRecords = new ArrayList<>();

        for (Map<String, Object> record : data) {
            try {
                String id = (String) record.getOrDefault("id", UUID.randomUUID().toString());

                // 添加时间戳
                Map<String, Object> enrichedRecord = new HashMap<>(record);
                enrichedRecord.put("id", id);
                enrichedRecord.putIfAbsent("created_at", Instant.now().toString());
                enrichedRecord.put("updated_at", Instant.now().toString());

                enrichedRecords.add(enrichedRecord);

            } catch (Exception e) {
                log.error("Failed to prepare record for Elasticsearch: {}", e.getMessage());
            }
        }

        // 批量写入
        if (!enrichedRecords.isEmpty()) {
            try {
                IndexCoordinates index = IndexCoordinates.of(properties.getIndexName());
                
                // 使用Elasticsearch的bulk操作
                for (Map<String, Object> record : enrichedRecords) {
                    try {
                        elasticsearchOperations.save(record, index);
                        written++;
                    } catch (Exception e) {
                        log.error("Failed to index document: {}", e.getMessage());
                    }
                }

                // 刷新索引以使数据立即可搜索
                if (properties.isRefreshAfterWrite()) {
                    elasticsearchOperations.indexOps(index).refresh();
                }

                log.info("Successfully wrote {} records to Elasticsearch", written);

            } catch (Exception e) {
                log.error("Failed to write batch to Elasticsearch: {}", e.getMessage(), e);
            }
        }

        return written;
    }

    @Override
    public Map<String, Object> transformData(Map<String, Object> sourceData) {
        log.debug("Transforming data for Elasticsearch storage");

        Map<String, Object> transformed = new HashMap<>();

        // 复制所有字段
        transformed.putAll(sourceData);

        // 标准化字段
        String id = (String) sourceData.getOrDefault("id", UUID.randomUUID().toString());
        transformed.put("id", id);
        transformed.put("source_storage", "elasticsearch");
        transformed.put("transfer_timestamp", Instant.now().toString());

        // Elasticsearch特定的元数据
        Map<String, Object> metadata = new HashMap<>();
        metadata.put("original_id", sourceData.get("id"));
        metadata.put("transferred_from", sourceData.getOrDefault("source_storage", "unknown"));
        metadata.put("transformation_version", "1.0");
        metadata.put("index_name", properties.getIndexName());
        transformed.put("transfer_metadata", metadata);

        // 优化全文搜索：合并主要文本字段
        if (sourceData.containsKey("content") || sourceData.containsKey("text")) {
            String searchableText = String.valueOf(sourceData.getOrDefault("content", 
                                   sourceData.getOrDefault("text", "")));
            transformed.put("searchable_content", searchableText);
        }

        return transformed;
    }

    @Override
    public TransferResult batchTransfer(Map<String, Object> sourceQuery, int batchSize) {
        log.info("Starting batch transfer from Elasticsearch storage");
        long startTime = System.currentTimeMillis();

        try {
            // 读取源数据
            List<Map<String, Object>> sourceData = readFromSource(sourceQuery);
            int totalRecords = sourceData.size();

            log.info("Read {} records from source", totalRecords);

            int successCount = 0;
            int failureCount = 0;

            // 分批处理
            for (int i = 0; i < sourceData.size(); i += batchSize) {
                int end = Math.min(i + batchSize, sourceData.size());
                List<Map<String, Object>> batch = sourceData.subList(i, end);

                log.debug("Processing batch {}/{}: {} records",
                        i / batchSize + 1,
                        (sourceData.size() + batchSize - 1) / batchSize,
                        batch.size());

                // 转换数据
                List<Map<String, Object>> transformedBatch = batch.stream()
                        .map(this::transformData)
                        .collect(Collectors.toList());

                // 写入目标
                int written = writeToTarget(transformedBatch);
                successCount += written;
                failureCount += (batch.size() - written);
            }

            long duration = System.currentTimeMillis() - startTime;

            log.info("Batch transfer completed: {} total, {} succeeded, {} failed, duration={}ms",
                    totalRecords, successCount, failureCount, duration);

            return new TransferResult(totalRecords, successCount, failureCount, duration);

        } catch (Exception e) {
            long duration = System.currentTimeMillis() - startTime;
            log.error("Batch transfer failed: {}", e.getMessage(), e);

            return new TransferResult(0, 0, 0, duration);
        }
    }

    @Override
    public Map<String, Object> getTransferStatistics() {
        Map<String, Object> stats = new HashMap<>();

        try {
            IndexCoordinates index = IndexCoordinates.of(properties.getIndexName());

            // 获取文档数量
            Query countQuery = Query.findAll();
            long totalRecords = elasticsearchOperations.count(countQuery, index);
            stats.put("total_records", totalRecords);

            stats.put("storage_type", "elasticsearch");
            stats.put("index_name", properties.getIndexName());

            // 获取索引统计信息
            try {
                var indexOps = elasticsearchOperations.indexOps(index);
                if (indexOps.exists()) {
                    stats.put("index_exists", true);
                    // 注意：详细的索引统计需要使用Elasticsearch REST客户端
                } else {
                    stats.put("index_exists", false);
                }
            } catch (Exception e) {
                log.warn("Failed to get index info: {}", e.getMessage());
            }

            stats.put("refresh_after_write", properties.isRefreshAfterWrite());
            stats.put("last_accessed", Instant.now().toString());

            // 按类型统计（使用聚合）
            if (totalRecords < 10000) {
                try {
                    // 简化版本：直接查询不同的type值
                    Query typeQuery = Query.findAll();
                    SearchHits<Map> results = elasticsearchOperations.search(
                            typeQuery,
                            Map.class,
                            index
                    );

                    Map<String, Long> typeDistribution = results.stream()
                            .map(SearchHit::getContent)
                            .collect(Collectors.groupingBy(
                                    content -> String.valueOf(content.getOrDefault("type", "unknown")),
                                    Collectors.counting()
                            ));

                    stats.put("type_distribution", typeDistribution);

                } catch (Exception e) {
                    log.warn("Failed to get type distribution: {}", e.getMessage());
                }
            }

        } catch (Exception e) {
            log.error("Failed to get transfer statistics: {}", e.getMessage(), e);
            stats.put("error", e.getMessage());
        }

        log.debug("Retrieved transfer statistics: {}", stats);
        return stats;
    }
}
