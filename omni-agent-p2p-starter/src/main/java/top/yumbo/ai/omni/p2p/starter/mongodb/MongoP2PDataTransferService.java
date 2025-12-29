package top.yumbo.ai.omni.p2p.starter.mongodb;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;

import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

/**
 * MongoDB P2P数据传输服务实现
 * (MongoDB P2P Data Transfer Service Implementation)
 *
 * <p>基于MongoDB的文档存储数据传输实现</p>
 * <p>MongoDB-based document storage data transfer implementation</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class MongoP2PDataTransferService implements P2PDataTransferService {

    private final MongoTemplate mongoTemplate;
    private final MongoP2PProperties properties;

    public MongoP2PDataTransferService(MongoTemplate mongoTemplate, MongoP2PProperties properties) {
        this.mongoTemplate = mongoTemplate;
        this.properties = properties;
        log.info("MongoP2PDataTransferService initialized with MongoDB");
    }

    @Override
    public List<Map<String, Object>> readFromSource(Map<String, Object> query) {
        log.debug("Reading from MongoDB with query: {}", query);

        Query mongoQuery = new Query();

        // 构建查询条件
        if (query.containsKey("type")) {
            mongoQuery.addCriteria(Criteria.where("type").is(query.get("type")));
        }

        if (query.containsKey("id")) {
            mongoQuery.addCriteria(Criteria.where("id").is(query.get("id")));
        }

        // 支持更多查询条件
        if (query.containsKey("created_after")) {
            mongoQuery.addCriteria(Criteria.where("created_at").gte(query.get("created_after")));
        }

        if (query.containsKey("created_before")) {
            mongoQuery.addCriteria(Criteria.where("created_at").lte(query.get("created_before")));
        }

        // 应用分页
        int offset = (int) query.getOrDefault("offset", 0);
        int limit = (int) query.getOrDefault("limit", 1000);
        mongoQuery.skip(offset).limit(limit);

        // 排序
        if (query.containsKey("sort_by")) {
            String sortBy = (String) query.get("sort_by");
            boolean ascending = (boolean) query.getOrDefault("ascending", true);
            mongoQuery.with(org.springframework.data.domain.Sort.by(
                    ascending ? org.springframework.data.domain.Sort.Direction.ASC
                             : org.springframework.data.domain.Sort.Direction.DESC,
                    sortBy
            ));
        }

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> results = (List<Map<String, Object>>) (List<?>) 
                mongoTemplate.find(mongoQuery, Map.class, properties.getDataCollectionName());

        log.debug("Read {} records from MongoDB", results.size());
        return results;
    }

    @Override
    public int writeToTarget(List<Map<String, Object>> data) {
        log.debug("Writing {} records to MongoDB", data.size());

        int written = 0;
        List<Map<String, Object>> enrichedRecords = new ArrayList<>();

        for (Map<String, Object> record : data) {
            try {
                String id = (String) record.getOrDefault("id", UUID.randomUUID().toString());

                // 添加时间戳
                Map<String, Object> enrichedRecord = new HashMap<>(record);
                enrichedRecord.put("id", id);
                enrichedRecord.put("_id", id); // MongoDB使用_id作为主键
                enrichedRecord.putIfAbsent("created_at", Instant.now().toString());
                enrichedRecord.put("updated_at", Instant.now().toString());

                enrichedRecords.add(enrichedRecord);

            } catch (Exception e) {
                log.error("Failed to prepare record for MongoDB: {}", e.getMessage());
            }
        }

        // 批量写入
        if (!enrichedRecords.isEmpty()) {
            try {
                mongoTemplate.insertAll(enrichedRecords);
                written = enrichedRecords.size();
                log.info("Successfully wrote {} records to MongoDB", written);
            } catch (Exception e) {
                // 如果批量插入失败，尝试逐个插入（处理重复键等问题）
                log.warn("Batch insert failed, trying individual inserts: {}", e.getMessage());
                for (Map<String, Object> record : enrichedRecords) {
                    try {
                        mongoTemplate.save(record, properties.getDataCollectionName());
                        written++;
                    } catch (Exception ex) {
                        log.error("Failed to write individual record: {}", ex.getMessage());
                    }
                }
            }
        }

        return written;
    }

    @Override
    public Map<String, Object> transformData(Map<String, Object> sourceData) {
        log.debug("Transforming data for MongoDB storage");

        Map<String, Object> transformed = new HashMap<>();

        // 复制所有字段
        transformed.putAll(sourceData);

        // 标准化字段
        String id = (String) sourceData.getOrDefault("id", UUID.randomUUID().toString());
        transformed.put("id", id);
        transformed.put("_id", id); // MongoDB主键
        transformed.put("source_storage", "mongodb");
        transformed.put("transfer_timestamp", Instant.now().toString());

        // MongoDB特定的元数据
        Map<String, Object> metadata = new HashMap<>();
        metadata.put("original_id", sourceData.get("id"));
        metadata.put("transferred_from", sourceData.getOrDefault("source_storage", "unknown"));
        metadata.put("transformation_version", "1.0");
        metadata.put("collection_name", properties.getDataCollectionName());
        transformed.put("transfer_metadata", metadata);

        return transformed;
    }

    @Override
    public TransferResult batchTransfer(Map<String, Object> sourceQuery, int batchSize) {
        log.info("Starting batch transfer from MongoDB storage");
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

        // 获取文档数量
        long totalRecords = mongoTemplate.count(new Query(), properties.getDataCollectionName());
        stats.put("total_records", totalRecords);
        stats.put("storage_type", "mongodb");
        stats.put("collection_name", properties.getDataCollectionName());
        stats.put("database_name", mongoTemplate.getDb().getName());

        // 获取集合统计信息
        try {
            org.bson.Document collStats = mongoTemplate.getDb()
                    .runCommand(new org.bson.Document("collStats", properties.getDataCollectionName()));
            
            stats.put("storage_size_bytes", collStats.get("storageSize"));
            stats.put("index_size_bytes", collStats.get("totalIndexSize"));
            stats.put("average_object_size_bytes", collStats.get("avgObjSize"));
            
        } catch (Exception e) {
            log.warn("Failed to get collection stats: {}", e.getMessage());
        }

        stats.put("last_accessed", Instant.now().toString());

        // 按类型统计（限制数量避免性能问题）
        if (totalRecords < 10000) {
            try {
                List<org.bson.Document> pipeline = Arrays.asList(
                        new org.bson.Document("$group", new org.bson.Document("_id", "$type")
                                .append("count", new org.bson.Document("$sum", 1)))
                );
                
                List<org.bson.Document> results = mongoTemplate.getDb()
                        .getCollection(properties.getDataCollectionName())
                        .aggregate(pipeline)
                        .into(new ArrayList<>());

                Map<String, Object> typeDistribution = new HashMap<>();
                for (org.bson.Document doc : results) {
                    String type = doc.getString("_id");
                    Integer count = doc.getInteger("count");
                    typeDistribution.put(type != null ? type : "unknown", count);
                }
                stats.put("type_distribution", typeDistribution);
                
            } catch (Exception e) {
                log.warn("Failed to get type distribution: {}", e.getMessage());
            }
        }

        log.debug("Retrieved transfer statistics: {}", stats);
        return stats;
    }
}
