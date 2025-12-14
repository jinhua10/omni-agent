package top.yumbo.ai.p2p.starter.redis;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import top.yumbo.ai.p2p.api.P2PDataTransferService;

import java.time.Instant;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Redis P2P数据传输服务实现
 * (Redis P2P Data Transfer Service Implementation)
 *
 * <p>基于Redis的分布式数据传输实现</p>
 * <p>Redis-based distributed data transfer implementation</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class RedisP2PDataTransferService implements P2PDataTransferService {

    private final RedisTemplate<String, Object> redisTemplate;
    private final RedisP2PProperties properties;
    private final ObjectMapper objectMapper;

    private static final String DATA_PREFIX = "p2p:data:";
    private static final String STATS_KEY = "p2p:stats";

    public RedisP2PDataTransferService(RedisTemplate<String, Object> redisTemplate,
                                      RedisP2PProperties properties,
                                      ObjectMapper objectMapper) {
        this.redisTemplate = redisTemplate;
        this.properties = properties;
        this.objectMapper = objectMapper;
        log.info("RedisP2PDataTransferService initialized with Redis: {}:{}",
                properties.getHost(), properties.getPort());
    }

    @Override
    public List<Map<String, Object>> readFromSource(Map<String, Object> query) {
        log.debug("Reading from Redis with query: {}", query);

        String keyPattern = DATA_PREFIX + "*";
        if (query.containsKey("key_pattern")) {
            keyPattern = DATA_PREFIX + query.get("key_pattern");
        }

        Set<String> keys = redisTemplate.keys(keyPattern);
        if (keys == null || keys.isEmpty()) {
            log.debug("No keys found matching pattern: {}", keyPattern);
            return Collections.emptyList();
        }

        List<Map<String, Object>> results = new ArrayList<>();
        for (String key : keys) {
            try {
                @SuppressWarnings("unchecked")
                Map<String, Object> record = (Map<String, Object>) redisTemplate.opsForValue().get(key);
                if (record != null) {
                    results.add(record);
                }
            } catch (Exception e) {
                log.warn("Failed to read key {}: {}", key, e.getMessage());
            }
        }

        // 应用过滤条件
        if (query.containsKey("type")) {
            String type = (String) query.get("type");
            results = results.stream()
                    .filter(record -> type.equals(record.get("type")))
                    .collect(Collectors.toList());
        }

        // 应用分页
        int offset = (int) query.getOrDefault("offset", 0);
        int limit = (int) query.getOrDefault("limit", Integer.MAX_VALUE);

        results = results.stream()
                .skip(offset)
                .limit(limit)
                .collect(Collectors.toList());

        log.debug("Read {} records from Redis", results.size());
        return results;
    }

    @Override
    public int writeToTarget(List<Map<String, Object>> data) {
        log.debug("Writing {} records to Redis", data.size());

        int written = 0;
        for (Map<String, Object> record : data) {
            try {
                String id = (String) record.getOrDefault("id", UUID.randomUUID().toString());
                String key = DATA_PREFIX + id;

                // 添加时间戳
                Map<String, Object> enrichedRecord = new HashMap<>(record);
                enrichedRecord.put("id", id);
                enrichedRecord.putIfAbsent("created_at", Instant.now().toString());
                enrichedRecord.put("updated_at", Instant.now().toString());

                // 写入Redis
                redisTemplate.opsForValue().set(key, enrichedRecord);

                // 设置过期时间（如果配置了）
                if (properties.getDataTtlHours() > 0) {
                    redisTemplate.expire(key, properties.getDataTtlHours(), TimeUnit.HOURS);
                }

                written++;

                // 更新统计
                redisTemplate.opsForHash().increment(STATS_KEY, "total_writes", 1);

            } catch (Exception e) {
                log.error("Failed to write record to Redis: {}", e.getMessage());
            }
        }

        log.info("Successfully wrote {} records to Redis", written);
        return written;
    }

    @Override
    public Map<String, Object> transformData(Map<String, Object> sourceData) {
        log.debug("Transforming data for Redis storage");

        Map<String, Object> transformed = new HashMap<>();

        // 复制所有字段
        transformed.putAll(sourceData);

        // 标准化字段
        transformed.putIfAbsent("id", UUID.randomUUID().toString());
        transformed.put("source_storage", "redis");
        transformed.put("transfer_timestamp", Instant.now().toString());

        // Redis特定的元数据
        Map<String, Object> metadata = new HashMap<>();
        metadata.put("original_id", sourceData.get("id"));
        metadata.put("transferred_from", sourceData.getOrDefault("source_storage", "unknown"));
        metadata.put("transformation_version", "1.0");
        metadata.put("redis_host", properties.getHost());
        metadata.put("redis_port", properties.getPort());
        transformed.put("transfer_metadata", metadata);

        return transformed;
    }

    @Override
    public TransferResult batchTransfer(Map<String, Object> sourceQuery, int batchSize) {
        log.info("Starting batch transfer from Redis storage");
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

                // 使用Pipeline批量写入
                redisTemplate.executePipelined((org.springframework.data.redis.core.RedisCallback<?>) connection -> {
                    transformedBatch.forEach(record -> {
                        String id = (String) record.get("id");
                        String key = DATA_PREFIX + id;
                        redisTemplate.opsForValue().set(key, record);
                    });
                    return null;
                });

                successCount += batch.size();
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

        // 获取键数量
        Set<String> keys = redisTemplate.keys(DATA_PREFIX + "*");
        int totalRecords = keys != null ? keys.size() : 0;

        stats.put("total_records", totalRecords);
        stats.put("storage_type", "redis");
        stats.put("redis_host", properties.getHost());
        stats.put("redis_port", properties.getPort());

        // 从统计哈希表获取写入次数
        Object totalWrites = redisTemplate.opsForHash().get(STATS_KEY, "total_writes");
        stats.put("cumulative_writes", totalWrites != null ? totalWrites : 0);

        stats.put("data_ttl_hours", properties.getDataTtlHours());
        stats.put("last_accessed", Instant.now().toString());

        // 按类型统计（需要扫描所有键）
        if (totalRecords < 1000) { // 仅在数据量不大时统计
            Map<String, Long> typeDistribution = new HashMap<>();
            if (keys != null) {
                for (String key : keys) {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> record = (Map<String, Object>) redisTemplate.opsForValue().get(key);
                    if (record != null) {
                        String type = (String) record.getOrDefault("type", "unknown");
                        typeDistribution.put(type, typeDistribution.getOrDefault(type, 0L) + 1);
                    }
                }
            }
            stats.put("type_distribution", typeDistribution);
        }

        log.debug("Retrieved transfer statistics: {}", stats);
        return stats;
    }

    private String getKey(String suffix) {
        return properties.getKeyPrefix() + suffix;
    }
}
