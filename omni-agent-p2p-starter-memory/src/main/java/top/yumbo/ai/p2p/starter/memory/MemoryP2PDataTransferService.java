package top.yumbo.ai.p2p.starter.memory;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.p2p.api.P2PDataTransferService;

import java.time.Instant;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 内存P2P数据传输服务实现
 * (Memory P2P Data Transfer Service Implementation)
 *
 * <p>基于内存的数据传输实现，适合开发和测试</p>
 * <p>Memory-based data transfer implementation, suitable for development and testing</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class MemoryP2PDataTransferService implements P2PDataTransferService {

    // 内存存储
    private final Map<String, Map<String, Object>> storage = new ConcurrentHashMap<>();
    private long recordCount = 0;

    @Override
    public List<Map<String, Object>> readFromSource(Map<String, Object> query) {
        log.debug("Reading from memory storage with query: {}", query);

        List<Map<String, Object>> results = new ArrayList<>(storage.values());

        // 应用过滤条件
        if (query.containsKey("type")) {
            String type = (String) query.get("type");
            results = results.stream()
                    .filter(record -> type.equals(record.get("type")))
                    .collect(Collectors.toList());
        }

        if (query.containsKey("id")) {
            String id = (String) query.get("id");
            results = results.stream()
                    .filter(record -> id.equals(record.get("id")))
                    .collect(Collectors.toList());
        }

        // 应用分页
        int offset = (int) query.getOrDefault("offset", 0);
        int limit = (int) query.getOrDefault("limit", Integer.MAX_VALUE);

        results = results.stream()
                .skip(offset)
                .limit(limit)
                .collect(Collectors.toList());

        log.debug("Read {} records from memory storage", results.size());
        return results;
    }

    @Override
    public int writeToTarget(List<Map<String, Object>> data) {
        log.debug("Writing {} records to memory storage", data.size());

        int written = 0;
        for (Map<String, Object> record : data) {
            try {
                String id = (String) record.getOrDefault("id", UUID.randomUUID().toString());
                
                // 添加时间戳
                Map<String, Object> enrichedRecord = new HashMap<>(record);
                enrichedRecord.put("id", id);
                enrichedRecord.putIfAbsent("created_at", Instant.now().toString());
                enrichedRecord.put("updated_at", Instant.now().toString());

                storage.put(id, enrichedRecord);
                written++;
                recordCount++;
            } catch (Exception e) {
                log.error("Failed to write record to memory: {}", e.getMessage());
            }
        }

        log.info("Successfully wrote {} records to memory storage", written);
        return written;
    }

    @Override
    public Map<String, Object> transformData(Map<String, Object> sourceData) {
        log.debug("Transforming data for memory storage");

        Map<String, Object> transformed = new HashMap<>();

        // 复制所有字段
        transformed.putAll(sourceData);

        // 标准化字段
        transformed.putIfAbsent("id", UUID.randomUUID().toString());
        transformed.put("source_storage", "memory");
        transformed.put("transfer_timestamp", Instant.now().toString());

        // 元数据
        Map<String, Object> metadata = new HashMap<>();
        metadata.put("original_id", sourceData.get("id"));
        metadata.put("transferred_from", sourceData.getOrDefault("source_storage", "unknown"));
        metadata.put("transformation_version", "1.0");
        transformed.put("transfer_metadata", metadata);

        return transformed;
    }

    @Override
    public TransferResult batchTransfer(Map<String, Object> sourceQuery, int batchSize) {
        log.info("Starting batch transfer from memory storage");
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
        stats.put("total_records", storage.size());
        stats.put("cumulative_writes", recordCount);
        stats.put("storage_type", "memory");
        stats.put("memory_size_estimate_kb", storage.size() * 2); // 粗略估计
        stats.put("last_accessed", Instant.now().toString());
        
        // 按类型统计
        Map<String, Long> typeDistribution = storage.values().stream()
                .collect(Collectors.groupingBy(
                        record -> (String) record.getOrDefault("type", "unknown"),
                        Collectors.counting()
                ));
        stats.put("type_distribution", typeDistribution);

        log.debug("Retrieved transfer statistics: {}", stats);
        return stats;
    }

    /**
     * 清除所有数据（测试用）
     */
    public void clear() {
        storage.clear();
        recordCount = 0;
        log.info("Cleared memory storage");
    }

    /**
     * 获取当前存储的记录数
     */
    public int size() {
        return storage.size();
    }
}
