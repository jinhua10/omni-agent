package top.yumbo.ai.omni.p2p.starter.h2;

import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

/**
 * H2 P2P数据传输服务
 * (H2 P2P Data Transfer Service)
 *
 * <p>从H2数据库读取数据，支持传输到其他存储</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class H2P2PDataTransferService implements P2PDataTransferService {

    private final JdbcTemplate jdbcTemplate;
    private final H2P2PProperties properties;
    
    // 统计信息
    private final AtomicInteger totalReadCount = new AtomicInteger(0);
    private final AtomicInteger totalWriteCount = new AtomicInteger(0);
    private final AtomicLong totalDuration = new AtomicLong(0);

    public H2P2PDataTransferService(JdbcTemplate jdbcTemplate, H2P2PProperties properties) {
        this.jdbcTemplate = jdbcTemplate;
        this.properties = properties;
        log.info("H2P2PDataTransferService initialized with database: {}", properties.getJdbcUrl());
        
        if (properties.isAutoCreateTable()) {
            initializeTable();
        }
    }

    private void initializeTable() {
        try {
            String createTableSql = String.format(
                "CREATE TABLE IF NOT EXISTS %s (" +
                "id VARCHAR(255) PRIMARY KEY, " +
                "content TEXT, " +
                "type VARCHAR(100), " +
                "metadata TEXT, " +
                "created_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP, " +
                "updated_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP" +
                ")", properties.getSourceTable()
            );
            jdbcTemplate.execute(createTableSql);
            log.info("Table {} initialized in H2 database", properties.getSourceTable());
        } catch (Exception e) {
            log.error("Failed to initialize H2 table", e);
        }
    }

    @Override
    public List<Map<String, Object>> readFromSource(Map<String, Object> query) {
        try {
            String sql = buildSelectSql(query);
            List<Object> params = extractParams(query);
            
            List<Map<String, Object>> results = jdbcTemplate.queryForList(sql, params.toArray());
            
            log.info("Read {} records from H2 table: {}", results.size(), properties.getSourceTable());
            return results;
            
        } catch (Exception e) {
            log.error("Failed to read from H2 database", e);
            return Collections.emptyList();
        }
    }

    @Override
    public int writeToTarget(List<Map<String, Object>> data) {
        if (data == null || data.isEmpty()) {
            return 0;
        }

        try {
            int count = 0;
            for (Map<String, Object> record : data) {
                String sql = String.format(
                    "MERGE INTO %s (id, content, type, metadata, updated_time) " +
                    "KEY(id) VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP)",
                    properties.getSourceTable()
                );
                
                jdbcTemplate.update(sql,
                    record.get("id"),
                    record.get("content"),
                    record.get("type"),
                    record.get("metadata")
                );
                count++;
            }
            
            totalWriteCount.addAndGet(count);
            log.info("Wrote {} records to H2 table: {}", count, properties.getSourceTable());
            return count;
            
        } catch (Exception e) {
            log.error("Failed to write to H2 database", e);
            return 0;
        }
    }

    @Override
    public Map<String, Object> transformData(Map<String, Object> sourceData) {
        // 默认不做转换，直接返回原数据
        return sourceData;
    }

    @Override
    public TransferResult batchTransfer(Map<String, Object> sourceQuery, int batchSize) {
        long startTime = System.currentTimeMillis();
        List<String> errors = new ArrayList<>();
        
        try {
            List<Map<String, Object>> data = readFromSource(sourceQuery);
            totalReadCount.addAndGet(data.size());
            
            int totalCount = data.size();
            int successCount = 0;
            
            // 分批处理
            for (int i = 0; i < totalCount; i += batchSize) {
                int end = Math.min(i + batchSize, totalCount);
                List<Map<String, Object>> batch = data.subList(i, end);
                
                try {
                    successCount += writeToTarget(batch);
                } catch (Exception e) {
                    errors.add("Batch " + i + "-" + end + " failed: " + e.getMessage());
                    log.error("Batch transfer failed", e);
                }
            }
            
            long duration = System.currentTimeMillis() - startTime;
            totalDuration.addAndGet(duration);
            
            TransferResult result = new TransferResult(
                totalCount,
                successCount,
                totalCount - successCount,
                duration
            );
            result.setErrors(errors);
            
            return result;
            
        } catch (Exception e) {
            long duration = System.currentTimeMillis() - startTime;
            totalDuration.addAndGet(duration);
            log.error("Transfer failed", e);
            
            errors.add("Transfer failed: " + e.getMessage());
            TransferResult result = new TransferResult(0, 0, 0, duration);
            result.setErrors(errors);
            return result;
        }
    }

    @Override
    public Map<String, Object> getTransferStatistics() {
        Map<String, Object> stats = new HashMap<>();
        
        try {
            String countSql = String.format("SELECT COUNT(*) FROM %s", properties.getSourceTable());
            Integer count = jdbcTemplate.queryForObject(countSql, Integer.class);
            
            stats.put("total_records", count);
            stats.put("table_name", properties.getSourceTable());
            stats.put("database_mode", properties.getMode());
            stats.put("database_path", properties.getDatabasePath());
            stats.put("total_read_count", totalReadCount.get());
            stats.put("total_write_count", totalWriteCount.get());
            stats.put("total_duration_ms", totalDuration.get());
            
            log.debug("H2 transfer statistics: {}", stats);
            
        } catch (Exception e) {
            log.error("Failed to get H2 statistics", e);
            stats.put("error", e.getMessage());
        }
        
        return stats;
    }

    /**
     * 构建SELECT SQL语句
     * (Build SELECT SQL statement)
     */
    private String buildSelectSql(Map<String, Object> query) {
        StringBuilder sql = new StringBuilder("SELECT * FROM ");
        sql.append(properties.getSourceTable());
        
        if (query != null && !query.isEmpty()) {
            sql.append(" WHERE ");
            List<String> conditions = new ArrayList<>();
            
            for (String key : query.keySet()) {
                if ("limit".equals(key) || "offset".equals(key)) {
                    continue;
                }
                conditions.add(key + " = ?");
            }
            
            sql.append(String.join(" AND ", conditions));
        }
        
        // 处理分页
        if (query != null && query.containsKey("limit")) {
            sql.append(" LIMIT ").append(query.get("limit"));
        }
        if (query != null && query.containsKey("offset")) {
            sql.append(" OFFSET ").append(query.get("offset"));
        }
        
        return sql.toString();
    }

    /**
     * 从查询条件中提取参数
     * (Extract parameters from query conditions)
     */
    private List<Object> extractParams(Map<String, Object> query) {
        if (query == null || query.isEmpty()) {
            return Collections.emptyList();
        }
        
        return query.entrySet().stream()
            .filter(entry -> !"limit".equals(entry.getKey()) && !"offset".equals(entry.getKey()))
            .map(Map.Entry::getValue)
            .collect(Collectors.toList());
    }
}
