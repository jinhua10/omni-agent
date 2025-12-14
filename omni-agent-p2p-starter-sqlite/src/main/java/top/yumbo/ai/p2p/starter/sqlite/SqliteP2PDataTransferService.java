package top.yumbo.ai.p2p.starter.sqlite;

import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import top.yumbo.ai.p2p.api.P2PDataTransferService;

import java.util.*;
import java.util.stream.Collectors;

/**
 * SQLite P2P数据传输服务
 * (SQLite P2P Data Transfer Service)
 *
 * <p>从SQLite读取数据，支持传输到其他存储</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class SqliteP2PDataTransferService implements P2PDataTransferService {

    private final JdbcTemplate jdbcTemplate;
    private final SqliteP2PProperties properties;

    public SqliteP2PDataTransferService(JdbcTemplate jdbcTemplate, SqliteP2PProperties properties) {
        this.jdbcTemplate = jdbcTemplate;
        this.properties = properties;
        log.info("SqliteP2PDataTransferService initialized with database: {}", properties.getDatabasePath());
        
        if (properties.isAutoCreateTable()) {
            initializeTable();
        }
    }

    private void initializeTable() {
        try {
            String createTableSql = String.format(
                "CREATE TABLE IF NOT EXISTS %s (" +
                "id TEXT PRIMARY KEY, " +
                "content TEXT, " +
                "type TEXT, " +
                "metadata TEXT, " +
                "created_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP, " +
                "updated_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP" +
                ")", properties.getSourceTable()
            );
            jdbcTemplate.execute(createTableSql);
            log.info("Table {} initialized", properties.getSourceTable());
        } catch (Exception e) {
            log.error("Failed to initialize table", e);
        }
    }

    @Override
    public List<Map<String, Object>> readFromSource(Map<String, Object> query) {
        try {
            String sql = buildSelectSql(query);
            List<Object> params = extractParams(query);
            
            List<Map<String, Object>> results = jdbcTemplate.queryForList(sql, params.toArray());
            
            log.info("Read {} records from SQLite table: {}", results.size(), properties.getSourceTable());
            return results;
            
        } catch (Exception e) {
            log.error("Failed to read from SQLite", e);
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
                    "INSERT OR REPLACE INTO %s (id, content, type, metadata, updated_time) VALUES (?, ?, ?, ?, datetime('now'))",
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
            
            log.info("Wrote {} records to SQLite table: {}", count, properties.getSourceTable());
            return count;
            
        } catch (Exception e) {
            log.error("Failed to write to SQLite", e);
            return 0;
        }
    }

    @Override
    public Map<String, Object> transformData(Map<String, Object> sourceData) {
        // 基本转换：确保数据结构标准化
        Map<String, Object> transformed = new HashMap<>();
        
        transformed.put("id", sourceData.getOrDefault("id", UUID.randomUUID().toString()));
        transformed.put("content", sourceData.get("content"));
        transformed.put("type", sourceData.getOrDefault("type", "unknown"));
        transformed.put("metadata", sourceData.get("metadata"));
        transformed.put("source", "sqlite");
        transformed.put("timestamp", System.currentTimeMillis());
        
        return transformed;
    }

    @Override
    public TransferResult batchTransfer(Map<String, Object> sourceQuery, int batchSize) {
        long startTime = System.currentTimeMillis();
        
        List<Map<String, Object>> allData = readFromSource(sourceQuery);
        int totalRecords = allData.size();
        int successCount = 0;
        int failureCount = 0;
        
        // 分批处理
        for (int i = 0; i < allData.size(); i += batchSize) {
            int end = Math.min(i + batchSize, allData.size());
            List<Map<String, Object>> batch = allData.subList(i, end);
            
            // 转换数据
            List<Map<String, Object>> transformed = batch.stream()
                .map(this::transformData)
                .collect(Collectors.toList());
            
            int written = writeToTarget(transformed);
            successCount += written;
            failureCount += (batch.size() - written);
        }
        
        long duration = System.currentTimeMillis() - startTime;
        
        log.info("Batch transfer completed: total={}, success={}, failure={}, duration={}ms",
            totalRecords, successCount, failureCount, duration);
        
        return new TransferResult(totalRecords, successCount, failureCount, duration);
    }

    @Override
    public Map<String, Object> getTransferStatistics() {
        Map<String, Object> stats = new HashMap<>();
        
        try {
            String countSql = String.format("SELECT COUNT(*) FROM %s", properties.getSourceTable());
            Integer count = jdbcTemplate.queryForObject(countSql, Integer.class);
            
            stats.put("total_records", count != null ? count : 0);
            stats.put("table_name", properties.getSourceTable());
            stats.put("database_path", properties.getDatabasePath());
            stats.put("storage_type", "sqlite");
            
        } catch (Exception e) {
            log.error("Failed to get statistics", e);
            stats.put("error", e.getMessage());
        }
        
        return stats;
    }

    // ========== 辅助方法 ==========

    private String buildSelectSql(Map<String, Object> query) {
        StringBuilder sql = new StringBuilder("SELECT * FROM ");
        sql.append(properties.getSourceTable());
        
        if (query != null && !query.isEmpty()) {
            sql.append(" WHERE ");
            List<String> conditions = new ArrayList<>();
            
            for (String key : query.keySet()) {
                if (!key.equals("limit") && !key.equals("offset")) {
                    conditions.add(key + " = ?");
                }
            }
            
            sql.append(String.join(" AND ", conditions));
        }
        
        // 添加limit和offset
        if (query != null) {
            if (query.containsKey("limit")) {
                sql.append(" LIMIT ").append(query.get("limit"));
            }
            if (query.containsKey("offset")) {
                sql.append(" OFFSET ").append(query.get("offset"));
            }
        }
        
        return sql.toString();
    }

    private List<Object> extractParams(Map<String, Object> query) {
        if (query == null || query.isEmpty()) {
            return Collections.emptyList();
        }
        
        return query.entrySet().stream()
            .filter(e -> !e.getKey().equals("limit") && !e.getKey().equals("offset"))
            .map(Map.Entry::getValue)
            .collect(Collectors.toList());
    }
}
