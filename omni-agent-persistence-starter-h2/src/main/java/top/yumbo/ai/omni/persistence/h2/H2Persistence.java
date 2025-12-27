package top.yumbo.ai.omni.persistence.h2;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.omni.persistence.api.model.QuestionTypeConfig;

import java.sql.*;
import java.util.*;

/**
 * H2 数据库持久化实现 - 嵌入式 SQL 数据库
 * (H2 Database Persistence Implementation - Embedded SQL Database)
 *
 * <p>
 * 特点 (Features):
 * - 嵌入式数据库，无需外部服务
 * - SQL 查询支持，复杂查询能力
 * - 事务支持，ACID 保证
 * - 适合中等规模（<100K 类型）
 * - 使用 HikariCP 连接池保证线程安全
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - H2 Starter 实现
 */
@Slf4j
public class H2Persistence implements QuestionClassifierPersistence {

    private final HikariDataSource dataSource;
    private final ObjectMapper objectMapper;

    public H2Persistence(H2PersistenceProperties properties) {
        this.objectMapper = new ObjectMapper();
        this.dataSource = createDataSource(properties);

        if (properties.isAutoCreateTables()) {
            createTables();
        }

        log.info("H2Persistence initialized at: {}", properties.getUrl());
    }

    private HikariDataSource createDataSource(H2PersistenceProperties properties) {
        HikariConfig config = new HikariConfig();
        config.setJdbcUrl(properties.getUrl());
        config.setUsername(properties.getUsername());
        config.setPassword(properties.getPassword());
        config.setMaximumPoolSize(10);
        config.setMinimumIdle(2);
        config.setConnectionTimeout(30000);
        config.setIdleTimeout(600000);
        config.setMaxLifetime(1800000);

        return new HikariDataSource(config);
    }

    private void createTables() {
        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement()) {

            // 问题类型主表
            stmt.execute("""
                CREATE TABLE IF NOT EXISTS question_types (
                    id VARCHAR(255) PRIMARY KEY,
                    name VARCHAR(255) NOT NULL,
                    name_en VARCHAR(255),
                    priority INTEGER DEFAULT 0,
                    complexity VARCHAR(50),
                    suggested_layer VARCHAR(50),
                    enabled BOOLEAN DEFAULT TRUE,
                    data CLOB,
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """);

            // 关键词表
            stmt.execute("""
                CREATE TABLE IF NOT EXISTS keywords (
                    type_id VARCHAR(255),
                    keyword VARCHAR(500),
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    PRIMARY KEY (type_id, keyword),
                    FOREIGN KEY (type_id) REFERENCES question_types(id) ON DELETE CASCADE
                )
            """);

            // 创建关键词索引
            stmt.execute("""
                CREATE INDEX IF NOT EXISTS idx_keywords_type_id ON keywords(type_id)
            """);

            // 模式表
            stmt.execute("""
                CREATE TABLE IF NOT EXISTS patterns (
                    type_id VARCHAR(255),
                    pattern VARCHAR(1000),
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                    PRIMARY KEY (type_id, pattern),
                    FOREIGN KEY (type_id) REFERENCES question_types(id) ON DELETE CASCADE
                )
            """);

            // 创建模式索引
            stmt.execute("""
                CREATE INDEX IF NOT EXISTS idx_patterns_type_id ON patterns(type_id)
            """);

            // 变更历史表
            stmt.execute("""
                CREATE TABLE IF NOT EXISTS change_history (
                    id VARCHAR(255) PRIMARY KEY,
                    change_type VARCHAR(50),
                    type_id VARCHAR(255),
                    description VARCHAR(1000),
                    timestamp BIGINT,
                    user_id VARCHAR(255),
                    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """);

            // 元数据表
            stmt.execute("""
                CREATE TABLE IF NOT EXISTS metadata (
                    key VARCHAR(255) PRIMARY KEY,
                    value VARCHAR(1000),
                    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
                )
            """);

            // 创建启用状态索引
            stmt.execute("""
                CREATE INDEX IF NOT EXISTS idx_question_types_enabled ON question_types(enabled)
            """);

            log.debug("H2 tables created successfully");
        } catch (SQLException e) {
            log.error("Failed to create H2 tables", e);
            throw new RuntimeException("Failed to initialize H2 database", e);
        }
    }

    /**
     * 关闭数据源
     */
    public void shutdown() {
        if (dataSource != null && !dataSource.isClosed()) {
            dataSource.close();
            log.info("H2 DataSource closed");
        }
    }

    // ========== QuestionTypeConfig CRUD ==========

    @Override
    public boolean saveQuestionType(QuestionTypeConfig config) {
        String sql = """
            MERGE INTO question_types (id, name, name_en, priority, complexity, suggested_layer, enabled, data, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
        """;

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            ps.setString(1, config.getId());
            ps.setString(2, config.getName());
            ps.setString(3, config.getNameEn());
            ps.setInt(4, config.getPriority());
            ps.setString(5, config.getComplexity());
            ps.setString(6, config.getSuggestedLayer());
            ps.setBoolean(7, config.isEnabled());
            ps.setString(8, objectMapper.writeValueAsString(config));

            ps.executeUpdate();
            log.debug("Saved question type: {}", config.getId());
            return true;
        } catch (Exception e) {
            log.error("Failed to save question type: {}", config.getId(), e);
            return false;
        }
    }

    @Override
    public int saveQuestionTypes(List<QuestionTypeConfig> configs) {
        String sql = """
            MERGE INTO question_types (id, name, name_en, priority, complexity, suggested_layer, enabled, data, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP)
        """;

        int count = 0;
        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            conn.setAutoCommit(false);

            for (QuestionTypeConfig config : configs) {
                ps.setString(1, config.getId());
                ps.setString(2, config.getName());
                ps.setString(3, config.getNameEn());
                ps.setInt(4, config.getPriority());
                ps.setString(5, config.getComplexity());
                ps.setString(6, config.getSuggestedLayer());
                ps.setBoolean(7, config.isEnabled());
                ps.setString(8, objectMapper.writeValueAsString(config));
                ps.addBatch();
            }

            int[] results = ps.executeBatch();
            conn.commit();

            for (int result : results) {
                if (result > 0) count++;
            }

            log.debug("Saved {} question types", count);
        } catch (Exception e) {
            log.error("Failed to save question types", e);
        }

        return count;
    }

    @Override
    public Optional<QuestionTypeConfig> getQuestionType(String typeId) {
        String sql = "SELECT data FROM question_types WHERE id = ?";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            ps.setString(1, typeId);
            ResultSet rs = ps.executeQuery();

            if (rs.next()) {
                String json = rs.getString("data");
                QuestionTypeConfig config = objectMapper.readValue(json, QuestionTypeConfig.class);
                return Optional.of(config);
            }
        } catch (Exception e) {
            log.error("Failed to get question type: {}", typeId, e);
        }

        return Optional.empty();
    }

    @Override
    public List<QuestionTypeConfig> getAllQuestionTypes() {
        List<QuestionTypeConfig> result = new ArrayList<>();
        String sql = "SELECT data FROM question_types WHERE enabled = TRUE ORDER BY priority DESC";

        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            while (rs.next()) {
                String json = rs.getString("data");
                QuestionTypeConfig config = objectMapper.readValue(json, QuestionTypeConfig.class);
                result.add(config);
            }
        } catch (Exception e) {
            log.error("Failed to get all question types", e);
        }

        return result;
    }

    @Override
    public boolean updateQuestionType(QuestionTypeConfig config) {
        return saveQuestionType(config);
    }

    @Override
    public boolean deleteQuestionType(String typeId) {
        String sql = "DELETE FROM question_types WHERE id = ?";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            ps.setString(1, typeId);
            int rows = ps.executeUpdate();

            if (rows > 0) {
                log.debug("Deleted question type: {}", typeId);
                return true;
            }
        } catch (SQLException e) {
            log.error("Failed to delete question type: {}", typeId, e);
        }

        return false;
    }

    // ========== Keywords Management ==========

    @Override
    public boolean saveKeywords(String typeId, List<String> keywords) {
        try (Connection conn = dataSource.getConnection()) {
            conn.setAutoCommit(false);

            // 删除旧关键词
            try (PreparedStatement ps = conn.prepareStatement("DELETE FROM keywords WHERE type_id = ?")) {
                ps.setString(1, typeId);
                ps.executeUpdate();
            }

            // 插入新关键词
            boolean result = addKeywordsBatch(conn, typeId, keywords);
            conn.commit();

            log.debug("Saved {} keywords for type: {}", keywords.size(), typeId);
            return result;
        } catch (SQLException e) {
            log.error("Failed to save keywords for: {}", typeId, e);
            return false;
        }
    }

    @Override
    public boolean addKeywords(String typeId, List<String> keywords) {
        try (Connection conn = dataSource.getConnection()) {
            return addKeywordsBatch(conn, typeId, keywords);
        } catch (SQLException e) {
            log.error("Failed to add keywords for: {}", typeId, e);
            return false;
        }
    }

    private boolean addKeywordsBatch(Connection conn, String typeId, List<String> keywords) throws SQLException {
        String sql = "MERGE INTO keywords (type_id, keyword) VALUES (?, ?)";

        try (PreparedStatement ps = conn.prepareStatement(sql)) {
            for (String keyword : keywords) {
                ps.setString(1, typeId);
                ps.setString(2, keyword);
                ps.addBatch();
            }
            ps.executeBatch();
            return true;
        }
    }

    @Override
    public List<String> getKeywords(String typeId) {
        List<String> result = new ArrayList<>();
        String sql = "SELECT keyword FROM keywords WHERE type_id = ?";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            ps.setString(1, typeId);
            ResultSet rs = ps.executeQuery();

            while (rs.next()) {
                result.add(rs.getString("keyword"));
            }
        } catch (SQLException e) {
            log.error("Failed to get keywords for: {}", typeId, e);
        }

        return result;
    }

    @Override
    public Map<String, List<String>> getAllKeywords() {
        Map<String, List<String>> result = new HashMap<>();
        String sql = "SELECT type_id, keyword FROM keywords ORDER BY type_id";

        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            while (rs.next()) {
                String typeId = rs.getString("type_id");
                String keyword = rs.getString("keyword");
                result.computeIfAbsent(typeId, k -> new ArrayList<>()).add(keyword);
            }
        } catch (SQLException e) {
            log.error("Failed to get all keywords", e);
        }

        return result;
    }

    // ========== Patterns Management ==========

    @Override
    public boolean savePatterns(String typeId, List<String> patterns) {
        try (Connection conn = dataSource.getConnection()) {
            conn.setAutoCommit(false);

            // 删除旧模式
            try (PreparedStatement ps = conn.prepareStatement("DELETE FROM patterns WHERE type_id = ?")) {
                ps.setString(1, typeId);
                ps.executeUpdate();
            }

            // 插入新模式
            boolean result = addPatternsBatch(conn, typeId, patterns);
            conn.commit();

            log.debug("Saved {} patterns for type: {}", patterns.size(), typeId);
            return result;
        } catch (SQLException e) {
            log.error("Failed to save patterns for: {}", typeId, e);
            return false;
        }
    }

    @Override
    public boolean addPatterns(String typeId, List<String> patterns) {
        try (Connection conn = dataSource.getConnection()) {
            return addPatternsBatch(conn, typeId, patterns);
        } catch (SQLException e) {
            log.error("Failed to add patterns for: {}", typeId, e);
            return false;
        }
    }

    private boolean addPatternsBatch(Connection conn, String typeId, List<String> patterns) throws SQLException {
        String sql = "MERGE INTO patterns (type_id, pattern) VALUES (?, ?)";

        try (PreparedStatement ps = conn.prepareStatement(sql)) {
            for (String pattern : patterns) {
                ps.setString(1, typeId);
                ps.setString(2, pattern);
                ps.addBatch();
            }
            ps.executeBatch();
            return true;
        }
    }

    @Override
    public List<String> getPatterns(String typeId) {
        List<String> result = new ArrayList<>();
        String sql = "SELECT pattern FROM patterns WHERE type_id = ?";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            ps.setString(1, typeId);
            ResultSet rs = ps.executeQuery();

            while (rs.next()) {
                result.add(rs.getString("pattern"));
            }
        } catch (SQLException e) {
            log.error("Failed to get patterns for: {}", typeId, e);
        }

        return result;
    }

    @Override
    public Map<String, List<String>> getAllPatterns() {
        Map<String, List<String>> result = new HashMap<>();
        String sql = "SELECT type_id, pattern FROM patterns ORDER BY type_id";

        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            while (rs.next()) {
                String typeId = rs.getString("type_id");
                String pattern = rs.getString("pattern");
                result.computeIfAbsent(typeId, k -> new ArrayList<>()).add(pattern);
            }
        } catch (SQLException e) {
            log.error("Failed to get all patterns", e);
        }

        return result;
    }

    // ========== Backup & Restore ==========

    @Override
    public String createBackup() {
        String backupId = "h2_backup_" + System.currentTimeMillis();
        String backupPath = "./data/backups/" + backupId + ".sql";

        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement()) {

            stmt.execute("SCRIPT TO '" + backupPath + "'");
            log.info("Created H2 backup: {}", backupId);
            return backupId;
        } catch (SQLException e) {
            log.error("Failed to create backup", e);
            return null;
        }
    }

    @Override
    public boolean restoreFromBackup(String backupId) {
        String backupPath = "./data/backups/" + backupId + ".sql";

        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement()) {

            stmt.execute("RUNSCRIPT FROM '" + backupPath + "'");
            log.info("Restored from H2 backup: {}", backupId);
            return true;
        } catch (SQLException e) {
            log.error("Failed to restore from backup: {}", backupId, e);
            return false;
        }
    }

    @Override
    public List<String> listBackups() {
        // 简化实现：返回空列表
        // 可以扩展为扫描 ./data/backups/ 目录
        return new ArrayList<>();
    }

    // ========== Version Management ==========

    @Override
    public String getVersion() {
        String sql = "SELECT value FROM metadata WHERE key = 'version'";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            ResultSet rs = ps.executeQuery();
            if (rs.next()) {
                return rs.getString("value");
            }
        } catch (SQLException e) {
            log.error("Failed to get version", e);
        }

        return "1.0.0"; // 默认版本
    }

    @Override
    public boolean saveVersion(String version) {
        String sql = "MERGE INTO metadata (key, value, updated_at) VALUES ('version', ?, CURRENT_TIMESTAMP)";

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            ps.setString(1, version);
            ps.executeUpdate();
            log.debug("Saved version: {}", version);
            return true;
        } catch (SQLException e) {
            log.error("Failed to save version: {}", version, e);
            return false;
        }
    }

    // ========== Change History ==========

    @Override
    public List<ChangeRecord> getChangeHistory(int limit) {
        List<ChangeRecord> result = new ArrayList<>();
        String sql = """
            SELECT id, change_type, type_id, description, timestamp, user_id
            FROM change_history
            ORDER BY timestamp DESC
            LIMIT ?
        """;

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            ps.setInt(1, limit);
            ResultSet rs = ps.executeQuery();

            while (rs.next()) {
                result.add(new H2ChangeRecord(
                    rs.getString("id"),
                    rs.getString("type_id"),
                    rs.getString("change_type"),
                    rs.getString("user_id"),
                    rs.getLong("timestamp"),
                    Map.of("description", rs.getString("description"))
                ));
            }
        } catch (SQLException e) {
            log.error("Failed to get change history", e);
        }

        return result;
    }

    @Override
    public boolean recordChange(ChangeRecord change) {
        String sql = """
            INSERT INTO change_history (id, change_type, type_id, description, timestamp, user_id)
            VALUES (?, ?, ?, ?, ?, ?)
        """;

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            ps.setString(1, change.getId());
            ps.setString(2, change.getAction());
            ps.setString(3, change.getTypeId());
            ps.setString(4, change.getDetails().get("description").toString());
            ps.setLong(5, change.getTimestamp());
            ps.setString(6, change.getOperator());

            ps.executeUpdate();
            log.debug("Recorded change: {}", change.getId());
            return true;
        } catch (SQLException e) {
            log.error("Failed to record change", e);
            return false;
        }
    }

    // ========== 内部类：H2ChangeRecord ==========

    private static class H2ChangeRecord implements ChangeRecord {
        private final String id;
        private final String typeId;
        private final String action;
        private final String operator;
        private final long timestamp;
        private final Map<String, Object> details;

        public H2ChangeRecord(String id, String typeId, String action, String operator, long timestamp, Map<String, Object> details) {
            this.id = id;
            this.typeId = typeId;
            this.action = action;
            this.operator = operator;
            this.timestamp = timestamp;
            this.details = details;
        }

        @Override
        public String getId() {
            return id;
        }

        @Override
        public String getTypeId() {
            return typeId;
        }

        @Override
        public String getAction() {
            return action;
        }

        @Override
        public String getOperator() {
            return operator;
        }

        @Override
        public long getTimestamp() {
            return timestamp;
        }

        @Override
        public Map<String, Object> getDetails() {
            return details;
        }
    }
}

