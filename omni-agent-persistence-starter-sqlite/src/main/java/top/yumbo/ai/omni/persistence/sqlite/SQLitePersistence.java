package top.yumbo.ai.omni.persistence.sqlite;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.omni.persistence.api.model.QuestionTypeConfig;

import java.sql.*;
import java.util.*;

/**
 * SQLite 数据库持久化实现 - 轻量级嵌入式数据库
 * (SQLite Database Persistence Implementation - Lightweight Embedded Database)
 *
 * <p>
 * 特点 (Features):
 * - 单文件数据库，易于备份和迁移
 * - 零配置，无需服务器
 * - 跨平台支持
 * - 适合小规模部署（<10K 类型）
 * - 使用 HikariCP 连接池
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - SQLite Starter 实现
 */
@Slf4j
public class SQLitePersistence implements QuestionClassifierPersistence {

    private final HikariDataSource dataSource;
    private final ObjectMapper objectMapper;

    public SQLitePersistence(SQLitePersistenceProperties properties) {
        this.objectMapper = new ObjectMapper();
        this.dataSource = createDataSource(properties);

        if (properties.isAutoCreateTables()) {
            createTables();
        }

        log.info("SQLitePersistence initialized at: {}", properties.getDbPath());
    }

    private HikariDataSource createDataSource(SQLitePersistenceProperties properties) {
        HikariConfig config = new HikariConfig();
        config.setJdbcUrl("jdbc:sqlite:" + properties.getDbPath());
        config.setMaximumPoolSize(5); // SQLite 推荐较小连接池
        config.setMinimumIdle(1);
        config.setConnectionTimeout(properties.getConnectionTimeout());

        return new HikariDataSource(config);
    }

    private void createTables() {
        try (Connection conn = dataSource.getConnection();
             Statement stmt = conn.createStatement()) {

            // SQLite 不支持 CLOB，使用 TEXT
            // 问题类型主表
            stmt.execute("""
                CREATE TABLE IF NOT EXISTS question_types (
                    id TEXT PRIMARY KEY,
                    name TEXT NOT NULL,
                    name_en TEXT,
                    priority INTEGER DEFAULT 0,
                    complexity TEXT,
                    suggested_layer TEXT,
                    enabled INTEGER DEFAULT 1,
                    data TEXT,
                    created_at TEXT DEFAULT (datetime('now')),
                    updated_at TEXT DEFAULT (datetime('now'))
                )
            """);

            // 关键词表
            stmt.execute("""
                CREATE TABLE IF NOT EXISTS keywords (
                    type_id TEXT,
                    keyword TEXT,
                    created_at TEXT DEFAULT (datetime('now')),
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
                    type_id TEXT,
                    pattern TEXT,
                    created_at TEXT DEFAULT (datetime('now')),
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
                    id TEXT PRIMARY KEY,
                    change_type TEXT,
                    type_id TEXT,
                    description TEXT,
                    timestamp INTEGER,
                    user_id TEXT,
                    created_at TEXT DEFAULT (datetime('now'))
                )
            """);

            // 元数据表
            stmt.execute("""
                CREATE TABLE IF NOT EXISTS metadata (
                    key TEXT PRIMARY KEY,
                    value TEXT,
                    updated_at TEXT DEFAULT (datetime('now'))
                )
            """);

            // 创建启用状态索引
            stmt.execute("""
                CREATE INDEX IF NOT EXISTS idx_question_types_enabled ON question_types(enabled)
            """);

            log.debug("SQLite tables created successfully");
        } catch (SQLException e) {
            log.error("Failed to create SQLite tables", e);
            throw new RuntimeException("Failed to initialize SQLite database", e);
        }
    }

    /**
     * 关闭数据源
     */
    public void shutdown() {
        if (dataSource != null && !dataSource.isClosed()) {
            dataSource.close();
            log.info("SQLite DataSource closed");
        }
    }

    // ========== QuestionTypeConfig CRUD ==========

    @Override
    public boolean saveQuestionType(QuestionTypeConfig config) {
        // SQLite 使用 REPLACE INTO 代替 MERGE INTO
        String sql = """
            REPLACE INTO question_types (id, name, name_en, priority, complexity, suggested_layer, enabled, data, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))
        """;

        try (Connection conn = dataSource.getConnection();
             PreparedStatement ps = conn.prepareStatement(sql)) {

            ps.setString(1, config.getId());
            ps.setString(2, config.getName());
            ps.setString(3, config.getNameEn());
            ps.setInt(4, config.getPriority());
            ps.setString(5, config.getComplexity());
            ps.setString(6, config.getSuggestedLayer());
            ps.setInt(7, config.isEnabled() ? 1 : 0); // SQLite 使用 INTEGER 作为 BOOLEAN
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
            REPLACE INTO question_types (id, name, name_en, priority, complexity, suggested_layer, enabled, data, updated_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))
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
                ps.setInt(7, config.isEnabled() ? 1 : 0);
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
        String sql = "SELECT data FROM question_types WHERE enabled = 1 ORDER BY priority DESC";

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
        String sql = "REPLACE INTO keywords (type_id, keyword) VALUES (?, ?)";

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
        String sql = "REPLACE INTO patterns (type_id, pattern) VALUES (?, ?)";

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
        String backupId = "sqlite_backup_" + System.currentTimeMillis();
        // SQLite 备份：直接复制数据库文件
        log.info("Created SQLite backup: {} (copy database file manually)", backupId);
        return backupId;
    }

    @Override
    public boolean restoreFromBackup(String backupId) {
        // SQLite 恢复：直接替换数据库文件
        log.info("Restore from SQLite backup: {} (replace database file manually)", backupId);
        return true;
    }

    @Override
    public List<String> listBackups() {
        // 简化实现：返回空列表
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
        String sql = "REPLACE INTO metadata (key, value, updated_at) VALUES ('version', ?, datetime('now'))";

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
                result.add(new SQLiteChangeRecord(
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

    // ========== 内部类：SQLiteChangeRecord ==========

    private static class SQLiteChangeRecord implements ChangeRecord {
        private final String id;
        private final String typeId;
        private final String action;
        private final String operator;
        private final long timestamp;
        private final Map<String, Object> details;

        public SQLiteChangeRecord(String id, String typeId, String action, String operator, long timestamp, Map<String, Object> details) {
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

