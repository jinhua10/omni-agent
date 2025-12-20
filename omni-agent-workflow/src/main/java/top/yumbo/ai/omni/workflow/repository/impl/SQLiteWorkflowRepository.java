package top.yumbo.ai.omni.workflow.repository.impl;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import top.yumbo.ai.omni.workflow.Workflow;
import top.yumbo.ai.omni.workflow.market.MarketWorkflow;
import top.yumbo.ai.omni.workflow.market.WorkflowInstallation;
import top.yumbo.ai.omni.workflow.market.WorkflowRating;
import top.yumbo.ai.omni.workflow.repository.WorkflowRepository;

import java.util.*;

/**
 * SQLite 工作流存储实现
 * (SQLite Workflow Repository Implementation)
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
public class SQLiteWorkflowRepository implements WorkflowRepository {

    private final JdbcTemplate jdbcTemplate;
    private final ObjectMapper objectMapper;

    // 建表 SQL
    private static final String CREATE_TABLES_SQL = """
        CREATE TABLE IF NOT EXISTS market_workflows (
            id TEXT PRIMARY KEY,
            name TEXT NOT NULL,
            version TEXT NOT NULL,
            description TEXT,
            category TEXT,
            tags TEXT,
            author_id TEXT,
            author_name TEXT,
            workflow_definition TEXT,
            status TEXT,
            is_public INTEGER,
            license TEXT,
            download_count INTEGER DEFAULT 0,
            install_count INTEGER DEFAULT 0,
            favorite_count INTEGER DEFAULT 0,
            rating REAL DEFAULT 0,
            rating_count INTEGER DEFAULT 0,
            created_at INTEGER,
            updated_at INTEGER,
            published_at INTEGER,
            metadata TEXT,
            dependencies TEXT,
            required_agents TEXT,
            UNIQUE(name, version)
        );
        
        CREATE INDEX IF NOT EXISTS idx_name ON market_workflows(name);
        CREATE INDEX IF NOT EXISTS idx_category ON market_workflows(category);
        CREATE INDEX IF NOT EXISTS idx_author ON market_workflows(author_id);
        CREATE INDEX IF NOT EXISTS idx_status ON market_workflows(status);
        CREATE INDEX IF NOT EXISTS idx_public ON market_workflows(is_public);
        CREATE INDEX IF NOT EXISTS idx_download ON market_workflows(download_count);
        CREATE INDEX IF NOT EXISTS idx_rating ON market_workflows(rating);
        
        CREATE TABLE IF NOT EXISTS workflow_ratings (
            id TEXT PRIMARY KEY,
            workflow_id TEXT NOT NULL,
            user_id TEXT NOT NULL,
            user_name TEXT,
            rating INTEGER,
            comment TEXT,
            created_at INTEGER,
            FOREIGN KEY(workflow_id) REFERENCES market_workflows(id),
            UNIQUE(workflow_id, user_id)
        );
        
        CREATE INDEX IF NOT EXISTS idx_rating_workflow ON workflow_ratings(workflow_id);
        
        CREATE TABLE IF NOT EXISTS workflow_installations (
            id TEXT PRIMARY KEY,
            workflow_id TEXT NOT NULL,
            workflow_version TEXT,
            user_id TEXT NOT NULL,
            installed_at INTEGER,
            enabled INTEGER,
            FOREIGN KEY(workflow_id) REFERENCES market_workflows(id),
            UNIQUE(workflow_id, user_id)
        );
        
        CREATE INDEX IF NOT EXISTS idx_install_user ON workflow_installations(user_id);
    """;

    public SQLiteWorkflowRepository(JdbcTemplate jdbcTemplate, ObjectMapper objectMapper) {
        this.jdbcTemplate = jdbcTemplate;
        this.objectMapper = objectMapper;
        initDatabase();
    }

    private void initDatabase() {
        try {
            jdbcTemplate.execute(CREATE_TABLES_SQL);
            log.info("✅ SQLite 工作流表初始化完成");
        } catch (Exception e) {
            log.error("❌ SQLite 工作流表初始化失败", e);
        }
    }

    @Override
    public String save(MarketWorkflow workflow) {
        String sql = """
            INSERT INTO market_workflows (
                id, name, version, description, category, tags,
                author_id, author_name, workflow_definition, status,
                is_public, license, download_count, install_count,
                favorite_count, rating, rating_count, created_at,
                updated_at, published_at, metadata, dependencies, required_agents
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """;

        try {
            jdbcTemplate.update(sql,
                    workflow.getId(),
                    workflow.getName(),
                    workflow.getVersion(),
                    workflow.getDescription(),
                    workflow.getCategory(),
                    toJson(workflow.getTags()),
                    workflow.getAuthorId(),
                    workflow.getAuthorName(),
                    toJson(workflow.getWorkflowDefinition()),
                    workflow.getStatus(),
                    workflow.isPublic() ? 1 : 0,
                    workflow.getLicense(),
                    workflow.getDownloadCount(),
                    workflow.getInstallCount(),
                    workflow.getFavoriteCount(),
                    workflow.getRating(),
                    workflow.getRatingCount(),
                    workflow.getCreatedAt(),
                    workflow.getUpdatedAt(),
                    workflow.getPublishedAt(),
                    toJson(workflow.getMetadata()),
                    toJson(workflow.getDependencies()),
                    toJson(workflow.getRequiredAgents())
            );

            log.debug("保存工作流: id={}, name={}", workflow.getId(), workflow.getName());
            return workflow.getId();

        } catch (Exception e) {
            log.error("保存工作流失败", e);
            return null;
        }
    }

    @Override
    public boolean update(MarketWorkflow workflow) {
        String sql = """
            UPDATE market_workflows SET
                description = ?, category = ?, tags = ?,
                workflow_definition = ?, status = ?, is_public = ?,
                license = ?, updated_at = ?, metadata = ?,
                dependencies = ?, required_agents = ?
            WHERE id = ?
        """;

        try {
            int rows = jdbcTemplate.update(sql,
                    workflow.getDescription(),
                    workflow.getCategory(),
                    toJson(workflow.getTags()),
                    toJson(workflow.getWorkflowDefinition()),
                    workflow.getStatus(),
                    workflow.isPublic() ? 1 : 0,
                    workflow.getLicense(),
                    System.currentTimeMillis(),
                    toJson(workflow.getMetadata()),
                    toJson(workflow.getDependencies()),
                    toJson(workflow.getRequiredAgents()),
                    workflow.getId()
            );

            return rows > 0;

        } catch (Exception e) {
            log.error("更新工作流失败", e);
            return false;
        }
    }

    @Override
    public boolean delete(String workflowId) {
        try {
            int rows = jdbcTemplate.update("DELETE FROM market_workflows WHERE id = ?", workflowId);
            return rows > 0;
        } catch (Exception e) {
            log.error("删除工作流失败", e);
            return false;
        }
    }

    @Override
    public Optional<MarketWorkflow> findById(String workflowId) {
        String sql = "SELECT * FROM market_workflows WHERE id = ?";
        try {
            MarketWorkflow workflow = jdbcTemplate.queryForObject(sql, workflowRowMapper(), workflowId);
            return Optional.ofNullable(workflow);
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    @Override
    public Optional<MarketWorkflow> findByNameAndVersion(String name, String version) {
        String sql = "SELECT * FROM market_workflows WHERE name = ? AND version = ?";
        try {
            MarketWorkflow workflow = jdbcTemplate.queryForObject(sql, workflowRowMapper(), name, version);
            return Optional.ofNullable(workflow);
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    @Override
    public List<MarketWorkflow> findAllVersions(String name) {
        String sql = "SELECT * FROM market_workflows WHERE name = ? ORDER BY created_at DESC";
        return jdbcTemplate.query(sql, workflowRowMapper(), name);
    }

    @Override
    public Optional<MarketWorkflow> findLatestVersion(String name) {
        String sql = "SELECT * FROM market_workflows WHERE name = ? ORDER BY created_at DESC LIMIT 1";
        try {
            MarketWorkflow workflow = jdbcTemplate.queryForObject(sql, workflowRowMapper(), name);
            return Optional.ofNullable(workflow);
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    @Override
    public List<MarketWorkflow> findPublic(int page, int size) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE is_public = 1 AND status = 'published'
            ORDER BY created_at DESC
            LIMIT ? OFFSET ?
        """;

        return jdbcTemplate.query(sql, workflowRowMapper(), size, page * size);
    }

    @Override
    public List<MarketWorkflow> findByCategory(String category, int page, int size) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE is_public = 1 AND status = 'published' AND category = ?
            ORDER BY created_at DESC
            LIMIT ? OFFSET ?
        """;

        return jdbcTemplate.query(sql, workflowRowMapper(), category, size, page * size);
    }

    @Override
    public List<MarketWorkflow> findByTag(String tag, int page, int size) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE is_public = 1 AND status = 'published' AND tags LIKE ?
            ORDER BY created_at DESC
            LIMIT ? OFFSET ?
        """;

        String pattern = "%" + tag + "%";
        return jdbcTemplate.query(sql, workflowRowMapper(), pattern, size, page * size);
    }

    @Override
    public List<MarketWorkflow> findByAuthor(String authorId, int page, int size) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE author_id = ?
            ORDER BY created_at DESC
            LIMIT ? OFFSET ?
        """;

        return jdbcTemplate.query(sql, workflowRowMapper(), authorId, size, page * size);
    }

    @Override
    public List<MarketWorkflow> search(String keyword, int page, int size) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE is_public = 1 AND status = 'published'
            AND (name LIKE ? OR description LIKE ? OR tags LIKE ?)
            ORDER BY download_count DESC, rating DESC
            LIMIT ? OFFSET ?
        """;

        String pattern = "%" + keyword + "%";
        return jdbcTemplate.query(sql, workflowRowMapper(),
                pattern, pattern, pattern, size, page * size);
    }

    @Override
    public List<MarketWorkflow> findPopular(int limit) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE is_public = 1 AND status = 'published'
            ORDER BY download_count DESC, rating DESC
            LIMIT ?
        """;

        return jdbcTemplate.query(sql, workflowRowMapper(), limit);
    }

    @Override
    public List<MarketWorkflow> findRecent(int limit) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE is_public = 1 AND status = 'published'
            ORDER BY published_at DESC
            LIMIT ?
        """;

        return jdbcTemplate.query(sql, workflowRowMapper(), limit);
    }

    @Override
    public List<MarketWorkflow> findTopRated(int limit) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE is_public = 1 AND status = 'published' AND rating_count > 0
            ORDER BY rating DESC, rating_count DESC
            LIMIT ?
        """;

        return jdbcTemplate.query(sql, workflowRowMapper(), limit);
    }

    @Override
    public void incrementDownloadCount(String workflowId) {
        String sql = "UPDATE market_workflows SET download_count = download_count + 1 WHERE id = ?";
        jdbcTemplate.update(sql, workflowId);
    }

    @Override
    public void incrementInstallCount(String workflowId) {
        String sql = "UPDATE market_workflows SET install_count = install_count + 1 WHERE id = ?";
        jdbcTemplate.update(sql, workflowId);
    }

    @Override
    public void incrementFavoriteCount(String workflowId) {
        String sql = "UPDATE market_workflows SET favorite_count = favorite_count + 1 WHERE id = ?";
        jdbcTemplate.update(sql, workflowId);
    }

    @Override
    public void updateRating(String workflowId, double rating, long ratingCount) {
        String sql = "UPDATE market_workflows SET rating = ?, rating_count = ? WHERE id = ?";
        jdbcTemplate.update(sql, rating, ratingCount, workflowId);
    }

    @Override
    public String saveRating(WorkflowRating rating) {
        String sql = """
            INSERT OR REPLACE INTO workflow_ratings (
                id, workflow_id, user_id, user_name, rating, comment, created_at
            ) VALUES (?, ?, ?, ?, ?, ?, ?)
        """;

        try {
            jdbcTemplate.update(sql,
                    rating.getId(),
                    rating.getWorkflowId(),
                    rating.getUserId(),
                    rating.getUserName(),
                    rating.getRating(),
                    rating.getComment(),
                    rating.getCreatedAt()
            );

            return rating.getId();

        } catch (Exception e) {
            log.error("保存评分失败", e);
            return null;
        }
    }

    @Override
    public List<WorkflowRating> findRatings(String workflowId, int page, int size) {
        String sql = """
            SELECT * FROM workflow_ratings 
            WHERE workflow_id = ?
            ORDER BY created_at DESC
            LIMIT ? OFFSET ?
        """;

        return jdbcTemplate.query(sql, ratingRowMapper(), workflowId, size, page * size);
    }

    @Override
    public Optional<WorkflowRating> findUserRating(String workflowId, String userId) {
        String sql = "SELECT * FROM workflow_ratings WHERE workflow_id = ? AND user_id = ?";
        try {
            WorkflowRating rating = jdbcTemplate.queryForObject(sql, ratingRowMapper(), workflowId, userId);
            return Optional.ofNullable(rating);
        } catch (Exception e) {
            return Optional.empty();
        }
    }

    @Override
    public String saveInstallation(WorkflowInstallation installation) {
        String sql = """
            INSERT OR REPLACE INTO workflow_installations (
                id, workflow_id, workflow_version, user_id, installed_at, enabled
            ) VALUES (?, ?, ?, ?, ?, ?)
        """;

        try {
            jdbcTemplate.update(sql,
                    installation.getId(),
                    installation.getWorkflowId(),
                    installation.getWorkflowVersion(),
                    installation.getUserId(),
                    installation.getInstalledAt(),
                    installation.getEnabled() ? 1 : 0
            );

            return installation.getId();

        } catch (Exception e) {
            log.error("保存安装记录失败", e);
            return null;
        }
    }

    @Override
    public List<WorkflowInstallation> findUserInstallations(String userId) {
        String sql = "SELECT * FROM workflow_installations WHERE user_id = ? ORDER BY installed_at DESC";
        return jdbcTemplate.query(sql, installationRowMapper(), userId);
    }

    @Override
    public boolean isInstalled(String workflowId, String userId) {
        String sql = "SELECT COUNT(*) FROM workflow_installations WHERE workflow_id = ? AND user_id = ?";
        Integer count = jdbcTemplate.queryForObject(sql, Integer.class, workflowId, userId);
        return count != null && count > 0;
    }

    // ========== RowMapper ==========

    private RowMapper<MarketWorkflow> workflowRowMapper() {
        return (rs, rowNum) -> {
            try {
                return MarketWorkflow.builder()
                        .id(rs.getString("id"))
                        .name(rs.getString("name"))
                        .version(rs.getString("version"))
                        .description(rs.getString("description"))
                        .category(rs.getString("category"))
                        .tags(fromJson(rs.getString("tags"), new TypeReference<List<String>>() {}))
                        .authorId(rs.getString("author_id"))
                        .authorName(rs.getString("author_name"))
                        .workflowDefinition(fromJson(rs.getString("workflow_definition"), Workflow.class))
                        .status(rs.getString("status"))
                        .isPublic(rs.getInt("is_public") == 1)
                        .license(rs.getString("license"))
                        .downloadCount(rs.getLong("download_count"))
                        .installCount(rs.getLong("install_count"))
                        .favoriteCount(rs.getLong("favorite_count"))
                        .rating(rs.getDouble("rating"))
                        .ratingCount(rs.getLong("rating_count"))
                        .createdAt(rs.getLong("created_at"))
                        .updatedAt(rs.getLong("updated_at"))
                        .publishedAt(rs.getLong("published_at"))
                        .metadata(fromJson(rs.getString("metadata"), new TypeReference<Map<String, Object>>() {}))
                        .dependencies(fromJson(rs.getString("dependencies"), new TypeReference<List<String>>() {}))
                        .requiredAgents(fromJson(rs.getString("required_agents"), new TypeReference<List<String>>() {}))
                        .build();
            } catch (Exception e) {
                throw new RuntimeException("解析工作流失败", e);
            }
        };
    }

    private RowMapper<WorkflowRating> ratingRowMapper() {
        return (rs, rowNum) -> WorkflowRating.builder()
                .id(rs.getString("id"))
                .workflowId(rs.getString("workflow_id"))
                .userId(rs.getString("user_id"))
                .userName(rs.getString("user_name"))
                .rating(rs.getInt("rating"))
                .comment(rs.getString("comment"))
                .createdAt(rs.getLong("created_at"))
                .build();
    }

    private RowMapper<WorkflowInstallation> installationRowMapper() {
        return (rs, rowNum) -> WorkflowInstallation.builder()
                .id(rs.getString("id"))
                .workflowId(rs.getString("workflow_id"))
                .workflowVersion(rs.getString("workflow_version"))
                .userId(rs.getString("user_id"))
                .installedAt(rs.getLong("installed_at"))
                .enabled(rs.getInt("enabled") == 1)
                .build();
    }

    // ========== JSON 转换 ==========

    private String toJson(Object obj) {
        if (obj == null) {
            return null;
        }
        try {
            return objectMapper.writeValueAsString(obj);
        } catch (Exception e) {
            log.error("JSON 序列化失败", e);
            return null;
        }
    }

    private <T> T fromJson(String json, Class<T> type) {
        if (json == null || json.isEmpty()) {
            return null;
        }
        try {
            return objectMapper.readValue(json, type);
        } catch (Exception e) {
            log.debug("JSON 反序列化失败: {}", e.getMessage());
            return null;
        }
    }

    private <T> T fromJson(String json, TypeReference<T> typeRef) {
        if (json == null || json.isEmpty()) {
            return null;
        }
        try {
            return objectMapper.readValue(json, typeRef);
        } catch (Exception e) {
            log.debug("JSON 反序列化失败: {}", e.getMessage());
            return null;
        }
    }
}

