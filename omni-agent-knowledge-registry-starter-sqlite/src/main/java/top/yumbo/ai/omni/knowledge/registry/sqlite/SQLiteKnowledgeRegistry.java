package top.yumbo.ai.omni.knowledge.registry.sqlite;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.exception.KnowledgeRegistryException;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.RoleStatus;

import jakarta.annotation.PostConstruct;
import java.util.List;
import java.util.Optional;

/**
 * 基于 SQLite 数据库的知识注册表实现
 *
 * <p>使用 SQLite 文件数据库存储知识域信息</p>
 * <p>适合单机部署和嵌入式应用</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RequiredArgsConstructor
public class SQLiteKnowledgeRegistry implements KnowledgeRegistry {

    private final JdbcTemplate jdbcTemplate;
    private final ObjectMapper objectMapper;
    private final String tableName;
    private final String roleTableName;

    /**
     * 获取知识域的 RowMapper
     */
    private RowMapper<KnowledgeDomain> getDomainRowMapper() {
        return (rs, rowNum) -> {
            try {
                String json = rs.getString("data");
                return objectMapper.readValue(json, KnowledgeDomain.class);
            } catch (Exception e) {
                log.error("反序列化知识域失败", e);
                return null;
            }
        };
    }

    /**
     * 获取知识角色的 RowMapper
     */
    private RowMapper<KnowledgeRole> getRoleRowMapper() {
        return (rs, rowNum) -> {
            try {
                String json = rs.getString("data");
                return objectMapper.readValue(json, KnowledgeRole.class);
            } catch (Exception e) {
                log.error("反序列化知识角色失败", e);
                return null;
            }
        };
    }

    @PostConstruct
    public void init() {
        createTableIfNotExists();
        createRoleTableIfNotExists();
    }

    private void createTableIfNotExists() {
        String sql = String.format("""
            CREATE TABLE IF NOT EXISTS %s (
                domain_id TEXT PRIMARY KEY,
                domain_name TEXT,
                domain_type TEXT,
                status TEXT,
                data TEXT,
                created_at TEXT,
                updated_at TEXT
            )
            """, tableName);

        try {
            jdbcTemplate.execute(sql);
            log.info("✅ SQLite 表已初始化: {}", tableName);
        } catch (Exception e) {
            log.error("创建 SQLite 表失败", e);
            throw new KnowledgeRegistryException("Failed to create SQLite table", e);
        }
    }

    private void createRoleTableIfNotExists() {
        String sql = String.format("""
            CREATE TABLE IF NOT EXISTS %s (
                role_id TEXT PRIMARY KEY,
                role_name TEXT,
                status TEXT,
                data TEXT,
                created_at TEXT,
                updated_at TEXT
            )
            """, roleTableName);

        try {
            jdbcTemplate.execute(sql);
            log.info("✅ SQLite 角色表已初始化: {}", roleTableName);
        } catch (Exception e) {
            log.error("创建 SQLite 角色表失败", e);
            throw new KnowledgeRegistryException("Failed to create SQLite role table", e);
        }
    }

    @Override
    public String saveDomain(KnowledgeDomain domain) {
        try {
            domain.prePersist();
            String json = objectMapper.writeValueAsString(domain);

            String sql = String.format("""
                INSERT OR REPLACE INTO %s (domain_id, domain_name, domain_type, status, data, created_at, updated_at)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                """, tableName);

            jdbcTemplate.update(sql,
                    domain.getDomainId(),
                    domain.getDomainName(),
                    domain.getDomainType().name(),
                    domain.getStatus().name(),
                    json,
                    domain.getCreatedAt().toString(),
                    domain.getUpdatedAt().toString());

            log.info("✅ 保存知识域到 SQLite: {} ({})", domain.getDomainName(), domain.getDomainId());
            return domain.getDomainId();
        } catch (JsonProcessingException e) {
            log.error("序列化知识域失败: {}", domain.getDomainId(), e);
            throw new KnowledgeRegistryException("Failed to serialize domain", e);
        }
    }

    @Override
    public Optional<KnowledgeDomain> findDomainById(String domainId) {
        try {
            String sql = String.format("SELECT * FROM %s WHERE domain_id = ?", tableName);
            List<KnowledgeDomain> results = jdbcTemplate.query(sql, getDomainRowMapper(), domainId);
            return results.isEmpty() ? Optional.empty() : Optional.of(results.get(0));
        } catch (Exception e) {
            log.error("从 SQLite 查询知识域失败: {}", domainId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeDomain> findAllDomains() {
        try {
            String sql = String.format("SELECT * FROM %s", tableName);
            return jdbcTemplate.query(sql, getDomainRowMapper());
        } catch (Exception e) {
            log.error("从 SQLite 查询所有知识域失败", e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByType(DomainType type) {
        try {
            String sql = String.format("SELECT * FROM %s WHERE domain_type = ?", tableName);
            return jdbcTemplate.query(sql, getDomainRowMapper(), type.name());
        } catch (Exception e) {
            log.error("从 SQLite 按类型查询知识域失败: {}", type, e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByStatus(DomainStatus status) {
        try {
            String sql = String.format("SELECT * FROM %s WHERE status = ?", tableName);
            return jdbcTemplate.query(sql, getDomainRowMapper(), status.name());
        } catch (Exception e) {
            log.error("从 SQLite 按状态查询知识域失败: {}", status, e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByLinkedEntity(String linkedEntityId) {
        // 从 JSON 中查询
        return findAllDomains().stream()
                .filter(d -> linkedEntityId.equals(d.getLinkedEntityId()))
                .toList();
    }

    @Override
    public boolean updateDomain(KnowledgeDomain domain) {
        return saveDomain(domain) != null;
    }

    @Override
    public boolean deleteDomain(String domainId) {
        try {
            String sql = String.format("DELETE FROM %s WHERE domain_id = ?", tableName);
            int rows = jdbcTemplate.update(sql, domainId);

            if (rows > 0) {
                log.info("✅ 从 SQLite 删除知识域: {}", domainId);
                return true;
            }
            return false;
        } catch (Exception e) {
            log.error("从 SQLite 删除知识域失败: {}", domainId, e);
            return false;
        }
    }

    @Override
    public boolean domainExists(String domainId) {
        try {
            String sql = String.format("SELECT COUNT(*) FROM %s WHERE domain_id = ?", tableName);
            Integer count = jdbcTemplate.queryForObject(sql, Integer.class, domainId);
            return count != null && count > 0;
        } catch (Exception e) {
            log.error("检查 SQLite 中知识域是否存在失败: {}", domainId, e);
            return false;
        }
    }

    @Override
    public long countDomains() {
        try {
            String sql = String.format("SELECT COUNT(*) FROM %s", tableName);
            Long count = jdbcTemplate.queryForObject(sql, Long.class);
            return count != null ? count : 0;
        } catch (Exception e) {
            log.error("统计 SQLite 中知识域数量失败", e);
            return 0;
        }
    }

    @Override
    public long countDomainsByType(DomainType type) {
        try {
            String sql = String.format("SELECT COUNT(*) FROM %s WHERE domain_type = ?", tableName);
            Long count = jdbcTemplate.queryForObject(sql, Long.class, type.name());
            return count != null ? count : 0;
        } catch (Exception e) {
            log.error("统计 SQLite 中指定类型知识域数量失败: {}", type, e);
            return 0;
        }
    }

    // ========== 知识角色管理实现 ==========

    @Override
    public String saveRole(KnowledgeRole role) {
        try {
            role.prePersist();
            String json = objectMapper.writeValueAsString(role);

            String sql = String.format("""
                INSERT OR REPLACE INTO %s (role_id, role_name, status, data, created_at, updated_at)
                VALUES (?, ?, ?, ?, ?, ?)
                """, roleTableName);

            jdbcTemplate.update(sql,
                    role.getRoleId(),
                    role.getRoleName(),
                    role.getStatus().name(),
                    json,
                    role.getCreatedAt().toString(),
                    role.getUpdatedAt().toString());

            log.info("✅ 保存知识角色到 SQLite: {} ({})", role.getRoleName(), role.getRoleId());
            return role.getRoleId();
        } catch (JsonProcessingException e) {
            log.error("序列化知识角色失败: {}", role.getRoleId(), e);
            throw new KnowledgeRegistryException("Failed to serialize role", e);
        }
    }

    @Override
    public Optional<KnowledgeRole> findRoleById(String roleId) {
        try {
            String sql = String.format("SELECT * FROM %s WHERE role_id = ?", roleTableName);
            List<KnowledgeRole> results = jdbcTemplate.query(sql, getRoleRowMapper(), roleId);
            return results.isEmpty() ? Optional.empty() : Optional.of(results.get(0));
        } catch (Exception e) {
            log.error("从 SQLite 查询知识角色失败: {}", roleId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeRole> findAllRoles() {
        try {
            String sql = String.format("SELECT * FROM %s", roleTableName);
            return jdbcTemplate.query(sql, getRoleRowMapper());
        } catch (Exception e) {
            log.error("从 SQLite 查询所有知识角色失败", e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeRole> findRolesByStatus(RoleStatus status) {
        try {
            String sql = String.format("SELECT * FROM %s WHERE status = ?", roleTableName);
            return jdbcTemplate.query(sql, getRoleRowMapper(), status.name());
        } catch (Exception e) {
            log.error("从 SQLite 按状态查询知识角色失败: {}", status, e);
            return List.of();
        }
    }

    @Override
    public boolean updateRole(KnowledgeRole role) {
        return saveRole(role) != null;
    }

    @Override
    public boolean deleteRole(String roleId) {
        try {
            String sql = String.format("DELETE FROM %s WHERE role_id = ?", roleTableName);
            int rows = jdbcTemplate.update(sql, roleId);

            if (rows > 0) {
                log.info("✅ 从 SQLite 删除知识角色: {}", roleId);
                return true;
            }
            return false;
        } catch (Exception e) {
            log.error("从 SQLite 删除知识角色失败: {}", roleId, e);
            return false;
        }
    }

    @Override
    public boolean roleExists(String roleId) {
        try {
            String sql = String.format("SELECT COUNT(*) FROM %s WHERE role_id = ?", roleTableName);
            Integer count = jdbcTemplate.queryForObject(sql, Integer.class, roleId);
            return count != null && count > 0;
        } catch (Exception e) {
            log.error("检查 SQLite 中知识角色是否存在失败: {}", roleId, e);
            return false;
        }
    }

    @Override
    public long countRoles() {
        try {
            String sql = String.format("SELECT COUNT(*) FROM %s", roleTableName);
            Long count = jdbcTemplate.queryForObject(sql, Long.class);
            return count != null ? count : 0;
        } catch (Exception e) {
            log.error("统计 SQLite 中知识角色数量失败", e);
            return 0;
        }
    }
}

