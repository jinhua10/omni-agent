package top.yumbo.ai.omni.knowledge.registry.h2;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.exception.KnowledgeRegistryException;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;

import jakarta.annotation.PostConstruct;
import java.util.List;
import java.util.Optional;

/**
 * 基于 H2 数据库的知识注册表实现
 *
 * <p>使用 H2 内存或文件数据库存储知识域信息</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RequiredArgsConstructor
public class H2KnowledgeRegistry implements KnowledgeRegistry {

    private final JdbcTemplate jdbcTemplate;
    private final ObjectMapper objectMapper;
    private final String tableName;

    private final RowMapper<KnowledgeDomain> rowMapper = (rs, rowNum) -> {
        try {
            String json = rs.getString("data");
            return objectMapper.readValue(json, KnowledgeDomain.class);
        } catch (Exception e) {
            log.error("反序列化知识域失败", e);
            return null;
        }
    };

    @PostConstruct
    public void init() {
        createTableIfNotExists();
    }

    private void createTableIfNotExists() {
        String sql = String.format("""
            CREATE TABLE IF NOT EXISTS %s (
                domain_id VARCHAR(255) PRIMARY KEY,
                domain_name VARCHAR(500),
                domain_type VARCHAR(50),
                status VARCHAR(50),
                data TEXT,
                created_at TIMESTAMP,
                updated_at TIMESTAMP
            )
            """, tableName);

        try {
            jdbcTemplate.execute(sql);
            log.info("✅ H2 表已初始化: {}", tableName);
        } catch (Exception e) {
            log.error("创建 H2 表失败", e);
            throw new KnowledgeRegistryException("Failed to create H2 table", e);
        }
    }

    @Override
    public String saveDomain(KnowledgeDomain domain) {
        try {
            domain.prePersist();
            String json = objectMapper.writeValueAsString(domain);

            String sql = String.format("""
                MERGE INTO %s (domain_id, domain_name, domain_type, status, data, created_at, updated_at)
                VALUES (?, ?, ?, ?, ?, ?, ?)
                """, tableName);

            jdbcTemplate.update(sql,
                    domain.getDomainId(),
                    domain.getDomainName(),
                    domain.getDomainType().name(),
                    domain.getStatus().name(),
                    json,
                    domain.getCreatedAt(),
                    domain.getUpdatedAt());

            log.info("✅ 保存知识域到 H2: {} ({})", domain.getDomainName(), domain.getDomainId());
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
            List<KnowledgeDomain> results = jdbcTemplate.query(sql, rowMapper, domainId);
            return results.isEmpty() ? Optional.empty() : Optional.of(results.get(0));
        } catch (Exception e) {
            log.error("从 H2 查询知识域失败: {}", domainId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeDomain> findAllDomains() {
        try {
            String sql = String.format("SELECT * FROM %s", tableName);
            return jdbcTemplate.query(sql, rowMapper);
        } catch (Exception e) {
            log.error("从 H2 查询所有知识域失败", e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByType(DomainType type) {
        try {
            String sql = String.format("SELECT * FROM %s WHERE domain_type = ?", tableName);
            return jdbcTemplate.query(sql, rowMapper, type.name());
        } catch (Exception e) {
            log.error("从 H2 按类型查询知识域失败: {}", type, e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByStatus(DomainStatus status) {
        try {
            String sql = String.format("SELECT * FROM %s WHERE status = ?", tableName);
            return jdbcTemplate.query(sql, rowMapper, status.name());
        } catch (Exception e) {
            log.error("从 H2 按状态查询知识域失败: {}", status, e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByLinkedEntity(String linkedEntityId) {
        // 需要从 JSON 中查询，H2 不直接支持，返回过滤后的结果
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
                log.info("✅ 从 H2 删除知识域: {}", domainId);
                return true;
            }
            return false;
        } catch (Exception e) {
            log.error("从 H2 删除知识域失败: {}", domainId, e);
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
            log.error("检查 H2 中知识域是否存在失败: {}", domainId, e);
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
            log.error("统计 H2 中知识域数量失败", e);
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
            log.error("统计 H2 中指定类型知识域数量失败: {}", type, e);
            return 0;
        }
    }
}

