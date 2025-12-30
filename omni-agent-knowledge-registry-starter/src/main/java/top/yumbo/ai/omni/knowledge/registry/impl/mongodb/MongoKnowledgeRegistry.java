package top.yumbo.ai.omni.knowledge.registry.impl.mongodb;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.exception.KnowledgeRegistryException;
import top.yumbo.ai.omni.knowledge.registry.model.domain.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.domain.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.domain.KnowledgeDomain;
import top.yumbo.ai.omni.knowledge.registry.model.role.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.role.RoleStatus;

import java.util.List;
import java.util.Optional;

/**
 * 基于 MongoDB 的知识注册表实现
 *
 * <p>使用 MongoDB 存储知识域信息</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RequiredArgsConstructor
public class MongoKnowledgeRegistry implements KnowledgeRegistry {

    private final MongoTemplate mongoTemplate;
    private final String collectionName;
    private final String roleCollectionName;

    @Override
    public String saveDomain(KnowledgeDomain domain) {
        try {
            domain.prePersist();
            mongoTemplate.save(domain, collectionName);
            log.info("✅ 保存知识域到 MongoDB: {} ({})", domain.getDomainName(), domain.getDomainId());
            return domain.getDomainId();
        } catch (Exception e) {
            log.error("保存知识域到 MongoDB 失败: {}", domain.getDomainId(), e);
            throw new KnowledgeRegistryException("Failed to save domain to MongoDB", e);
        }
    }

    @Override
    public Optional<KnowledgeDomain> findDomainById(String domainId) {
        try {
            Query query = new Query(Criteria.where("domainId").is(domainId));
            KnowledgeDomain domain = mongoTemplate.findOne(query, KnowledgeDomain.class, collectionName);
            return Optional.ofNullable(domain);
        } catch (Exception e) {
            log.error("从 MongoDB 查询知识域失败: {}", domainId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeDomain> findAllDomains() {
        try {
            return mongoTemplate.findAll(KnowledgeDomain.class, collectionName);
        } catch (Exception e) {
            log.error("从 MongoDB 查询所有知识域失败", e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByType(DomainType type) {
        try {
            Query query = new Query(Criteria.where("domainType").is(type));
            return mongoTemplate.find(query, KnowledgeDomain.class, collectionName);
        } catch (Exception e) {
            log.error("从 MongoDB 按类型查询知识域失败: {}", type, e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByStatus(DomainStatus status) {
        try {
            Query query = new Query(Criteria.where("status").is(status));
            return mongoTemplate.find(query, KnowledgeDomain.class, collectionName);
        } catch (Exception e) {
            log.error("从 MongoDB 按状态查询知识域失败: {}", status, e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByLinkedEntity(String linkedEntityId) {
        try {
            Query query = new Query(Criteria.where("linkedEntityId").is(linkedEntityId));
            return mongoTemplate.find(query, KnowledgeDomain.class, collectionName);
        } catch (Exception e) {
            log.error("从 MongoDB 按关联实体查询知识域失败: {}", linkedEntityId, e);
            return List.of();
        }
    }

    @Override
    public boolean updateDomain(KnowledgeDomain domain) {
        try {
            domain.preUpdate();
            mongoTemplate.save(domain, collectionName);
            log.info("✅ 更新 MongoDB 中的知识域: {} ({})", domain.getDomainName(), domain.getDomainId());
            return true;
        } catch (Exception e) {
            log.error("更新 MongoDB 中的知识域失败: {}", domain.getDomainId(), e);
            return false;
        }
    }

    @Override
    public boolean deleteDomain(String domainId) {
        try {
            Query query = new Query(Criteria.where("domainId").is(domainId));
            mongoTemplate.remove(query, KnowledgeDomain.class, collectionName);
            log.info("✅ 从 MongoDB 删除知识域: {}", domainId);
            return true;
        } catch (Exception e) {
            log.error("从 MongoDB 删除知识域失败: {}", domainId, e);
            return false;
        }
    }

    @Override
    public boolean domainExists(String domainId) {
        try {
            Query query = new Query(Criteria.where("domainId").is(domainId));
            return mongoTemplate.exists(query, KnowledgeDomain.class, collectionName);
        } catch (Exception e) {
            log.error("检查 MongoDB 中知识域是否存在失败: {}", domainId, e);
            return false;
        }
    }

    @Override
    public long countDomains() {
        try {
            return mongoTemplate.count(new Query(), KnowledgeDomain.class, collectionName);
        } catch (Exception e) {
            log.error("统计 MongoDB 中知识域数量失败", e);
            return 0;
        }
    }

    @Override
    public long countDomainsByType(DomainType type) {
        try {
            Query query = new Query(Criteria.where("domainType").is(type));
            return mongoTemplate.count(query, KnowledgeDomain.class, collectionName);
        } catch (Exception e) {
            log.error("统计 MongoDB 中指定类型知识域数量失败: {}", type, e);
            return 0;
        }
    }

    // ========== 知识角色管理实现 ==========

    @Override
    public String saveRole(KnowledgeRole role) {
        try {
            role.prePersist();
            mongoTemplate.save(role, roleCollectionName);
            log.info("✅ 保存知识角色到 MongoDB: {} ({})", role.getRoleName(), role.getRoleId());
            return role.getRoleId();
        } catch (Exception e) {
            log.error("保存知识角色到 MongoDB 失败: {}", role.getRoleId(), e);
            throw new KnowledgeRegistryException("Failed to save role to MongoDB", e);
        }
    }

    @Override
    public Optional<KnowledgeRole> findRoleById(String roleId) {
        try {
            Query query = new Query(Criteria.where("roleId").is(roleId));
            KnowledgeRole role = mongoTemplate.findOne(query, KnowledgeRole.class, roleCollectionName);
            return Optional.ofNullable(role);
        } catch (Exception e) {
            log.error("从 MongoDB 查询知识角色失败: {}", roleId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeRole> findAllRoles() {
        try {
            return mongoTemplate.findAll(KnowledgeRole.class, roleCollectionName);
        } catch (Exception e) {
            log.error("从 MongoDB 查询所有知识角色失败", e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeRole> findRolesByStatus(RoleStatus status) {
        try {
            Query query = new Query(Criteria.where("status").is(status));
            return mongoTemplate.find(query, KnowledgeRole.class, roleCollectionName);
        } catch (Exception e) {
            log.error("从 MongoDB 按状态查询知识角色失败: {}", status, e);
            return List.of();
        }
    }

    @Override
    public boolean updateRole(KnowledgeRole role) {
        try {
            role.preUpdate();
            mongoTemplate.save(role, roleCollectionName);
            log.info("✅ 更新 MongoDB 中的知识角色: {} ({})", role.getRoleName(), role.getRoleId());
            return true;
        } catch (Exception e) {
            log.error("更新 MongoDB 中的知识角色失败: {}", role.getRoleId(), e);
            return false;
        }
    }

    @Override
    public boolean deleteRole(String roleId) {
        try {
            Query query = new Query(Criteria.where("roleId").is(roleId));
            mongoTemplate.remove(query, KnowledgeRole.class, roleCollectionName);
            log.info("✅ 从 MongoDB 删除知识角色: {}", roleId);
            return true;
        } catch (Exception e) {
            log.error("从 MongoDB 删除知识角色失败: {}", roleId, e);
            return false;
        }
    }

    @Override
    public boolean roleExists(String roleId) {
        try {
            Query query = new Query(Criteria.where("roleId").is(roleId));
            return mongoTemplate.exists(query, KnowledgeRole.class, roleCollectionName);
        } catch (Exception e) {
            log.error("检查 MongoDB 中知识角色是否存在失败: {}", roleId, e);
            return false;
        }
    }

    @Override
    public long countRoles() {
        try {
            return mongoTemplate.count(new Query(), KnowledgeRole.class, roleCollectionName);
        } catch (Exception e) {
            log.error("统计 MongoDB 中知识角色数量失败", e);
            return 0;
        }
    }
}

