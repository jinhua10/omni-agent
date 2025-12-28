package top.yumbo.ai.omni.knowledge.registry.redis;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.exception.KnowledgeRegistryException;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.RoleStatus;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 基于 Redis 的知识注册表实现
 *
 * <p>使用 Redis Hash 存储知识域信息</p>
 * <p>数据结构：</p>
 * <ul>
 *     <li>knowledge:domain:{domainId} - 域详情 (String - JSON)</li>
 *     <li>knowledge:domains:all - 所有域ID (Set)</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RequiredArgsConstructor
public class RedisKnowledgeRegistry implements KnowledgeRegistry {

    private final RedisTemplate<String, String> redisTemplate;
    private final ObjectMapper objectMapper;
    private final String keyPrefix;
    private final String domainListKey;
    private final String roleKeyPrefix;
    private final String roleListKey;

    @Override
    public String saveDomain(KnowledgeDomain domain) {
        try {
            domain.prePersist();

            String key = keyPrefix + domain.getDomainId();
            String json = objectMapper.writeValueAsString(domain);

            redisTemplate.opsForValue().set(key, json);
            redisTemplate.opsForSet().add(domainListKey, domain.getDomainId());

            log.info("✅ 保存知识域到 Redis: {} ({})", domain.getDomainName(), domain.getDomainId());
            return domain.getDomainId();
        } catch (JsonProcessingException e) {
            log.error("序列化知识域失败: {}", domain.getDomainId(), e);
            throw new KnowledgeRegistryException("Failed to serialize domain", e);
        }
    }

    @Override
    public Optional<KnowledgeDomain> findDomainById(String domainId) {
        try {
            String key = keyPrefix + domainId;
            String json = redisTemplate.opsForValue().get(key);

            if (json == null) {
                return Optional.empty();
            }

            KnowledgeDomain domain = objectMapper.readValue(json, KnowledgeDomain.class);
            return Optional.of(domain);
        } catch (Exception e) {
            log.error("从 Redis 查询知识域失败: {}", domainId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeDomain> findAllDomains() {
        try {
            Set<String> domainIds = redisTemplate.opsForSet().members(domainListKey);

            if (domainIds == null || domainIds.isEmpty()) {
                return List.of();
            }

            return domainIds.stream()
                    .map(this::findDomainById)
                    .filter(Optional::isPresent)
                    .map(Optional::get)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("从 Redis 查询所有知识域失败", e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByType(DomainType type) {
        return findAllDomains().stream()
                .filter(d -> d.getDomainType() == type)
                .collect(Collectors.toList());
    }

    @Override
    public List<KnowledgeDomain> findDomainsByStatus(DomainStatus status) {
        return findAllDomains().stream()
                .filter(d -> d.getStatus() == status)
                .collect(Collectors.toList());
    }

    @Override
    public List<KnowledgeDomain> findDomainsByLinkedEntity(String linkedEntityId) {
        return findAllDomains().stream()
                .filter(d -> linkedEntityId.equals(d.getLinkedEntityId()))
                .collect(Collectors.toList());
    }

    @Override
    public boolean updateDomain(KnowledgeDomain domain) {
        try {
            domain.preUpdate();

            String key = keyPrefix + domain.getDomainId();
            String json = objectMapper.writeValueAsString(domain);

            redisTemplate.opsForValue().set(key, json);

            log.info("✅ 更新 Redis 中的知识域: {} ({})", domain.getDomainName(), domain.getDomainId());
            return true;
        } catch (Exception e) {
            log.error("更新 Redis 中的知识域失败: {}", domain.getDomainId(), e);
            return false;
        }
    }

    @Override
    public boolean deleteDomain(String domainId) {
        try {
            String key = keyPrefix + domainId;
            redisTemplate.delete(key);
            redisTemplate.opsForSet().remove(domainListKey, domainId);

            log.info("✅ 从 Redis 删除知识域: {}", domainId);
            return true;
        } catch (Exception e) {
            log.error("从 Redis 删除知识域失败: {}", domainId, e);
            return false;
        }
    }

    @Override
    public boolean domainExists(String domainId) {
        try {
            String key = keyPrefix + domainId;
            return Boolean.TRUE.equals(redisTemplate.hasKey(key));
        } catch (Exception e) {
            log.error("检查 Redis 中知识域是否存在失败: {}", domainId, e);
            return false;
        }
    }

    @Override
    public long countDomains() {
        try {
            Long count = redisTemplate.opsForSet().size(domainListKey);
            return count != null ? count : 0;
        } catch (Exception e) {
            log.error("统计 Redis 中知识域数量失败", e);
            return 0;
        }
    }

    @Override
    public long countDomainsByType(DomainType type) {
        return findDomainsByType(type).size();
    }

    // ========== 知识角色管理实现 ==========

    @Override
    public String saveRole(KnowledgeRole role) {
        try {
            role.prePersist();

            String key = roleKeyPrefix + role.getRoleId();
            String json = objectMapper.writeValueAsString(role);

            redisTemplate.opsForValue().set(key, json);
            redisTemplate.opsForSet().add(roleListKey, role.getRoleId());

            log.info("✅ 保存知识角色到 Redis: {} ({})", role.getRoleName(), role.getRoleId());
            return role.getRoleId();
        } catch (JsonProcessingException e) {
            log.error("序列化知识角色失败: {}", role.getRoleId(), e);
            throw new KnowledgeRegistryException("Failed to serialize role", e);
        }
    }

    @Override
    public Optional<KnowledgeRole> findRoleById(String roleId) {
        try {
            String key = roleKeyPrefix + roleId;
            String json = redisTemplate.opsForValue().get(key);

            if (json == null) {
                return Optional.empty();
            }

            KnowledgeRole role = objectMapper.readValue(json, KnowledgeRole.class);
            return Optional.of(role);
        } catch (Exception e) {
            log.error("从 Redis 查询知识角色失败: {}", roleId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeRole> findAllRoles() {
        try {
            Set<String> roleIds = redisTemplate.opsForSet().members(roleListKey);

            if (roleIds == null || roleIds.isEmpty()) {
                return List.of();
            }

            return roleIds.stream()
                    .map(this::findRoleById)
                    .filter(Optional::isPresent)
                    .map(Optional::get)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("从 Redis 查询所有知识角色失败", e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeRole> findRolesByStatus(RoleStatus status) {
        return findAllRoles().stream()
                .filter(r -> r.getStatus() == status)
                .collect(Collectors.toList());
    }

    @Override
    public boolean updateRole(KnowledgeRole role) {
        try {
            role.preUpdate();

            String key = roleKeyPrefix + role.getRoleId();
            String json = objectMapper.writeValueAsString(role);

            redisTemplate.opsForValue().set(key, json);

            log.info("✅ 更新 Redis 中的知识角色: {} ({})", role.getRoleName(), role.getRoleId());
            return true;
        } catch (Exception e) {
            log.error("更新 Redis 中的知识角色失败: {}", role.getRoleId(), e);
            return false;
        }
    }

    @Override
    public boolean deleteRole(String roleId) {
        try {
            String key = roleKeyPrefix + roleId;
            redisTemplate.delete(key);
            redisTemplate.opsForSet().remove(roleListKey, roleId);

            log.info("✅ 从 Redis 删除知识角色: {}", roleId);
            return true;
        } catch (Exception e) {
            log.error("从 Redis 删除知识角色失败: {}", roleId, e);
            return false;
        }
    }

    @Override
    public boolean roleExists(String roleId) {
        try {
            String key = roleKeyPrefix + roleId;
            return Boolean.TRUE.equals(redisTemplate.hasKey(key));
        } catch (Exception e) {
            log.error("检查 Redis 中知识角色是否存在失败: {}", roleId, e);
            return false;
        }
    }

    @Override
    public long countRoles() {
        try {
            Long count = redisTemplate.opsForSet().size(roleListKey);
            return count != null ? count : 0;
        } catch (Exception e) {
            log.error("统计 Redis 中知识角色数量失败", e);
            return 0;
        }
    }
}

