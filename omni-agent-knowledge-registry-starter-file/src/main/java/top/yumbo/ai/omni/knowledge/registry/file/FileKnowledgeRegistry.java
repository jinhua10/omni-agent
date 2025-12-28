package top.yumbo.ai.omni.knowledge.registry.file;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.exception.KnowledgeRegistryException;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.RoleStatus;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * 基于文件的知识注册表实现
 *
 * <p>使用 JSON 文件存储知识域信息，每个域一个独立的 JSON 文件</p>
 *
 * <p>存储结构：</p>
 * <pre>
 * data/knowledge-network/registry/
 *   ├── domains/
 *   │   ├── domain-1.json
 *   │   ├── domain-2.json
 *   │   └── ...
 * </pre>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class FileKnowledgeRegistry implements KnowledgeRegistry {

    private final String basePath;
    private final ObjectMapper objectMapper;
    private final Path domainsDir;
    private final Path rolesDir;

    public FileKnowledgeRegistry(String basePath, boolean prettyPrint) {
        this.basePath = basePath;
        this.objectMapper = createObjectMapper(prettyPrint);
        this.domainsDir = Paths.get(basePath, "domains");
        this.rolesDir = Paths.get(basePath, "roles");

        // 初始化目录
        initDirectories();
    }

    /**
     * 创建 ObjectMapper
     */
    private ObjectMapper createObjectMapper(boolean prettyPrint) {
        ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new JavaTimeModule());
        mapper.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);

        if (prettyPrint) {
            mapper.enable(SerializationFeature.INDENT_OUTPUT);
        }

        return mapper;
    }

    /**
     * 初始化目录结构
     */
    private void initDirectories() {
        try {
            Files.createDirectories(domainsDir);
            Files.createDirectories(rolesDir);
            log.info("✅ 知识注册表目录已初始化: domains={}, roles={}", domainsDir, rolesDir);
        } catch (IOException e) {
            throw new KnowledgeRegistryException("Failed to create directories", e);
        }
    }

    // ========== 知识域管理 ==========

    @Override
    public String saveDomain(KnowledgeDomain domain) {
        domain.prePersist();

        Path filePath = getDomainFilePath(domain.getDomainId());

        try {
            objectMapper.writeValue(filePath.toFile(), domain);
            log.info("✅ 保存知识域: {} ({})", domain.getDomainName(), domain.getDomainId());
            return domain.getDomainId();
        } catch (IOException e) {
            log.error("保存知识域失败: {}", domain.getDomainId(), e);
            throw new KnowledgeRegistryException("Failed to save domain: " + domain.getDomainId(), e);
        }
    }

    @Override
    public Optional<KnowledgeDomain> findDomainById(String domainId) {
        Path filePath = getDomainFilePath(domainId);

        if (!Files.exists(filePath)) {
            return Optional.empty();
        }

        try {
            KnowledgeDomain domain = objectMapper.readValue(filePath.toFile(), KnowledgeDomain.class);
            return Optional.of(domain);
        } catch (IOException e) {
            log.error("读取知识域失败: {}", domainId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeDomain> findAllDomains() {
        try {
            if (!Files.exists(domainsDir)) {
                return Collections.emptyList();
            }

            return Files.list(domainsDir)
                    .filter(p -> p.toString().endsWith(".json"))
                    .map(this::readDomainFromFile)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            log.error("列出知识域失败", e);
            return Collections.emptyList();
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
        domain.preUpdate();

        Path filePath = getDomainFilePath(domain.getDomainId());

        if (!Files.exists(filePath)) {
            log.warn("域不存在，无法更新: {}", domain.getDomainId());
            return false;
        }

        try {
            objectMapper.writeValue(filePath.toFile(), domain);
            log.info("✅ 更新知识域: {} ({})", domain.getDomainName(), domain.getDomainId());
            return true;
        } catch (IOException e) {
            log.error("更新知识域失败: {}", domain.getDomainId(), e);
            return false;
        }
    }

    @Override
    public boolean deleteDomain(String domainId) {
        Path filePath = getDomainFilePath(domainId);

        try {
            boolean deleted = Files.deleteIfExists(filePath);
            if (deleted) {
                log.info("✅ 删除知识域: {}", domainId);
            }
            return deleted;
        } catch (IOException e) {
            log.error("删除知识域失败: {}", domainId, e);
            return false;
        }
    }

    // ========== 通用方法 ==========

    @Override
    public boolean domainExists(String domainId) {
        return Files.exists(getDomainFilePath(domainId));
    }

    @Override
    public long countDomains() {
        try {
            if (!Files.exists(domainsDir)) {
                return 0;
            }
            return Files.list(domainsDir)
                    .filter(p -> p.toString().endsWith(".json"))
                    .count();
        } catch (IOException e) {
            log.error("统计知识域数量失败", e);
            return 0;
        }
    }

    @Override
    public long countDomainsByType(DomainType type) {
        return findDomainsByType(type).size();
    }

    // ========== 辅助方法 ==========

    /**
     * 获取域文件路径
     */
    private Path getDomainFilePath(String domainId) {
        return domainsDir.resolve(domainId + ".json");
    }

    /**
     * 从文件读取域
     */
    private KnowledgeDomain readDomainFromFile(Path filePath) {
        try {
            return objectMapper.readValue(filePath.toFile(), KnowledgeDomain.class);
        } catch (IOException e) {
            log.warn("读取域文件失败: {}", filePath, e);
            return null;
        }
    }

    // ========== 知识角色管理实现 ==========

    @Override
    public String saveRole(KnowledgeRole role) {
        role.prePersist();

        Path filePath = getRoleFilePath(role.getRoleId());

        try {
            objectMapper.writeValue(filePath.toFile(), role);
            log.info("✅ 保存知识角色: {} ({})", role.getRoleName(), role.getRoleId());
            return role.getRoleId();
        } catch (IOException e) {
            log.error("保存知识角色失败: {}", role.getRoleId(), e);
            throw new KnowledgeRegistryException("Failed to save role: " + role.getRoleId(), e);
        }
    }

    @Override
    public Optional<KnowledgeRole> findRoleById(String roleId) {
        Path filePath = getRoleFilePath(roleId);

        if (!Files.exists(filePath)) {
            return Optional.empty();
        }

        try {
            KnowledgeRole role = objectMapper.readValue(filePath.toFile(), KnowledgeRole.class);
            return Optional.of(role);
        } catch (IOException e) {
            log.error("读取知识角色失败: {}", roleId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeRole> findAllRoles() {
        try {
            if (!Files.exists(rolesDir)) {
                return Collections.emptyList();
            }

            return Files.list(rolesDir)
                    .filter(p -> p.toString().endsWith(".json"))
                    .map(this::readRoleFromFile)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            log.error("列出知识角色失败", e);
            return Collections.emptyList();
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
        role.preUpdate();

        Path filePath = getRoleFilePath(role.getRoleId());

        if (!Files.exists(filePath)) {
            log.warn("角色不存在，无法更新: {}", role.getRoleId());
            return false;
        }

        try {
            objectMapper.writeValue(filePath.toFile(), role);
            log.info("✅ 更新知识角色: {} ({})", role.getRoleName(), role.getRoleId());
            return true;
        } catch (IOException e) {
            log.error("更新知识角色失败: {}", role.getRoleId(), e);
            return false;
        }
    }

    @Override
    public boolean deleteRole(String roleId) {
        Path filePath = getRoleFilePath(roleId);

        try {
            boolean deleted = Files.deleteIfExists(filePath);
            if (deleted) {
                log.info("✅ 删除知识角色: {}", roleId);
            }
            return deleted;
        } catch (IOException e) {
            log.error("删除知识角色失败: {}", roleId, e);
            return false;
        }
    }

    @Override
    public boolean roleExists(String roleId) {
        return Files.exists(getRoleFilePath(roleId));
    }

    @Override
    public long countRoles() {
        try {
            if (!Files.exists(rolesDir)) {
                return 0;
            }
            return Files.list(rolesDir)
                    .filter(p -> p.toString().endsWith(".json"))
                    .count();
        } catch (IOException e) {
            log.error("统计知识角色数量失败", e);
            return 0;
        }
    }

    /**
     * 获取角色文件路径
     */
    private Path getRoleFilePath(String roleId) {
        return rolesDir.resolve(roleId + ".json");
    }

    /**
     * 从文件读取角色
     */
    private KnowledgeRole readRoleFromFile(Path filePath) {
        try {
            return objectMapper.readValue(filePath.toFile(), KnowledgeRole.class);
        } catch (IOException e) {
            log.warn("读取角色文件失败: {}", filePath, e);
            return null;
        }
    }
}

