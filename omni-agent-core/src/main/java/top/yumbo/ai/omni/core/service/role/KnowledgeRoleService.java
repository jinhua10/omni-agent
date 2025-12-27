package top.yumbo.ai.omni.core.service.role;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.RoleStatus;
import top.yumbo.ai.omni.core.dto.domain.CreateDomainRequest;
import top.yumbo.ai.omni.core.dto.role.CreateRoleRequest;
import top.yumbo.ai.omni.core.dto.role.UpdateRoleRequest;
import top.yumbo.ai.omni.core.service.domain.KnowledgeDomainService;

import java.util.List;
import java.util.UUID;

/**
 * 知识角色管理服务
 *
 * <p>负责角色的生命周期管理</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class KnowledgeRoleService {

    private final KnowledgeRegistry knowledgeRegistry;
    private final KnowledgeDomainService domainService;

    /**
     * 创建知识角色
     *
     * @param request 创建请求
     * @return 创建的角色
     */
    public KnowledgeRole createRole(CreateRoleRequest request) {
        log.info("创建知识角色: {}", request.getRoleName());

        // 1. 生成角色ID
        String roleId = UUID.randomUUID().toString();

        // 2. 为角色创建专属知识域
        CreateDomainRequest domainRequest = CreateDomainRequest.builder()
                .domainName(request.getRoleName() + " - 知识库")
                .domainType(DomainType.ROLE_KNOWLEDGE)
                .description("角色 " + request.getRoleName() + " 的专属知识库")
                .linkedEntityId(roleId)
                .build();

        KnowledgeDomain domain = domainService.createDomain(domainRequest);

        // 3. 创建角色实体
        KnowledgeRole role = KnowledgeRole.builder()
                .roleId(roleId)
                .roleName(request.getRoleName())
                .description(request.getDescription())
                .responsibilities(request.getResponsibilities())
                .knowledgeDomainId(domain.getDomainId())
                .sourceDomainIds(request.getSourceDomainIds())
                .status(RoleStatus.ACTIVE)
                .learningProgress(0)
                .config(request.getConfig())
                .build();

        // 4. 保存角色
        knowledgeRegistry.saveRole(role);

        log.info("✅ 知识角色创建成功: {} ({})", role.getRoleName(), roleId);
        return role;
    }

    /**
     * 获取知识角色
     *
     * @param roleId 角色ID
     * @return 知识角色
     */
    public KnowledgeRole getRole(String roleId) {
        return knowledgeRegistry.findRoleById(roleId)
                .orElseThrow(() -> new RuntimeException("Role not found: " + roleId));
    }

    /**
     * 列出所有知识角色
     *
     * @return 角色列表
     */
    public List<KnowledgeRole> listAllRoles() {
        return knowledgeRegistry.findAllRoles();
    }

    /**
     * 根据状态列出角色
     *
     * @param status 角色状态
     * @return 角色列表
     */
    public List<KnowledgeRole> listRolesByStatus(RoleStatus status) {
        return knowledgeRegistry.findRolesByStatus(status);
    }

    /**
     * 更新知识角色
     *
     * @param roleId 角色ID
     * @param request 更新请求
     * @return 更新后的角色
     */
    public KnowledgeRole updateRole(String roleId, UpdateRoleRequest request) {
        log.info("更新知识角色: {}", roleId);

        KnowledgeRole role = getRole(roleId);

        // 更新字段
        if (request.getRoleName() != null) {
            role.setRoleName(request.getRoleName());
        }
        if (request.getDescription() != null) {
            role.setDescription(request.getDescription());
        }
        if (request.getResponsibilities() != null) {
            role.setResponsibilities(request.getResponsibilities());
        }
        if (request.getStatus() != null) {
            role.setStatus(request.getStatus());
        }
        if (request.getSourceDomainIds() != null) {
            role.setSourceDomainIds(request.getSourceDomainIds());
        }

        // 保存更新
        knowledgeRegistry.updateRole(role);

        log.info("✅ 知识角色更新成功: {}", roleId);
        return role;
    }

    /**
     * 删除知识角色
     *
     * @param roleId 角色ID
     */
    public void deleteRole(String roleId) {
        log.info("删除知识角色: {}", roleId);

        // 检查角色是否存在
        KnowledgeRole role = getRole(roleId);

        // 删除角色
        knowledgeRegistry.deleteRole(roleId);

        // 可选：删除关联的知识域
        if (role.getKnowledgeDomainId() != null) {
            try {
                domainService.deleteDomain(role.getKnowledgeDomainId());
            } catch (Exception e) {
                log.warn("删除角色关联的知识域失败: {}", role.getKnowledgeDomainId(), e);
            }
        }

        log.info("✅ 知识角色删除成功: {}", roleId);
    }

    /**
     * 统计角色数量
     *
     * @return 总数量
     */
    public long countRoles() {
        return knowledgeRegistry.countRoles();
    }
}

