package top.yumbo.ai.omni.core.service.role;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.RoleStatus;
import top.yumbo.ai.omni.core.dto.role.LearnFromDomainsRequest;

import java.time.LocalDateTime;

/**
 * 角色学习服务
 *
 * <p>负责角色从知识域学习知识的逻辑</p>
 * <p>这是一个基础实现，后续可扩展AI提炼功能</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class RoleLearningService {

    private final KnowledgeRegistry knowledgeRegistry;
    private final KnowledgeRoleService roleService;

    /**
     * 从指定域学习知识
     *
     * @param roleId 角色ID
     * @param request 学习请求
     */
    public void learnFromDomains(String roleId, LearnFromDomainsRequest request) {
        log.info("角色 {} 开始从 {} 个域学习知识", roleId, request.getSourceDomainIds().size());

        // 1. 获取角色
        KnowledgeRole role = roleService.getRole(roleId);

        // 2. 更新状态为学习中
        role.setStatus(RoleStatus.LEARNING);
        role.setLearningProgress(0);
        knowledgeRegistry.updateRole(role);

        try {
            // 3. 遍历源域进行学习
            int totalDomains = request.getSourceDomainIds().size();
            for (int i = 0; i < totalDomains; i++) {
                String sourceDomainId = request.getSourceDomainIds().get(i);

                log.info("正在从域 {} 学习... ({}/{})", sourceDomainId, i + 1, totalDomains);

                // TODO: 实现实际的学习逻辑
                // 1. 从源域获取文档
                // 2. 根据角色职责筛选相关文档
                // 3. 使用AI提炼知识（如果启用）
                // 4. 存储到角色知识库

                // 模拟学习过程
                learnFromDomain(role, sourceDomainId, request);

                // 更新进度
                int progress = (int) ((i + 1) * 100.0 / totalDomains);
                role.setLearningProgress(progress);
                knowledgeRegistry.updateRole(role);
            }

            // 4. 学习完成
            role.setStatus(RoleStatus.ACTIVE);
            role.setLearningProgress(100);
            role.setLastLearnedAt(LocalDateTime.now());

            // 更新源域列表
            if (!role.getSourceDomainIds().containsAll(request.getSourceDomainIds())) {
                role.getSourceDomainIds().addAll(request.getSourceDomainIds());
            }

            knowledgeRegistry.updateRole(role);

            log.info("✅ 角色 {} 学习完成", roleId);

        } catch (Exception e) {
            log.error("角色 {} 学习失败", roleId, e);

            // 恢复状态
            role.setStatus(RoleStatus.ACTIVE);
            knowledgeRegistry.updateRole(role);

            throw new RuntimeException("Learning failed: " + e.getMessage(), e);
        }
    }

    /**
     * 从单个域学习知识（基础实现）
     *
     * @param role 角色
     * @param sourceDomainId 源域ID
     * @param request 学习请求
     */
    private void learnFromDomain(KnowledgeRole role, String sourceDomainId, LearnFromDomainsRequest request) {
        // TODO: 完整实现需要：
        // 1. 获取源域的RAG服务
        // 2. 根据角色职责查询相关文档
        // 3. 提取关键知识
        // 4. 如果启用AI，使用AI模型提炼
        // 5. 存储到角色的知识域

        log.info("基础实现：从域 {} 提取知识到角色 {}", sourceDomainId, role.getRoleName());

        // 当前为占位实现，等待后续完善
        // 实际应用中需要集成RAG服务和AI模型
    }

    /**
     * 停止学习
     *
     * @param roleId 角色ID
     */
    public void stopLearning(String roleId) {
        log.info("停止角色 {} 的学习", roleId);

        KnowledgeRole role = roleService.getRole(roleId);

        if (role.getStatus() == RoleStatus.LEARNING) {
            role.setStatus(RoleStatus.PAUSED);
            knowledgeRegistry.updateRole(role);
            log.info("✅ 角色 {} 学习已暂停", roleId);
        }
    }
}

