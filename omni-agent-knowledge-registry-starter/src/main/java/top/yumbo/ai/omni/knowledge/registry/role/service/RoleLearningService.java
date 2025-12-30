package top.yumbo.ai.omni.knowledge.registry.role.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.dto.role.LearnFromDomainsRequest;
import top.yumbo.ai.omni.knowledge.registry.model.document.KnowledgeDocument;
import top.yumbo.ai.omni.knowledge.registry.model.role.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.refinement.RefinedKnowledge;
import top.yumbo.ai.omni.knowledge.registry.model.role.RoleStatus;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeExtractionService;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRefinementService;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeStorageService;


import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * 角色学习服务
 *
 * <p>负责角色从知识域学习知识的逻辑</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class RoleLearningService {

    @Autowired(required = false)
    private KnowledgeRegistry knowledgeRegistry;

    @Autowired
    private KnowledgeRoleService roleService;

    @Autowired
    private KnowledgeExtractionService extractionService;

    @Autowired
    private KnowledgeRefinementService refinementService;

    @Autowired
    private KnowledgeStorageService storageService;

    @jakarta.annotation.PostConstruct
    public void init() {
        if (knowledgeRegistry == null) {
            log.warn("⚠️ KnowledgeRegistry not available - RoleLearningService will use fallback mode");
        } else {
            log.info("✅ RoleLearningService initialized with KnowledgeRegistry");
        }
    }

    /**
     * 从指定域学习知识
     *
     * @param roleId 角色ID
     * @param request 学习请求
     */
    public void learnFromDomains(String roleId, LearnFromDomainsRequest request) {
        log.info("🎓 角色 {} 开始从 {} 个域学习知识", roleId, request.getSourceDomainIds().size());

        // 1. 获取角色
        KnowledgeRole role = roleService.getRole(roleId);

        // 2. 更新状态为学习中
        role.setStatus(RoleStatus.LEARNING);
        role.setLearningProgress(0);
        knowledgeRegistry.updateRole(role);

        try {
            // 3. 遍历源域进行学习
            int totalDomains = request.getSourceDomainIds().size();
            List<RefinedKnowledge> allKnowledge = new ArrayList<>();

            for (int i = 0; i < totalDomains; i++) {
                String sourceDomainId = request.getSourceDomainIds().get(i);

                log.info("📚 正在从域 {} 学习... ({}/{})", sourceDomainId, i + 1, totalDomains);

                // 从单个域学习
                List<RefinedKnowledge> domainKnowledge = learnFromDomain(
                        role,
                        sourceDomainId,
                        request
                );

                allKnowledge.addAll(domainKnowledge);

                // 更新进度
                int progress = (int) ((i + 1) * 100.0 / totalDomains);
                role.setLearningProgress(progress);
                knowledgeRegistry.updateRole(role);
            }

            // 4. 批量存储所有学到的知识
            if (!allKnowledge.isEmpty()) {
                log.info("💾 存储 {} 条学到的知识到角色知识库", allKnowledge.size());
                storageService.batchStoreKnowledge(allKnowledge, role.getKnowledgeDomainId());
            }

            // 5. 学习完成
            role.setStatus(RoleStatus.ACTIVE);
            role.setLearningProgress(100);
            role.setLastLearnedAt(LocalDateTime.now());

            // 更新源域列表
            if (!role.getSourceDomainIds().containsAll(request.getSourceDomainIds())) {
                role.getSourceDomainIds().addAll(request.getSourceDomainIds());
            }

            knowledgeRegistry.updateRole(role);

            log.info("✅ 角色 {} 学习完成！共学习了 {} 条知识", roleId, allKnowledge.size());

        } catch (Exception e) {
            log.error("❌ 角色 {} 学习失败", roleId, e);

            // 恢复状态
            role.setStatus(RoleStatus.ACTIVE);
            knowledgeRegistry.updateRole(role);

            throw new RuntimeException("Learning failed: " + e.getMessage(), e);
        }
    }

    /**
     * 从单个域学习知识（完整实现）
     *
     * @param role 角色
     * @param sourceDomainId 源域ID
     * @param request 学习请求
     * @return 学到的知识列表
     */
    private List<RefinedKnowledge> learnFromDomain(
            KnowledgeRole role,
            String sourceDomainId,
            LearnFromDomainsRequest request) {

        List<RefinedKnowledge> knowledgeList = new ArrayList<>();

        try {
            // 1. 从源域提取文档
            log.info("📖 从域 {} 提取文档...", sourceDomainId);

            // 将职责列表转换为查询字符串
            String query = String.join(" ", role.getResponsibilities());

            List<KnowledgeDocument> documents = extractionService.extractDocumentsByQuery(
                    query,
                    List.of(sourceDomainId),
                    request.getMaxDocuments()
            );

            if (documents.isEmpty()) {
                log.warn("⚠️ 从域 {} 未提取到任何文档", sourceDomainId);
                return knowledgeList;
            }

            log.info("📄 提取到 {} 个文档", documents.size());

            // 2. 文档已经根据查询进行了筛选，直接使用
            log.info("🔍 使用提取的相关文档，共 {} 个", documents.size());

            // 3. 对每个文档进行知识提炼
            int docCount = 0;
            for (KnowledgeDocument doc : documents) {
                docCount++;
                log.info("⚙️ 提炼文档 {}/{}: {}", docCount, documents.size(), doc.getTitle());

                try {
                    // 使用 AI 提炼知识（如果启用）
                    RefinedKnowledge knowledge = refinementService.refineKnowledge(
                            doc,
                            role,
                            request.getUseAIRefinement()
                    );

                    knowledgeList.add(knowledge);
                    log.info("✓ 提炼完成: {}", knowledge.getTitle());

                } catch (Exception e) {
                    log.error("❌ 提炼文档失败: {}", doc.getTitle(), e);
                    // 继续处理下一个文档
                }
            }

            log.info("✅ 从域 {} 学习了 {} 条知识", sourceDomainId, knowledgeList.size());

        } catch (Exception e) {
            log.error("❌ 从域 {} 学习失败", sourceDomainId, e);
            // 不抛出异常，继续处理其他域
        }

        return knowledgeList;
    }

    /**
     * 停止学习
     *
     * @param roleId 角色ID
     */
    public void stopLearning(String roleId) {
        log.info("⏸️ 停止角色 {} 的学习", roleId);

        KnowledgeRole role = roleService.getRole(roleId);

        if (role.getStatus() == RoleStatus.LEARNING) {
            role.setStatus(RoleStatus.PAUSED);
            knowledgeRegistry.updateRole(role);
            log.info("✅ 角色 {} 学习已暂停", roleId);
        } else {
            log.warn("⚠️ 角色 {} 当前不在学习状态", roleId);
        }
    }
}


