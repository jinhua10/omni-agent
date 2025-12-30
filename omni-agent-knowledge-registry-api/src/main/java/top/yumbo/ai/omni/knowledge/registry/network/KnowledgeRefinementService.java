package top.yumbo.ai.omni.knowledge.registry.network;

import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDocument;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.RefinedKnowledge;
import java.util.List;

/**
 * 知识提炼服务接口
 *
 * <p>负责使用 AI 或规则从文档中提炼关键知识</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface KnowledgeRefinementService {

    /**
     * 使用 AI 提炼知识
     *
     * @param document 原始文档
     * @param role 角色信息
     * @param useAI 是否使用 AI 提炼
     * @return 提炼后的知识
     */
    RefinedKnowledge refineKnowledge(
            KnowledgeDocument document,
            KnowledgeRole role,
            boolean useAI
    );

    /**
     * 批量提炼知识
     *
     * @param documents 文档列表
     * @param role 角色信息
     * @param useAI 是否使用 AI
     * @return 提炼后的知识列表
     */
    List<RefinedKnowledge> batchRefineKnowledge(
            List<KnowledgeDocument> documents,
            KnowledgeRole role,
            boolean useAI
    );
}

