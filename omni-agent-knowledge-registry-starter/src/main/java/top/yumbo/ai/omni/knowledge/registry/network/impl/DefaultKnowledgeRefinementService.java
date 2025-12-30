package top.yumbo.ai.omni.knowledge.registry.network.impl;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.knowledge.registry.model.document.KnowledgeDocument;
import top.yumbo.ai.omni.knowledge.registry.model.role.KnowledgeRole;
import top.yumbo.ai.omni.knowledge.registry.model.refinement.RefinedKnowledge;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRefinementService;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * 默认知识提炼服务实现
 *
 * <p>使用 AI 或规则从文档中提炼关键知识</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class DefaultKnowledgeRefinementService implements KnowledgeRefinementService {

    private final AIService aiService;

    public DefaultKnowledgeRefinementService(AIService aiService) {
        this.aiService = aiService;
        log.info("✅ DefaultKnowledgeRefinementService 已初始化（基于 AIService）");
    }

    @Override
    public RefinedKnowledge refineKnowledge(
            KnowledgeDocument document,
            KnowledgeRole role,
            boolean useAI) {
        log.debug("提炼知识: documentId={}, useAI={}", document.getId(), useAI);

        try {
            if (useAI && aiService != null) {
                return refineWithAI(document, role);
            } else {
                return refineWithRules(document, role);
            }
        } catch (Exception e) {
            log.error("❌ 知识提炼失败: documentId={}", document.getId(), e);
            // 降级：返回基础提炼结果
            return refineWithRules(document, role);
        }
    }

    @Override
    public List<RefinedKnowledge> batchRefineKnowledge(
            List<KnowledgeDocument> documents,
            KnowledgeRole role,
            boolean useAI) {
        log.debug("批量提炼知识: count={}, useAI={}", documents.size(), useAI);

        return documents.stream()
                .map(doc -> refineKnowledge(doc, role, useAI))
                .collect(Collectors.toList());
    }

    /**
     * 使用 AI 提炼知识
     */
    private RefinedKnowledge refineWithAI(KnowledgeDocument document, KnowledgeRole role) {
        try {
            // 构建提示词
            String prompt = buildRefinementPrompt(document, role);

            // 调用 AI 服务
            String aiResponse = aiService.chat(prompt);

            // 解析 AI 响应
            return parseAIResponse(document, aiResponse, role);

        } catch (Exception e) {
            log.warn("⚠️ AI 提炼失败，使用规则提炼: {}", e.getMessage());
            return refineWithRules(document, role);
        }
    }

    /**
     * 使用规则提炼知识（不依赖 AI）
     */
    private RefinedKnowledge refineWithRules(KnowledgeDocument document, KnowledgeRole role) {
        // 基础提炼：提取摘要和关键信息
        String summary = extractSummary(document.getContent());

        return RefinedKnowledge.builder()
                .knowledgeId(document.getId() != null ? document.getId() : UUID.randomUUID().toString())
                .title(document.getTitle() != null ? document.getTitle() : "未命名文档")
                .refinedContent(summary)
                .knowledgeType(document.getDocumentType() != null ? document.getDocumentType() : "GENERAL")
                .importance(5.0)
                .sourceDocumentId(document.getId())
                .build();
    }

    /**
     * 构建 AI 提炼提示词
     */
    private String buildRefinementPrompt(KnowledgeDocument document, KnowledgeRole role) {
        String roleContext = role != null ?
                String.format("作为 %s 角色，", role.getRoleName()) : "";

        return String.format("""
                %s请从以下文档中提炼关键知识：
                
                标题：%s
                内容：%s
                
                请提取：
                1. 核心概念
                2. 关键要点
                3. 实用建议
                
                以简洁的方式总结，突出最重要的信息。
                """,
                roleContext,
                document.getTitle() != null ? document.getTitle() : "未命名",
                truncateContent(document.getContent(), 1000)
        );
    }

    /**
     * 解析 AI 响应
     */
    private RefinedKnowledge parseAIResponse(KnowledgeDocument document, String aiResponse, KnowledgeRole role) {
        return RefinedKnowledge.builder()
                .knowledgeId(document.getId() != null ? document.getId() : UUID.randomUUID().toString())
                .title(document.getTitle() != null ? document.getTitle() : "AI 提炼知识")
                .refinedContent(aiResponse)
                .knowledgeType(document.getDocumentType() != null ? document.getDocumentType() : "AI_REFINED")
                .importance(7.0) // AI 提炼的知识优先级较高
                .sourceDocumentId(document.getId())
                .build();
    }

    /**
     * 提取摘要（简化版）
     */
    private String extractSummary(String content) {
        if (content == null || content.isEmpty()) {
            return "空文档";
        }

        // 简单策略：取前 200 个字符
        int summaryLength = Math.min(200, content.length());
        String summary = content.substring(0, summaryLength);

        if (content.length() > summaryLength) {
            summary += "...";
        }

        return summary;
    }

    /**
     * 截断内容
     */
    private String truncateContent(String content, int maxLength) {
        if (content == null) {
            return "";
        }
        if (content.length() <= maxLength) {
            return content;
        }
        return content.substring(0, maxLength) + "...";
    }
}

