package top.yumbo.ai.omni.core.service.knowledge;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeRole;
import top.yumbo.ai.omni.core.model.KnowledgeDocument;
import top.yumbo.ai.omni.core.model.RefinedKnowledge;

import java.util.UUID;

/**
 * 知识提炼服务
 *
 * <p>使用 AI 模型从文档中提炼关键知识</p>
 * <p>注意：当前为基础实现，实际应用中需要集成 AI 模型服务</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class KnowledgeRefinementService {

    // TODO: 注入 AI 模型服务
    // private final AIModelService aiModelService;

    /**
     * 使用 AI 提炼知识
     *
     * @param document 原始文档
     * @param role 角色信息
     * @param useAI 是否使用 AI 提炼
     * @return 提炼后的知识
     */
    public RefinedKnowledge refineKnowledge(
            KnowledgeDocument document,
            KnowledgeRole role,
            boolean useAI) {

        log.info("提炼知识: 文档={}, 角色={}, 使用AI={}",
                document.getTitle(), role.getRoleName(), useAI);

        String refinedContent;

        if (useAI) {
            // 使用 AI 模型提炼
            refinedContent = refineWithAI(document, role);
        } else {
            // 简单提取（不使用 AI）
            refinedContent = simpleRefine(document, role);
        }

        return RefinedKnowledge.builder()
                .knowledgeId(UUID.randomUUID().toString())
                .title(document.getTitle())
                .refinedContent(refinedContent)
                .sourceDocumentId(document.getId())
                .sourceDomainId(document.getSourceDomainId())
                .roleId(role.getRoleId())
                .knowledgeType(determineKnowledgeType(document, role))
                .importance(calculateImportance(document, role))
                .build();
    }

    /**
     * 使用 AI 模型提炼知识
     */
    private String refineWithAI(KnowledgeDocument document, KnowledgeRole role) {
        // TODO: 实际应用中调用 AI 模型 API
        // String prompt = buildPrompt(document, role);
        // return aiModelService.generate(prompt);

        // 当前为模拟实现
        log.info("使用 AI 提炼知识（模拟）");

        return String.format("""
                ## 提炼的知识（由 %s 提炼）
                
                **原始文档：** %s
                **来源域：** %s
                
                ### 关键要点
                
                1. 根据职责 "%s" 提取的核心知识点
                2. 专业术语和概念总结
                3. 实践建议和最佳实践
                
                ### 详细内容
                
                %s
                
                ### 应用建议
                
                基于角色职责的实际应用建议...
                
                ---
                *提炼时间：%s*
                *提炼方式：AI 模型*
                """,
                role.getRoleName(),
                document.getTitle(),
                document.getSourceDomainId(),
                role.getResponsibilities(),
                truncateContent(document.getContent(), 500),
                java.time.LocalDateTime.now()
        );
    }

    /**
     * 简单提炼（不使用 AI）
     */
    private String simpleRefine(KnowledgeDocument document, KnowledgeRole role) {
        log.info("简单提炼知识（不使用 AI）");

        return String.format("""
                ## 知识摘要（由 %s 整理）
                
                **文档标题：** %s
                **来源域：** %s
                **文档摘要：** %s
                
                ### 内容节选
                
                %s
                
                ---
                *整理时间：%s*
                *整理方式：简单提取*
                """,
                role.getRoleName(),
                document.getTitle(),
                document.getSourceDomainId(),
                document.getSummary() != null ? document.getSummary() : "无",
                truncateContent(document.getContent(), 800),
                java.time.LocalDateTime.now()
        );
    }

    /**
     * 确定知识类型
     */
    private String determineKnowledgeType(KnowledgeDocument document, KnowledgeRole role) {
        // 根据文档类型和角色职责确定知识类型
        if (role.getResponsibilities() != null) {
            String resp = role.getResponsibilities().toLowerCase();
            if (resp.contains("安全") || resp.contains("漏洞")) {
                return "SECURITY_KNOWLEDGE";
            } else if (resp.contains("架构") || resp.contains("设计")) {
                return "ARCHITECTURE_KNOWLEDGE";
            } else if (resp.contains("代码") || resp.contains("质量")) {
                return "CODE_QUALITY_KNOWLEDGE";
            }
        }
        return "GENERAL_KNOWLEDGE";
    }

    /**
     * 计算重要性等级
     */
    private Integer calculateImportance(KnowledgeDocument document, KnowledgeRole role) {
        // 基于相关性得分计算重要性
        if (document.getRelevanceScore() != null) {
            double score = document.getRelevanceScore();
            if (score >= 0.8) return 5;
            if (score >= 0.6) return 4;
            if (score >= 0.4) return 3;
            if (score >= 0.2) return 2;
            return 1;
        }
        return 3; // 默认中等重要性
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
        return content.substring(0, maxLength) + "\n\n... (内容已截断) ...";
    }

    /**
     * 构建 AI 提示词（供未来使用）
     */
    private String buildPrompt(KnowledgeDocument document, KnowledgeRole role) {
        return String.format("""
                你是一个 %s，你的职责是：%s
                
                请从以下文档中提炼出与你职责最相关的关键知识点：
                
                【文档标题】%s
                【文档内容】
                %s
                
                请按以下格式输出：
                
                ## 关键要点
                （列出3-5个关键要点）
                
                ## 专业术语
                （解释相关的专业术语）
                
                ## 实践建议
                （基于你的职责给出实践建议）
                
                要求：
                1. 只提取与职责直接相关的内容
                2. 使用专业术语
                3. 结构化输出
                4. Markdown 格式
                """,
                role.getRoleName(),
                role.getResponsibilities(),
                document.getTitle(),
                document.getContent()
        );
    }
}

