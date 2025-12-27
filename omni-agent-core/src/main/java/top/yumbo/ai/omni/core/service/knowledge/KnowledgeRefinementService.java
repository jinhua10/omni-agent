package top.yumbo.ai.omni.core.service.knowledge;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeRole;
import top.yumbo.ai.omni.core.model.KnowledgeDocument;
import top.yumbo.ai.omni.core.model.RefinedKnowledge;
import top.yumbo.ai.omni.ai.api.AIService;

import java.util.UUID;

/**
 * 知识提炼服务
 *
 * <p>使用 AI 模型从文档中提炼关键知识</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class KnowledgeRefinementService {

    @Autowired(required = false)
    private AIService aiService;

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

        if (useAI && aiService != null) {
            // 使用 AI 模型提炼
            try {
                refinedContent = refineWithAI(document, role);
            } catch (Exception e) {
                log.warn("AI 提炼失败，降级到简单提取: {}", e.getMessage());
                refinedContent = simpleRefine(document, role);
            }
        } else {
            if (useAI) {
                log.warn("AI 服务未配置，使用简单提取");
            }
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
     * 使用 AI 模型提炼知识（真实实现）
     */
    private String refineWithAI(KnowledgeDocument document, KnowledgeRole role) {
        log.info("🤖 使用 AI 模型提炼知识");

        // 1. 构建提示词
        String prompt = buildPrompt(document, role);

        // 2. 调用 AI 服务
        String aiResponse = aiService.chat(prompt);

        // 3. 格式化输出
        return String.format("""
                # %s
                
                > 由 %s 通过 AI 提炼
                > 来源域：%s
                
                %s
                
                ---
                
                **元信息**
                - 原始文档：%s
                - 提炼时间：%s
                - 提炼方式：AI 模型
                """,
                document.getTitle(),
                role.getRoleName(),
                document.getSourceDomainId(),
                aiResponse,
                document.getId(),
                java.time.LocalDateTime.now()
        );
    }

    /**
     * 简单提炼（不使用 AI）
     */
    private String simpleRefine(KnowledgeDocument document, KnowledgeRole role) {
        log.info("📝 简单提炼知识（不使用 AI）");

        return String.format("""
                # %s
                
                > 由 %s 整理
                > 来源域：%s
                
                ## 文档摘要
                
                %s
                
                ## 内容节选
                
                %s
                
                ---
                
                **元信息**
                - 原始文档：%s
                - 文档类型：%s
                - 相关性得分：%.2f
                - 整理时间：%s
                - 整理方式：简单提取
                """,
                document.getTitle(),
                role.getRoleName(),
                document.getSourceDomainId(),
                document.getSummary() != null ? document.getSummary() : "无摘要",
                truncateContent(document.getContent(), 800),
                document.getId(),
                document.getDocumentType(),
                document.getRelevanceScore() != null ? document.getRelevanceScore() : 0.0,
                java.time.LocalDateTime.now()
        );
    }

    /**
     * 构建 AI 提示词
     */
    private String buildPrompt(KnowledgeDocument document, KnowledgeRole role) {
        return String.format("""
                你是一个专业的知识管理助手。现在需要为一个特定角色提炼知识。
                
                ## 角色信息
                - 角色名称：%s
                - 角色职责：%s
                
                ## 任务
                从以下文档中提炼出与该角色职责最相关的关键知识点。
                
                ## 文档内容
                **标题**：%s
                
                **内容**：
                %s
                
                ## 输出要求
                
                请按以下 Markdown 格式输出：
                
                ## 核心要点
                
                （列出 3-5 个与角色职责直接相关的关键要点，每个要点用一个段落说明）
                
                ## 专业术语解释
                
                （解释文档中出现的与角色职责相关的专业术语，如果没有则省略此节）
                
                ## 实践建议
                
                （基于该角色的职责，给出如何应用这些知识的具体建议）
                
                ## 注意事项
                
                （如果有需要特别注意的地方，列出来；如果没有则省略此节）
                
                要求：
                1. 只提取与角色职责直接相关的内容
                2. 使用简洁专业的语言
                3. 结构化输出，便于阅读
                4. 使用 Markdown 格式
                5. 不要包含无关内容
                """,
                role.getRoleName(),
                role.getResponsibilities(),
                document.getTitle(),
                truncateContent(document.getContent(), 4000) // 限制输入长度
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
}
