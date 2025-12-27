package top.yumbo.ai.omni.core.service.knowledge;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.core.model.RefinedKnowledge;
import top.yumbo.ai.omni.core.service.rag.RAGServiceFactory;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.HashMap;
import java.util.Map;

/**
 * 知识存储服务
 *
 * <p>将提炼的知识存储到角色的知识域</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class KnowledgeStorageService {

    private final KnowledgeRegistry knowledgeRegistry;

    @Autowired(required = false)
    private RAGServiceFactory ragServiceFactory;

    public KnowledgeStorageService(KnowledgeRegistry knowledgeRegistry) {
        this.knowledgeRegistry = knowledgeRegistry;
    }

    /**
     * 存储提炼的知识到角色知识域
     *
     * @param knowledge 提炼的知识
     * @param roleDomainId 角色的知识域ID
     */
    public void storeKnowledge(RefinedKnowledge knowledge, String roleDomainId) {
        log.info("存储知识到域 {}: {}", roleDomainId, knowledge.getTitle());

        try {
            // 1. 获取域信息
            KnowledgeDomain domain = knowledgeRegistry.findDomainById(roleDomainId)
                    .orElseThrow(() -> new RuntimeException("Domain not found: " + roleDomainId));

            // 2. 存储到文件系统（基础实现）
            storeToFileSystem(knowledge, domain);

            // 3. 索引到 RAG 向量数据库（如果可用）
            if (ragServiceFactory != null && ragServiceFactory.isRAGServiceAvailable()) {
                indexToRAG(knowledge, domain);
            } else {
                log.warn("RAG服务不可用，跳过向量索引");
            }

            log.info("✅ 知识存储成功: {}", knowledge.getKnowledgeId());

        } catch (Exception e) {
            log.error("存储知识失败: {}", knowledge.getKnowledgeId(), e);
            throw new RuntimeException("Failed to store knowledge", e);
        }
    }

    /**
     * 存储到文件系统
     */
    private void storeToFileSystem(RefinedKnowledge knowledge, KnowledgeDomain domain) throws IOException {
        // 构建存储路径
        String basePath = domain.getStoragePath();
        if (basePath == null) {
            basePath = "data/knowledge-network/domains/" + domain.getDomainId();
        }

        Path knowledgeDir = Paths.get(basePath, "learned-knowledge");
        Files.createDirectories(knowledgeDir);

        // 按知识类型分类存储
        String knowledgeType = knowledge.getKnowledgeType() != null ?
                knowledge.getKnowledgeType() : "general";
        Path typeDir = knowledgeDir.resolve(knowledgeType.toLowerCase());
        Files.createDirectories(typeDir);

        // 生成文件名
        String fileName = String.format("%s_%s.md",
                knowledge.getKnowledgeId(),
                sanitizeFileName(knowledge.getTitle()));
        Path filePath = typeDir.resolve(fileName);

        // 构建完整的 Markdown 内容
        String fullContent = buildMarkdownContent(knowledge);

        // 写入文件
        Files.writeString(filePath, fullContent,
                StandardOpenOption.CREATE,
                StandardOpenOption.TRUNCATE_EXISTING);

        log.info("知识已存储到文件: {}", filePath);
    }

    /**
     * 构建 Markdown 内容
     */
    private String buildMarkdownContent(RefinedKnowledge knowledge) {
        return String.format("""
                # %s
                
                **知识ID：** `%s`
                **来源域：** %s
                **原始文档：** %s
                **知识类型：** %s
                **重要性：** %s ⭐
                
                ---
                
                %s
                
                ---
                
                **元数据**
                - 角色ID: %s
                - 生成时间: %s
                """,
                knowledge.getTitle(),
                knowledge.getKnowledgeId(),
                knowledge.getSourceDomainId(),
                knowledge.getSourceDocumentId(),
                knowledge.getKnowledgeType(),
                "★".repeat(knowledge.getImportance() != null ? knowledge.getImportance() : 3),
                knowledge.getRefinedContent(),
                knowledge.getRoleId(),
                java.time.LocalDateTime.now()
        );
    }

    /**
     * 清理文件名中的非法字符
     */
    private String sanitizeFileName(String fileName) {
        if (fileName == null) {
            return "untitled";
        }
        // 移除或替换非法字符
        String sanitized = fileName.replaceAll("[\\\\/:*?\"<>|]", "_");
        // 限制长度
        if (sanitized.length() > 50) {
            sanitized = sanitized.substring(0, 50);
        }
        return sanitized;
    }

    /**
     * 索引到 RAG
     */
    private void indexToRAG(RefinedKnowledge knowledge, KnowledgeDomain domain) {
        try {
            log.info("开始索引知识到RAG: {}", knowledge.getKnowledgeId());

            // 1. 获取域的 RAG 服务
            RagService RagService = ragServiceFactory.getOrCreateRAGService(domain.getDomainId());

            // 2. 将知识转换为 RAG 文档
            Document ragDocument = convertToRAGDocument(knowledge, domain);

            // 3. 索引到向量数据库（使用 batchIndex）
            RagService.batchIndex(java.util.List.of(ragDocument));

            log.info("✅ 知识已索引到RAG: knowledgeId={}",
                    knowledge.getKnowledgeId());

        } catch (Exception e) {
            log.error("索引知识到RAG失败: {}", knowledge.getKnowledgeId(), e);
            // 不抛出异常，RAG索引失败不应阻止知识存储
        }
    }

    /**
     * 将精炼知识转换为 RAG 文档
     */
    private Document convertToRAGDocument(RefinedKnowledge knowledge, KnowledgeDomain domain) {
        // 构建文档内容（包含标题和内容）
        String fullContent = String.format("%s\n\n%s",
                knowledge.getTitle(),
                knowledge.getRefinedContent());

        // 构建元数据
        Map<String, Object> metadata = new HashMap<>();
        metadata.put("knowledgeId", knowledge.getKnowledgeId());
        metadata.put("knowledgeType", knowledge.getKnowledgeType());
        metadata.put("title", knowledge.getTitle());
        metadata.put("sourceDocumentId", knowledge.getSourceDocumentId());
        metadata.put("sourceDomainId", knowledge.getSourceDomainId());
        metadata.put("roleDomainId", domain.getDomainId());
        metadata.put("roleId", knowledge.getRoleId());
        metadata.put("importance", knowledge.getImportance());
        metadata.put("createdAt", java.time.LocalDateTime.now().toString());

        // 构建 RAG 文档
        return Document.builder()
                .id(knowledge.getKnowledgeId())
                .content(fullContent)
                .title(knowledge.getTitle())
                .metadata(metadata)
                .createdAt(System.currentTimeMillis())
                .build();
    }

    /**
     * 批量存储知识
     *
     * @param knowledgeList 知识列表
     * @param roleDomainId 角色域ID
     */
    public void batchStoreKnowledge(java.util.List<RefinedKnowledge> knowledgeList, String roleDomainId) {
        log.info("批量存储 {} 条知识到域 {}", knowledgeList.size(), roleDomainId);

        int successCount = 0;
        int failCount = 0;

        for (RefinedKnowledge knowledge : knowledgeList) {
            try {
                storeKnowledge(knowledge, roleDomainId);
                successCount++;
            } catch (Exception e) {
                log.error("存储知识失败: {}", knowledge.getKnowledgeId(), e);
                failCount++;
            }
        }

        log.info("批量存储完成: 成功={}, 失败={}", successCount, failCount);
    }
}


