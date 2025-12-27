package top.yumbo.ai.omni.core.service.knowledge;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.core.model.KnowledgeDocument;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * 知识提取服务
 *
 * <p>从知识域中提取文档</p>
 * <p>注意：这是一个基础实现，实际应用中需要集成 RAG 服务</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class KnowledgeExtractionService {

    private final KnowledgeRegistry knowledgeRegistry;

    /**
     * 从域中提取相关文档
     *
     * @param domainId 域ID
     * @param query 查询关键词（用于筛选）
     * @param maxDocuments 最大文档数量
     * @return 文档列表
     */
    public List<KnowledgeDocument> extractDocuments(String domainId, String query, int maxDocuments) {
        log.info("从域 {} 提取文档，查询: {}, 最大数量: {}", domainId, query, maxDocuments);

        // 1. 获取域信息
        KnowledgeDomain domain = knowledgeRegistry.findDomainById(domainId)
                .orElseThrow(() -> new RuntimeException("Domain not found: " + domainId));

        // 2. 从域中查询文档
        // TODO: 实际应用中应该调用 RAG 服务进行语义搜索
        // 当前为模拟实现
        List<KnowledgeDocument> documents = simulateDocumentExtraction(domain, query, maxDocuments);

        log.info("从域 {} 提取了 {} 个文档", domainId, documents.size());
        return documents;
    }

    /**
     * 模拟文档提取（占位实现）
     *
     * <p>实际应用中应该：</p>
     * <ul>
     *     <li>调用 RAG 服务进行语义搜索</li>
     *     <li>根据相关性得分排序</li>
     *     <li>返回最相关的文档</li>
     * </ul>
     */
    private List<KnowledgeDocument> simulateDocumentExtraction(
            KnowledgeDomain domain,
            String query,
            int maxDocuments) {

        List<KnowledgeDocument> documents = new ArrayList<>();

        // 模拟生成一些文档
        for (int i = 0; i < Math.min(5, maxDocuments); i++) {
            KnowledgeDocument doc = KnowledgeDocument.builder()
                    .id(UUID.randomUUID().toString())
                    .title("来自 " + domain.getDomainName() + " 的文档 " + (i + 1))
                    .content("这是一个模拟文档内容，实际应用中应该从 RAG 索引中检索真实内容。\n" +
                            "查询关键词: " + query + "\n" +
                            "域类型: " + domain.getDomainType())
                    .summary("文档摘要 " + (i + 1))
                    .sourceDomainId(domain.getDomainId())
                    .documentType(domain.getDomainType().name())
                    .relevanceScore(0.9 - i * 0.1)
                    .build();

            documents.add(doc);
        }

        return documents;
    }

    /**
     * 根据角色职责筛选相关文档
     *
     * @param documents 文档列表
     * @param responsibilities 角色职责描述
     * @return 筛选后的文档列表
     */
    public List<KnowledgeDocument> filterRelevantDocuments(
            List<KnowledgeDocument> documents,
            String responsibilities) {

        if (responsibilities == null || responsibilities.isEmpty()) {
            return documents;
        }

        // 简单的关键词匹配筛选
        // 实际应用中可以使用更复杂的语义匹配
        String[] keywords = responsibilities.toLowerCase().split("[,，、\\s]+");

        return documents.stream()
                .filter(doc -> {
                    String content = (doc.getContent() + " " + doc.getTitle()).toLowerCase();
                    for (String keyword : keywords) {
                        if (content.contains(keyword)) {
                            return true;
                        }
                    }
                    return false;
                })
                .toList();
    }
}


