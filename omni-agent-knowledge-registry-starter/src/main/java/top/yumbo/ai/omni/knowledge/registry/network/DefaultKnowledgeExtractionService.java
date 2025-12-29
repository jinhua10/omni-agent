package top.yumbo.ai.omni.knowledge.registry.network;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDocument;
import top.yumbo.ai.omni.knowledge.registry.model.RefinedKnowledge;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 默认知识提取服务实现
 *
 * <p>基于 KnowledgeStorageService 实现知识提取功能</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class DefaultKnowledgeExtractionService implements KnowledgeExtractionService {

    private final KnowledgeStorageService storageService;

    public DefaultKnowledgeExtractionService(KnowledgeStorageService storageService) {
        this.storageService = storageService;
        log.info("✅ DefaultKnowledgeExtractionService 已初始化（基于 KnowledgeStorageService）");
    }

    @Override
    public List<KnowledgeDocument> extractDocumentsFromDomain(String domainId, int maxResults) {
        log.debug("从域中提取文档: domainId={}, maxResults={}", domainId, maxResults);

        try {
            // 使用空字符串搜索获取域中的所有知识
            List<RefinedKnowledge> knowledgeList = storageService.searchKnowledge("", domainId, maxResults);

            // 转换为 KnowledgeDocument
            List<KnowledgeDocument> documents = knowledgeList.stream()
                    .map(this::convertToKnowledgeDocument)
                    .collect(Collectors.toList());

            log.debug("✅ 从域 {} 提取到 {} 个文档", domainId, documents.size());
            return documents;

        } catch (Exception e) {
            log.error("❌ 从域提取文档失败: domainId={}", domainId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public List<KnowledgeDocument> extractDocumentsByQuery(
            String query,
            List<String> domainIds,
            int maxResults) {
        log.debug("根据查询提取文档: query={}, domainIds={}, maxResults={}",
                query, domainIds, maxResults);

        try {
            List<KnowledgeDocument> allDocuments = new ArrayList<>();
            int perDomainLimit = Math.max(1, maxResults / domainIds.size());

            // 在每个域中搜索
            for (String domainId : domainIds) {
                try {
                    List<RefinedKnowledge> knowledgeList = storageService.searchKnowledge(
                            query, domainId, perDomainLimit);

                    List<KnowledgeDocument> documents = knowledgeList.stream()
                            .map(this::convertToKnowledgeDocument)
                            .collect(Collectors.toList());

                    allDocuments.addAll(documents);
                } catch (Exception e) {
                    log.warn("⚠️ 域 {} 搜索失败: {}", domainId, e.getMessage());
                    continue;
                }
            }

            // 限制总结果数
            List<KnowledgeDocument> result = allDocuments.stream()
                    .limit(maxResults)
                    .collect(Collectors.toList());

            log.debug("✅ 根据查询提取到 {} 个文档", result.size());
            return result;

        } catch (Exception e) {
            log.error("❌ 根据查询提取文档失败: query={}", query, e);
            return new ArrayList<>();
        }
    }

    @Override
    public KnowledgeDocument extractDocumentDetails(String documentId, String domainId) {
        log.debug("提取文档详情: documentId={}, domainId={}", documentId, domainId);

        try {
            RefinedKnowledge knowledge = storageService.getKnowledge(documentId, domainId);

            if (knowledge == null) {
                log.warn("⚠️ 文档不存在: documentId={}, domainId={}", documentId, domainId);
                return null;
            }

            KnowledgeDocument document = convertToKnowledgeDocument(knowledge);
            log.debug("✅ 提取文档详情成功: {}", documentId);
            return document;

        } catch (Exception e) {
            log.error("❌ 提取文档详情失败: documentId={}, domainId={}", documentId, domainId, e);
            return null;
        }
    }

    /**
     * 转换 RefinedKnowledge 为 KnowledgeDocument
     */
    private KnowledgeDocument convertToKnowledgeDocument(RefinedKnowledge knowledge) {
        return KnowledgeDocument.builder()
                .id(knowledge.getKnowledgeId())
                .title(knowledge.getTitle())
                .content(knowledge.getRefinedContent())
                .documentType(knowledge.getKnowledgeType())
                .build();
    }
}

