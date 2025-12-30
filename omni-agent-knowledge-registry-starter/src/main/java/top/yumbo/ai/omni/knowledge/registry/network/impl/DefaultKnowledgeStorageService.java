package top.yumbo.ai.omni.knowledge.registry.network.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.knowledge.registry.model.refinement.RefinedKnowledge;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeStorageService;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.api.model.DocumentMetadata;

import java.util.ArrayList;
import java.util.List;

/**
 * 默认知识存储服务实现
 *
 * <p>基于 DocumentStorageService 实现，将知识序列化为 JSON 后存储为文档</p>
 * <p>存储路径：knowledge/{domainId}/{knowledgeId}.json</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class DefaultKnowledgeStorageService implements KnowledgeStorageService {

    private final DocumentStorageService documentStorage;
    private final ObjectMapper objectMapper;
    private static final String KNOWLEDGE_PREFIX = "knowledge";

    public DefaultKnowledgeStorageService(DocumentStorageService documentStorage) {
        this.documentStorage = documentStorage;
        this.objectMapper = new ObjectMapper();
        log.info("✅ DefaultKnowledgeStorageService 已初始化（基于 DocumentStorageService）");
    }

    @Override
    public boolean storeKnowledge(RefinedKnowledge knowledge, String domainId) {
        try {
            // 构建存储路径：knowledge/{domainId}/{knowledgeId}.json
            String documentId = buildDocumentId(domainId, knowledge.getKnowledgeId());

            // 序列化为 JSON
            byte[] jsonData = objectMapper.writeValueAsBytes(knowledge);

            // 存储到 DocumentStorage
            documentStorage.saveDocument(documentId,
                knowledge.getKnowledgeId() + ".json",
                jsonData);

            log.debug("✅ 存储知识: id={}, domain={}", knowledge.getKnowledgeId(), domainId);
            return true;
        } catch (Exception e) {
            log.error("❌ 存储知识失败: id={}, domain={}", knowledge.getKnowledgeId(), domainId, e);
            return false;
        }
    }

    @Override
    public int batchStoreKnowledge(List<RefinedKnowledge> knowledgeList, String domainId) {
        int count = 0;
        for (RefinedKnowledge knowledge : knowledgeList) {
            if (storeKnowledge(knowledge, domainId)) {
                count++;
            }
        }
        log.info("✅ 批量存储知识: 成功 {}/{}, domain={}", count, knowledgeList.size(), domainId);
        return count;
    }

    @Override
    public boolean updateKnowledge(RefinedKnowledge knowledge, String domainId) {
        try {
            // 更新操作与存储相同（覆盖写入）
            return storeKnowledge(knowledge, domainId);
        } catch (Exception e) {
            log.error("❌ 更新知识失败: id={}, domain={}", knowledge.getKnowledgeId(), domainId, e);
            return false;
        }
    }

    @Override
    public boolean deleteKnowledge(String knowledgeId, String domainId) {
        try {
            String documentId = buildDocumentId(domainId, knowledgeId);
            documentStorage.deleteDocument(documentId);
            log.debug("✅ 删除知识: id={}, domain={}", knowledgeId, domainId);
            return true;
        } catch (Exception e) {
            log.error("❌ 删除知识失败: id={}, domain={}", knowledgeId, domainId, e);
            return false;
        }
    }

    @Override
    public RefinedKnowledge getKnowledge(String knowledgeId, String domainId) {
        try {
            String documentId = buildDocumentId(domainId, knowledgeId);

            // 从 DocumentStorage 读取
            var docOpt = documentStorage.getDocument(documentId);
            if (docOpt.isEmpty()) {
                log.debug("⚠️ 知识不存在: id={}, domain={}", knowledgeId, domainId);
                return null;
            }

            // 反序列化
            RefinedKnowledge knowledge = objectMapper.readValue(docOpt.get(), RefinedKnowledge.class);
            log.debug("✅ 查询知识: id={}, domain={}", knowledgeId, domainId);
            return knowledge;
        } catch (Exception e) {
            log.error("❌ 查询知识失败: id={}, domain={}", knowledgeId, domainId, e);
            return null;
        }
    }

    @Override
    public List<RefinedKnowledge> searchKnowledge(String query, String domainId, int maxResults) {
        try {
            log.debug("搜索知识: query={}, domain={}, maxResults={}", query, domainId, maxResults);

            // 1. 调用 DocumentStorage.searchDocuments() 搜索文档
            List<DocumentMetadata> searchResults = documentStorage.searchDocuments(query);

            // 2. 过滤出指定 domain 的知识，并反序列化
            List<RefinedKnowledge> knowledgeList = new ArrayList<>();
            String domainPrefix = KNOWLEDGE_PREFIX + "/" + domainId + "/";

            for (DocumentMetadata metadata : searchResults) {
                // 检查是否属于指定域
                if (!metadata.getDocumentId().startsWith(domainPrefix)) {
                    continue;
                }

                try {
                    // 读取文档内容
                    var docOpt = documentStorage.getDocument(metadata.getDocumentId());
                    if (docOpt.isEmpty()) {
                        continue;
                    }

                    // 反序列化为 RefinedKnowledge
                    RefinedKnowledge knowledge = objectMapper.readValue(docOpt.get(), RefinedKnowledge.class);
                    knowledgeList.add(knowledge);

                    // 限制结果数量
                    if (knowledgeList.size() >= maxResults) {
                        break;
                    }
                } catch (Exception e) {
                    log.warn("反序列化知识失败: documentId={}", metadata.getDocumentId(), e);
                    continue;
                }
            }

            log.debug("✅ 搜索知识完成: 找到 {} 条结果, domain={}", knowledgeList.size(), domainId);
            return knowledgeList;

        } catch (Exception e) {
            log.error("❌ 搜索知识失败: query={}, domain={}", query, domainId, e);
            return new ArrayList<>();
        }
    }

    /**
     * 构建文档ID
     * 格式：knowledge/{domainId}/{knowledgeId}
     */
    private String buildDocumentId(String domainId, String knowledgeId) {
        return String.format("%s/%s/%s", KNOWLEDGE_PREFIX, domainId, knowledgeId);
    }
}

