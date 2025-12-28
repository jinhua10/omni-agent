package top.yumbo.ai.omni.core.service.knowledge;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.core.model.KnowledgeDocument;
import top.yumbo.ai.omni.core.service.rag.RAGServiceFactory;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * 知识提取服务
 *
 * <p>从知识域中提取文档</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class KnowledgeExtractionService {

    @Autowired(required = false)
    private KnowledgeRegistry knowledgeRegistry;

    @Autowired(required = false)
    private RAGServiceFactory ragServiceFactory;

    public KnowledgeExtractionService() {
        log.info("🔧 KnowledgeExtractionService initialized");
    }

    /**
     * 初始化后检查依赖
     */
    @jakarta.annotation.PostConstruct
    public void init() {
        if (knowledgeRegistry == null) {
            log.warn("⚠️ KnowledgeRegistry not available - KnowledgeExtractionService will use fallback mode");
        } else {
            log.info("✅ KnowledgeExtractionService initialized with KnowledgeRegistry");
        }
    }

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

        // 检查依赖是否可用
        if (knowledgeRegistry == null) {
            log.warn("KnowledgeRegistry not available, returning empty list");
            return new ArrayList<>();
        }

        // 1. 获取域信息
        KnowledgeDomain domain = knowledgeRegistry.findDomainById(domainId)
                .orElseThrow(() -> new RuntimeException("Domain not found: " + domainId));

        // 2. 尝试使用 RAG 服务进行语义搜索
        if (ragServiceFactory != null && ragServiceFactory.isRAGServiceAvailable()) {
            try {
                return extractFromRAG(domain, query, maxDocuments);
            } catch (Exception e) {
                log.warn("从 RAG 提取文档失败，降级到模拟提取: {}", e.getMessage());
                return simulateDocumentExtraction(domain, query, maxDocuments);
            }
        } else {
            log.warn("RAG 服务不可用，使用模拟提取");
            return simulateDocumentExtraction(domain, query, maxDocuments);
        }
    }

    /**
     * 从 RAG 服务提取文档（真实实现）
     */
    private List<KnowledgeDocument> extractFromRAG(
            KnowledgeDomain domain,
            String query,
            int maxDocuments) {

        log.info("🔍 使用 RAG 服务从域 {} 检索文档", domain.getDomainId());

        try {
            // 1. 获取域的 RAG 服务
            RagService ragService = ragServiceFactory.getOrCreateRAGService(domain.getDomainId());

            // 2. 执行语义搜索
            List<Document> searchResults = ragService.semanticSearch(query, maxDocuments);

            if (searchResults == null || searchResults.isEmpty()) {
                log.warn("RAG 搜索未返回任何结果");
                return new ArrayList<>();
            }

            // 3. 转换为 KnowledgeDocument
            List<KnowledgeDocument> documents = searchResults.stream()
                    .map(doc -> convertToKnowledgeDocument(doc, domain))
                    .collect(Collectors.toList());

            log.info("✅ 从 RAG 提取了 {} 个文档", documents.size());
            return documents;

        } catch (Exception e) {
            log.error("RAG 提取失败", e);
            throw new RuntimeException("Failed to extract from RAG", e);
        }
    }

    /**
     * 将 RAG Document 转换为 KnowledgeDocument
     */
    private KnowledgeDocument convertToKnowledgeDocument(Document doc, KnowledgeDomain domain) {
        return KnowledgeDocument.builder()
                .id(doc.getId())
                .title(extractTitle(doc))
                .content(doc.getContent())
                .summary(extractSummary(doc))
                .sourceDomainId(domain.getDomainId())
                .documentType(domain.getDomainType().name())
                .relevanceScore(0.8) // 默认相关性，如果需要可以从元数据提取
                .build();
    }

    /**
     * 提取文档标题
     */
    private String extractTitle(Document doc) {
        // 尝试从元数据获取标题
        if (doc.getMetadata() != null && doc.getMetadata().containsKey("title")) {
            return String.valueOf(doc.getMetadata().get("title"));
        }

        // 从内容提取第一行作为标题
        String content = doc.getContent();
        if (content != null && !content.isEmpty()) {
            String[] lines = content.split("\n", 2);
            String firstLine = lines[0].trim();
            // 移除 Markdown 标题标记
            firstLine = firstLine.replaceAll("^#+\\s*", "");
            if (firstLine.length() > 100) {
                return firstLine.substring(0, 100) + "...";
            }
            return firstLine;
        }

        return "Untitled Document";
    }

    /**
     * 提取文档摘要
     */
    private String extractSummary(Document doc) {
        // 尝试从元数据获取摘要
        if (doc.getMetadata() != null && doc.getMetadata().containsKey("summary")) {
            return String.valueOf(doc.getMetadata().get("summary"));
        }

        // 生成简单摘要（前200字符）
        String content = doc.getContent();
        if (content != null && content.length() > 200) {
            return content.substring(0, 200) + "...";
        }
        return content;
    }

    /**
     * 模拟文档提取（降级方案）
     */
    private List<KnowledgeDocument> simulateDocumentExtraction(
            KnowledgeDomain domain,
            String query,
            int maxDocuments) {

        log.warn("⚠️ 使用模拟文档提取（降级方案）");

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
        String[] keywords = responsibilities.toLowerCase().split("[,，、\\s]+");

        return documents.stream()
                .filter(doc -> {
                    String content = (doc.getContent() + " " + doc.getTitle()).toLowerCase();
                    for (String keyword : keywords) {
                        if (!keyword.isEmpty() && content.contains(keyword)) {
                            return true;
                        }
                    }
                    return false;
                })
                .collect(Collectors.toList());
    }
}


