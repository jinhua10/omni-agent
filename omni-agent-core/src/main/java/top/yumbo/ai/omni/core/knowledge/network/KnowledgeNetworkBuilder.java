package top.yumbo.ai.omni.core.knowledge.network;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeBuildResult;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeBuildStatus;
import top.yumbo.ai.omni.core.service.domain.KnowledgeDomainService;
import top.yumbo.ai.omni.core.model.RefinedKnowledge;
import top.yumbo.ai.omni.core.service.knowledge.KnowledgeStorageService;
import top.yumbo.ai.omni.document.storage.api.DocumentStorageService;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 知识网络构建器
 *
 * <p>独立后台服务，基于已提取的文本构建知识网络</p>
 *
 * <h3>核心功能：</h3>
 * <ul>
 *   <li>从存储中读取已提取的文本</li>
 *   <li>调用AI服务分析文本，提取知识</li>
 *   <li>构建知识网络（知识域、概念、关系）</li>
 *   <li>持久化知识到对应的知识域</li>
 * </ul>
 *
 * <h3>设计理念：</h3>
 * <ul>
 *   <li>异步执行，不阻塞文档处理流程</li>
 *   <li>支持多种存储后端（file, redis, mongodb, elasticsearch等）</li>
 *   <li>可配置AI服务（ollama, online-api, onnx等）</li>
 *   <li>支持增量更新（监控文本变更）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class KnowledgeNetworkBuilder {

    @Autowired
    private DocumentStorageService documentStorage;

    @Autowired
    private KnowledgeDomainService domainService;

    @Autowired
    private KnowledgeStorageService knowledgeStorage;

    @Autowired(required = false)
    private AIService aiService;

    /**
     * 构建任务追踪
     * key: documentId, value: 构建状态
     */
    private final Map<String, BuildStatus> buildStatusMap = new ConcurrentHashMap<>();

    /**
     * 为指定文档构建知识网络（异步）
     *
     * @param documentId 文档ID
     * @param domainId 目标知识域ID
     * @return 异步构建任务
     */
    @Async
    public CompletableFuture<Boolean> buildKnowledgeNetworkAsync(String documentId, String domainId) {
        log.info("🔨 开始为文档 {} 构建知识网络（域: {}）", documentId, domainId);

        try {
            // 更新构建状态
            buildStatusMap.put(documentId, BuildStatus.PROCESSING);

            // 1. 获取已提取的文本
            Optional<String> extractedTextOpt = documentStorage.getExtractedText(documentId);
            if (extractedTextOpt.isEmpty()) {
                log.warn("⚠️ 文档 {} 未找到提取文本，跳过知识构建", documentId);
                buildStatusMap.put(documentId, BuildStatus.FAILED);
                return CompletableFuture.completedFuture(false);
            }

            String extractedText = extractedTextOpt.get();
            log.debug("📄 已获取文档 {} 的提取文本，长度: {}", documentId, extractedText.length());

            // 2. 获取知识域配置
            Optional<KnowledgeDomain> domainOpt = domainService.getDomain(domainId);
            if (domainOpt.isEmpty()) {
                log.warn("⚠️ 知识域 {} 不存在，跳过知识构建", domainId);
                buildStatusMap.put(documentId, BuildStatus.FAILED);
                return CompletableFuture.completedFuture(false);
            }

            KnowledgeDomain domain = domainOpt.get();

            // 3. 使用AI服务分析文本，提取知识
            List<RefinedKnowledge> knowledgeList = extractKnowledgeWithAI(
                    extractedText,
                    documentId,
                    domain
            );

            if (knowledgeList.isEmpty()) {
                log.warn("⚠️ 从文档 {} 未提取到任何知识", documentId);
                buildStatusMap.put(documentId, BuildStatus.COMPLETED);
                return CompletableFuture.completedFuture(true);
            }

            // 4. 存储知识到知识域
            knowledgeStorage.batchStoreKnowledge(knowledgeList, domainId);

            log.info("✅ 文档 {} 知识网络构建完成，提取了 {} 条知识", documentId, knowledgeList.size());
            buildStatusMap.put(documentId, BuildStatus.COMPLETED);

            return CompletableFuture.completedFuture(true);

        } catch (Exception e) {
            log.error("❌ 文档 {} 知识网络构建失败", documentId, e);
            buildStatusMap.put(documentId, BuildStatus.FAILED);
            return CompletableFuture.completedFuture(false);
        }
    }

    /**
     * 批量构建知识网络
     *
     * @param documentIds 文档ID列表
     * @param domainId 目标知识域ID
     * @return 异步构建任务列表
     */
    public List<CompletableFuture<Boolean>> batchBuildKnowledgeNetwork(
            List<String> documentIds,
            String domainId) {

        log.info("🔨 批量构建知识网络：{} 个文档 → 域 {}", documentIds.size(), domainId);

        List<CompletableFuture<Boolean>> futures = new ArrayList<>();
        for (String documentId : documentIds) {
            CompletableFuture<Boolean> future = buildKnowledgeNetworkAsync(documentId, domainId);
            futures.add(future);
        }

        return futures;
    }

    /**
     * 使用AI服务从文本中提取知识
     *
     * @param text 文本内容
     * @param documentId 文档ID
     * @param domain 知识域
     * @return 提取的知识列表
     */
    private List<RefinedKnowledge> extractKnowledgeWithAI(
            String text,
            String documentId,
            KnowledgeDomain domain) {

        if (aiService == null) {
            log.warn("⚠️ AI服务未配置，使用规则提取知识");
            return extractKnowledgeWithRules(text, documentId, domain);
        }

        try {
            log.debug("🤖 使用AI服务提取知识...");

            // 构建AI提示词
            String prompt = buildKnowledgeExtractionPrompt(text, domain);

            // 调用AI服务
            String aiResponse = aiService.chat(prompt);

            // 解析AI响应，提取知识
            List<RefinedKnowledge> knowledgeList = parseAIResponse(aiResponse, documentId, domain);

            log.debug("✅ AI提取到 {} 条知识", knowledgeList.size());

            return knowledgeList;

        } catch (Exception e) {
            log.error("❌ AI知识提取失败，回退到规则提取", e);
            return extractKnowledgeWithRules(text, documentId, domain);
        }
    }

    /**
     * 构建知识提取的AI提示词
     */
    private String buildKnowledgeExtractionPrompt(String text, KnowledgeDomain domain) {
        return String.format(
                "请从以下文本中提取关键知识点。\n\n" +
                "知识域类型：%s\n" +
                "知识域描述：%s\n\n" +
                "文本内容：\n%s\n\n" +
                "要求：\n" +
                "1. 提取3-10个关键知识点\n" +
                "2. 每个知识点包含：标题、内容摘要、重要性（0.0-1.0）\n" +
                "3. 以JSON格式输出\n" +
                "4. 格式示例：\n" +
                "[\n" +
                "  {\n" +
                "    \"title\": \"知识点标题\",\n" +
                "    \"content\": \"知识点内容摘要\",\n" +
                "    \"importance\": 0.85,\n" +
                "    \"type\": \"TECHNICAL\"\n" +
                "  }\n" +
                "]\n",
                domain.getDomainType(),
                domain.getDescription(),
                text.length() > 3000 ? text.substring(0, 3000) + "..." : text
        );
    }

    /**
     * 解析AI响应，提取知识
     */
    private List<RefinedKnowledge> parseAIResponse(
            String aiResponse,
            String documentId,
            KnowledgeDomain domain) {

        List<RefinedKnowledge> knowledgeList = new ArrayList<>();

        try {
            // 尝试解析JSON格式的响应
            // TODO: 使用 JSON 库解析（Jackson/Gson）
            // 简化实现：使用正则表达式或简单解析

            // 临时实现：将整个响应作为一条知识
            RefinedKnowledge knowledge = new RefinedKnowledge();
            knowledge.setKnowledgeId("knowledge-" + UUID.randomUUID().toString());
            knowledge.setTitle("从文档提取的知识: " + documentId);
            knowledge.setRefinedContent(aiResponse);
            knowledge.setKnowledgeType("EXTRACTED");
            knowledge.setSourceDocumentId(documentId);
            knowledge.setSourceDomainId(domain.getDomainId());
            knowledge.setImportance(0.7);
            knowledge.setCreatedAt(new Date());

            knowledgeList.add(knowledge);

        } catch (Exception e) {
            log.error("❌ 解析AI响应失败", e);
        }

        return knowledgeList;
    }

    /**
     * 基于规则的知识提取（AI不可用时的回退方案）
     */
    private List<RefinedKnowledge> extractKnowledgeWithRules(
            String text,
            String documentId,
            KnowledgeDomain domain) {

        log.debug("📋 使用规则提取知识...");

        List<RefinedKnowledge> knowledgeList = new ArrayList<>();

        // 简单实现：将文本分段作为知识点
        String[] paragraphs = text.split("\n\n");

        int maxKnowledge = Math.min(paragraphs.length, 10);

        for (int i = 0; i < maxKnowledge; i++) {
            String paragraph = paragraphs[i].trim();
            if (paragraph.length() < 50) {
                continue; // 跳过太短的段落
            }

            RefinedKnowledge knowledge = new RefinedKnowledge();
            knowledge.setKnowledgeId("knowledge-" + UUID.randomUUID().toString());
            knowledge.setTitle("段落 " + (i + 1));
            knowledge.setRefinedContent(paragraph);
            knowledge.setKnowledgeType("EXTRACTED");
            knowledge.setSourceDocumentId(documentId);
            knowledge.setSourceDomainId(domain.getDomainId());
            knowledge.setImportance(0.5 + (Math.random() * 0.3)); // 0.5-0.8
            knowledge.setCreatedAt(new Date());

            knowledgeList.add(knowledge);
        }

        log.debug("✅ 规则提取到 {} 条知识", knowledgeList.size());

        return knowledgeList;
    }

    /**
     * 获取文档的构建状态
     */
    public KnowledgeBuildStatus getBuildStatus(String documentId) {
        return buildStatusMap.getOrDefault(documentId, KnowledgeBuildStatus.NOT_STARTED);
    }

    /**
     * 清理构建状态
     */
    public void clearBuildStatus(String documentId) {
        buildStatusMap.remove(documentId);
    }

    /**
     * 构建成功结果
     */
    private KnowledgeBuildResult buildSuccessResult(
            String documentId,
            String domainId,
            List<RefinedKnowledge> knowledgeList,
            LocalDateTime startTime) {

        List<String> knowledgeIds = knowledgeList.stream()
                .map(RefinedKnowledge::getKnowledgeId)
                .toList();

        LocalDateTime endTime = LocalDateTime.now();
        long duration = java.time.Duration.between(startTime, endTime).toMillis();

        return KnowledgeBuildResult.builder()
                .documentId(documentId)
                .domainId(domainId)
                .success(true)
                .knowledgeCount(knowledgeList.size())
                .knowledgeIds(knowledgeIds)
                .startTime(startTime)
                .endTime(endTime)
                .duration(duration)
                .build();
    }

    /**
     * 构建失败结果
     */
    private KnowledgeBuildResult buildFailedResult(
            String documentId,
            String domainId,
            String errorMessage,
            LocalDateTime startTime) {

        LocalDateTime endTime = LocalDateTime.now();
        long duration = java.time.Duration.between(startTime, endTime).toMillis();

        return KnowledgeBuildResult.builder()
                .documentId(documentId)
                .domainId(domainId)
                .success(false)
                .knowledgeCount(0)
                .errorMessage(errorMessage)
                .startTime(startTime)
                .endTime(endTime)
                .duration(duration)
                .build();
    }
}

