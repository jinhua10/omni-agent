package top.yumbo.ai.omni.core.knowledge.network;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.core.service.domain.KnowledgeDomainService;
import top.yumbo.ai.omni.document.storage.DocumentStorageService;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 知识网络管理器
 *
 * <p>知识网络的总控制中心，负责：</p>
 * <ul>
 *   <li>监控已提取文本的变化</li>
 *   <li>调度知识网络构建任务</li>
 *   <li>管理知识域之间的关联</li>
 *   <li>提供知识网络状态查询</li>
 * </ul>
 *
 * <h3>工作流程：</h3>
 * <pre>
 * 1. 应用启动 → 扫描已提取文本
 * 2. 定期任务 → 检查新增/更新的文本
 * 3. 触发构建 → 调用 KnowledgeNetworkBuilder
 * 4. 后台处理 → 构建知识网络
 * 5. 持久化 → 存储到对应知识域
 * </pre>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class KnowledgeNetworkManager {

    @Autowired
    private DocumentStorageService documentStorage;

    @Autowired
    private KnowledgeDomainService domainService;

    @Autowired
    private KnowledgeNetworkBuilder networkBuilder;

    /**
     * 已处理的文档记录
     * key: documentId, value: 处理时间戳
     */
    private final ConcurrentHashMap<String, Long> processedDocuments = new ConcurrentHashMap<>();

    /**
     * 待处理队列
     */
    private final List<PendingBuildTask> pendingTasks = new ArrayList<>();

    /**
     * 启用知识网络构建（默认启用）
     */
    private boolean enabled = true;

    /**
     * 应用启动后执行初始化
     */
    @EventListener(ApplicationReadyEvent.class)
    public void onApplicationReady() {
        if (!enabled) {
            log.info("⏸️ 知识网络构建已禁用");
            return;
        }

        log.info("🚀 知识网络管理器启动...");

        // 异步扫描已提取文本
        CompletableFuture.runAsync(this::scanAndBuildKnowledgeNetwork);
    }

    /**
     * 扫描已提取文本并构建知识网络
     */
    public void scanAndBuildKnowledgeNetwork() {
        log.info("🔍 开始扫描已提取文本...");

        try {
            // 获取所有已提取文本的文档列表
            List<String> documentIds = documentStorage.listExtractedDocuments();

            if (documentIds.isEmpty()) {
                log.info("📭 未发现已提取文本，跳过知识网络构建");
                return;
            }

            log.info("📚 发现 {} 个已提取文本文档", documentIds.size());

            // 获取默认知识域
            String defaultDomainId = getDefaultDomainId();

            // 过滤出未处理的文档
            List<String> unprocessedDocs = documentIds.stream()
                    .filter(docId -> !processedDocuments.containsKey(docId))
                    .toList();

            if (unprocessedDocs.isEmpty()) {
                log.info("✅ 所有文档已处理完成");
                return;
            }

            log.info("🔨 准备为 {} 个文档构建知识网络", unprocessedDocs.size());

            // 批量构建知识网络
            buildKnowledgeNetworkForDocuments(unprocessedDocs, defaultDomainId);

        } catch (Exception e) {
            log.error("❌ 扫描已提取文本失败", e);
        }
    }

    /**
     * 为文档列表构建知识网络
     */
    private void buildKnowledgeNetworkForDocuments(List<String> documentIds, String domainId) {
        AtomicInteger successCount = new AtomicInteger(0);
        AtomicInteger failureCount = new AtomicInteger(0);

        List<CompletableFuture<Boolean>> futures = networkBuilder.batchBuildKnowledgeNetwork(
                documentIds,
                domainId
        );

        // 等待所有任务完成
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
                .thenRun(() -> {
                    // 统计结果
                    futures.forEach(future -> {
                        try {
                            if (future.get()) {
                                successCount.incrementAndGet();
                            } else {
                                failureCount.incrementAndGet();
                            }
                        } catch (Exception e) {
                            failureCount.incrementAndGet();
                        }
                    });

                    log.info("✅ 知识网络构建完成：成功 {}, 失败 {}",
                            successCount.get(), failureCount.get());

                    // 记录已处理文档
                    documentIds.forEach(docId ->
                            processedDocuments.put(docId, System.currentTimeMillis()));
                });
    }

    /**
     * 定期检查新增的提取文本（每5分钟）
     */
    @Scheduled(fixedDelay = 300000, initialDelay = 60000)
    public void periodicCheckNewExtractedTexts() {
        if (!enabled) {
            return;
        }

        log.debug("🔍 定期检查新增提取文本...");

        try {
            List<String> allDocumentIds = documentStorage.listExtractedDocuments();

            // 找出新增的文档
            List<String> newDocuments = allDocumentIds.stream()
                    .filter(docId -> !processedDocuments.containsKey(docId))
                    .toList();

            if (!newDocuments.isEmpty()) {
                log.info("📄 发现 {} 个新增文档，开始构建知识网络", newDocuments.size());
                String defaultDomainId = getDefaultDomainId();
                buildKnowledgeNetworkForDocuments(newDocuments, defaultDomainId);
            }

        } catch (Exception e) {
            log.error("❌ 定期检查失败", e);
        }
    }

    /**
     * 手动触发知识网络构建
     *
     * @param documentId 文档ID
     * @param domainId 目标知识域ID
     * @return 构建任务
     */
    public CompletableFuture<Boolean> triggerBuild(String documentId, String domainId) {
        log.info("🎯 手动触发知识网络构建：文档 {} → 域 {}", documentId, domainId);

        return networkBuilder.buildKnowledgeNetworkAsync(documentId, domainId)
                .thenApply(success -> {
                    if (success) {
                        processedDocuments.put(documentId, System.currentTimeMillis());
                    }
                    return success;
                });
    }

    /**
     * 获取默认知识域ID
     */
    private String getDefaultDomainId() {
        // 尝试获取默认文档域
        List<KnowledgeDomain> domains = domainService.listDomains();

        return domains.stream()
                .filter(d -> "DOCUMENT".equals(d.getDomainType()))
                .findFirst()
                .map(KnowledgeDomain::getDomainId)
                .orElse("default-domain");
    }

    /**
     * 启用/禁用知识网络构建
     */
    @Override
    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
        log.info("🔧 知识网络构建已{}", enabled ? "启用" : "禁用");
    }

    /**
     * 获取知识网络统计信息
     */
    @Override
    public KnowledgeNetworkStatistics getStatistics() {
        return KnowledgeNetworkStatistics.builder()
                .processedDocuments(processedDocuments.size())
                .pendingTasks(pendingTasks.size())
                .enabled(enabled)
                .build();
    }

    /**
     * 待处理任务
     */
    private record PendingBuildTask(String documentId, String domainId, long timestamp) {}
}

