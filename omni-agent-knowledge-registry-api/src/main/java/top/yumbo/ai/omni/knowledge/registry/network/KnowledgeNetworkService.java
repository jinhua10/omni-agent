package top.yumbo.ai.omni.knowledge.registry.network;

import java.util.List;
import java.util.concurrent.CompletableFuture;

/**
 * 知识网络服务接口
 *
 * <p>知识网络是在知识域基础上构建的增强层，负责：</p>
 * <ul>
 *     <li>从已提取的文本构建知识网络</li>
 *     <li>提取知识点、概念、关系</li>
 *     <li>跨域知识关联</li>
 *     <li>知识图谱构建</li>
 * </ul>
 *
 * <h3>设计理念：</h3>
 * <p>知识网络是独立运行的后台服务，不影响原有的文档处理流程。</p>
 * <p>它基于已提取的文本（extracted text）进行知识构建，支持增量更新。</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface KnowledgeNetworkService {

    /**
     * 为指定文档构建知识网络（异步）
     *
     * @param documentId 文档ID
     * @param domainId   目标知识域ID
     * @return 构建任务（异步）
     */
    CompletableFuture<KnowledgeBuildResult> buildKnowledgeNetworkAsync(String documentId, String domainId);

    /**
     * 批量构建知识网络
     *
     * @param documentIds 文档ID列表
     * @param domainId    目标知识域ID
     * @return 构建任务列表
     */
    List<CompletableFuture<KnowledgeBuildResult>> batchBuildKnowledgeNetwork(
            List<String> documentIds,
            String domainId
    );

    /**
     * 扫描已提取文本并构建知识网络
     * <p>用于初始化或手动触发全量构建</p>
     */
    void scanAndBuildKnowledgeNetwork();

    /**
     * 手动触发知识网络构建
     *
     * @param documentId 文档ID
     * @param domainId   目标知识域ID
     * @return 构建任务
     */
    CompletableFuture<KnowledgeBuildResult> triggerBuild(String documentId, String domainId);

    /**
     * 获取文档的构建状态
     *
     * @param documentId 文档ID
     * @return 构建状态
     */
    KnowledgeBuildStatus getBuildStatus(String documentId);

    /**
     * 获取知识网络统计信息
     *
     * @return 统计信息
     */
    KnowledgeNetworkStatistics getStatistics();

    /**
     * 启用/禁用知识网络构建
     *
     * @param enabled true=启用, false=禁用
     */
    void setEnabled(boolean enabled);

    /**
     * 清理构建状态
     *
     * @param documentId 文档ID
     */
    void clearBuildStatus(String documentId);
}


