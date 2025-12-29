package top.yumbo.ai.omni.rag;

/**
 * RAG 服务工厂
 *
 * <p>负责创建和管理不同域的 RAG 服务实例</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface RagServiceFactory {

    /**
     * 获取或创建域的 RAG 服务
     *
     * @param domainId 域ID
     * @return RAG 服务实例
     */
    RagService getOrCreateRagService(String domainId);

    /**
     * 检查域是否已有 RAG 服务
     *
     * @param domainId 域ID
     * @return 是否存在
     */
    boolean hasRagService(String domainId);

    /**
     * 移除域的 RAG 服务
     *
     * @param domainId 域ID
     */
    void removeRagService(String domainId);
}

